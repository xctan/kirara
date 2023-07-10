use std::fmt::Formatter;
use std::marker::PhantomData;
use std::hash::{Hash, Hasher};
use std::fmt::Debug;
use std::{ops, mem};

#[derive(Clone, Debug)]
pub struct Arena<T> {
    items: Vec<Entry<T>>,
    generation: usize,
    free_list_head: Option<usize>,
    len: usize,
}

#[derive(Clone, Debug)]
enum Entry<T> {
    Occupied { value: T, generation: usize },
    Vacant { next_free: Option<usize> },
}

pub struct Index<T> {
    index: usize,
    generation: usize,
    _ty: PhantomData<T>,
}

pub use Index as Id;

impl<T> Index<T> {
    pub fn from_raw_parts(index: usize, generation: usize) -> Self {
        Self { index, generation, _ty: PhantomData }
    }

    #[allow(unused)]
    pub fn into_raw_parts(self) -> (usize, usize) {
        (self.index, self.generation)
    }
}

impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.generation == other.generation
    }
}

impl<T> PartialOrd for Index<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.index.cmp(&other.index))
    }
}

impl<T> Hash for Index<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        Self { index: self.index, generation: self.generation, _ty: PhantomData }
    }
}

impl<T> Debug for Index<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Index")
            .field("index", &self.index)
            .field("generation", &self.generation)
            .finish()
    }
}

impl<T> Copy for Index<T> {}

impl<T> Eq for Index<T> {}

impl<T> Ord for Index<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Arena<T> {
        Arena::new()
    }
}

impl<T> Arena<T> {
    pub fn new() -> Arena<T> {
        Self::with_capacity(4)
    }

    pub fn with_capacity(capacity: usize) -> Arena<T> {
        Arena {
            items: Vec::with_capacity(capacity),
            generation: 0,
            free_list_head: None,
            len: 0,
        }
    }

    #[allow(unused)]
    pub fn clear(&mut self) {
        self.items.clear();
        
        let end = self.items.capacity();
        self.items.extend((0..end).map(|i|
            if i == end - 1 { Entry::Vacant { next_free: None } }
            else { Entry::Vacant { next_free: Some(i + 1) } }
        ));

        self.generation += 1;
        self.free_list_head = Some(0);
        self.len = 0;
    }

    pub fn alloc(&mut self, value: T) -> Index<T> {
        self
            .alloc_fast(value)
            .unwrap_or_else(|value| self.alloc_slow(value))
    }

    #[inline]
    fn alloc_fast(&mut self, value: T) -> Result<Index<T>, T> {
        match self.alloc_next_index_fast() {
            Some(index) => {
                self.items[index.index] = Entry::Occupied { value, generation: self.generation };
                Ok(index)
            }
            None => Err(value),
        }
    }

    #[inline]
    fn alloc_next_index_fast(&mut self) -> Option<Index<T>> {
        match self.free_list_head {
            None => None,
            Some(index) => {
                match self.items[index] {
                    Entry::Occupied { .. } => panic!("currupted free list"),
                    Entry::Vacant { next_free } => {
                        self.free_list_head = next_free;
                        self.len += 1;
                        Some(Index::from_raw_parts(index, self.generation))
                    }
                }
            }
        }
    }

    #[inline(never)]
    fn alloc_slow(&mut self, value: T) -> Index<T> {
        let additional = if self.capacity() == 0 {
            1
        } else {
            self.items.len()
        };
        self.reserve(additional);
        self.alloc_fast(value).map_err(|_| ()).expect("failed to allocate")
    }

    pub fn capacity(&self) -> usize {
        self.items.len()
    }

    pub fn reserve(&mut self, additional: usize) {
        let start = self.items.len();
        let end = start + additional;
        let old_head = self.free_list_head;
        self.items.reserve_exact(additional);
        self.items.extend((start..end).map(|i|
            if i == end - 1 { Entry::Vacant { next_free: old_head } }
            else { Entry::Vacant { next_free: Some(i + 1) } }
        ));
        self.free_list_head = Some(start);
    }

    pub fn get(&self, i: Index<T>) -> Option<&T> {
        match self.items.get(i.index) {
            Some(Entry::Occupied { value, generation }) if *generation == i.generation => Some(value),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, i: Index<T>) -> Option<&mut T> {
        match self.items.get_mut(i.index) {
            Some(Entry::Occupied { value, generation }) if *generation == i.generation => Some(value),
            _ => None,
        }
    }

    pub fn remove(&mut self, i: Index<T>) -> Option<T> {
        if i.index >= self.items.len() {
            return None;
        }

        match self.items[i.index] {
            Entry::Occupied { generation, .. } if generation == i.generation => {
                let entry = mem::replace(
                    &mut self.items[i.index],
                    Entry::Vacant { next_free: self.free_list_head },
                );
                self.free_list_head = Some(i.index);
                self.generation += 1;
                self.len -= 1;

                match entry {
                    Entry::Occupied { value, .. } => Some(value),
                    _ => unreachable!(),
                }
            }
            _ => None,
        }
    }
}

impl<T> ops::Index<Index<T>> for Arena<T> {
    type Output = T;

    fn index(&self, i: Index<T>) -> &Self::Output {
        self.get(i).expect("invalid index")
    }
}

impl<T> ops::IndexMut<Index<T>> for Arena<T> {
    fn index_mut(&mut self, i: Index<T>) -> &mut Self::Output {
        self.get_mut(i).expect("invalid index")
    }
}