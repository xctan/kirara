//! Reference: https://llvm.org/docs/AliasAnalysis.html
//! See also: llvm-project/llvm/lib/Analysis/BasicAliasAnalysis.cpp

use crate::ctype::{BinaryOpType, Type};

use super::{
    structure::TransUnit,
    value::{
        ConstantValue, InstructionValue, ValueId, ValueTrait,
        ValueType,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasResult {
    NoAlias,
    MayAlias,
    // PartialAlias, offset maybe?
    MustAlias,
}

struct AAQueryInfo {
    alias_cache: Vec<((AACacheLoc, AACacheLoc), CacheEntry)>,
    depth: u32,
    // num_assumption_uses: i32,
    maybe_cross_iteration: bool,
}

impl AAQueryInfo {
    fn new() -> Self {
        Self {
            alias_cache: Vec::new(),
            depth: 0,
            maybe_cross_iteration: false,
        }
    }
}

#[derive(PartialEq, Eq)]
struct AACacheLoc {
    id: ValueId,
    size: usize,
    maybe_cross_iteration: bool,
}

#[derive(Clone, Copy)]
struct CacheEntry {
    result: AliasResult,
    // num_assumption_uses: i32,
}

// impl CacheEntry {
//     fn is_definitive(&self) -> bool {
//         self.num_assumption_uses < 0
//     }
// }

#[derive(Clone)]
struct DecomposedGEP {
    base: ValueId,
    offset: isize,
    var_indices: Vec<(ValueId, isize)>,
}

impl TransUnit {
    pub fn alias(&self, v1: ValueId, v1size: usize, v2: ValueId, v2size: usize) -> AliasResult {
        let mut aaqi = AAQueryInfo::new();
        self.alias2(v1, v1size, v2, v2size, &mut aaqi)
    }

    fn alias2(
        &self,
        v1: ValueId,
        v1size: usize,
        v2: ValueId,
        v2size: usize,
        aaqi: &mut AAQueryInfo
    ) -> AliasResult {
        aaqi.depth += 1;
        // currently only abridged version of basic-aa
        let result = self.alias_check(v1, v1size, v2, v2size, aaqi);
        aaqi.depth -= 1;
        result
    }

    fn alias_check(
        &self,
        v1: ValueId,
        v1size: usize,
        v2: ValueId,
        v2size: usize,
        aaqi: &mut AAQueryInfo,
    ) -> AliasResult {
        if v1size == 0 || v2size == 0 {
            return AliasResult::NoAlias;
        }

        // explicit cast is unimplemented!

        if self.values[v1].value.is_undef() || self.values[v2].value.is_undef() {
            return AliasResult::NoAlias;
        }

        if self.is_value_equal_in_potential_cycles(v1, v2, aaqi) {
            return AliasResult::MustAlias;
        }

        if !self.values[v1].ty().is_ptr() || !self.values[v2].ty().is_ptr() {
            return AliasResult::NoAlias;
        }

        let o1 = self.get_underlying_object(v1);
        let o2 = self.get_underlying_object(v2);

        // null pointers???

        if o1 != o2 {
            if self.is_identified_object(o1) && self.is_identified_object(o2) {
                return AliasResult::NoAlias;
            }

            let o1val = &self.values[o1];
            let o2val = &self.values[o2];
            if o1val.value.is_constant()
                && self.is_identified_object(o2)
                && !o2val.value.is_constant()
                || o2val.value.is_constant()
                    && self.is_identified_object(o1)
                    && !o1val.value.is_constant()
            {
                return AliasResult::NoAlias;
            }

            if o1val.value.is_argument() && self.is_identified_function_local(o2)
                || o2val.value.is_argument() && self.is_identified_function_local(o1)
            {
                return AliasResult::NoAlias;
            }
        }

        // size check

        if aaqi.depth >= 512 {
            return AliasResult::MayAlias;
        }

        let mut locs = (
            AACacheLoc {
                id: v1,
                size: v1size,
                maybe_cross_iteration: aaqi.maybe_cross_iteration,
            },
            AACacheLoc {
                id: v2,
                size: v2size,
                maybe_cross_iteration: aaqi.maybe_cross_iteration,
            },
        );
        let swapped = locs.0.id > locs.1.id;
        if swapped {
            std::mem::swap(&mut locs.0, &mut locs.1);
        }
        if let Some((_, v)) = aaqi
            .alias_cache
            .iter()
            .find(|(k, _)| k.0 == locs.0 && k.1 == locs.1)
        {
            return v.result;
        }
        let idx = aaqi.alias_cache.len();
        aaqi.alias_cache.push((
            locs,
            CacheEntry {
                result: AliasResult::MayAlias,
            },
        ));

        let result = self.alias_check_recursive(v1, v1size, v2, v2size, aaqi, o1, o2);
        aaqi.alias_cache[idx].1.result = result;

        return result;
    }

    fn is_value_equal_in_potential_cycles(
        &self,
        v1: ValueId,
        v2: ValueId,
        aaqi: &mut AAQueryInfo,
    ) -> bool {
        if v1 != v2 {
            return false;
        }

        if !aaqi.maybe_cross_iteration {
            return true;
        }

        let inst = &self.values[v1];
        if inst.value.is_inst() {
            let parent = self.inst_bb[&v1];
            let parent = &self.blocks[parent];
            if parent.is_entry {
                return true;
            }
        }

        let parent = self.inst_bb[&v1];
        let mut succ = self.succ(parent);
        !succ.is_empty() && !super::cfg::is_potentially_reachable_from_many(self, &mut succ, parent)
    }

    fn get_underlying_object(&self, mut v: ValueId) -> ValueId {
        if !self.values[v].ty().is_ptr() {
            return v;
        }

        loop {
            let value = self.values[v].value.as_inst();
            if let InstructionValue::GetElemPtr(gep) = value {
                v = gep.ptr;
            } else {
                if let InstructionValue::Phi(phi) = value {
                    // LCSSA
                    if phi.args.len() == 1 {
                        v = phi.args[0].0;
                        continue;
                    }
                }
                break v;
            }
        }
    }

    fn is_identified_object(&self, v: ValueId) -> bool {
        let value = &self.values[v];
        if value.value.is_inst() {
            if let InstructionValue::Alloca(_) = value.value.as_inst() {
                return true;
            }
        }
        if let ValueType::Global(_) = value.value {
            // no global alias
            return true;
        }
        // inter-proc alias not available!
        if self.is_no_alias_or_by_val_argument(v) {
            return true;
        }
        return false;
    }

    fn is_identified_function_local(&self, v: ValueId) -> bool {
        let value = &self.values[v];
        if value.value.is_inst() {
            if let InstructionValue::Alloca(_) = value.value.as_inst() {
                return true;
            }
        }
        // inter-proc alias not available!
        if self.is_no_alias_or_by_val_argument(v) {
            return true;
        }
        return false;
    }

    fn is_no_alias_or_by_val_argument(&self, v: ValueId) -> bool {
        // restrict keyword unimplemented and sysy disallows explicit pointers, so ...
        let value = &self.values[v];
        matches!(value.value, ValueType::Parameter(_))
    }

    fn is_base_of_object(&self, v: ValueId) -> bool {
        let value = &self.values[v];
        if value.value.is_inst() {
            if let InstructionValue::Alloca(_) = value.value.as_inst() {
                return true;
            }
        }
        if let ValueType::Global(_) = value.value {
            // no global alias
            return true;
        }
        return false;
    }

    fn alias_check_recursive(
        &self,
        v1: ValueId,
        v1size: usize,
        v2: ValueId,
        v2size: usize,
        aaqi: &mut AAQueryInfo,
        o1: ValueId,
        o2: ValueId,
    ) -> AliasResult {
        let value1 = &self.values[v1];
        let value2 = &self.values[v2];
        if let InstructionValue::GetElemPtr(_gep) = value1.value.as_inst() {
            let result = self.alias_gep(v1, v1size, v2, v2size, aaqi, o1, o2);
            if result != AliasResult::MayAlias {
                return result;
            }
        } else if let InstructionValue::GetElemPtr(_gep) = value2.value.as_inst() {
            let result = self.alias_gep(v1, v2size, v1, v1size, aaqi, o2, o1);
            if result != AliasResult::MayAlias {
                return result;
            }
        }

        if let InstructionValue::Phi(_phi) = value1.value.as_inst() {
            let result = self.alias_phi(v1, v1size, v2, v2size, aaqi, o1, o2);
            if result != AliasResult::MayAlias {
                return result;
            }
        } else if let InstructionValue::Phi(_phi) = value2.value.as_inst() {
            let result = self.alias_phi(v1, v2size, v1, v1size, aaqi, o2, o1);
            if result != AliasResult::MayAlias {
                return result;
            }
        }

        // skip PartialAlias check

        AliasResult::MayAlias
    }

    fn alias_gep(
        &self,
        v1: ValueId,
        v1size: usize,
        v2: ValueId,
        v2size: usize,
        aaqi: &mut AAQueryInfo,
        o1: ValueId,
        o2: ValueId,
    ) -> AliasResult {
        if v1size >= 1 << 31 && v2size >= 1 << 31 {
            let value2 = &self.values[v2];
            if let InstructionValue::GetElemPtr(_) = value2.value.as_inst() {
                let base_alias = self.alias2(o1, 1 << 31, o2, 1 << 31, aaqi);
                if base_alias == AliasResult::NoAlias {
                    return AliasResult::NoAlias;
                }
            }
            return AliasResult::MayAlias;
        }

        let mut decomp_gep1 = self.decompose_gep_expression(v1);
        let decomp_gep2 = self.decompose_gep_expression(v2);

        if decomp_gep1.base == v1 && decomp_gep2.base == v2 {
            return AliasResult::MayAlias;
        }

        self.subtract_decomposed_geps(&mut decomp_gep1, &decomp_gep2, aaqi);

        if decomp_gep1.var_indices.is_empty()
            && v2size < 1 << 31
            && decomp_gep1.offset > v2size as isize
            && self.is_base_of_object(v2)
        {
            return AliasResult::NoAlias;
        }
        let value2 = &self.values[v2];
        if value2.value.is_inst() {
            if let InstructionValue::GetElemPtr(_) = value2.value.as_inst() {
                if decomp_gep1.var_indices.is_empty()
                    && v1size < 1 << 31
                    && decomp_gep1.offset < -(v1size as isize)
                    && self.is_base_of_object(v1)
                {
                    return AliasResult::NoAlias;
                }
            }
        }

        if decomp_gep1.offset == 0 && decomp_gep1.var_indices.is_empty() {
            return self.alias2(
                decomp_gep1.base,
                v1size,
                decomp_gep2.base,
                v2size,
                aaqi
            )
        }

        let base_alias = self.alias2(
            decomp_gep1.base,
            1 << 31,
            decomp_gep2.base,
            1 << 31,
            aaqi
        );

        if base_alias != AliasResult::MustAlias {
            return base_alias;
        }

        if decomp_gep1.var_indices.is_empty() {
            // partial alias unimplemented!
            return AliasResult::MayAlias;
        }

        AliasResult::NoAlias
    }

    fn alias_phi(
        &self,
        _v1: ValueId,
        _v1size: usize,
        _v2: ValueId,
        _v2size: usize,
        _aaqi: &mut AAQueryInfo,
        _o1: ValueId,
        _o2: ValueId,
    ) -> AliasResult {
        AliasResult::MayAlias
    }

    fn decompose_gep_expression(&self, mut v: ValueId) -> DecomposedGEP {
        let mut base;
        let mut offset = 0;
        let mut var_indices = vec![];
        loop {
            let value = &self.values[v];
            let gep = if let InstructionValue::GetElemPtr(gep) = value.value.as_inst() {
                gep
            } else {
                if let InstructionValue::Phi(phi) = value.value.as_inst() {
                    // LCSSA
                    if phi.args.len() == 1 {
                        v = phi.args[0].0;
                        continue;
                    }
                }

                base = v;
                return DecomposedGEP {
                    base,
                    offset,
                    var_indices,
                };
            };

            // let mut gep_has_constant_offset = true;
            let mut ty = Type::ptr_to(gep.base_ty.clone());
            for index in &gep.indices {
                // dismiss struct type

                ty = ty.base_type();

                let idx_value = &self.values[*index];
                if idx_value.value.is_constant() {
                    let index = match idx_value.value.as_constant() {
                        ConstantValue::I32(i) => *i as isize,
                        _ => unimplemented!("gep constant index with {:?}", idx_value),
                    };
                    // no alloca support, so no scalable index
                    offset += ty.base_type().size() * index;
                    continue;
                }
                // dismiss scalable index again

                // gep_has_constant_offset = false;

                // ((base * scale) + offset) * type_size
                let (base, mut scale, mut offset2) = self.get_linear_expression(*index, 0);
                scale *= ty.base_type().size();
                offset2 *= ty.base_type().size();
                offset += offset2;

                for i in 0..var_indices.len() {
                    if var_indices[i].0 == base {
                        scale += var_indices[i].1;
                        var_indices.remove(i);
                        break;
                    }
                }

                if scale != 0 {
                    var_indices.push((base, scale));
                }
            }

            v = gep.ptr;
        }

        // not limiting search depth
    }

    // return (base, scale, offset)
    fn get_linear_expression(&self, v: ValueId, depth: u32) -> (ValueId, isize, isize) {
        if depth == 6 {
            return (v, 1, 0);
        }

        let value = &self.values[v];
        if value.value.is_constant() {
            let offset2 = match value.value.as_constant() {
                ConstantValue::I32(i) => *i as isize,
                _ => unimplemented!("gep constant index with {:?}", value),
            };
            return (v, 0, offset2);
        }

        let mut scale = 1;
        let mut offset = 0;
        if let InstructionValue::Binary(bin) = value.value.as_inst() {
            let rhs_value = &self.values[bin.rhs];
            if !rhs_value.value.is_constant() {
                return (v, 1, 0);
            }
            let rhs_const = match rhs_value.value.as_constant() {
                ConstantValue::I32(i) => *i as isize,
                _ => unimplemented!("gep constant index with {:?}", rhs_value),
            };
            match bin.op {
                BinaryOpType::Add => {
                    let (base, scale2, offset2) = self.get_linear_expression(bin.lhs, depth + 1);
                    scale = scale2;
                    offset = offset2 + rhs_const;
                    return (base, scale, offset);
                }
                BinaryOpType::Sub => {
                    let (base, scale2, offset2) = self.get_linear_expression(bin.lhs, depth + 1);
                    scale = scale2;
                    offset = offset2 - rhs_const;
                    return (base, scale, offset);
                }
                BinaryOpType::Mul => {
                    let (base, scale2, offset2) = self.get_linear_expression(bin.lhs, depth + 1);
                    scale = scale2 * rhs_const;
                    offset = offset2 * rhs_const;
                    return (base, scale, offset);
                }
                _ => {}
            }
        }

        (v, scale, offset)
    }

    fn subtract_decomposed_geps(
        &self,
        gep1: &mut DecomposedGEP,
        gep2: &DecomposedGEP,
        aaqi: &mut AAQueryInfo,
    ) {
        gep1.offset -= gep2.offset;
        for (src, src_scale) in &gep2.var_indices {
            let mut found = None;
            for (idx, (dst, dst_scale)) in gep1.var_indices.iter_mut().enumerate() {
                if !self.is_value_equal_in_potential_cycles(*dst, *src, aaqi) {
                    continue;
                }

                // not tracking "no signed wrap"
                *dst_scale -= src_scale;
                found = Some(idx);
                break;
            }
            if let Some(idx) = found {
                if gep1.var_indices[idx].1 == 0 {
                    gep1.var_indices.remove(idx);
                }
            } else {
                gep1.var_indices.push((*src, -*src_scale));
            }
        }
    }
}
