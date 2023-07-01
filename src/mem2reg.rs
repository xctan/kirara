fn mem2reg(unit: &mut TransUnit){
    for func in unit.funcs.values(){
        let mut ord: Vec<BasicBlock> = Vec::new();
        let mut father: HashMap<BlockId,BlockId> = HashMap::new();
        let mut entry_id: BlockId = func.entry_id;
        ord.push(entry_id);
        dfs(&mut TransUnit,&mut entry_id,&mut ord,&mut father);

        let mut Sdom: HashMap<BlockId,BlockId> = HashMap::new();
        let mut Uni: HashMap<BlockId,BlockId> = HashMap::new();
        let mut Mn: HashMap<BlockId,BlockId> = HashMap::new();
        let mut Idom: HashMap<BlockId,BlockId> = HashMap::new();
        
        Lengauer_Tarjan(&mut unit,&mut ord,&mut Sdom,&mut Uni,&mut Mn,&mut Idom,father);

        CalcIdom(&mut unit,&mut ord,&mut Idom);

        let mut DF: HashMap<BlockId,Vec<BlockId>> = HashMap::new();
        let mut son: HashMap<BlockId,Vec<BlockId>> = HashMap::new();
        getDF(&mut unit,&mut ord,&mut Idom,&mut DF,Edges,son);

        let mut valueIds: Vec<valueId> = Vec::new();
        let mut Defs: HashMap<valueId,Vec<BlockId>> = HashMap::new();
        getDefs(&mut unit,&mut ord,&mut Idom,&mut valueIds,&mut Defs);
        for valueId in valueIds{
            InsertPhi(&mut unit,&mut ord,&mut DF,&mut valueId,&mut Defs[valueId]);
        }
        modifyValue(&mut unit,&mut ord,&mut valuleIds,son);
    }
}
fn dfs(unit: &mut TransUnit,current_id: BlockId,
    ord:Vec<BasicBlock>,father: HashMap<BlockId,BlockId>){
    let mut nextIds: Vec<BlockId> = Vec::new();
    get_next(&mut unit,&mut current_id,&mut nextIds);
    for nextId in nextIds{
        if ord.contains(nextId){
            continue;
        }
        ord.push(nextId);
        father.insert(nextId,current_id);
        dfs(unit,nextId,ord,father);
    }
}

fn Lengauer_Tarjan(unit: &mut TransUnit,ord:Vec<BasicBlock>,Sdom: HashMap<BlockId,BlockId>,
    Uni: HashMap<BlockId,BlockId>,Mn: HashMap<BlockId,BlockId>,Idom: HashMap<BlockId,BlockId>,
    father:HashMap<BlockId,BlockId>>){
    let mut SdomSet:HashMap(BlockId,Vec<BlockId>) = HashMap::new();
    //对Sdom,Uni,Mn,SdomSet进行初始化
    for i in ord{
        Sdom.insert(i,i);
        Uni.insert(i,i);
        Mn.insert(i,i);
        SdomSet.insert(i,Vec::new());
    }
    //按dfs树的逆序遍历所有节点
    for i in ord.iter().skip(1).rev(){
        for j in i.preds{
            uni_query(j,Sdom,Uni,Mn);
        }
        if Dfn(ord,Sdom[i])>Dfn(ord,Sdom[Mn[j]]){
            Sdom[i] = Sdom[Mn[j]];
        }
        Uni[i] = father[i];

        SdomSet[Sdom[i]].push(i);

        for j in SdomSet[father[i]]{
            uni_query(j);
            if Dfn(ord,Sdom[Mn[j]]) < Dfn(ord,father[i]){
                Idom[j] = Mn[j];
            }
            else{
                Idom[j] = father[i];
            }
        }

        SdomSet[father[i]].clear();
    }
}

fn CalcIdom(ord:Vec<BasicBlock>,Sdom: HashMap<BlockId,BlockId>,Idom: HashMap<BlockId,
    BlockId>){
    for i in ord.iter().skip(1){
        if Sdom[i] == Idom[i]{
            ;
        }
        else{
            Idom[i] = Idom[Idom[i]];
        }
    }
}

fn getDF(unit: &mut TransUnit,ord: Vec<BasicBlock>,Idom: HashMap<BlockId,BlockId>,
    DF: HashMap<BlockId,Vec<BlockId>>,Edges: Vec<(BlockId,BlockId)>){
    //将支配边界初始化为空
        for i in ord{
        DF.insert(i,Vec::new());
    }

    //初始化流向边集合
    for i in ord{
        let mut back = unit.blocks[i];
        for j in back.preds{
            Edges.push(i,j);
        }
    }

    /*DFS遍历支配者树，维护每个节点i的开始时间戳first[i]和结束时间戳second[i]，以方便地确定一个节点
    A是否是另一个节点B的直接支配者，也即一个节点A是否是另一个节点B在支配树上的祖先，如果
    first[A]<first[B]<second[A],则A是B的祖先，否则就不是*/

    //初始化son
    for i in ord{
        son.insert(i,Vec::new());
    }

    //根据每个节点的直接支配者Idom来建立son
    for (key,value) in Idom.iter(){
        son[value].push(key);
    }

    //利用son，DFS遍历支配树，并记录每个节点的开始时间戳first和结束时间戳second
    //建立first时间戳
    let mut first: HashMap<BlockId,i32> = HashMap::new();
    let mut second: HashMap<BlockId,i32> = HashMap::new();
    let mut count:i32 = 0;
    getDF_dfs(ord[0],first,second,&mut count,son);

    /*根据建立DF的算法，遍历每条边a->b，如果a不严格支配b，那么就把b加入a的支配边界集合DF，并将a
    转向a的父亲，重复此操作，直到a严格支配b*/

    for &(first,second) in Edges.iter(){
        let mut x = first;
        while !(first[A]<first[B]<second[A]){
            DF[A].push(B);
            A = Idom[A];
        }
    }
}

//利用son，DFS遍历支配树，并记录每个节点的开始时间戳first和结束时间戳second
fn getDF_dfs(current: BlockId,first: HashMap<BlockId,i32>,second: HashMap<BlockId,i32>,
    count: &mut i32,son:HashMap<BlockId,Vec<BlockId>>){
    
    //开始时记录时间戳
    first.insert(current,count);
    count = count + 1;

    //依次遍历所有子节点
    for i in son[current]{
        getDF_dfs(i);
    }

    //结束时记录时间戳
    second.insert(current,count);
    count = count + 1;
}

/*getDefs需要找到所有基本块中使用了alloca的valueId并存储在valueIds中，并且对于每个valueId,
需要把对其store过的所有基本块都存储在Defs中*/
fn getDefs(unit: &mut TransUnit,ord:Vec<BasicBlock> ,Idom: HashMap<BlockId,BlockId>,valueIds:Vec<valueId>,Defs:HashMap<valueId,Vec<BlockId>>){
    //遍历所有基本块的所有指令，找到alloca指令，并把对应的valueId记录在valueIds中
    // 遍历基本块id
    for i in ord.iter(){
        //找到对应的BasicBlock
        let mut block = unit.blocks[i];

        //获取第一个指令
        insts = block.insts_start;

        //从第一个指令开始，遍历所有指令,并找出所有alloca指令，记录变量的valueId
        while let Some(inst) = current_valueId{
            let inst = arena.get(inst).unwrap;
            insts = inst.next;
            match inst.value{
                ValueType::Instruction(ref insn) =>{
                    match *insn {
                        InstructionValue::Alloca(ref insn) => {
                            if !velueIds.contains(insn.name){
                                valueIds.push(insn.name);
                            }
                        }
                    }
                }
            }
        }
    }
    //初始化Defs
    for i in valueIds.iter(){
        Defs.insert(i,Vec.new());
    }

    //遍历所有基本块的所有指令，找到store指令，并把基本块id和valueId的对应关系记录在Defs中
    for i in ord.iter(){
        //找到对应的BasicBlock
        let mut block = unit.blocks[i];

        //获取第一个指令
        insts = block.insts_start;

        //从第一个指令开始，遍历所有指令
        while let Some(inst) = current_valueId{
            let inst = arena.get(inst).unwrap;
            insts = inst.next;
            match inst.value{
                ValueType::Instruction(ref insn) =>{
                    match *insn {
                        InstructionValue::Store(ref insn) => {
                            if ! Defs[insn.ptr].contains(i){
                                Defs[insn.ptr].push(i);
                            }
                        }
                    }
                }
            }
        }
    }
}
//关于某一变量valueId,选择适当的基本块，在这些基本块开头插入空的Phi节点
//基本块的选择方法是：以Def为基础集合，DF为每个节点的支配边界，计算出Def的迭代支配边界。
//在迭代支配边界中的基本块需要插入Phi结点
fn InsertPhi(unit: &mut TransUnit,ord: Vec<BasicBlock>,DF: HashMap<BlockId,Vec<BlockId>>,valueId: valueId,Def: Vec<BlockId>){
    //定义并初始化迭代支配边界集合
    let mut DF_iter: Vec<BlockId> = Vec::new();

    //定义并初始化迭代支配边界集合的增量集合
    let mut DF_iter_add: Vec<BlockId> = Vec::new();
    for i in Def{
        DF_iter_add.push(i);
    }

    //记录已经计算过迭代边界的集合
    let mut DF_iter_calced: Vec<BlockId> = Vec::new();

    //迭代计算迭代支配边界
    while !DF_iter_add.is_empty(){
        let mut x = DF_iter_add.pop();
        DF_iter_calced.push(x);
        for i in DF[x].iter(){
            if !DF_iter.contains(i){
                DF_iter.push(i);
            }
            if !DF_iter_calced.contains(i) && !DF_iter_add.contains(i) {
                DF_iter_add.push(i);
            }
            //暂时地在i所对应基本块的开头插入一条phi指令，其中目标变量为valueId以暂时标识其所属的指针变量
            insertAPhi(unit,i,valueId);
        }
    }
}
/*按DFS方法遍历支配树，每经过一个基本块就遍历其中的所有指令，如果是alloca指令，直接删除，
对于Phi指令，需要创建一个新的变量，将新的变量作为Phi指令的目标变量。并且记录下新的变量是其所对应的指针变量的到达定义，同时记录下该条
Phi指令所对应的指针变量。对于Store指令，将值变量的替换对象为指针变量的到达定义，但要注意此时的值变量本身可能是已被替换了的，所以要先进行变量替换再
进行Store的到达定义记录（保证到达定义就是最终需要替换进去的变量）。对所有非Phi指令，都需要将其中
需要读取的变量先进行替换。对于Load指令，需要将指针变量的到达定义作为目标变量的替换对象。在每个基本块
末尾，都需要访问所有的后向基本块，在这些后向基本块开头的Phi指令中，将本基本块对应指针变量的到达定义
作为Phi指令的一个分支。在改变指针变量到达定义时，要记录下原先的到达定义，在递归函数返回时，恢复原先的
到达定义*/
fn modifyValue(unit:TransUnit,ord: Vec<BasicBlock>,valuleIds: Vec<ValueId>, son: HashMap<BlockId,Vec<BlockId>>){
    //定义并初始化reachingDef,表示每个指针变量的到达定义,或者是普通变量的替换对象
    let mut reachingDef: HashMap<ValueId,ValueId> = HashMap::new();
    for i in valueIds.iter(){
        reachingDef.insert(i,i);
    }

    //定义并初始化phi_ptr，表示每条phi指令初始所对应的指针变量
    let mut phi_ptr: HashMap<ValueId,ValueId> = HashMap::new();

    modifyValue_dfs(unit,ord[0],reachingDef,phi_ptr,son);
}

fn modifyValue_dfs(unit:TransUnit,cur:ValueId,reachingDef: HashMap<ValueId,ValueId>,phi_ptr: HashMap<ValueId,ValueId>,son: HashMap<BlockId,Vec<BlockId>>){
    //初始化rechingDef_return，记录下哪些指针变量的到达定义被改变过，需要在返回时恢复
    let mut reachingDef_return: HashMap<valueId,valueId> = HashMap::new();

    //遍历该基本块的所有指令

    //获取第一个指令
    insts = cur.insts_start;

    //从第一个指令开始，遍历所有指令
    while let Some(inst) = current_valueId{
        let inst = arena.get(inst).unwrap;
        insts = inst.next;
        //首先考虑将读取的变量替换成其替换对象
        change(insts,reachingDef);

        //其次考虑对Phi指令、Store指令、Load、alloca指令分别处理
        match *insn {
            InstructionValue::Load(ref insn) => {
                reachingDef[insn.name] = reachingDef[insn.ptr];
            }
            InstructionValue::Store(ref insn) => {
                if !reachingDef_return.contains_key(insn.ptr){
                    reachingDef_return.insert(insn.ptr,reachingDef[insn.ptr]);
                }
                if reachingDef.contains_key(insn.value){
                    reachingDef[insn.ptr] = reachingDef[insn.value];
                }
                else{
                    reachingDef[insn.ptr] = insn.value;
                }
            }
            InstructionValue::Alloca(ref insn) => {
                delete_value(insn);
            }
            InstructionValue::Phi(ref insn) => {
                //记录下phi指令对应的指针变量
                phi_ptr.insert(current_valueId,insn.name);

                //申请一个新的变量并替换Phi指令的目标变量
                let mut new_id = getNewId();
                insn.name = new_id;

                //更新指针变量的到达定义
                if !reachingDef_return.contains_key(insn.ptr){
                    reachingDef_return.insert(insn.ptr,reachingDef[insn.ptr]);
                }
                reachingDef[phi_ptr[current_valueId]] = new_id;
            }
        }
    }

    for i in son[cur]{
        //找到其所有后继基本块开头的phi指令

        //获取第一个指令
        insts = i.insts_start;

        //从第一个指令开始，遍历所有指令
        while let Some(inst) = current_valueId{
            let inst = arena.get(inst).unwrap;
            insts = inst.next;

            //在所有phi指令中插入一个分支
            match *insn {
                InstructionValue::Phi(ref insn) => {
                    let mut ptr_id: valueId;
                    if phi_ptr.contains_key(current_valueId){
                        ptr_id = phi_ptr[current_valueId];
                    }
                    else{
                        ptr_id = inst.name;
                    }
                    insertABranch(inst,cur,reachingDef[ptr_id]);
                }
                _ =>{
                    break;
                }
            }
        }
    }

    for (key,value) in reachingDef_return.iter(){
        reachingDef[key] = value;
    }
}
