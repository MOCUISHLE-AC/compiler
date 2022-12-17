#include "Ast.h"
#include <stack>
#include <string>
#include "IRBuilder.h"
#include "Instruction.h"
#include "SymbolTable.h"
#include "Type.h"
#include "Unit.h"
extern Unit unit;

#include <iostream>

extern FILE* yyout;
int Node::counter = 0;
IRBuilder* Node::builder;

Node::Node() {
    seq = counter++;
    next = nullptr;
}
//回填技术
void Node::setNext(Node* node) {
    Node* n = this;
    while (n->getNext()) {
        n = n->getNext();
    }
    if (n == this) {
        this->next = node;
    } else {
        n->setNext(node);
    }
}

void Node::backPatch(std::vector<Instruction*>& list, BasicBlock* bb) {
    for (auto& inst : list) {
        if (inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if (inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

std::vector<Instruction*> Node::merge(std::vector<Instruction*>& list1,
                                      std::vector<Instruction*>& list2) {
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit* unit) {
    IRBuilder* builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void FunctionDef::genCode() {
    Unit* unit = builder->getUnit();
    Function* func = new Function(unit, se);
    BasicBlock* entry = func->getEntry();
    builder->setInsertBB(entry);
    if (decl)
        decl->genCode();
    // function中的stmt节点是用compoundstmt进行初始化的
    if (stmt)
        stmt->genCode();
    /**
     * Construct control flow graph. You need do set successors and predecessors
     * for each basic block. 
     * Todo
     */
    for (auto block = func->begin(); block != func->end(); block++) {
        //获取该块的最后一条指令
        Instruction* index = (*block)->begin();
        Instruction* last = (*block)->rbegin();
        //去除基础块中，不是last的跳转语句
        while (index != last) 
        {
            if (index->isCond() || index->isUncond()) 
            {
                //去除
                (*block)->remove(index);
            }
            index = index->getNext();
        }
        //last不是return，void类型
        if (!last->isRet()) {
            if (((FunctionType*)(se->getType()))->getRetType() ==
                TypeSystem::voidType) {
                new RetInstruction(nullptr, *block);
            }
        }
        //条件跳转return 和 if配合使用
        if (last->isCond()) {
            BasicBlock *truebranch, *falsebranch;
            //ture false 分支
            truebranch =dynamic_cast<CondBrInstruction*>(last)->getTrueBranch();
            falsebranch =dynamic_cast<CondBrInstruction*>(last)->getFalseBranch();
            if (truebranch->empty()) 
            {
                //函数返回指令
                new RetInstruction(nullptr, truebranch);

            } 
            else if (falsebranch->empty()) 
            {
                //函数返回指令
                new RetInstruction(nullptr, falsebranch);
            }
            //设定流图中的前向边和后向边
            (*block)->addSucc(truebranch);
            (*block)->addSucc(falsebranch);
            truebranch->addPred(*block);
            falsebranch->addPred(*block);
        } 
        //return 是无条件跳转
        else if (last->isUncond())
        {
            BasicBlock* dst =dynamic_cast<UncondBrInstruction*>(last)->getBranch();
            //前向边和后向边
            (*block)->addSucc(dst);
            dst->addPred(*block);
            if (dst->empty()) 
            {
                //返回值int
                if (((FunctionType*)(se->getType()))->getRetType() ==TypeSystem::intType)
                {
                    new RetInstruction(new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)),dst);
                } 
                //返回值void
                else if (((FunctionType*)(se->getType()))->getRetType() ==TypeSystem::floatType)
                {
                    new RetInstruction(new Operand(new ConstantSymbolEntry(TypeSystem::floatType, 0)),dst);
                } 
                //void
                else if (((FunctionType*)(se->getType()))->getRetType() ==TypeSystem::voidType)
                {
                    new RetInstruction(nullptr, dst);
                } 
            }
        }
    }
}

BinaryExpr::BinaryExpr(SymbolEntry* se,
                       int op,
                       ExprNode* expr1,
                       ExprNode* expr2)
    : ExprNode(se), op(op), expr1(expr1), expr2(expr2) {
    dst = new Operand(se);
    std::string op_str;
    //找出运算符号
    switch (op) {
        case ADD:
            op_str = "+";
            break;
        case SUB:
            op_str = "-";
            break;
        case MUL:
            op_str = "*";
            break;
        case DIV:
            op_str = "/";
            break;
        case MOD:
            op_str = "%";
            break;
        case AND:
            op_str = "&&";
            break;
        case OR:
            op_str = "||";
            break;
        case LESS:
            op_str = "<";
            break;
        case LESSEQUAL:
            op_str = "<=";
            break;
        case GREATER:
            op_str = ">";
            break;
        case GREATEREQUAL:
            op_str = ">=";
            break;
        case EQUAL:
            op_str = "==";
            break;
        case NOTEQUAL:
            op_str = "!=";
            break;
    }
    //出现操作数类型出现void
    if (expr1->getType()->isVoid() || expr2->getType()->isVoid()) 
    {
        fprintf(stderr,"BinaryExpr %s have void_type erro\n",op_str.c_str());
    }
    //将BinaryExpr中的类型确定
    if (op >= BinaryExpr::AND && op <= BinaryExpr::NOTEQUAL) 
    {
        type = TypeSystem::boolType;
        //将BinaryExpr中的类型确定
        //实验指导书上没说，但是测试样例中有
        if (op == BinaryExpr::AND || op == BinaryExpr::OR) 
        {
            if (expr1->getType()->isInt() && expr1->getType()->getSize() == 32) 
            {
                //隐式转换int-》bool
                ImplictCastExpr* temp = new ImplictCastExpr(expr1);
                this->expr1 = temp;
            }
            if (expr2->getType()->isInt() && expr2->getType()->getSize() == 32) 
            {
                ImplictCastExpr* temp = new ImplictCastExpr(expr2);
                this->expr2 = temp;
            }
        }
    } 
    else 
    {
        //应该不会进入这里，错误处理
        if(expr1->getType()->isFloat()||expr2->getType()->isFloat())
        {
            //float
            type = TypeSystem::floatType;
        }
        else
        {
            //int
            type = TypeSystem::intType;
        }
        type = TypeSystem::intType;
    }
};

void BinaryExpr::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    if (op == AND) 
    {
        //trueBB第二个子表达式生成的指令需要插入的位置
        BasicBlock* trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
        expr1->genCode();
        backPatch(expr1->trueList(), trueBB);
        builder->setInsertBB(trueBB);  // set the insert point to the trueBB so that intructions
        expr2->genCode();               // generated by expr2 will be inserted into it.

        //因为当前仍不能确定子表达式二的 true_list 的目的基本块，因此我们将其插入到当前结点的 true_list 
        true_list = expr2->trueList();
        //也不能知道两个子表达式的 false_list 的跳转基本块，便只能将其插入到当前结点的 false_list中，
        //让父结点回填当前结点的 true_list 和 false_list。
        false_list = merge(expr1->falseList(), expr2->falseList());
    } else if (op == OR) {
        // Todo
        BasicBlock* falseBB = new BasicBlock(func);
        expr1->genCode();
        //只有当expr1错误的时候，考虑expr2,回填falselist
        backPatch(expr1->falseList(), falseBB);
        //expr2入口
        builder->setInsertBB(falseBB);
        expr2->genCode();
        //expr1 || expr2
        true_list = merge(expr1->trueList(), expr2->trueList());
        // ！expr1 && ！expr2, 由于expr1错了，才考虑expr2
        false_list = expr2->falseList();
    } 
    else if (op >= LESS && op <= NOTEQUAL) {
        // Todo
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        //类型转转换bool->int
        if (src1->getType()==TypeSystem::boolType &&src1->getType()->getSize() == 1) {
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            //扩展指令
            new ZextInstruction(dst, src1, bb);
            src1 = dst;
        }
        // float 大小判断
        else if (src1->getType()==TypeSystem::floatType)
        {
            //目的操作数
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel()));
            src1 = dst;
        }
        if (src2->getType()==TypeSystem::boolType && src2->getType()->getSize() == 1) {
            Operand* dst = new Operand(new TemporarySymbolEntry(
                TypeSystem::intType, SymbolTable::getLabel()));
            //扩展指令
            new ZextInstruction(dst, src2, bb);
            src2 = dst;
        }
         // float 大小判断
        else if (src2->getType()==TypeSystem::floatType)
        {
            //目的操作数
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel()));
            src2 = dst;
        }

        int cmparecode;
        switch (op) {
            case LESS:
                cmparecode = CmpInstruction::L;
                break;
            case LESSEQUAL:
                cmparecode = CmpInstruction::LE;
                break;
            case GREATER:
                cmparecode = CmpInstruction::G;
                break;
            case GREATEREQUAL:
                cmparecode = CmpInstruction::GE;
                break;
            case EQUAL:
                cmparecode = CmpInstruction::E;
                break;
            case NOTEQUAL:
                cmparecode = CmpInstruction::NE;
                break;
        }
        //比较指令
        new CmpInstruction(cmparecode, dst, src1, src2, bb);

        BasicBlock *truebb, *falsebb, *tempbb;

        truebb = new BasicBlock(func);
        falsebb = new BasicBlock(func);
        tempbb = new BasicBlock(func);
        //放入true、false 分支
        true_list.push_back(new CondBrInstruction(truebb, tempbb, dst, bb));
        false_list.push_back(new UncondBrInstruction(falsebb, tempbb));
    } 
    else if (op >= ADD && op <= MOD) {
        expr1->genCode();
        expr2->genCode();
        //获取操作数
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode;
        switch (op) {
            case ADD:
                opcode = BinaryInstruction::ADD;
                break;
            case SUB:
                opcode = BinaryInstruction::SUB;
                break;
            case MUL:
                opcode = BinaryInstruction::MUL;
                break;
            case DIV:
                opcode = BinaryInstruction::DIV;
                break;
            case MOD:
                opcode = BinaryInstruction::MOD;
                break;
        }
        //照葫芦画瓢，new BinaryInstruction
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void Constant::genCode() {
    // Todo
}

void Id::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Operand* addr =
        dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    //如果是int或者float则直接生成
    if (type->isInt()||type->isFloat())
    {
        new LoadInstruction(dst, addr, bb);
    }
    else if (type->isArray()) {
        //还没有实现
    }
}

void IfStmt::genCode() {
    Function* func;
    BasicBlock *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    //回填
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    //生成thenStmt 结点中间代码的过程中可能改变指令的插入点，因此更新插入点
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode() {
     //Todo
    Function* func;
    BasicBlock *then_bb, *else_bb, *end_bb /*, *bb*/;
    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    //cond,有条件跳转回填
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), else_bb);


    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    //无条件跳转
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    //无条件跳转
    new UncondBrInstruction(end_bb, else_bb);
    builder->setInsertBB(end_bb);
}

void CompoundStmt::genCode() {
    // Todo
    if (stmt)
        stmt->genCode();
}

void SeqNode::genCode() {
    // Todo
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode() {
    IdentifierSymbolEntry* se =
        dynamic_cast<IdentifierSymbolEntry*>(id->getSymbolEntry());
    if (se->isGlobal()) {
        Operand* addr;
        SymbolEntry* addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        unit.insertGlobal(se);
    } else if (se->isLocal() || se->isParam()) {
        Function* func = builder->getInsertBB()->getParent();
        BasicBlock* entry = func->getEntry();
        Instruction* alloca;
        Operand* addr;
        SymbolEntry* addr_se;
        Type* type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);
                                    // allocate space for local id in function stack.
        entry->insertFront(alloca);  // allocate instructions should be inserted
                                     // into the begin of the entry block.
        Operand* temp;
        if (se->isParam())
            temp = se->getAddr();
        se->setAddr(addr);  // set the addr operand in symbol entry so that
                            // we can use it in subsequent code generation.
                            // can use it in subsequent code generation.
        if (expr) {
            if (expr->isInitValueListExpr()) 
            {
                //数组没实现
            } 
            else 
            {
                BasicBlock* bb = builder->getInsertBB();
                expr->genCode();
                Operand* src = expr->getOperand();
                new StoreInstruction(addr, src, bb);
            }
        }
        //例如 int a，b，c.......
        if (se->isParam()) {
            BasicBlock* bb = builder->getInsertBB();
            new StoreInstruction(addr, temp, bb);
        }
    }
    if (this->getNext()) {
        this->getNext()->genCode();
    }
}

void ReturnStmt::genCode() {
    // Todo
    BasicBlock* bb = builder->getInsertBB();
    Operand* src = nullptr;
    //retValue 不为0时
    if (retValue) 
    {
        //生成
        retValue->genCode();
        //获取操作符
        src = retValue->getOperand();
    }
    new RetInstruction(src, bb);
}
void ExprStmt::genCode() {
    // Todo
    expr->genCode();
}
void ContinueStmt::genCode() {
   // Todo
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    //无条件跳转
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_cond_bb(), bb);
    //continue_next，插入
    BasicBlock* continue_next = new BasicBlock(func);
    builder->setInsertBB(continue_next);
}
void BreakStmt::genCode() {
     // Todo
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    //无条件跳转
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_end_bb(), bb);
    //break_next，插入
    BasicBlock* break_next = new BasicBlock(func);
    builder->setInsertBB(break_next);
}
void WhileStmt::genCode() {
     Function* func;
    BasicBlock *cond_bb, *while_bb, *end_bb, *bb;
    bb = builder->getInsertBB();
    func = builder->getInsertBB()->getParent();
    //有条件跳转
    cond_bb = new BasicBlock(func);
    //while内部语句
    while_bb = new BasicBlock(func);
    //while后面的语句
    end_bb = new BasicBlock(func);

    this->cond_bb = cond_bb;
    this->end_bb = end_bb;
    //首先判断语句
    new UncondBrInstruction(cond_bb, bb);
    builder->setInsertBB(cond_bb);
    cond->genCode();
    //回填true、false
    backPatch(cond->trueList(), while_bb);
    backPatch(cond->falseList(), end_bb);

    builder->setInsertBB(while_bb);
    stmt->genCode();

    while_bb = builder->getInsertBB();
    new UncondBrInstruction(cond_bb, while_bb);
    //end_bb直接插入
    builder->setInsertBB(end_bb);
}
void BlankStmt::genCode() {
    // Todo
}
void InitValueListExpr::genCode() {
    // Todo
}
void CallExpr::genCode() {
    //调用函数expr
    std::vector<Operand*> operands;
    ExprNode* index = param;
    while (index!=nullptr) 
    {
        index->genCode();
        //放入表中
        operands.push_back(index->getOperand());
        //后移
        index = ((ExprNode*)index->getNext());
    }
    BasicBlock* bb = builder->getInsertBB();
    new CallInstruction(dst, symbolEntry, operands, bb);
}
void UnaryExpr::genCode() {
    // Todo
    expr->genCode();
    if (op == NOT) {
        BasicBlock* bb = builder->getInsertBB();
        Operand* src = expr->getOperand();
        //一元，第一个操作数默认为0
        //int -> bool 取反
        if (expr->getType()==TypeSystem::intType &&expr->getType()->getSize() == 32) 
        {
            Operand* temp = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
            new CmpInstruction(CmpInstruction::NE, temp, src,new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)),bb);
            src = temp;
        }
        //异或指令，与0异或
        new XorInstruction(dst, src, bb);
    } 
    //符号
    else if (op == SUB) 
    {
        Operand* src2;
        BasicBlock* bb = builder->getInsertBB();
        Operand* src1 = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
        //bool->int
        //一元，第一个操作数默认为0
        if (expr->getType()==TypeSystem::boolType &&expr->getType()->getSize() == 1) {
            src2 = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            //扩展指令
            new ZextInstruction(src2, expr->getOperand(), bb);
            new BinaryInstruction(BinaryInstruction::SUB, dst, src1, src2, bb);
        } 
         //int
        if(expr->getType()==TypeSystem::intType)
        {
            //一元，第一个操作数默认为0
            Operand* src1=new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
            src2 = expr->getOperand();
            new BinaryInstruction(BinaryInstruction::SUB, dst, src1, src2, bb);
        }
        //float
        if(expr->getType()==TypeSystem::floatType)
        {
            //一元，第一个操作数默认为0
            //不需要做bool->float的转换
            Operand* src1=new Operand(new ConstantSymbolEntry(TypeSystem::floatType, 0));
            src2 = expr->getOperand();
            new BinaryInstruction(BinaryInstruction::SUB, dst, src1, src2, bb);
        }
    }
    //正号加上去反而出错
    else if(op==ADD)
    {
        Operand* src2;
        BasicBlock* bb = builder->getInsertBB();
        //int
        if(expr->getType()==TypeSystem::intType)
        {
            //一元，第一个操作数默认为0
            //不需要做bool->int的转换
            Operand* src1=new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
            src2 = expr->getOperand();
            new BinaryInstruction(BinaryInstruction::ADD, dst, src1, src2, bb);
        }
        //float
        else if(expr->getType()==TypeSystem::floatType)
        {
            //一元，第一个操作数默认为0
            //不需要做bool->float的转换
            Operand* src1=new Operand(new ConstantSymbolEntry(TypeSystem::floatType, 0));
            src2 = expr->getOperand();
            new BinaryInstruction(BinaryInstruction::ADD, dst, src1, src2, bb);
        }
    }
    
}
void ExprNode::genCode() {
    // Todo
}
//隐式转换int-》bool
void ImplictCastExpr::genCode() {
    expr->genCode();
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    BasicBlock* trueBB = new BasicBlock(func);
    BasicBlock* tempbb = new BasicBlock(func);
    BasicBlock* falseBB = new BasicBlock(func);

    new CmpInstruction(CmpInstruction::NE, this->dst, this->expr->getOperand(),new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)), bb);
    this->trueList().push_back(new CondBrInstruction(trueBB, tempbb, this->dst, bb));
    this->falseList().push_back(new UncondBrInstruction(falseBB, tempbb));
}

void ContinueStmt::typeCheck(Type* retType) {
    //Todo
}
void BreakStmt::typeCheck(Type* retType) {
    //Todo
}
void WhileStmt::typeCheck(Type* retType) {
    if (stmt)
        stmt->typeCheck(retType);
    //Todo
}
void BlankStmt::typeCheck(Type* retType) {
    //Todo
}
void InitValueListExpr::typeCheck(Type* retType) {
    //Todo
}
void CallExpr::typeCheck(Type* retType) {
    //Todo
}
void UnaryExpr::typeCheck(Type* retType) {
    //Todo
}

void ExprStmt::typeCheck(Type* retType) {
    //Todo
}
//赋值语句
void AssignStmt::genCode() {
   BasicBlock* bb = builder->getInsertBB();
    expr->genCode();
    Operand* addr;
    //int
    if (lval->getOriginType()->isInt())
        addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymbolEntry())->getAddr();
    //float
    else if (lval->getOriginType()->isFloat())
        addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymbolEntry())->getAddr();
    //string
    else if (lval->getOriginType()->isString())
        addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymbolEntry())->getAddr();
    //Array,没有实现
    else if (lval->getOriginType()->isArray()) 
    {
        ((Id*)lval)->setLeft();
        lval->genCode();
        addr = lval->getOperand();
    }
    Operand* src = expr->getOperand();
    new StoreInstruction(addr, src, bb);
    //数组还没有实现
}

void Ast::typeCheck(Type* retType) {
    if (root != nullptr)
        root->typeCheck();
}

void FunctionDef::typeCheck(Type* retType) {
    //函数定义的检查
    SymbolEntry* se = this->getSymbolEntry();
    Type* ret = ((FunctionType*)(se->getType()))->getRetType();
    //返回值类型
    StmtNode* stmt = this->stmt;
    if (stmt == nullptr) 
    {
        if (ret != TypeSystem::voidType)
        {
            fprintf(stderr, "non_void function does not return a value\n");
        }
    }
    //递归调用
    //若调用的为ReturnStmt，则存在return，其余情况不存在
    stmt->typeCheck(ret);
    if (!isReturnstmt) 
    {
        fprintf(stderr, "function does not have a return statement\n");
    }
}

void BinaryExpr::typeCheck(Type* retType) {
    //根据实验指导书，只需检查两者类型
    Type *type1 = expr1->getType();
    Type *type2 = expr2->getType();
    if(type1 != type2)
    {
        fprintf(stderr, "type %s and %s mismatch in BinaryExpr",
        type1->toStr().c_str(), type2->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    symbolEntry->setType(type1);
}

void Constant::typeCheck(Type* retType) {

}

void Id::typeCheck(Type* retType) {

}

void IfStmt::typeCheck(Type* retType) {
    if (thenStmt)
        thenStmt->typeCheck(retType);

}

void IfElseStmt::typeCheck(Type* retType) {
    if (thenStmt)
        thenStmt->typeCheck(retType);
    if (elseStmt)
        elseStmt->typeCheck(retType);

}

void CompoundStmt::typeCheck(Type* retType) {
    if (stmt)
        stmt->typeCheck(retType);

}

void SeqNode::typeCheck(Type* retType) {
    if (stmt1)
        stmt1->typeCheck(retType);
    if (stmt2)
        stmt2->typeCheck(retType);

}

void DeclStmt::typeCheck(Type* retType) {

}

void ReturnStmt::typeCheck(Type* retType) {
    //来到函数return isRet赋值为true
    //获取return type
    Type* type = retValue->getType();
    isReturnstmt=true;
    //DeBug:
    fprintf(stderr, "Debug \t FuncDef: %s \t Return %s\n",retType->toStr().c_str(),type->toStr().c_str());
    if (!retType) 
    {
        fprintf(stderr, "FunctionDef is not anything \n");
    }
    //函数不是void，无返回值
    if (retValue == nullptr && !retType->isVoid())
    {
        fprintf(stderr,"return have no value, but function have return \n");
    }
    //函数void，有返回值
    if (retValue && retType->isVoid()) 
    {
        fprintf(stderr,"Return have a value, but functionDef return void\n");
    }
    //返回值类型不同
    if (type != retType) 
    {
       fprintf(stderr,"%s mismatch %s \n",retType->toStr().c_str(), type->toStr().c_str());
    }
    
}

void AssignStmt::typeCheck(Type* retType) {
    //return false;
}

CallExpr::CallExpr(SymbolEntry* se, ExprNode* param)
    : ExprNode(se), param(param) {
    // 做参数的检查
    dst = nullptr;
    SymbolEntry* s = se;
    //记录param个数
    int param_index = 0;
    ExprNode* temp = param;
    //temp 取到最后一个node，计算param个数
    while (temp) 
    {
        param_index++;
        temp = (ExprNode*)(temp->getNext());
    }
    while (s) 
    {
        Type* type = s->getType();
        std::vector<Type*> params = ((FunctionType*)type)->getParamsType();
        //取到最后一个
        if ((long unsigned int)param_index == params.size()) {
            this->symbolEntry = s;
            break;
        }
        //用于解决嵌套调用
        s = s->getNext();
    }
    if (symbolEntry) 
    {
        Type* type = symbolEntry->getType();
        this->type = ((FunctionType*)type)->getRetType();
        //不是voidType ,return
        if (this->type != TypeSystem::voidType) 
        {
            SymbolEntry* se =new TemporarySymbolEntry(this->type, SymbolTable::getLabel());
            //目的操作数赋值
            dst = new Operand(se);
        }
        std::vector<Type*> params = ((FunctionType*)type)->getParamsType();
        //temp 放入AST中的node，检验
        ExprNode* index = param;
        for (auto it = params.begin(); it != params.end(); it++) 
        {
            if(index==nullptr)
            {
                fprintf(stderr, "params is less than the function defined\n");
                break;
            }
            //比较params的类型
            else if((*it)->getKind()!=index->getType()->getKind())
            {
                fprintf(stderr, "params isn't match\n");
            }
            //取AST中下一node，与param进行比较
            index = (ExprNode*)(index->getNext());
        }
        //AST中node数量多于Param中定义的
        if (index != nullptr) 
        {
            fprintf(stderr, "params is more than the function defined\n");
        }
    }
    if (((IdentifierSymbolEntry*)se)->isSysy()) {
        unit.insertDeclare(se);
    }
}
//赋值语句，lab更改
AssignStmt::AssignStmt(ExprNode* lval, ExprNode* expr)
    : lval(lval), expr(expr) {
    Type* type = ((Id*)lval)->getType();
    //SymbolEntry* se = lval->getSymbolEntry();
    bool flag = true;//判断赋值语句是否类型正确
    if(type->isInt())
    {
        if(((IntType*)type)->isConst())
        {
            //const 不能赋值为变量
            flag=false;
            fprintf(stderr,"cann't assign const value with variable");
        }
    }
    else if(type->isArray())
    {
        //数组不能直接赋值type
        fprintf(stderr, "array type is not assignable\n");
        flag = false;
    }
    else if(type->isFloat())
    {
        if(((FloatType*)type)->isConst())
        {
            //与int 同理
            //const 不能赋值为变量
            flag=false;
            fprintf(stderr,"cann't assign const value with variable");
        }
    }
    if(type->isFloat()&&!expr->getType()->isFloat())
    {
        flag=false;
        fprintf(stderr,"Float type erro \n");
    }
    else if(type->isInt()&&!expr->getType()->isInt())
    {
        flag=false;
        fprintf(stderr,"Int type erro \n");
    }
    if(!flag)
    {
        fprintf(stderr,"AssignStmt erro \n");
    }
}

Type* Id::getType() 
{
    SymbolEntry* se = this->getSymbolEntry();
    if (!se)
        return TypeSystem::voidType;
    //获取symboltable里的类型
    Type* type = se->getType();
    //int
    if(type->isInt())
        return TypeSystem::intType;
    //float
    else if(type->isFloat())
        return TypeSystem::floatType;
    //不会执行到这里
    return TypeSystem::intType;
}

void ExprNode::output(int level) {
    std::string name, type;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cconst string\ttype:%s\t%s\n", level, ' ', type.c_str(),
            name.c_str());
}

void Ast::output() {
    fprintf(yyout, "program\n");
    if (root != nullptr)
        root->output(4);
}

void BinaryExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case AND:
            op_str = "and";
            break;
        case OR:
            op_str = "or";
            break;
        case LESS:
            op_str = "less";
            break;
        case LESSEQUAL:
            op_str = "lessequal";
            break;
        case GREATER:
            op_str = "greater";
            break;
        case GREATEREQUAL:
            op_str = "greaterequal";
            break;
        case EQUAL:
            op_str = "equal";
            break;
        case NOTEQUAL:
            op_str = "notequal";
            break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\ttype: %s\n", level, ' ',
            op_str.c_str(), type->toStr().c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

int BinaryExpr::getValue() {
    int value;
    switch (op) {
        case ADD:
            value = expr1->getValue() + expr2->getValue();
            break;
        case SUB:
            value = expr1->getValue() - expr2->getValue();
            break;
        case MUL:
            value = expr1->getValue() * expr2->getValue();
            break;
        case DIV:
            value = expr1->getValue() / expr2->getValue();
            break;
        case MOD:
            value = expr1->getValue() % expr2->getValue();
            break;
        case AND:
            value = expr1->getValue() && expr2->getValue();
            break;
        case OR:
            value = expr1->getValue() || expr2->getValue();
            break;
        case LESS:
            value = expr1->getValue() < expr2->getValue();
            break;
        case LESSEQUAL:
            value = expr1->getValue() <= expr2->getValue();
            break;
        case GREATER:
            value = expr1->getValue() > expr2->getValue();
            break;
        case GREATEREQUAL:
            value = expr1->getValue() >= expr2->getValue();
            break;
        case EQUAL:
            value = expr1->getValue() == expr2->getValue();
            break;
        case NOTEQUAL:
            value = expr1->getValue() != expr2->getValue();
            break;
    }
    return value;
}
//flaot getfValue
float BinaryExpr::getfValue() {
    float value;
    switch (op) {
        case ADD:
            value = expr1->getfValue() + expr2->getfValue();
            break;
        case SUB:
            value = expr1->getfValue() - expr2->getfValue();
            break;
        case MUL:
            value = expr1->getfValue() * expr2->getfValue();
            break;
        case DIV:
            value = expr1->getfValue() / expr2->getfValue();
            break;
        case AND:
            value = expr1->getfValue() && expr2->getfValue();
            break;
        case OR:
            value = expr1->getfValue() || expr2->getfValue();
            break;
        case LESS:
            value = expr1->getfValue() < expr2->getfValue();
            break;
        case LESSEQUAL:
            value = expr1->getfValue() <= expr2->getfValue();
            break;
        case GREATER:
            value = expr1->getfValue() > expr2->getfValue();
            break;
        case GREATEREQUAL:
            value = expr1->getfValue() >= expr2->getfValue();
            break;
        case EQUAL:
            value = expr1->getfValue() == expr2->getfValue();
            break;
        case NOTEQUAL:
            value = expr1->getfValue() != expr2->getfValue();
            break;
    }
    return value;
}
//一元float
float UnaryExpr::getfValue() {
    float value;
    switch (op) {
        case NOT:
            value = !(expr->getfValue());
            break;
        case SUB:
            value = -(expr->getfValue());
            break;
    }
    return value;
}
//一元表达式运算
UnaryExpr::UnaryExpr(SymbolEntry* se, int op, ExprNode* expr)
    : ExprNode(se, UNARYEXPR), op(op), expr(expr) {
    std::string op_str = op == UnaryExpr::NOT ? "!" : "-";
    if (expr->getType()->isVoid()) {
        fprintf(stderr,
                "invalid operand of type \'void\' to unary \'opeartor%s\'\n",
                op_str.c_str());
    }
    if (op == UnaryExpr::NOT) {
        type = TypeSystem::intType;
        dst = new Operand(se);
        if (expr->isUnaryExpr()) 
        {
            UnaryExpr* ue = (UnaryExpr*)expr;
            if (ue->getOp() == UnaryExpr::NOT) 
            {
                if (ue->getType() == TypeSystem::intType)
                    //转为bool
                    ue->setType(TypeSystem::boolType);
                // type = TypeSystem::intType;
            }
            //目前不考虑float的取反操作
        }
        
    } 
    else if (op == UnaryExpr::SUB) 
    {
        //目前不考虑float的取反操作
        type = TypeSystem::intType;
        dst = new Operand(se);
        if (expr->isUnaryExpr()) 
        {
            UnaryExpr* ue = (UnaryExpr*)expr;
            if (ue->getOp() == UnaryExpr::NOT)
            {
                if (ue->getType() == TypeSystem::intType)
                    //转为bool
                    ue->setType(TypeSystem::boolType);
            }
        }
    }
};

void UnaryExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case NOT:
            op_str = "not";
            break;
        case SUB:
            op_str = "minus";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\ttype: %s\n", level, ' ',
            op_str.c_str(), type->toStr().c_str());
    expr->output(level + 4);
}

int UnaryExpr::getValue() {
    int value;
    switch (op) {
        case NOT:
            value = !(expr->getValue());
            break;
        case SUB:
            value = -(expr->getValue());
            break;
    }
    return value;
}

void CallExpr::output(int level) {
    std::string name, type;
    int scope;
    if (symbolEntry) {
        name = symbolEntry->toStr();
        type = symbolEntry->getType()->toStr();
        scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
        fprintf(yyout, "%*cCallExpr\tfunction name: %s\tscope: %d\ttype: %s\n",
                level, ' ', name.c_str(), scope, type.c_str());
        Node* temp = param;
        while (temp) {
            temp->output(level + 4);
            temp = temp->getNext();
        }
    }
}

void Constant::output(int level) {
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

//常量 int
int Constant::getValue() {
    return ((ConstantSymbolEntry*)symbolEntry)->getValue();
}
//常量float
float Constant::getfValue() {
    return ((ConstantSymbolEntry*)symbolEntry)->getfValue();
}

//ID int
int Id::getValue() {
    return ((IdentifierSymbolEntry*)symbolEntry)->getValue();
}
//ID float
float Id::getfValue() {
     return ((IdentifierSymbolEntry*)symbolEntry)->getfValue();
}
void Id::output(int level) {
    std::string name, type;
    int scope;
    if (symbolEntry) {
        name = symbolEntry->toStr();
        type = symbolEntry->getType()->toStr();
        scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
        fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
                name.c_str(), scope, type.c_str());
        if (arrIdx) {
            ExprNode* temp = arrIdx;
            int i = 0;
            while (temp) {
                temp->output(level + 4 + 4 * i++);
                temp = (ExprNode*)(temp->getNext());
            }
        }
    }
}

void InitValueListExpr::output(int level) {
    std::string type;
    if (symbolEntry->getType())
        type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cInitValueListExpr\ttype: %s\n", level, ' ',
            type.c_str());
    Node* temp = expr;
    while (temp) {
        temp->output(level + 4);
        temp = temp->getNext();
    }
}

void InitValueListExpr::addExpr(ExprNode* expr) {
    if (this->expr == nullptr) {
        assert(childCnt == 0);
        childCnt++;
        this->expr = expr;
    } else {
        childCnt++;
        this->expr->setNext(expr);
    }
}

bool InitValueListExpr::isFull() {
    ArrayType* type = (ArrayType*)(this->symbolEntry->getType());
    return childCnt == type->getLength();
}

void InitValueListExpr::fill() {
    Type* type = ((ArrayType*)(this->getType()))->getElementType();
    if (type->isArray()) {
        while (!isFull())
            this->addExpr(new InitValueListExpr(new ConstantSymbolEntry(type)));
        ExprNode* temp = expr;
        while (temp) {
            ((InitValueListExpr*)temp)->fill();
            temp = (ExprNode*)(temp->getNext());
        }
    }
    if (type->isInt()) {
        while (!isFull())
            this->addExpr(new Constant(new ConstantSymbolEntry(type, 0)));
        return;
    }
}

void ImplictCastExpr::output(int level) {
    fprintf(yyout, "%*cImplictCastExpr\ttype: %s to %s\n", level, ' ',
            expr->getType()->toStr().c_str(), type->toStr().c_str());
    this->expr->output(level + 4);
}

void CompoundStmt::output(int level) {
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    if (stmt)
        stmt->output(level + 4);
}

void SeqNode::output(int level) {
    // fprintf(yyout, "%*cSequence\n", level, ' ');
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level) {
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
    if (expr)
        expr->output(level + 4);
    if (this->getNext()) {
        this->getNext()->output(level);
    }
}

void BlankStmt::output(int level) {
    fprintf(yyout, "%*cBlankStmt\n", level, ' ');
}

void IfStmt::output(int level) {
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level) {
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void WhileStmt::output(int level) {
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    stmt->output(level + 4);
}
void BreakStmt::output(int level) {
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level) {
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}

void ReturnStmt::output(int level) {
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if (retValue != nullptr)
        retValue->output(level + 4);
}

void AssignStmt::output(int level) {
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void ExprStmt::output(int level) {
    fprintf(yyout, "%*cExprStmt\n", level, ' ');
    expr->output(level + 4);
}

void FunctionDef::output(int level) {
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine\tfunction name: %s\ttype: %s\n", level,
            ' ', name.c_str(), type.c_str());
    if (decl) {
        decl->output(level + 4);
    }
    stmt->output(level + 4);
}
