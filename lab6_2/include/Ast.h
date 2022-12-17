#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include <iostream>
#include <stack>
#include "Operand.h"
#include "Type.h"

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;


class Node {
   private:
    static int counter;
    int seq;
    Node* next;
   protected:
    std::vector<Instruction*> true_list;
    std::vector<Instruction*> false_list;// 回填技术控制流的翻译
    static IRBuilder *builder;
    void backPatch(std::vector<Instruction*> &list, BasicBlock*bb);//将list中所有指令的跳转地址设置为bb
    std::vector<Instruction*> merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2);//将两个列表进行合并

   public:
    Node();
    int getSeq() const { return seq; };
    static void setIRBuilder(IRBuilder* ib) { builder = ib; };
    virtual void output(int level) = 0;
    void setNext(Node* node);
    Node* getNext() { return next; }
    //virtual bool typeCheck(Type* retType = nullptr) = 0;
    virtual void typeCheck(Type* retType = nullptr) = 0;
    virtual void genCode() = 0;
    std::vector<Instruction*>& trueList() { return true_list; }
    std::vector<Instruction*>& falseList() { return false_list; }
};

class ExprNode : public Node {
   private:
    int kind;

   protected:
    //表达式-》数组初始化expr-》类型转化expr-》元expr
    enum { EXPR, INITVALUELISTEXPR, IMPLICTCASTEXPR, UNARYEXPR };
    Type* type;
    SymbolEntry* symbolEntry;
    Operand* dst;  // 目的操作数
   public:
    ExprNode(SymbolEntry* symbolEntry, int kind = EXPR)
        : kind(kind), symbolEntry(symbolEntry){};
    Operand* getOperand() { return dst; };
    void output(int level);
    virtual int getValue() { return -1; };
    //float
    virtual float getfValue(){return 0.0;};
    //用于判断类型
    bool isExpr() const { return kind == EXPR; };
    bool isInitValueListExpr() const { return kind == INITVALUELISTEXPR; };
    bool isImplictCastExpr() const { return kind == IMPLICTCASTEXPR; };
    bool isUnaryExpr() const { return kind == UNARYEXPR; };
    //获取符号表中的entry
    SymbolEntry* getSymbolEntry() { return symbolEntry; };

    virtual void typeCheck(Type* retType = nullptr) {};
    void genCode();
    virtual Type* getType() { return type; };
    Type* getOriginType() { return type; };
};

//二元操作
class BinaryExpr : public ExprNode {
   private:
    int op;
    ExprNode *expr1, *expr2;

   public:
    enum {
        ADD,
        SUB,
        MUL,
        DIV,
        MOD,
        AND,
        OR,
        LESS,
        LESSEQUAL,
        GREATER,
        GREATEREQUAL,
        EQUAL,
        NOTEQUAL
    };
    BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2);
    void output(int level);
    //int
    int getValue();
    //float
    float getfValue();
    void typeCheck(Type* retType = nullptr);
    void genCode();
};

//一元操作
class UnaryExpr : public ExprNode {
   private:
    int op;
    ExprNode* expr;

   public:
    //enum {NOT, SUB};
    //按位取反-》负号-》正号
    enum { NOT, SUB,ADD};
    UnaryExpr(SymbolEntry* se, int op, ExprNode* expr);
    void output(int level);
    int getValue();
     //float
    float getfValue();
    void typeCheck(Type* retType = nullptr);
    void genCode();
    int getOp() const { return op; };
    void setType(Type* type) { this->type = type; }
};

//函数调用expr
class CallExpr : public ExprNode {
   private:
    ExprNode* param;//用于记录参数

   public:
    CallExpr(SymbolEntry* se, ExprNode* param = nullptr);
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    void genCode();
};

//常量
class Constant : public ExprNode {
   public:
    Constant(SymbolEntry* se) : ExprNode(se) {
        dst = new Operand(se);
        //float int
        if(se->getType()==TypeSystem::intType)
            type = TypeSystem::intType;
        else if(se->getType()==TypeSystem::floatType)
            type = TypeSystem::floatType;
    };
    void output(int level);
    int getValue();
    //float
    float getfValue();
    void typeCheck(Type* retType = nullptr);
    void genCode();
};
//ID
class Id : public ExprNode {
   private:
    ExprNode* arrIdx;
    bool left = false;

   public:
    Id(SymbolEntry* se, ExprNode* arrIdx = nullptr)
        : ExprNode(se), arrIdx(arrIdx) {
         if (se) {
            type = se->getType();
            //INT
            if (type->isInt()) 
            {
                SymbolEntry* temp = new TemporarySymbolEntry(
                    TypeSystem::intType, SymbolTable::getLabel());
                //操作数
                dst = new Operand(temp);
            } 
            //array
            else if (type->isArray()) 
            {
                SymbolEntry* temp = new TemporarySymbolEntry(
                    new PointerType(((ArrayType*)type)->getElementType()),
                    SymbolTable::getLabel());
                //操作数
                dst = new Operand(temp);
            }
            //float
            else if(type->isFloat())
            {
                SymbolEntry* temp = new TemporarySymbolEntry(
                    TypeSystem::floatType, SymbolTable::getLabel());
                //操作数
                dst = new Operand(temp);
            }
        }
    };
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    void genCode();
    int getValue();
    //float
    float getfValue();
    //中间代码生成
    ExprNode* getArrIdx() { return arrIdx; };
    Type* getType();
    bool isLeft() const { return left; };
    void setLeft() { left = true; }
};

class ImplicitValueInitExpr : public ExprNode {
   public:
    ImplicitValueInitExpr(SymbolEntry* se) : ExprNode(se){};
    void output(int level);
};

class InitValueListExpr : public ExprNode {
   private:
    ExprNode* expr;
    //维数，此次实验中没完成
    int childCnt;

   public:
    InitValueListExpr(SymbolEntry* se, ExprNode* expr = nullptr)
        : ExprNode(se, INITVALUELISTEXPR), expr(expr) {
        type = se->getType();
        childCnt = 0;
    };
    void output(int level);
    ExprNode* getExpr() const { return expr; };
    void addExpr(ExprNode* expr);
    bool isEmpty() { return childCnt == 0; };
    bool isFull();
    void typeCheck(Type* retType = nullptr);
    void genCode();
    void fill();
};

// 仅用于int2bool
class ImplictCastExpr : public ExprNode {
   private:
    ExprNode* expr;

   public:
    ImplictCastExpr(ExprNode* expr)
        : ExprNode(nullptr, IMPLICTCASTEXPR), expr(expr) {
        type = TypeSystem::boolType;
        dst = new Operand(
            new TemporarySymbolEntry(type, SymbolTable::getLabel()));
    };
    void output(int level);
    ExprNode* getExpr() const { return expr; };
    void typeCheck(Type* retType = nullptr) { };
    void genCode();
};
//用于判断函数返回值
static bool isReturnstmt;
class StmtNode : public Node {
   private:
    int kind;
   protected:
    enum { IF, IFELSE, WHILE, COMPOUND, RETURN };

   public:
    StmtNode(int kind = -1) : kind(kind){isReturnstmt=false;};
    //判断是哪种类型
    bool isIf() const { return kind == IF; };
    bool isIfElse() const{ return kind==IFELSE;};
    bool isWhile() const{ return kind==WHILE;};
    bool isCOMPOUND() const{ return kind==COMPOUND;};
    bool isRETURN() const{ return kind==RETURN;};

    virtual void typeCheck(Type* retType = nullptr) = 0;
    //virtual bool get_return()=0;
};
//复合的stmt
class CompoundStmt : public StmtNode {
   private:
    StmtNode* stmt;

   public:
    CompoundStmt(StmtNode* stmt = nullptr) : stmt(stmt){};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return this->isReturnstmt;};
    void genCode();
};

class SeqNode : public StmtNode {
   private:
    StmtNode *stmt1, *stmt2;

   public:
    SeqNode(StmtNode* stmt1, StmtNode* stmt2) : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return this->isReturnstmt;};
    void genCode();
};

class DeclStmt : public StmtNode {
   private:
    Id* id;
    ExprNode* expr;

   public:
    DeclStmt(Id* id, ExprNode* expr = nullptr) : id(id) {
        if (expr) {
            this->expr = expr;
            if (expr->isInitValueListExpr())
                ((InitValueListExpr*)(this->expr))->fill();
        }
    };
    void output(int level);
    //bool get_return(){return this->isReturnstmt;};
    void typeCheck(Type* retType = nullptr);
    void genCode();
    Id* getId() { return id; };
};

class BlankStmt : public StmtNode {
   public:
    BlankStmt(){};
    void output(int level);
    //bool get_return(){return this->isReturnstmt;};
    void typeCheck(Type* retType = nullptr);
    void genCode();
};

class IfStmt : public StmtNode {
   private:
    ExprNode* cond;//条件判断，可能需要隐式转换
    StmtNode* thenStmt;

   public:
    IfStmt(ExprNode* cond, StmtNode* thenStmt)
        : cond(cond), thenStmt(thenStmt) {
        //只考虑int-》bool的隐式转换
        if (cond->getType()->isInt() && cond->getType()->getSize() == 32) {
            ImplictCastExpr* temp = new ImplictCastExpr(cond);
            this->cond = temp;
        }
        //bool 直接赋值即可
    };
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class IfElseStmt : public StmtNode {
   private:
    ExprNode* cond;
    StmtNode* thenStmt;
    StmtNode* elseStmt;

   public:
    IfElseStmt(ExprNode* cond, StmtNode* thenStmt, StmtNode* elseStmt)
        : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {
        if (cond->getType()->isInt() && cond->getType()->getSize() == 32) {
            ImplictCastExpr* temp = new ImplictCastExpr(cond);
            this->cond = temp;
        }
        //bool 直接赋值即可
    };
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class WhileStmt : public StmtNode {
   private:
    ExprNode* cond;//条件
    StmtNode* stmt;//while 内部的语句
    BasicBlock* cond_bb;//条件
    BasicBlock* end_bb;//while外语句
   public:
    WhileStmt(ExprNode* cond, StmtNode* stmt=nullptr) : cond(cond), stmt(stmt) {
        if (cond->getType()->isInt() && cond->getType()->getSize() == 32) {
            ImplictCastExpr* temp = new ImplictCastExpr(cond);
            this->cond = temp;
        }
        //bool 直接赋值
    };
    void setStmt(StmtNode* stmt){this->stmt = stmt;};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
    BasicBlock* get_cond_bb(){return this->cond_bb;};
    BasicBlock* get_end_bb(){return this->end_bb;};
};

class BreakStmt : public StmtNode {
    private:
    StmtNode * whileStmt;
   public:
    BreakStmt(StmtNode* whileStmt){this->whileStmt=whileStmt;};
    void output(int level);
    //空的代码块根本不需要类型检查
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class ContinueStmt : public StmtNode {
    private:
    StmtNode *whileStmt;
   public:
    ContinueStmt(StmtNode* whileStmt){this->whileStmt=whileStmt;};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class ReturnStmt : public StmtNode {
   private:
    ExprNode* retValue;

   public:
    ReturnStmt(ExprNode* retValue = nullptr) : retValue(retValue){};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class AssignStmt : public StmtNode {
   private:
    ExprNode* lval;
    ExprNode* expr;

   public:
    AssignStmt(ExprNode* lval, ExprNode* expr);
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class ExprStmt : public StmtNode {
   private:
    ExprNode* expr;

   public:
    ExprStmt(ExprNode* expr) : expr(expr){};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    //bool get_return(){return isReturnstmt;};
    void genCode();
};

class FunctionDef : public StmtNode {
   private:
    SymbolEntry* se;
    // 参数的定义 next连接
    DeclStmt* decl;
    StmtNode* stmt;

   public:
    FunctionDef(SymbolEntry* se, DeclStmt* decl, StmtNode* stmt)
        : se(se), decl(decl), stmt(stmt){};
    void output(int level);
    void typeCheck(Type* retType = nullptr);
    void genCode();
    SymbolEntry* getSymbolEntry() { return se; };
    //bool get_return(){return isReturnstmt;};
};

class Ast {
   private:
    Node* root;
   public:
    Ast() { root = nullptr; }
    void setRoot(Node* n) { root = n; }
    void output();
    void typeCheck(Type* retType = nullptr);
    void genCode(Unit* unit);
};
#endif
