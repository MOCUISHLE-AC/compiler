#ifndef __SYMBOLTABLE_H__
#define __SYMBOLTABLE_H__

#include <assert.h>
#include <map>
#include <string>

class Type;
class Operand;

class SymbolEntry {
   private:
    int kind;
    SymbolEntry* next;

   protected:
    enum { CONSTANT, VARIABLE, TEMPORARY };
    Type* type;

   public:
    SymbolEntry(Type* type, int kind);
    virtual ~SymbolEntry(){};
    bool isConstant() const { return kind == CONSTANT; };
    bool isTemporary() const { return kind == TEMPORARY; };
    bool isVariable() const { return kind == VARIABLE; };
    Type* getType() { return type; };
    void setType(Type* type) { this->type = type; };
    virtual std::string toStr() = 0;
    bool setNext(SymbolEntry* se);
    SymbolEntry* getNext() const { return next; };

    // You can add any function you need here.
};

// symbol table managing identifier symbol entries
class SymbolTable {
   private:
    std::map<std::string, SymbolEntry*> symbolTable;
    SymbolTable* prev;
    int level;
    static int counter;

   public:
    SymbolTable();
    SymbolTable(SymbolTable* prev);
    bool install(std::string name, SymbolEntry* entry);
    SymbolEntry* lookup(std::string name);
    SymbolTable* getPrev() { return prev; };
    int getLevel() { return level; };
    static int getLabel() { return counter++; };
};

/*
    Symbol entry for literal constant. Example:

    int a = 1;

    Compiler should create constant symbol entry for literal constant '1'.
*/
class ConstantSymbolEntry : public SymbolEntry {
   private:
    int value;
    //添加一个浮点型
    float fvalue;
    std::string strValue;

   public:
    ConstantSymbolEntry(Type* type, int value);
    ConstantSymbolEntry(Type* type, std::string strValue);
    ConstantSymbolEntry(Type* type);
    //float
    ConstantSymbolEntry(Type* type, float value);
    virtual ~ConstantSymbolEntry(){};
    int getValue() const;
    //float
    float getfValue() const;
    std::string getStrValue() const;
    std::string toStr();
    // You can add any function you need here.
};

/*
    Symbol entry for identifier. Example:

    int a;
    int b;
    void f(int c)
    {
        int d;
        {
            int e;
        }
    }

    Compiler should create identifier symbol entries for variables a, b, c, d
   and e:

    | variable | scope    |
    | a        | GLOBAL   |
    | b        | GLOBAL   |
    | c        | PARAM    |
    | d        | LOCAL    |
    | e        | LOCAL +1 |
*/
class IdentifierSymbolEntry : public SymbolEntry {
   private:
    enum { GLOBAL, PARAM, LOCAL };
    std::string name;
    int scope;
    int value;
    //添加浮点型
    float fvalue;
    int label;
    bool initial;
    bool sysy;
    int* arrayValue;
     //添加浮点型
    float* arrayfValue;
    bool allZero;
    Operand* addr;  // The address of the identifier.
                    // You can add any field you need here.

   public:
    IdentifierSymbolEntry(Type* type,
                          std::string name,
                          int scope,
                          bool sysy = false);
    virtual ~IdentifierSymbolEntry(){};
    std::string toStr();
    bool isGlobal() const { return scope == GLOBAL; };
    bool isParam() const { return scope == PARAM; };
    bool isLocal() const { return scope >= LOCAL; };
    bool isSysy() const { return sysy; };
    int getScope() const { return scope; };
    void setAddr(Operand* addr) { this->addr = addr; };
    Operand* getAddr() { return addr; };
    //float
    void setfValue(float fvalue);
    void setValue(int value);
    //float
    float getfValue() const { return fvalue;};
    int getValue() const { return value; };
    //float 
    void setArrayfValue(float* arrayValue);
    void setArrayValue(int* arrayValue);
    //float 
    float* getArrayfValue() const { return arrayfValue; };
    int* getArrayValue() const { return arrayValue; };
    int getLabel() const { return label; };
    void setLabel() { label = SymbolTable::getLabel(); };
    void setAllZero() { allZero = true; };
    bool isAllZero() const { return allZero; };

    // You can add any function you need here.
};

/*
    Symbol entry for temporary variable created by compiler. Example:

    int a;
    a = 1 + 2 + 3;

    The compiler would generate intermediate code like:

    t1 = 1 + 2
    t2 = t1 + 3
    a = t2

    So compiler should create temporary symbol entries for t1 and t2:

    | temporary variable | label |
    | t1                 | 1     |
    | t2                 | 2     |
*/
class TemporarySymbolEntry : public SymbolEntry {
   private:
    int label;

   public:
    TemporarySymbolEntry(Type* type, int label);
    virtual ~TemporarySymbolEntry(){};
    std::string toStr();
    int getLabel() const { return label; };
    // You can add any function you need here.
};

extern SymbolTable* identifiers;
extern SymbolTable* globals;

#endif
