#include "SymbolTable.h"
#include <iostream>
#include <sstream>
#include "Type.h"

bool SymbolEntry::setNext(SymbolEntry* se) {
    SymbolEntry* s = this;
    long unsigned int cnt =
        ((FunctionType*)(se->getType()))->getParamsType().size();
    if (cnt == ((FunctionType*)(s->getType()))->getParamsType().size())
        return false;
    while (s->getNext()) {
        if (cnt == ((FunctionType*)(s->getType()))->getParamsType().size())
            return false;
        s = s->getNext();
    }
    if (s == this) {
        this->next = se;
    } else {
        s->setNext(se);
    }
    return true;
}

SymbolEntry::SymbolEntry(Type* type, int kind) {
    this->type = type;
    this->kind = kind;
}
//int
ConstantSymbolEntry::ConstantSymbolEntry(Type* type, int value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    assert(type->isInt());
    this->value = value;
}
//float
ConstantSymbolEntry::ConstantSymbolEntry(Type* type, float value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    this->fvalue = value;
}
//string
ConstantSymbolEntry::ConstantSymbolEntry(Type* type, std::string value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    assert(type->isString());
    this->strValue = value;
}
//array
ConstantSymbolEntry::ConstantSymbolEntry(Type* type)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    // assert(type->isArray());
}

int ConstantSymbolEntry::getValue() const {
    return value;
}
//float
float ConstantSymbolEntry::getfValue() const {
    //assert(type->isInt());
    return fvalue;
}
//string
std::string ConstantSymbolEntry::getStrValue() const {
    assert(type->isString());
    return strValue;
}

std::string ConstantSymbolEntry::toStr() {
    std::ostringstream buffer;
    if (type->isInt())
    {
        buffer << value;
    }
        
    else if(type->isFloat())
    {
        buffer << fvalue;
    }
    else if (type->isString())
    {
        buffer << strValue;
    }
        
    return buffer.str();
}

IdentifierSymbolEntry::IdentifierSymbolEntry(Type* type,
                                             std::string name,
                                             int scope,
                                             bool sysy)
    : SymbolEntry(type, SymbolEntry::VARIABLE), name(name), sysy(sysy) {
    this->scope = scope;
    this->initial = false;
    this->label = -1;
    this->allZero = false;
}

void IdentifierSymbolEntry::setValue(int value) {
    if (((IntType*)(this->getType()))->isConst()) {
        if (!initial) {
            this->value = value;
            initial = true;
        } else {
            // 不会执行
        }
    } else {
        this->value = value;
    }
}

void IdentifierSymbolEntry::setfValue(float value) {
    if (((FloatType*)(this->getType()))->isConst()) {
        if (!initial) {
            this->fvalue = value;
            initial = true;
        } 
        else {
            // 不会执行
        }
    } 
    else {
        this->fvalue = value;
    }
}
void IdentifierSymbolEntry::setArrayValue(int* arrayValue) {
    if (((IntType*)(this->getType()))->isConst()) {
        if (!initial) {
            this->arrayValue = arrayValue;
            initial = true;
        } else {
            // 
        }
    } else {
        this->arrayValue = arrayValue;
    }
}

void IdentifierSymbolEntry::setArrayfValue(float* arrayValue) {
    if (((FloatType*)(this->getType()))->isConst()) {
        if (!initial) {
            this->arrayfValue = arrayValue;
            initial = true;
        } else {

        }
    } else {
        this->arrayfValue = arrayValue;
    }
}

std::string IdentifierSymbolEntry::toStr() {
    std::ostringstream buffer;
    if (label < 0)
        buffer << '@' << name;
    else
        buffer << "%t" << label;
    return buffer.str();
}

TemporarySymbolEntry::TemporarySymbolEntry(Type* type, int label)
    : SymbolEntry(type, SymbolEntry::TEMPORARY) {
    this->label = label;
}

std::string TemporarySymbolEntry::toStr() {
    std::ostringstream buffer;
    buffer << "%t" << label;
    return buffer.str();
}

SymbolTable::SymbolTable() {
    prev = nullptr;
    level = 0;
}

SymbolTable::SymbolTable(SymbolTable* prev) {
    //设定prev，prev索引所有的symboltable
    this->prev = prev;
    this->level = prev->level + 1;
}

/*
    Description: lookup the symbol entry of an identifier in the symbol table
    Parameters:
        name: identifier name
    Return: pointer to the symbol entry of the identifier

    hint:
    1. The symbol table is a stack. The top of the stack contains symbol entries
   in the current scope.
    2. Search the entry in the current symbol table at first.
    3. If it's not in the current table, search it in previous ones(along the
   'prev' link).
    4. If you find the entry, return it.
    5. If you can't find it in all symbol tables, return nullptr.
*/
SymbolEntry* SymbolTable::lookup(std::string name) {
        //用于索引symboltable
    SymbolTable* index = this;
    while (index != nullptr)
    {
        if (index->symbolTable.find(name) != index->symbolTable.end()) 
        {
            //在符号表中找到
            return index->symbolTable[name];
        } 
        else 
        {
            //去其他符号表中查找
            index = index->prev;
        }
    }
    //所有符号表中都没有
    return nullptr;
}

// install the entry into current symbol table.
bool SymbolTable::install(std::string name, SymbolEntry* entry) 
{
    if (this->symbolTable.find(name) != this->symbolTable.end()) 
    {
        SymbolEntry* se = this->symbolTable[name];
        if (se->getType()->isFunc())
            return se->setNext(entry);
        return false;
    } 
    else 
    {
        symbolTable[name] = entry;
        return true;
    }
}

int SymbolTable::counter = 0;
static SymbolTable t;
SymbolTable* identifiers = &t;
SymbolTable* globals = &t;
