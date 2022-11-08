#include "SymbolTable.h"
#include <iostream>
#include <sstream>
#include "Type.h"

SymbolEntry::SymbolEntry(Type* type, int kind) {
    this->type = type;
    this->kind = kind;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type, int value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    assert(type->isInt());
    this->value = value;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type, float value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    this->fvalue = value;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type, std::string value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    assert(type->isString());
    this->strValue = value;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    // assert(type->isArray());
}

int ConstantSymbolEntry::getValue() const {
    //assert(type->isInt());
    return value;
}
float ConstantSymbolEntry::getfValue() const {
    //assert(type->isInt());
    return fvalue;
}
std::string ConstantSymbolEntry::getStrValue() const {
    assert(type->isString());
    return strValue;
}

std::string ConstantSymbolEntry::toStr() {
    std::ostringstream buffer;
     if(type->isInt())
    {
        buffer << value;
    }
    else if(type->isFloat())
    {
        buffer << fvalue;
    }
    else if(type->isString())
    {
        buffer << strValue;
    }
    return buffer.str();
}

IdentifierSymbolEntry::IdentifierSymbolEntry(Type* type,
                                             std::string name,
                                             int scope)
    : SymbolEntry(type, SymbolEntry::VARIABLE), name(name) {
    this->scope = scope;
    this->initial = false;
}

void IdentifierSymbolEntry::setValue(int value) {
    if (((IntType*)(this->getType()))->isConst()) {
        if (!initial) {
            this->value = value;
            initial = true;
        } else {
            // 需要报错
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
        } else {
            // 需要报错
        }
    } else {
        this->fvalue = value;
    }
}
void IdentifierSymbolEntry::setArrayValue(int* arrayValue) {
    if (((IntType*)(this->getType()))->isConst()) {
        if (!initial) {
            this->arrayValue = arrayValue;
            initial = true;
        } else {

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
    return name;
}

TemporarySymbolEntry::TemporarySymbolEntry(Type* type, int label)
    : SymbolEntry(type, SymbolEntry::TEMPORARY) {
    this->label = label;
}

std::string TemporarySymbolEntry::toStr() {
    std::ostringstream buffer;
    buffer << "t" << label;
    return buffer.str();
}

SymbolTable::SymbolTable() {
    prev = nullptr;
    level = 0;
}

SymbolTable::SymbolTable(SymbolTable* prev) {
    /*
    设定prev，prev索引所有的symboltable
    */
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
    while (index != nullptr){
        if (index->symbolTable.find(name) != index->symbolTable.end()) {
            //在符号表中找到
            return index->symbolTable[name];
        } else {
            //去其他符号表中查找
            index = index->prev;
        }
    }
    //所有符号表中都没有
    return nullptr;
}

// install the entry into current symbol table.
void SymbolTable::install(std::string name, SymbolEntry* entry) {
    symbolTable[name] = entry;
}

int SymbolTable::counter = 0;
static SymbolTable t;
SymbolTable* identifiers = &t;
SymbolTable* globals = &t;
