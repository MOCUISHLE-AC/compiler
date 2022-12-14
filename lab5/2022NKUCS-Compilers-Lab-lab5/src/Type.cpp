#include "Type.h"
#include <assert.h>
#include <iostream>
#include <sstream>

IntType TypeSystem::commonInt = IntType(4);
IntType TypeSystem::commonConstInt = IntType(4, true);
VoidType TypeSystem::commonVoid = VoidType();
FloatType TypeSystem::commonFloat=FloatType(4);
FloatType TypeSystem::commonConstFloat = FloatType(4, true);


Type* TypeSystem::intType = &commonInt;
Type* TypeSystem::constIntType = &commonConstInt;
Type* TypeSystem::voidType = &commonVoid;
Type* TypeSystem::floatType = &commonFloat;
Type* TypeSystem::constFloatType = &commonConstFloat;

std::string IntType::toStr() {
    if (constant)
        return "const int";
    else
        return "int";
}

std::string VoidType::toStr() {
    return "void";
}

std::string ArrayType::toStr() {
    std::vector<std::string> vec;
    Type* temp = this;
    while (temp && temp->isArray()) {
        std::ostringstream buffer;
        if(temp == this && length == 0)
            buffer << '[' << ']';
        else
            buffer << '[' << ((ArrayType*)temp)->getLength() << ']';
        vec.push_back(buffer.str());
        temp = ((ArrayType*)temp)->getElementType();
        ;
    }
    //assert(temp->isInt());
    std::ostringstream buffer;
    if(temp->isInt()){
        if (constant)
            buffer << "const ";
        buffer << "int";
    }
    if(temp->isFloat()){
          if (constant)
            buffer << "const ";
        buffer << "float";
    }
    for (auto it = vec.begin(); it != vec.end(); it++)
        buffer << *it;
    return buffer.str();
}

std::string FunctionType::toStr() {
    std::ostringstream buffer;
    buffer << returnType->toStr() << "(";
    for (auto it = paramsType.begin(); it != paramsType.end(); it++) {
        buffer << (*it)->toStr();
        if (it + 1 != paramsType.end())
            buffer << ", ";
    }
    buffer << ')';
    return buffer.str();
}

std::string StringType::toStr() {
    std::ostringstream buffer;
    buffer << "const char[" << length << "]";
    return buffer.str();
}

std:: string FloatType::toStr(){
    if (constant)
        return "const float";
    else
        return "float";
}