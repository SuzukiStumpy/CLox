#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"
#include "object.h"
#include "memory.h"

ObjFunction* compile(const char* source);
void markCompilerRoots();

#endif //CLOX_COMPILER_H
