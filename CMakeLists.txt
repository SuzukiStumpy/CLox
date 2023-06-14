cmake_minimum_required(VERSION 3.25)
project(CLox)

set(CMAKE_CXX_STANDARD 17)

add_executable(CLox src/main.c src/common.h src/chunk.h src/chunk.c src/memory.h src/memory.c src/debug.h src/debug.c src/value.c src/value.h src/vm.c src/vm.h src/compiler.c src/compiler.h src/scanner.c src/scanner.h src/object.c src/object.h src/table.c src/table.h)
