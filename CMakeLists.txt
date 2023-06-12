cmake_minimum_required(VERSION 3.25)
project(CLox)

set(CMAKE_CXX_STANDARD 17)

add_executable(CLox main.c common.h chunk.h chunk.c memory.h memory.c debug.h debug.c value.c value.h)
