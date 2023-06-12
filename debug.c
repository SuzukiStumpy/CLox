#include <stdio.h>
#include "debug.h"

/**
 * Dumps the contents of a chunk to output as a disassembly
 * @param chunk The cunk to traverse
 * @param name The name to give to the dump
 */
void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

/**
 * Handles the dumping of a single-byte instruction to the disassembly
 * @param name The name of the instruction
 * @param offset The offset we're currently reading from
 * @return The offset for the next instruction
 */
static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

/**
 * Disassembles an individual instruction within a chunk and returns the offset to
 * the next instruction
 * @param chunk The chunk we're working with
 * @param offset The offset to disassemble at.
 * @return The start of the next instruction
 */
int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    uint8_t instruction = chunk->code[offset];

    switch (instruction) {
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);

        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}