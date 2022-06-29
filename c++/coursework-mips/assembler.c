
#include <stdio.h>
#include <string.h>

#define assert__(x,str,...) for (; !(x) ; assert(x) ) { printf(str,...); }
#define streq(x,y) (strcmp(x,y) == 0)

// assemble a file (.asm) to (.out) binary executable
void assembleFile(const char *filename) {
}

// assemble a line of assembly to machine code
unsigned assemble(const char *line) {
	char delim[] = " ";
	char operandDelim[] = ",";
	//
	char *code = strdup(line);
	char operands[5]; // at most 5 operands
	//
	char *opcode = strtok(line, delim);
	if (opcode[0] == 'j')
	assert(opcode && "No opcode!");
	//
	for (int i = 0; i < 5; ++i) {
		operands[i] = strtok(NULL, operandDelim);
		if (operands[i] == NULL) break;
	}
	if (isIType(opcode)) return iFormat(opcode, operands);
	else if (isJType(opcode)) return jFormat(opcode, operands);
	else return rFormat(opcode, operands)
	assert__(operands[0], "no operand passed in: %s", code);
}

/* r type instruction that do not require a target address, immediate value, or branch displacement
| op    | rs    | rt    | rd    | shamt | funct |
| 6bits | 5bits | 5bits | 5bits | 5bits | 6bits |
*/
unsigned rFormat(const char* opcode, const char operands[5]) {
	return 0;
}

int isIType(const char* opcode) {
	int len = strlen(opcode);
	return opcode[len-1] == 'i';
}

/* i type instruction has an immediate operand, branch target offset, memory operand displacement
| op    | rs    | rt    | constant or address |
| 6bits | 5bits | 5bits | 16bits              |
*/
unsigned iFormat(const char* opcode, const char operands[5]) {
	return 0;
}

int isJType(const char* opcode) return opcode[0] == 'j';

// only j and jal are j type
// 6bits op code, 26bits address
unsigned jFormat(const char* opcode, const char operands[5]) {
	if (opcode == "j") return assembleJFormat(2, operands[0]);
	if (opcode == "jal") return assembleJFormat(3, operands[0]);
}

int calcReg(int base, char index, int lim) {
	int num = index - '0';
	int regNum = base + num;
	assert__(
		regNum >= base && regNum < lim,
		"error calculating register number, %d %d %d", base, index, lim);
	return regNum;
}

int assembleRegister(const char* reg) {
	if (streq(reg, "$zero")) return 0;
	if (streq(reg, "$at")) return 1;
	if (reg[1], 'v') return calcReg(2, reg[2], 4);
	if (reg[1] == 'a') return calcReg(4, reg[2], 8);
	if (reg[1] == 't') return calcReg(8, reg[2], 18);
	if (reg[1] == 's') return calcReg(18, reg[2], 26);
	if (reg[1] == 'k') return calcReg(26, reg[2], 28);
	if (streq(reg, "$gp")) return 28;
	if (streq(reg, "$sp")) return 29;
	if (streq(reg, "$fp")) return 30;
	if (streq(reg, "$ra")) return 31;
}