
#include "mips.h"
#include "instruction-set.h"
#include <iostream>
#include <cassert>

using namespace std;

MIPS::MIPS() :
	registers(vector<unsigned>(32,0)),
	memory(vector<unsigned>(1024, 0)),
	code(vector<unsigned>(1024, 0)),
	pc(0)
{}

unsigned MIPS::getreg(unsigned reg) {
	assert(reg < this->registers.size());
	return this->registers[reg];
}

unsigned MIPS::getmem(unsigned addr) {
	assert(addr < this->memory.size());
	return this->memory[addr];
}

//

void MIPS::execute() {
	// TODO: determine how to halt the execution
	while (this->pc != this->code.size())
		this->cycle();
}

// void evaluate(MIPS* mips, unsigned code) {
// 	short opcode = (code & 0xfc000000) >> 26; // masked for top 6bits
// 	unsigned operands = (code & 0x3ffffff); // mask for last 26bits
// }

void MIPS::cycle() {
	this->decodeInstruction(this->fetchInstruction());
	this->regsRead();
	this->computeALU();
	this->memoryRegWrite();
	this->pc = signal.pcsrc ? data.reg2 : this->pc+1;
}

// fetch the instruction from pc
unsigned MIPS::fetchInstruction() {
	return this->code[pc];
}

// decode instruction and setup control signal
void MIPS::decodeInstruction(unsigned inst) {
	// TODO: implement instruction set
	if (inst == MOV) {
		signal.regwrite = signal.regread1 = ;
		signal.regread2 = ;
	} else if (inst == ADD) {
		signal.regwrite = signal.regread1 = ;
		signal.regread2 = ;
		signal.a_op = aluop::add;
	} else if (inst == MINUS) {
		signal.regwrite = signal.regread1 = ;
		signal.regread2 = ;
		signal.a_op = aluop::minus;
	} else if (inst == SHIFT) {
	} else if (inst == LOAD) {
		signal.regwrite = 1;
		signal.regread2 = 1;
	} else if (inst == STORE) {
	} else if (inst == TEST) {
	} else if (inst == JUMP) {
	} else if (inst == BRANCH) {
	} else {
		cout << "error" << endl;
		exit(1);
	}
}

// loading data into registers
void MIPS::regsRead() {
	data.reg1 = getreg(signal.regread1);
	data.reg2 = signal.alusrc ? signal.regread2 : getreg(signal.regread2);
	// data.regwrite = getreg(signal.regread1);
}

unsigned alu(unsigned f, unsigned a, unsigned b) {
	if (f == add) return a + b;
	else if (f == aluop::minus) return a - b;
	else if (f == shift) {
		signed c = (signed)b;
		return c > 0 ? a << b : a >> b;
	}
	else if (f == shift_r) return a >> b;
	cout << "error" << endl;
	exit(1);
}
// compute the data from registers
void MIPS::computeALU() {
	// TODO: implement alu function
	data.alu = alu(signal.a_op, data.reg1, data.reg2);
}

// write back the data from registers
void MIPS::memoryRegWrite() {
	if (signal.memwrite) {
		unsigned result = signal.memtoreg
			? data.alu
			: this->getmem(data.alu);
		this->registers[signal.regwrite] = result;
	}
}
