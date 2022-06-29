
#include <vector>

enum memop {
	read,
	write,
	nop
};

enum aluop {
	add,
	minus,
	shift
};

struct control {
	unsigned regread1; // registers to read
	unsigned regread2;
	unsigned regwrite; // register to write
	//
	unsigned alusrc; // reg or instruction immediate to alu
	aluop a_op;
	//
	unsigned memwrite;
	unsigned memtoreg; // alu or mem write back to reg
	//
	memop m_op;
	//
	unsigned pcsrc; // next inst or inst addr to pc
};

struct datapath {
	unsigned reg1;
	unsigned reg2;
	unsigned regwrite;
	unsigned writedata;
	//
	unsigned alu;
	//
	unsigned mem;
};

struct MIPS {
	std::vector<unsigned> registers;
	std::vector<unsigned> memory;
	std::vector<unsigned> code;
	unsigned pc;
	//
	control signal;
	datapath data;
	//
	MIPS();
	unsigned getreg(unsigned reg);
	unsigned getmem(unsigned reg);
	void execute();
	void cycle();
	//
	unsigned fetchInstruction();
	void decodeInstruction(unsigned inst);
	void regsRead();
	void computeALU();
	void memoryRegWrite();
};