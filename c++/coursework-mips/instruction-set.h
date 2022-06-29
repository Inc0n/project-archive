
// first register is usually the output

enum INST {
	MOV = 0, // mov register1 register2
	ADD,     // add register1 register2
	MINUS,	 // minus register1 register2
	SHIFT,	 // shift register1 num
	LOAD,	 // add register mem
	STORE,	 // add register mem
	TEST,	 // test register register0
	JUMP,	 // jump place register0
	BRANCH   // branch place register0
}

enum tagtype {
	imm, // immediate
	mem,
	reg
};

typedef unsigned int_t;

struct tag {
	tagtype type;
	int_t value;
};