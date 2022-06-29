// to compile, gcc assembler.c -o assembler
// No error check is provided.
// Variable names cannot start with 0-9.
// hexadecimals are twos complement.
// first address of the code section is zero, data section follows the code section.
//fout tables are formed: jump table, ldi table, label table and variable table.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <vector>
#include <iostream>
#include "Assembler.h"

using namespace std;

//Converts a hexadecimal string to integer.
int hex2int(char* hex) {
    int result = 0;

    while ((*hex) != '\0')
    {
        if (isdigit(*hex))
			result = result*16 + (*hex) - '0';
        else if (isalpha(*hex)) {
			int offset = islower(*hex) ? 'a' : 'A' + 10;
			result = result*16 + (*hex) - offset + 10;
		}
		/* TODO: maybe raise and error here for invalid character */
        hex++;
    }
    return result;
}

ostream& operator<<(ostream& os, const Pair p) {
	return os << "{.location=" << p.location << ", .name=" << p.name << "}" << endl;
}


Pair* findByName(vector<Pair> &pairs, const char* name) {
	for (int i = 0; i < pairs.size(); ++i) {
		if (pairs[i].name == name)
			return &(pairs[i]);
	}
	return NULL;
}

//


void readUntil(FILE *fp, const char *text) {
	char line[100];
	char* token;
	while (fgets(line, sizeof line, fp))  //skip till .data, if no .data, also ok.
	{
		token = strtok(line, "\n\t\r ");
		if (strcmp(token, text) == 0)
			break;
	}
}

int assembleTwoOperands() {
	char* op1 = strtok(NULL, "\n\t\r ");
	char* op2 = strtok(NULL, "\n\t\r ");
	return (op1[0]-'0') | ((op2[0]-'0')<<3);
}

Pair assembleJMP(int addr) {
	char* label = strtok(NULL,"\n\t\r ");           //read the label
	return (Pair) {.location=addr, label=strdup(label)};
}

int assemble3operands() {
	char* op1 = strtok(NULL,"\n\t\r ");
	char* op2 = strtok(NULL,"\n\t\r ");
	char* op3 = strtok(NULL,"\n\t\r ");
	return (op1[0]-'0') | ((op2[0]-'0')<<3) | ((op3[0]-'0')<<6);
}

int assembleInc() {
	char* op1 = strtok(NULL,"\n\t\r ");
	return (op1[0]-'0') | ((op1[0]-'0')<<3);
}

//
//

int main()
{
    FILE *fp;
	char line[100];
	char *token = NULL;
    char *op1, *label;
    int  chch;

    int program[1000];
    int counter=0;  //holds the address of the machine code instruction

// A label is a symbol which mark a location in a program. In the example
// program above, the string "lpp", "loop" and "lp1" are labels.
	vector<Pair> labels;

// Jump instructions cannot be assembled readily because we may not know the value of
// the label when we encountered a jump instruction. This happens if the label used by
// that jump instruction appear below that jump instruction. This is the situation
// with the label "loop" in the example program above. Hence, the location of jump
// instructions must be stored.
    vector<Pair> jumptable;

// The list of variables in .data section and their locations.
    vector<Pair> variables;

//Variables and labels are used by ldi instructions.
//The memory for the variables are traditionally allocated at the end of the code section.
//Hence their addresses are not known when we assemble a ldi instruction. Also, the value of
//a label may not be known when we encounter a ldi instruction which uses that label.
//Hence, the location of the ldi instructions must be kept, and these instructions must be
//modified when we discover the address of the label or variable that it uses.
    vector<Pair> lditable;


    fp = fopen("name_of_program","r");

    if (fp == NULL) {
		cout << "file not found!";
		return -1;
	}

	readUntil(fp, ".code"); //skip till .code section

	while(fgets(line,sizeof line,fp)!= NULL)
	{
		token = strtok(line,"\n\t\r ");  //get the instruction mnemonic or label

//========================================   FIRST PASS  ======================================================
		while (token)
		{   //---------------LDI INSTRUCTION--------------------
			if (strcmp(token,"ldi") == 0)
			{
				op1 = strtok(NULL,"\n\t\r ");            //get the 1st operand of ldi, which is the register that ldi loads
				program[counter] = 0x1000 + hex2int(op1); //generate the first 16-bit of the ldi instruction
				counter++;                               //move to the second 16-bit of the ldi instruction
				op1 = strtok(NULL,"\n\t\r ");            //get the 2nd operand of ldi, which is the data that is to be loaded
				if (strncmp(op1, "0x", 2) == 0)          //if the 2nd operand is twos complement hexadecimal
					program[counter]=hex2int(op1+2)&0xffff;  //convert it to integer and form the second 16-bit
				else if (op1[0] == '-' || isdigit(op1[0]))  //if the 2nd operand is decimal
					program[counter]=atoi(op1)&0xffff;   //convert it to integer and form the second 16-bit
				else                                     //if the second operand is not decimal or hexadecimal, it is a laber or a variable.
				{                                        //in this case, the 2nd 16-bits of the ldi instruction cannot be generated.
					//record the location of this 2nd 16-bit, in the lditable array.
					lditable.push_back({.location=counter, .name=strdup(op1)});
				}
				counter++;                               //skip to the next memory location
			}
			//------------LD INSTRUCTION---------------------
			else if (strcmp(token,"ld") == 0)
			{
				chch = assembleTwoOperands();
				//form the instruction and write it to memory
				program[counter] = 0x2000+(chch & 0x00ff);
				//skip to the next empty location in memory
				counter++;
			}
			//-------------ST INSTRUCTION--------------------
			else if (strcmp(token,"st")==0)
			{
				//to be added
			}
			//------------- CONDITIONAL JUMP ------------------
			else if (strcmp(token,"jz")==0)
			{
				//to be added
			}
			//-------------- JUMP -----------------------------
			else if (strcmp(token,"jmp")==0)
			{
				Pair jmp = assembleJMP(counter);
				jumptable.push_back(jmp);
				//write the incomplete instruction (just opcode) to memory
				program[counter] = 0x5000;
				counter++;
			}
			//----------------- ADD -------------------------------
			else if (strcmp(token,"add")==0)
			{
				chch = assemble3operands();
				program[counter]=0x7000+((chch)&0x00ff);
				counter++;
			}
			else if (strcmp(token,"sub")==0)
			{
				chch = assemble3operands();
				/* TODO: rename sub opcode */
				program[counter]=0x7000+((chch)&0x00ff);
				counter++;
				//to be added
			}
			else if (strcmp(token,"and")==0)
			{
				//to be added
			}
			else if (strcmp(token,"or")==0)
			{
				//to be added
			}
			else if (strcmp(token,"xor")==0)
			{
				//to be added
			}
			else if (strcmp(token,"not")==0)
			{
				chch = assembleTwoOperands();
				program[counter]=0x7500+(chch & 0x00ff);
				counter++;
			}
			else if (strcmp(token,"mov")==0)
			{
				//to be added
			}
			else if (strcmp(token,"inc")==0)
			{
				chch = assembleInc();
				program[counter]=0x7700+((chch)&0x00ff);
				counter++;
			}
			else if (strcmp(token,"dec")==0)
			{
				//to be added
			}
			else //------WHAT IS ENCOUNTERED IS NOT AN INSTRUCTION BUT A LABEL. UPDATE THE LABEL TABLE--------
			{
				//buraya bir counter koy. error check
				labels.push_back({.location=counter, .name=strdup(token)});
			}
			token = strtok(NULL,",\n\t\r ");
		}
	}


//================================= SECOND PASS ==============================

	//supply the address fields of the jump and jz instructions from the
	int i, j;
	//for all jump/jz instructions
	for (Pair j : jumptable) {
		Pair* label = findByName(labels, j.name);
		int relativeaddr = label->location - j.location - 1;
		//copy the jump address into memory.
		program[j.location] += relativeaddr & 0x0fff;
	}

	// search for the start of the .data segment
	rewind(fp);
	readUntil(fp, ".data"); //skip till .data, if no .data, also ok.

	// process the .data segment and generate the variables[] array.
	int dataarea = 0;
	while (fgets(line,sizeof line,fp)!= NULL)
	{
		token = strtok(line,"\n\t\r ");
		if (strcmp(token,".code") == 0)  //go till the .code segment
			break;
		else if (token[strlen(token)-1] == ':')
		{
			token[strlen(token)-1] = '\0';
			variables.push_back(
				{.location=counter+dataarea, .name=strdup(token)});
			token = strtok(NULL,",\n\t\r ");
			if (token == NULL) {
				program[counter + dataarea] = 0;
			} else if (strcmp(token, ".space") == 0) {
				token = strtok(NULL,"\n\t\r ");
				dataarea+=atoi(token);
			}
			else if (strncmp(token, "0x", 2) == 0)
				program[counter+dataarea] = hex2int(token+2) & 0xffff;
			else if (token[0] =='-' || isdigit(token[0]))
				program[counter+dataarea] = atoi(token) & 0xffff;
			dataarea++;
		} else {
			cout << "unexpected line" << line << endl;
		}
	}

// supply the address fields for the ldi instructions from the variable table
// and supply the address fields for the ldi instructions from the label table
	for (Pair p : lditable) {
		Pair* v = findByName(variables, p.name);
		if (v)
			program[p.location] = v->location;
		else {
			Pair* label = findByName(labels, p.name);
			if (label) {
				program[p.location] = label->location & 0x0fff;
				// printf("%d %d %d\n", i, j, label.location);
			}
		}
	}


//display the resulting tables
	cout << "LABEL TABLE\n";
	for (Pair label : labels)
		cout << label << endl;
	cout << "\nJUMP TABLE\n";
	for (Pair jump : jumptable)
		cout << jump << endl;
	cout << "\nVARIABLE TABLE\n";
	for (Pair v : variables)
		cout << v << endl;
	cout << "\nLDI INSTRUCTIONS\n";
	for (Pair l : lditable)
		cout << l << endl;
	cout << endl;
	fclose(fp);
	fp = fopen("RAM","w");
	fprintf(fp,"v2.0 raw\n");
	for (i = 0; i < counter+dataarea; i++)
		fprintf(fp,"%04x\n",program[i]);

	return 0;
}