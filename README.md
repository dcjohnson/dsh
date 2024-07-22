# dsh

# Syntax

All Strings Are Quoted

run command: @<exec command> <arg1> <arg2> ... <argn> 

pipe: | 

This writes the output of the shell command to the variable a

a := @echo reee 

fd: an integer

run command in background: background <run command>

file descriptor redirection: <run command> <fd> -> <fd>

string: "anb lasdjlajd "

string interpolation "abd{var}al"
	var is a string variable

We will have a "main" function entrypoint. If it doesn't have that then it is just a library. 

Functions have stack space for the following:

A location to return a value to. 
The address of the previous function call so we can reset the stack pointer.
All parameters and local variables. 

Here is how backgrounding function calls will work. 

There will be a fork instruction that will fork the process; a register called FPID will be updated accordingly. As part of the compilation of that background instruction, we will get the following instructions:

fork
jumpForked afterForkLabel (This will jump to the label if the PID matches FPID
functionCall
exit 
afterFork <some instruction>
