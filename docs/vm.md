# GLaDOS Virtual Machine

## Usage

The GLaDOS Virtual Machine uses standard input for reading all of the bytecodes
for its intepretation.

Example:

> `./glados -vm < out.bin`

The execution will start AFTER the reading of the entire input (unless EOF met).
