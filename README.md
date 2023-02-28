# formal-iss

Generate an instruction set simulator (ISS) from a formal [LibRISCV](https://github.com/nmeum/libriscv) ISA model.

## Status

Currently, the code generates a C implementation of the ADD instruction:

    $ cabal run
    Up to date
    void exec_add(void * instr)
    {
        write_register(instr_rd(instr),
                       read_register(instr_rs1(instr)) + read_register(instr_rs2(instr)));
    }

Additional instruction can be added after the software architecture is somewhat stable.
Function for interacting with the architecture state (e.g. `write_register`) are supposed to be provided separately.
