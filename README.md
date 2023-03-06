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

## Usage

In order to integrate the generated instruction implementations with an existing RISC-V simulator the following interface needs to be provided:

    #include <stdint.h>

    /* Register file */
    uint32_t read_register(unsigned idx);
    void write_register(unsigned idx, uint32_t value);

    /* Program counter */
    uint32_t read_next_pc(void);
    void write_pc(uint32_t newPC);

    /* Byte-addressable memory */
    uint32_t load_word(uint32_t addr);
    void store_word(uint32_t addr, uint32_t value);

    /* Decoder interface */
    uint32_t instr_rs1(void *instr);
    uint32_t instr_rs2(void *instr);
    uint32_t instr_rd(void *instr);
    uint32_t instr_immI(void *instr);
    uint32_t instr_immS(void *instr);
    uint32_t instr_immB(void *instr);
    uint32_t instr_immU(void *instr);
    uint32_t instr_immJ(void *instr);

## Development

Please format all code with [fourmolu](https://github.com/fourmolu/fourmolu).
For convince, a pre-commit Git hook for checking if files are properly formated is provided in `.githooks`.
It can be activated using:

    $ git config --local core.hooksPath .githooks
