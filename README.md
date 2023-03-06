# formal-iss

Generate an instruction set simulator (ISS) from a formal [LibRISCV](https://github.com/nmeum/libriscv) ISA model.

## Status

Currently, the implementation is capable of generating a significant subset of the RV32 base instruction set.
This subset has already been integrated with the existing [riscv-vp](https://github.com/agra-uni-bremen/riscv-vp) in the [libriscv-vp](https://github.com/nmeum/libriscv-vp) repository.

## Usage

To generate C code from the LibRISCV ISA model run the following command:

    $ cabal run formal-iss -- libriscv_generated.h

If the file argument is omitted, then the C code is written to standard out.
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
