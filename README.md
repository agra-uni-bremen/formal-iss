# formal-iss

Generate a RISC-V instruction set simulator from a formal [LibRISCV](https://github.com/agra-uni-bremen/libriscv) ISA model.

## About

The code generator provided here is capable of generating a simulator-agnostic implementation of the RV32I instruction set.
This generated code has been successfully integrated with the existing RISC-V simulators [Spike](https://github.com/agra-uni-bremen/spike-libriscv) and [riscv-vp](https://github.com/agra-uni-bremen/libriscv-vp).
More information on the code generator is provided in the publication [*Minimally Invasive Generation of RISC-V Instruction Set Simulators from Formal ISA Models*](https://agra.informatik.uni-bremen.de/doc/konf/FDL2023_ST.pdf), which will be published in the proceedings of the 2023 Forum on Specification & Design Languages (FDL).

## Usage

To generate C code from the LibRISCV ISA model run the following command:

    $ cabal run formal-iss -- libriscv_generated.h

If the file argument is omitted, then the C code is written to standard out.

## Interface Model

In order to integrate the generated instruction implementations with an existing RISC-V simulator the following interface needs to be implemented:

    #include <stdint.h>

    /* Register file */
    uint32_t read_register(void *core, unsigned idx);
    void write_register(void *core, unsigned idx, uint32_t value);

    /* Program counter */
    uint32_t read_next_pc(void *core);
    void write_pc(void *core, uint32_t newPC);

    /* Byte-addressable memory */
    uint8_t load_byte(void *core, uint32_t addr);
    uint16_t load_half(void *core, uint32_t addr);
    uint32_t load_word(void *core, uint32_t addr);
    void store_byte(void *core, uint32_t addr, uint8_t value);
    void store_half(void *core, uint32_t addr, uint16_t value);
    void store_word(void *core, uint32_t addr, uint32_t value);

    /* Decoder interface */
    uint32_t instr_rs1(void *instr);
    uint32_t instr_rs2(void *instr);
    uint32_t instr_rd(void *instr);
    uint32_t instr_immI(void *instr);
    uint32_t instr_immS(void *instr);
    uint32_t instr_immB(void *instr);
    uint32_t instr_immU(void *instr);
    uint32_t instr_immJ(void *instr);
    uint32_t instr_shamt(void *instr);

The functions described above are used by the generated code to abstract interaction with simulator-specific code.
For more information, refer to the implementation of this interface for Spike and RISC-V VP:

* https://github.com/agra-uni-bremen/spike-libriscv/blob/libriscv/riscv/interface.h
* https://github.com/agra-uni-bremen/libriscv-vp/blob/libriscv/vp/src/core/rv32/interface.h

## Development

Please format all code with [fourmolu](https://github.com/fourmolu/fourmolu).
For convince, a pre-commit Git hook for checking if files are properly formated is provided in `.githooks`.
It can be activated using:

    $ git config --local core.hooksPath .githooks

## How to Cite

More information is provided in the following publication which will be published as part of the proceedings of the 2023 *Forum on Specification & Design Languages (FDL)*:

    @inproceedings{tempel2023formal-iss,
        title     = {Minimally Invasive Generation of RISC-V Instruction Set Simulators from Formal ISA Models},
        booktitle = {2023 Forum on Specification & Design Languages (FDL)},
        author    = {Sören Tempel and Tobias Brandt and Christoph Lüth and Rolf Drechsler},
        year      = {2023},
        location  = {Turin, Italy}
    }

## Acknowledgements

This work was supported in part by the German Federal Ministry of Education and Research (BMBF) within the project EXCL under contract no. 01IW22002 and within the project Scale4Edge under contract no. 16ME0127.
