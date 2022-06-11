# fatfs-spin
------------

This is a P8X32A/Propeller, P2X8C4M64P/Propeller 2 FAT filesystem engine

**IMPORTANT**: This software is meant to be used with the [spin-standard-library](https://github.com/avsa242/spin-standard-library) (P8X32A) or ~~[p2-spin-standard-library](https://github.com/avsa242/p2-spin-standard-library) (P2X8C4M64P)~~. Please install the applicable library first before attempting to use this code, otherwise you will be missing several files required to build the project.

## Salient Features

* Supports FAT32 volumes

## Requirements

P1/SPIN1:
* spin-standard-library

~~P2/SPIN2:~~
* ~~p2-spin-standard-library~~

## Compiler Compatibility

| Processor | Language | Compiler               | Backend     | Status                |
|-----------|----------|------------------------|-------------|-----------------------|
| P1        | SPIN1    | FlexSpin (5.9.10-beta) | Bytecode    | OK                    |
| P1        | SPIN1    | FlexSpin (5.9.10-beta) | Native code | OK                    |
| P1        | SPIN1    | OpenSpin (1.00.81)     | Bytecode    | Untested (deprecated) |
| P2        | SPIN2    | FlexSpin (5.9.10-beta) | NuCode      | Untested              |
| P2        | SPIN2    | FlexSpin (5.9.10-beta) | Native code | Not yet implemented   |
| P1        | SPIN1    | Brad's Spin Tool (any) | Bytecode    | Unsupported           |
| P1, P2    | SPIN1, 2 | Propeller Tool (any)   | Bytecode    | Unsupported           |
| P1, P2    | SPIN1, 2 | PNut (any)             | Bytecode    | Unsupported           |

## Limitations

* Very early in development - may malfunction, or outright fail to build
* No subdir support
* Most methods are currently very naiive - assumptions are made that the sector buffer contains the appropriate data when reading it to populate the various filesystem metadata variables. Since the API methods mostly simply return data, they will 'fail successfully', if the data in the sector buffer isn't appropriate to the method being used.
* FAT12, FAT16 unsupported (TBD)
* No P2/SPIN2 support (TBD)
