CON

    BYTESPERSECT    = 512

' offsets within FS
    MBR             = 0
    BOOTSZ          = 446
    PART1ENT        = $1BE
    PART1STATE      = PART1ENT + $00
    PART1STARTCHS   = PART1ENT + $01            ' 3 bytes
    PART1TYPE       = PART1ENT + $04
    PART1ENDCHS     = PART1ENT + $05
    PART1START      = PART1ENT + $08            ' LE long at this location
    PART1TOTALSECT  = PART1ENT + $0C

    PART2ENT        = $1CE
    PART3ENT        = $1DE
    PART4ENT        = $1EE

    BOOTRECORD      = 0
    JMPBOOT         = BOOTRECORD+$00            ' 3
    OEMNAME         = BOOTRECORD+$03            ' 8
    BYTESPERLOGISECT= BOOTRECORD+$0B            ' 2
    SECPERCLUST     = BOOTRECORD+$0D            ' 1
    RSVDSECTS       = BOOTRECORD+$0E            ' 2
    FATCOPIES       = BOOTRECORD+$10            ' 1
    MEDIADESC       = BOOTRECORD+$15            ' 1
    SECTPERTRK      = BOOTRECORD+$18            ' 2
    NRHEADS         = BOOTRECORD+$1A            ' 2
    NRHIDDENSECT    = BOOTRECORD+$1C            ' 4
    SECTPERPART     = BOOTRECORD+$20            ' 4
    SECTPERFAT      = BOOTRECORD+$24            ' 4
    FLAGS           = BOOTRECORD+$28            ' 2
    FAT32VERS       = BOOTRECORD+$2A            ' 2
    ROOTDIRCLUST    = BOOTRECORD+$2C            ' 4
    FSINFOSECT      = BOOTRECORD+$30            ' 2
    BKUPBOOTSECT    = BOOTRECORD+$32            ' 2
    PARTLOGICLDN    = BOOTRECORD+$40            ' 1
    SIGX29          = BOOTRECORD+$42            ' 1
    PART_SN         = BOOTRECORD+$43            ' LE LONG
    VOLNAME         = BOOTRECORD+$47            ' 11 BYTES
    VOLNAME_LEN     = 11
    FATNAME         = BOOTRECORD+$52            ' 8 BYTES
    BOOTCODE        = BOOTRECORD+$5A            ' 420 BYTES
    MBRSIG          = $1FE
    SIG             = $55AA

    FSTYPE_FAT16    = $36                       ' "FAT16"
    FSTYPE_FAT32    = $52                       ' "FAT32"

VAR

    long _ptr_fatimg
    byte _vol_name[VOLNAME_LEN]

PUB Null{}
' This is not a top-level object

PUB Init(ptr_fatimg)
' Initialize 
    _ptr_fatimg := ptr_fatimg

PUB DeInit{}
    _ptr_fatimg := 0
    bytefill(@_vol_name, 0, VOLNAME_LEN)

PUB Heads{}: h
' Number of heads
'   Returns: word
    bytemove(@h, _ptr_fatimg+NRHEADS, 2)

PUB LogicalSectorBytes{}: b
' Size of logical sector, in bytes
'   Returns: word
'   NOTE: Values returned should be powers of 2 only
    bytemove(@b, _ptr_fatimg+BYTESPERLOGISECT, 2)

PUB MediaType{}: t
' Media type of FAT
    return byte[_ptr_fatimg][MEDIADESC]

PUB NumberFATs{}: n
' Number of copies of FAT
    return byte[_ptr_fatimg][FATCOPIES]

PUB Partition1St{}: sect
' Partition 1 starting offset
'   Returns: long
    bytemove(@sect, _ptr_fatimg+PART1START, 4)

PUB ReservedSectors{}: r
' Number of reserved sectors
    bytemove(@r, _ptr_fatimg+RSVDSECTS, 2)

PUB SectorsPerCluster{}: spc
' Sectors per cluster
'   Returns: byte
'   NOTE: Values returned should be powers of 2 only
    return byte[_ptr_fatimg][SECPERCLUST]

PUB SectorsPerTrack{}: spt
' Sectors per track
'   Retunrs: word
    bytemove(@spt, _ptr_fatimg+SECTPERTRK, 2)

PUB VolumeName{}: ptr_str
' Volume name of FAT partition
'   Returns: pointer to 11-char string
    bytefill(@_vol_name, 0, VOLNAME_LEN)                 ' clear string buffer

    ' copy volume name string from boot record to string buffer
    bytemove(@_vol_name, _ptr_fatimg+VOLNAME, VOLNAME_LEN)
    return @_vol_name

