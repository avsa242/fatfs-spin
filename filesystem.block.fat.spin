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
    FATOEMNAME      = BOOTRECORD+$03            ' 8
    FATOEMNAME_LEN  = 8                         ' text + NUL
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
        FATMIRROR   = 15
        ACTVFAT     = 8                         ' LOWER 7 BITS
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
    FATNAME_LEN     = 8
    BOOTCODE        = BOOTRECORD+$5A            ' 420 BYTES
    BOOTCODE_LEN    = 420
    MBRSIG          = $1FE
    SIG             = $AA55

    FSINFOSIG1      = $000
    FSISIG1         = $41615252                 ' RRaA
    FSINFOSIG2      = $1E4
    FSISIG2         = $61417272                 ' rrAa
    LASTKNWNFRCLST  = $1E8
    LASTALLOCCLST   = $1EC
    FSINFOSIG3      = $1FC
    FSISIG3         = $AA550000

    FSTYPE_FAT16    = $36                       ' "FAT16"
    FSTYPE_FAT32    = $52                       ' "FAT32"

VAR

    long _ptr_fatimg
    byte _vol_name[VOLNAME_LEN]
    byte _fat_name[FATNAME_LEN]
    byte _boot_code[BOOTCODE_LEN]
    byte _oem_name[FATOEMNAME_LEN+1]

PUB Null{}
' This is not a top-level object

PUB Init(ptr_fatimg)
' Initialize 
    _ptr_fatimg := ptr_fatimg

PUB DeInit{}
    _ptr_fatimg := 0
    bytefill(@_vol_name, 0, VOLNAME_LEN)

PUB ActiveFAT{}: fat_nr
' Active FAT copy
'   Returns: u7
    return (byte[_ptr_fatimg][FLAGS] >> ACTVFAT) & $7F

PUB BackupBootSect{}: s
' Backup boot sector number
'   Returns: word
    bytemove(@s, _ptr_fatimg+BKUPBOOTSECT, 2)

PUB BootCodePtr{}: ptr
' Boot code
'   Returns: pointer to buffer containing boot code
    bytemove(@_boot_code, _ptr_fatimg+BOOTCODE, BOOTCODE_LEN)
    return @_boot_code

PUB FAT32Name{}: s
' FAT name (usually FAT32)
'   Returns: pointer to string buffer
    bytemove(@_fat_name, _ptr_fatimg+FATNAME, FATNAME_LEN)
    return @_fat_name
    
PUB FAT32Version{}: v
' Version of FAT32 driver
'   Returns word [major..minor]
    bytemove(@v, _ptr_fatimg+FAT32VERS, 2)

PUB FATFlags{}: f
' Flags
'   Returns: word
    bytemove(@f, _ptr_fatimg+FLAGS, 2)

PUB FATMirroring{}: state
' Flag indicating FAT mirroring is enabled
'   Returns: boolean
    bytemove(@state, _ptr_fatimg+FLAGS, 2)
    return ((state >> FATMIRROR) & 1) == 0

PUB FInfoSector{}: s
' Filesystem info sector
'   Returns: word
    bytemove(@s, _ptr_fatimg+FSINFOSECT, 2)

PUB FISSigValidMask{}: m | tmp
' FS information signatures valid
'   Returns: 3bit mask [SIG3..SIG2..SIG1]
'       0: signature not valid, 1: signature valid
    bytemove(@tmp, _ptr_fatimg+FSINFOSIG1, 4)
    if tmp == FSISIG1
        m |= %001

    bytemove(@tmp, _ptr_fatimg+FSINFOSIG2, 4)
    if tmp == FSISIG2
        m |= %010

    bytemove(@tmp, _ptr_fatimg+FSINFOSIG3, 4)
    if tmp == FSISIG3
        m |= %100

PUB Heads{}: h
' Number of heads
'   Returns: word
    bytemove(@h, _ptr_fatimg+NRHEADS, 2)

PUB HiddenSectors{}: s
' Number of hidden sectors in partition
'   Returns: long
    bytemove(@s, _ptr_fatimg+NRHIDDENSECT, 4)

PUB LogicalDrvNum{}: n
' Logical drive number of partition
'   Returns: byte
    return byte[_ptr_fatimg][PARTLOGICLDN]

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

PUB OEMName{}: ptr_name
' OEM name string
'   Returns: pointer to string buffer
    bytefill(@_oem_name, 0, FATOEMNAME_LEN+1)
    bytemove(@_oem_name, _ptr_fatimg+FATOEMNAME, FATOEMNAME_LEN+1)
    return @_oem_name

PUB Partition1St{}: sect
' Partition 1 starting offset
'   Returns: long
    bytemove(@sect, _ptr_fatimg+PART1START, 4)

PUB PartSectors{}: s
' Sectors in partition
'   Returns: long
    bytemove(@s, _ptr_fatimg+SECTPERPART, 4)

PUB PartSN{}: s
' Partition serial number
'   Returns: long
    bytemove(@s, _ptr_fatimg+PART_SN, 4)

PUB ReservedSectors{}: r
' Number of reserved sectors
    bytemove(@r, _ptr_fatimg+RSVDSECTS, 2)

PUB RootDirCluster{}: c
' Cluster number of the start of the root directory
'   Returns: long
    bytemove(@c, _ptr_fatimg+ROOTDIRCLUST, 4)

PUB SectorsPerCluster{}: spc
' Sectors per cluster
'   Returns: byte
'   NOTE: Values returned should be powers of 2 only
    return byte[_ptr_fatimg][SECPERCLUST]

PUB SectorsPerFAT{}: spf
' Sectors per FAT
'   Returns: long
    bytemove(@spf, _ptr_fatimg+SECTPERFAT, 4)

PUB SectorsPerTrack{}: spt
' Sectors per track
'   Retunrs: word
    bytemove(@spt, _ptr_fatimg+SECTPERTRK, 2)

PUB Sig0x29Valid{}: bool
' Flag indicating signature byte 0x29 is valid
'   Returns: boolean
    return byte[_ptr_fatimg][SIGX29] == $29

PUB Sig0xAA55Valid{}: bool
' Flag indicating signature word 0xAA55 is valid
'   Returns: boolean
    bytemove(@bool, _ptr_fatimg+MBRSIG, 2)
    return (bool == SIG)

PUB VolumeName{}: ptr_str
' Volume name of FAT partition
'   Returns: pointer to 11-char string
    bytefill(@_vol_name, 0, VOLNAME_LEN)                 ' clear string buffer

    ' copy volume name string from boot record to string buffer
    bytemove(@_vol_name, _ptr_fatimg+VOLNAME, VOLNAME_LEN)
    return @_vol_name

