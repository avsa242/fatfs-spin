CON

    BYTESPERSECT    = 512
    DIRSZ           = 32

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
    'xxx make a complete structure - the size of the sector?

    long _ptr_fatimg
    word _dir_sz

    ' filesystem structure
    long _clust_shf
    long _clust_total
    long _clust_rtdir_st
    long _clust_sz

    long _data_regn
    long _endofchain
    long _sect_fat1_st
    long _part_sn
    long _part_st
    long _rootdir
    long _rootdirend
    long _root_ents
    long _sect_hidn
    long _sect_per_fat
    long _sect_rtdir_st
    long _sig_fsi1
    long _sig_fsi2
    long _sig_fsi3

    word _fat_actv
    word _fat_flags
    word _fat_ver
    word _nr_heads
    word _rsvd
    word _sect_bkupboot
    word _sect_bytes
    word _sect_fsinfo
    word _sect_rsvd

    word _sect_sz 'xxx some method to compare this to requested sect size on Init()?
    word _sect_per_part
    word _sect_per_trk
    word _sigxaa55

    byte _drv_num
    byte _fat_medtyp
    byte _nr_fats
    byte _sect_per_clust 'xxx check is a power of 2
    byte _sigx29
    byte _fstype[8] 'xxx just one byte and store 0 or 1?
    byte _twofats

    byte _boot_code[BOOTCODE_LEN]
    byte _str_fatnm[FATNAME_LEN+1]
    byte _str_oem_nm[FATOEMNAME_LEN+1]
    byte _str_vol_nm[VOLNAME_LEN+1]

OBJ

    math    : "math.int"

PUB Null{}
' This is not a top-level object

PUB Init(ptr_fatimg, sector_sz)
' Initialize
'   ptr_fatimg: pointer to FAT sector buffer
'   sector_sz: data bytes per sector
    _ptr_fatimg := ptr_fatimg
    _dir_sz := DIRSZ

PUB DeInit{}

    _ptr_fatimg := 0

PUB SyncPart{}

    bytemove(@_part_st, _ptr_fatimg+PART1START, 4)

PUB SyncFS{}

    'xxx create 1:1 structure in VAR the full size of the BDB? then a simple byte/word/longmove
    ' of the whole thing
    ' xxx maybe not - not every datum is by itself in a byte
    _fat_actv := (byte[_ptr_fatimg][FLAGS] >> ACTVFAT) & $7F
    bytemove(@_sect_bkupboot, _ptr_fatimg+BKUPBOOTSECT, 2)
    bytemove(@_boot_code, _ptr_fatimg+BOOTCODE, BOOTCODE_LEN)
    bytemove(@_str_fatnm, _ptr_fatimg+FATNAME, FATNAME_LEN)
    bytemove(@_fat_ver, _ptr_fatimg+FAT32VERS, 2)
    bytemove(@_fat_flags, _ptr_fatimg+FLAGS, 2) ' does FATFlags() and FATMirroring()
    bytemove(@_sect_fsinfo, _ptr_fatimg+FSINFOSECT, 2)
    bytemove(@_sig_fsi1, _ptr_fatimg+FSINFOSIG1, 4)
    bytemove(@_sig_fsi2, _ptr_fatimg+FSINFOSIG2, 4)
    bytemove(@_sig_fsi3, _ptr_fatimg+FSINFOSIG3, 4)
    bytemove(@_nr_heads, _ptr_fatimg+NRHEADS, 2)
    bytemove(@_sect_hidn, _ptr_fatimg+NRHIDDENSECT, 4)
    _drv_num := byte[_ptr_fatimg][PARTLOGICLDN]
    bytemove(@_sect_bytes, _ptr_fatimg+BYTESPERLOGISECT, 2)
    _fat_medtyp := byte[_ptr_fatimg][MEDIADESC]
    _nr_fats := byte[_ptr_fatimg][FATCOPIES]
    bytefill(@_str_oem_nm, 0, FATOEMNAME_LEN+1)
     bytemove(@_str_oem_nm, _ptr_fatimg+FATOEMNAME, FATOEMNAME_LEN+1)
'    bytemove(@_part_st, _ptr_fatimg+PART1START, 4)
    bytemove(@_sect_per_part, _ptr_fatimg+SECTPERPART, 4)
    bytemove(@_part_sn, _ptr_fatimg+PART_SN, 4)
    bytemove(@_sect_rsvd, _ptr_fatimg+RSVDSECTS, 2)
    bytemove(@_clust_rtdir_st, _ptr_fatimg+ROOTDIRCLUST, 4)
    _sect_per_clust := byte[_ptr_fatimg][SECPERCLUST]
    bytemove(@_sect_per_fat, _ptr_fatimg+SECTPERFAT, 4)
    bytemove(@_sect_per_trk, _ptr_fatimg+SECTPERTRK, 2)
    _sigx29 := byte[_ptr_fatimg][SIGX29]
    bytemove(@_sigxaa55, _ptr_fatimg+MBRSIG, 2)
    bytefill(@_str_vol_nm, 0, VOLNAME_LEN)                 ' clear string buffer
    ' copy volume name string from boot record to string buffer
    bytemove(@_str_vol_nm, _ptr_fatimg+VOLNAME, VOLNAME_LEN)

    _sect_fat1_st := _part_st + _sect_rsvd
    _clust_shf := math.log2(_sect_per_clust)

    _root_ents := 16 << _clust_shf 'xxx where's 16 come from?
    _data_regn := (_sect_fat1_st + 2 * _sect_per_fat) - 2 * _sect_per_clust
    _rootdir := (_data_regn << _clust_rtdir_st << _clust_shf) << math.log2(_sect_sz)
    _rootdirend := _rootdir + (_root_ents << math.log2(_dir_sz))
    _clust_total := ((_sect_per_part - _data_regn + _part_st) >> _clust_shf)
    _sect_rtdir_st := _sect_fat1_st + (_nr_fats * _sect_per_fat)

PUB ActiveFAT{}: fat_nr
' Active FAT copy
'   Returns: u7
    return _fat_actv & $7F

PUB BackupBootSect{}: s
' Backup boot sector number
'   Returns: word
    return _sect_bkupboot

PUB BootCodePtr{}: ptr
' Boot code
'   Returns: pointer to buffer containing boot code
    return @_boot_code

PUB FAT32Name{}: s
' FAT name (usually FAT32)
'   Returns: pointer to string buffer
    return @_str_fatnm
    
PUB FAT32Version{}: v
' Version of FAT32 driver
'   Returns word [major..minor]
    return _fat_ver

PUB FATFlags{}: f
' Flags
'   Returns: word
    return _fat_flags

PUB FATMirroring{}: state
' Flag indicating FAT mirroring is enabled
'   Returns: boolean
    return ((_fat_flags >> FATMIRROR) & 1) == 0

PUB FInfoSector{}: s
' Filesystem info sector
'   Returns: word
    return _sect_fsinfo

PUB FISSigValidMask{}: m
' FS information signatures valid
'   Returns: 3bit mask [SIG3..SIG2..SIG1]
'       0: signature not valid, 1: signature valid
    if _sig_fsi1 == FSISIG1
        m |= %001

    if _sig_fsi2 == FSISIG2
        m |= %010

    if _sig_fsi3 == FSISIG3
        m |= %100

PUB Heads{}: h
' Number of heads
'   Returns: word
    return _nr_heads

PUB HiddenSectors{}: s
' Number of hidden sectors in partition
'   Returns: long
    return _sect_hidn

PUB LogicalDrvNum{}: n
' Logical drive number of partition
'   Returns: byte
    return _drv_num

PUB LogicalSectorBytes{}: b
' Size of logical sector, in bytes
'   Returns: word
'   NOTE: Values returned should be powers of 2 only
    return _sect_bytes

PUB MediaType{}: t
' Media type of FAT
    return _fat_medtyp

PUB NumberFATs{}: n
' Number of copies of FAT
    return _nr_fats

PUB OEMName{}: ptr_name
' OEM name string
'   Returns: pointer to string buffer
    return @_str_oem_nm

PUB PartStart{}: sect
' Partition starting offset
'   Returns: long
    return _part_st

PUB PartSectors{}: s
' Sectors in partition
'   Returns: long
    return _sect_per_part

PUB PartSN{}: s
' Partition serial number
'   Returns: long
    return _part_sn

PUB ReservedSectors{}: r
' Number of reserved sectors
    return _sect_rsvd

PUB RootDirCluster{}: c
' Cluster number of the start of the root directory
'   Returns: long
    return _clust_rtdir_st

PUB RootDirSector{}: s
' Starting sector of root directory entry
'   Returns: long
    return _sect_rtdir_st

PUB SectorsPerCluster{}: spc
' Sectors per cluster
'   Returns: byte
'   NOTE: Values returned should be powers of 2 only
    return _sect_per_clust

PUB SectorsPerFAT{}: spf
' Sectors per FAT
'   Returns: long
    return _sect_per_fat

PUB SectorsPerTrack{}: spt
' Sectors per track
'   Retunrs: word
    return _sect_per_trk

PUB Sig0x29Valid{}: bool
' Flag indicating signature byte 0x29 is valid
'   Returns: boolean
    return _sigx29 == $29

PUB Sig0xAA55Valid{}: bool
' Flag indicating signature word 0xAA55 is valid
'   Returns: boolean
    return (_sigxaa55 == SIG)

PUB VolumeName{}: ptr_str
' Volume name of FAT partition
'   Returns: pointer to 11-char string
    return @_str_vol_nm

