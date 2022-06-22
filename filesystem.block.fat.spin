{
    --------------------------------------------
    Filename: filesystem.block.fat.spin
    Author: Jesse Burt
    Description: FAT filesystem engine
    Copyright (c) 2022
    Started Aug 1, 2021
    Updated Jun 22, 2022
    See end of file for terms of use.
    --------------------------------------------
}

CON

{ FAT32 structure on disk }

    { offsets within FS }
    BOOT_LEN        = 446
    MBR             = 0                         ' these two refer to the same
    BOOTREC         = 0
    JMPBOOT         = BOOTREC+$00               ' 3 bytes
    FATOEMNM        = BOOTREC+$03               ' 8
    FATOEMNM_LEN    = 8                         ' text + NUL
    BYTESPERLSECT   = BOOTREC+$0B               ' 2
    SECPERCLUST     = BOOTREC+$0D               ' 1
    RSVDSECTS       = BOOTREC+$0E               ' 2
    FATCOPIES       = BOOTREC+$10               ' 1
    MEDIADESC       = BOOTREC+$15               ' 1
    SECTPERTRK      = BOOTREC+$18               ' 2
    NRHEADS         = BOOTREC+$1A               ' 2
    NRHIDDENSECT    = BOOTREC+$1C               ' 4
    SECTPERPART     = BOOTREC+$20               ' 4
    SECTPERFAT      = BOOTREC+$24               ' 4
    FLAGS           = BOOTREC+$28               ' 2
    FATMIRROR       = 15
    ACTVFAT         = 8                         ' LOWER 7 BITS
    FAT32VERS       = BOOTREC+$2A               ' 2
    RTDIRCLUST      = BOOTREC+$2C               ' 4
    FS_INFOSECT     = BOOTREC+$30               ' 2
    BKUPBOOTSECT    = BOOTREC+$32               ' 2
    PARTLOGICLDN    = BOOTREC+$40               ' 1
    SIGX29          = BOOTREC+$42               ' 1
    PART_SN         = BOOTREC+$43               ' LE LONG
    VOLNM           = BOOTREC+$47               ' 11 BYTES
    VOLNM_LEN       = 11
    FATNM           = BOOTREC+$52               ' 8 BYTES
    FATNM_LEN       = 8
    BOOTCODE        = BOOTREC+$5A               ' 420 BYTES
    BOOTCODE_LEN    = 420

    PARTENT_LEN     = 16
    PART1ENT        = $1BE
    PART2ENT        = $1CE
    PART3ENT        = $1DE
    PART4ENT        = $1EE
    PART_STATE      = $00
    PART_STARTCHS   = $01                       ' 3 bytes
    PART_TYPE       = $04
    PART_ENDCHS     = $05
    PART_START      = $08                       ' LE long at this location
    PART_TOTALSECT  = $0C
    P1_STATE        = PART1ENT+PART_STATE
    P1_STARTCHS     = PART1ENT+PART_STARTCHS
    P1_TYPE         = PART1ENT+PART_TYPE
    P1_ENDCHS       = PART1ENT+PART_ENDCHS
    P1_START        = PART1ENT+PART_START
    P1_TOTALSECT    = PART1ENT+PART_TOTALSECT

    MBRSIG          = $1FE
    PARTSIG         = $1FE
    SIG_WORD        = $AA55

    { partition boot record }

    FSTYPE_FAT16    = $36                       ' "FAT16"
    FSTYPE_FAT32    = $52                       ' "FAT32"

    { FS information sector }
    FSINFOSIG1      = $000
    FSISIG1         = $41_61_52_52              ' RRaA
    FSINFOSIG2      = $1E4
    FSISIG2         = $61_41_72_72              ' rrAa
    LSTKNWNFRCLUST  = $1E8
    LSTALLOCCLUST   = $1EC
    FSINFOSIG3      = $1FC
    FSISIG3         = $AA550000

    CLUST_EOC0      = $0FFF_FFF8
    CLUST_EOC       = $0FFF_FFFF                ' end of chain (last cluster)

VAR

    long _ptr_fatimg

    { BIOS parameter block (BPB) }
    long _clust_shf
    long _clust_total
    long _clust_rtdir_st
    long _clust_sz

    long _data_region
    long _endofchain
    long _sect_fat1_st
    long _part_st
    long _rootdir
    long _rootdirend
    long _root_ents
    long _sect_per_fat
    long _sect_rtdir_st
    long _sig_fsi1
    long _sig_fsi2
    long _sig_fsi3

    word _fat_actv
    word _fat_ver
    word _rsvd
    word _sect_sz
    word _sect_rsvd

    word _sect_per_part
    word _sigxaa55

    byte _nr_fats
    byte _sect_per_clust
    byte _sigx29

    byte _str_fatnm[FATNM_LEN+1]
    byte _str_vol_nm[VOLNM_LEN+1]

CON
{ Directory entry ("dirent") }
    { offsets within root dir sector }
    DIRENT_LEN      = 32
    DIRENTS         = 16                        ' dirents per sector
    DIRENT_FN       = $00                       ' filename
    DIRENT_EXT      = $08                       ' filename extension
    DIRENT_ATTRS    = $0B                       ' file attributes
    DIRENT_TSC_MS   = $0D                       ' creation timestamp (ms)
    DIRENT_TSC      = $0E                       ' creation timestamp
    DIRENT_DSC      = $10                       ' creation datestamp
    DIRENT_DSXS     = $12                       ' last accessed datestamp
    DIRENT_FCLUST_H = $14                       ' first cluster (MSW, FAT32)
    DIRENT_TSM      = $16                       ' last written timestamp
    DIRENT_DSM      = $18                       ' last written datestamp
    DIRENT_FCLUST_L = $1A                       ' first cluster (LSW)
    DIRENT_SZ       = $1C                       ' size

    { file attributes }
    FATTR_ARC       = 1 << 5                    ' is an archive
    FATTR_SUBDIR    = 1 << 4                    ' is a sub-directory
    FATTR_VOL_NM    = 1 << 3                    ' is the volume name
    FATTR_SYSFIL    = 1 << 2                    ' is a system file
    FATTR_HIDDEN    = 1 << 1                    ' is hidden
    FATTR_WRPROT    = 1                         ' is write-protected
    FATTR_DEL       = $E5                       ' FN first char, if deleted

VAR
{ Directory entry cache }
    long _file_nr                               ' file number within root dir
    byte _dirent[DIRENT_LEN]

    { current file }
    long _next_clust, _prev_clust, _last_clust
    long _fseek_pos, _fseek_sect
    byte _fname[8+1], _fext[3+1], _fmode

OBJ

    math    : "math.int"

PUB Null{}
' This is not a top-level object

PUB Init(ptr_fatimg)
' Initialize
'   ptr_fatimg: pointer to FAT sector buffer
    _ptr_fatimg := ptr_fatimg
    _file_nr := -1

PUB DeInit{}

    _ptr_fatimg := 0

PUB ReadPart{} | tmp
' Synchronize partition start offset with pointer in currently active
'   sector buffer (currently hardcoded for partition #1)

    { validate the image first: don't read metadata unless it contains the FAT signature }
    wordmove(@tmp, _ptr_fatimg+MBRSIG, 1)
    if (tmp <> SIG_WORD)
        return -1

    bytemove(@_part_st, _ptr_fatimg+PART1ENT+PART_START, 4)

PUB ReadBPB{} | tmp
' Synchronize BIOS Parameter Block data with currently active sector buffer

    { validate the image first: don't read metadata unless it contains the FAT signature }
    wordmove(@tmp, _ptr_fatimg+MBRSIG, 1)
    if (tmp <> SIG_WORD)
        return -1

    _fat_actv := (byte[_ptr_fatimg][FLAGS] >> ACTVFAT) & $7F
    bytemove(@_str_fatnm, _ptr_fatimg+FATNM, FATNM_LEN)
    bytemove(@_fat_ver, _ptr_fatimg+FAT32VERS, 2)

    bytemove(@_sig_fsi1, _ptr_fatimg+FSINFOSIG1, 4)
    bytemove(@_sig_fsi2, _ptr_fatimg+FSINFOSIG2, 4)
    bytemove(@_sig_fsi3, _ptr_fatimg+FSINFOSIG3, 4)
    bytemove(@_sect_sz, _ptr_fatimg+BYTESPERLSECT, 2)
    _nr_fats := byte[_ptr_fatimg][FATCOPIES]
    bytemove(@_sect_per_part, _ptr_fatimg+SECTPERPART, 4)
    bytemove(@_sect_rsvd, _ptr_fatimg+RSVDSECTS, 2)
    bytemove(@_clust_rtdir_st, _ptr_fatimg+ROOTDIRCLUST, 4)
    _sect_per_clust := byte[_ptr_fatimg][SECPERCLUST]
    bytemove(@_sect_per_fat, _ptr_fatimg+SECTPERFAT, 4)
    _sigx29 := byte[_ptr_fatimg][SIGX29]
    bytemove(@_sigxaa55, _ptr_fatimg+MBRSIG, 2)
    bytefill(@_str_vol_nm, 0, VOLNM_LEN)      ' clear string buffer
    { copy volume name string from boot record to string buffer }
    bytemove(@_str_vol_nm, _ptr_fatimg+VOLNM, VOLNM_LEN)

    _sect_fat1_st := _part_st + _sect_rsvd
    _clust_shf := math.log2(_sect_per_clust)

    _root_ents := 16 << _clust_shf 'xxx where's 16 come from?
    { block data starts at }
    _data_region := (_sect_fat1_st + 2 * _sect_per_fat) - 2 * _sect_per_clust
    _rootdir := (_data_region << _clust_rtdir_st << _clust_shf) << math.log2(_sect_sz)
    _rootdirend := _rootdir + (_root_ents << math.log2(DIRENT_LEN))
    _clust_total := ((_sect_per_part - _data_region + _part_st) >> _clust_shf)
    _sect_rtdir_st := _sect_fat1_st + (_nr_fats * _sect_per_fat)
    _clust_sz := _sect_per_clust * _sect_sz

PUB ActiveFAT{}: fat_nr
' Active FAT copy
'   Returns: u7
    return _fat_actv & $7F

PUB ClustSz{}: b
' Number of bytes per cluster:
'   Returns: long
    return _clust_sz

PUB Clust2Sect(clust_nr): sect
' Starting sector of cluster number
'   Returns: long
    { convert cluster number to sector number, then offset by where the volume's data
        starts }
    return _data_region + (_sect_per_clust * clust_nr)

PUB ClustIsEOC(clust_nr): iseoc
' Flag indicating cluster is end-of-chain
    return (lookdown(clust_nr: CLUST_EOC0..CLUST_EOC) <> 0)

PUB ClustLastSect{}: sect
' Last sector of cluster
'   Returns: long
    return (clust2sect(_next_clust) + _sect_per_clust)-1

PUB ClustNum2FATSect(cl_nr): fat_sect
' Convert cluster number to _relative_ FAT sector #
'   Returns: long
    return (cl_nr >> 7)

PUB ClustNum2Offs(cl_nr): sect_offs
' Convert cluster number to byte offset within FAT sector
'   Returns:
'       0..508
    return ((cl_nr & $7f) * 4)

PUB ClustRd(cl_nr): val
' Read cluster number from FAT entry
'   Returns: cluster number
    val := 0
    bytemove(@val, (_ptr_fatimg + clustnum2offs(cl_nr)), 4)

PUB ClustWr(cl_nr, val)
' Write cluster number to FAT entry
'   cl_nr: FAT entry
'   val: cluster number to write into entry
    bytemove((_ptr_fatimg + clustnum2offs(cl_nr)), @val, 4)

PUB Dirent2AbsSect(ent_nr): sect
' Convert directory entry number to _absolute_ directory sector number
'   (dirents are 32 bytes in length, for a total of 16 entries in a 512-byte sector;
'   (0..15) / 16 = 0, (16..31) / 16 = 1, etc
    return (rootdirsect{} + (ent_nr >> 4))

PUB Dirent2Sect(ent_nr): sect
' Convert directory entry number to _relative_ directory sector number
'   (dirents are 32 bytes in length, for a total of 16 entries in a 512-byte sector;
'   (0..15) / 16 = 0, (16..31) / 16 = 1, etc
    return (ent_nr >> 4)

PUB DirentNeverUsed{}: bool
' Flag indicating directory entry never used
'   Returns: boolean
    { first character of filename is NUL? Directory entry was never used }
    return (_dirent[0] == $00)

PUB DirentStart(ent_nr)
' Offset within directory entry sector, given entry number
'   Returns: integer
    return (ent_nr * DIRENT_LEN)

PUB FAT1Start{}: s
' Starting sector of FAT1
'   Returns: long
    return _sect_fat1_st

PUB FAT32Name{}: s
' FAT name (usually FAT32)
'   Returns: pointer to string buffer
    return @_str_fatnm
    
PUB FAT32Version{}: v
' Version of FAT32 driver
'   Returns word [major..minor]
    return _fat_ver

PUB FATEnt2Clust(fat_chn): cl
' Get cluster number pointed to by FAT entry/chain
    bytemove(@cl, _ptr_fatimg+(fat_chn * 4), 4)

PUB FAttrs{}: a
' File attributes
'   Returns: bitmap
'       bit 5: is an archive
'           4: is a subdirectory
'           3: is the volume name
'           2: is a system file
'           1: is a hidden file
'           0: is write-protected
    return _dirent[DIRENT_ATTRS] & $3f

PUB FClose{}
' Close currently open file
    { clear dirent cache, reset file number, reset seek pointers and file open mode }
    bytefill(@_dirent, 0, DIRENT_LEN)
    _file_nr := -1
    _fseek_pos := 0
    _fseek_sect := 0
    _fmode := 0
    return 0

PUB FDateAcc{}: dsxs
' Date file was last accessed
'   Returns: bitmap
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    bytemove(@dsxs, @_dirent+DIRENT_DSXS, 2)

PUB FDateCreated{}: dsc
' Date file was created
'   Returns: bitmap
'       bit 15..9: year from 1980 (e.g., 41 = 2021 (80+41-100=21))
'       bit 8..5: month
'       bit 4..0: day
    bytemove(@dsc, @_dirent+DIRENT_DSC, 2)

PUB FDeleted{}: d
' Flag indicating file is deleted
'   Returns: boolean
    return (_dirent[0] == FATTR_DEL)            ' FN first char is $E5?

PUB FFirstClust{}: fcl
' First cluster of file
'   Returns: long
    bytemove(@fcl.byte[2], @_dirent[DIRENT_FCLUST_H], 2)
    bytemove(@fcl.byte[0], @_dirent[DIRENT_FCLUST_L], 2)

PUB FFirstSect{}: s
' First sector of file
'   Returns: long
    return clust2sect(ffirstclust{})

PUB FEnd{}: p
' Last position in file
    return (fsize{}-1)

PUB FIsDir{}: bool
' Flag indicating file is a (sub)directory
'   Returns: boolean
    return ((fattrs{} & FATTR_SUBDIR) <> 0)

PUB FIsVolNm{}: bool
' Flag indicating file is the volume name
'   Returns: boolean
    return ((fattrs{} & FATTR_VOL_NM) <> 0)

PUB FLastClust{}: cl_nr
' Last cluster number of file
    return _last_clust

PUB FName{}: ptr_str
' File name
'   Returns: pointer to string
    bytefill(@_fname, 0, 9)
    bytemove(@_fname, @_dirent, 8)
    return @_fname

PUB FNameExt{}: ptr_str
' File name extension
'   Returns: pointer to string
    bytefill(@_fext, 0, 4)
    bytemove(@_fext, @_dirent+DIRENT_EXT, 3)
    return @_fext

PUB FNextClust{}: c
' Next cluster used by file
    return _next_clust

PUB FNumber{}: f_no
' File number within directory
'   Returns: integer
    return _file_nr

PUB FPrevClust{}: c
' Previous cluster used by file
    return _prev_clust

PUB FSetAttrs(attrs)
' Set file attributes (byte)
    _dirent[DIRENT_ATTRS] := attrs

PUB FSetDateAccessed(dsxs)
' Set file last access date (word)
    bytemove(@_dirent+DIRENT_DSXS, @dsxs, 2)

PUB FSetDateCreated(dsc)
' Set file creation date (word)
    bytemove(@_dirent+DIRENT_DSC, @dsc, 2)

PUB FSetDateMod(dsm)
' Set file last modified date (word)
    bytemove(@_dirent+DIRENT_DSM, @dsm, 2)

PUB FSetDeleted{}
' Mark file as deleted
    _dirent[0] := FATTR_DEL

PUB FSetExt(ptr_str)
' Set filename extension (pointer to 3-byte string)
    bytemove(@_dirent+DIRENT_EXT, ptr_str, 3)

PUB FSetFirstClust(clust_nr)
' Set file first cluster number (long)
    bytemove(@_dirent+DIRENT_FCLUST_H, @clust_nr.byte[2], 2)
    bytemove(@_dirent+DIRENT_FCLUST_L, @clust_nr.byte[0], 2)

PUB FSetFname(ptr_str)
' Set filename (pointer to 8-byte string)
    bytemove(@_dirent, ptr_str, 8)

PUB FSetSize(sz)
' Set file size (long)
    bytemove(@_dirent+DIRENT_SZ, @sz, 4)

PUB FSetTimeCreated(tsc)
' Set file creation time (word)
    bytemove(@_dirent+DIRENT_TSC, @tsc, 2)

PUB FSetTimeMod(tsm)
' Set file last modified time (word)
    bytemove(@_dirent+DIRENT_TSM, @tsm, 2)

PUB FSize{}: sz
' File size, in bytes
'   Returns: long
    bytemove(@sz, @_dirent+DIRENT_SZ, 4)

PUB FModDate{}: dsm
' Date file was last modified
'   Returns: bitmap
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    bytemove(@dsm, @_dirent+DIRENT_DSM, 2)

PUB FModTime{}: tsm
' Time file was last modified
'   Returns: bitmap
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    bytemove(@tsm, @_dirent+DIRENT_TSM, 2)

PUB FTimeCreated{}: tsc
' Time file was created
'   Returns: bitmap (word)
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    bytemove(@tsc, @_dirent+DIRENT_TSC, 2)

PUB FTimeCreated_ms{}: ms
' Timestamp of file creation, in milliseconds
'   Returns: byte
    return _dirent[DIRENT_TSC_MS]

PUB FTotalClust{}: c
' Total number of clusters occupied by file
'   Returns: long
'   NOTE: This value is inferred from known file size, bytes per sector,
'       and sectors per cluster
    return 1 #> (fsize{} / (_sect_sz * _sect_per_clust))

PUB FTotalSect{}: s
' Total number of sectors occupied by file
'   Returns: long
'   NOTE: This value is inferred from known file size and bytes per sector
    return (filesize{} / _sect_sz)

PUB LogicalSectSz{}: b
' Size of logical sector, in bytes
'   Returns: word
'   NOTE: Values returned should be powers of 2 only
    return _sect_sz

PUB NextClust{}: c
' Get next cluster number in chain
'   Returns: long
    c := 0
    { update the next and prev cluster pointers, by following the chain
        read from the FAT }
    _prev_clust := _next_clust
    _next_clust := fatent2clust(_prev_clust & $7f)
    if (_next_clust == CLUST_EOC)               ' End-of-Chain marker reached
        return -1                               '   no more clusters
    return _next_clust

PUB PartStart{}: sect
' Partition starting offset
'   Returns: long
    return _part_st

PUB ReadDirEnt(fnum) | sect_offs
' Read metadata about file from dirent # fnum
'   NOTE: No validation is performed on data in sector buffer
    _file_nr := fnum

    { copy the entry into the dirent cache }
    sect_offs := _ptr_fatimg + direntstart(fnum)
    bytefill(@_dirent, 0, DIRENT_LEN)
    bytemove(@_dirent, sect_offs, DIRENT_LEN)
    bytemove(@_fname, @_dirent, 8)
    bytemove(@_fext, @_dirent+DIRENT_EXT, 3)

    { when opening the file, initialize next and prev cluster numbers with
        the file's first cluster number }
    bytemove(@_next_clust.byte[2], @_dirent+DIRENT_FCLUST_H, 2)
    bytemove(@_next_clust.byte[0], @_dirent+DIRENT_FCLUST_L, 2)
    _prev_clust := _next_clust

PUB RootDirClust{}: c
' Cluster number of the start of the root directory
'   Returns: long
    return _clust_rtdir_st

PUB RootDirSect{}: s
' Starting sector of root directory entry
'   Returns: long
    return _sect_rtdir_st

PUB Sect2Clust(sect): clust
' Map given sector number to a cluster number
'   Returns: long
    clust := ((sect - _data_region) / _sect_per_clust)

PUB SectOffs2Clust(sect_offs)
' Convert byte offset within FAT sector to cluster number
'   Valid values: 0..508
    ifnot (lookdown(sect_offs: 0..508))
        return -1
    return (sect_offs / 4)

PUB SectOffs2AbsClust(sect_offs, fat_sect)
' Convert byte offset within FAT sector to absolute cluster number
'   Valid values:
'       sect_offs: 0..508
'       fat_sect: 0..n (n is dependent on total number of FAT sectors in the filesystem)
    ifnot (lookdown(sect_offs: 0..508))
        return -1
    return ((fat_sect << 7) + (sect_offs / 4))

PUB SectsPerClust{}: spc
' Sectors per cluster (usually 32)
'   Returns: byte
'   NOTE: Values returned should be powers of 2 only
    return _sect_per_clust

PUB SectsPerFAT{}: spf
' Sectors per FAT
'   Returns: long
    return _sect_per_fat

PUB SectSz{}: b
' Number of bytes per sector
'   Returns: word
    return _sect_sz

PUB Sig0x29Valid{}: bool
' Flag indicating signature byte 0x29 is valid
'   Returns: boolean
    return _sigx29 == $29

PUB Sig0xAA55Valid{}: bool
' Flag indicating signature word 0xAA55 is valid
'   Returns: boolean
    return (_sigxaa55 == SIG_WORD)

PUB VolName{}: ptr_str
' Volume name of FAT partition
'   Returns: pointer to 11-char string
    return @_str_vol_nm

DAT
{
    --------------------------------------------------------------------------------------------------------
    TERMS OF USE: MIT License

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
    associated documentation files (the "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
    following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial
    portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
    LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    --------------------------------------------------------------------------------------------------------
}

