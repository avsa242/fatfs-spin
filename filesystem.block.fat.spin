{
    --------------------------------------------
    Filename: filesystem.block.fat.spin
    Author: Jesse Burt
    Description: FAT filesystem engine
    Copyright (c) 2022
    Started Aug 1, 2021
    Updated Jun 9, 2022
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
    SIG             = $AA55

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
{ Directory entry }
    'xxx byte array? cache currently open file's dirent, then can write a modified one back to disk
    long _file_nr                               ' file number within root dir
    byte _str_fn[8+1]                           ' filename string
    byte _str_fext[3+1]                         ' filename extension string
    byte _file_attr                             ' file attributes
    byte _time_cr_ms                            ' creation time, milliseconds
    word _time_cr                               ' creation time
    word _date_cr                               ' creation date
    word _date_lastxs                           ' last access date
    word _clust_file_h                          ' first cluster # (MSW, FAT32)
    word _time_lastwr                           ' last written time
    word _date_lastwr                           ' last written date
    word _clust_file_l                          ' last cluster # (LSW)
    long _file_sz                               ' file size

    { current file }
    long _next_clust, _prev_clust

OBJ

    math    : "math.int"

PUB Null{}
' This is not a top-level object

PUB Init(ptr_fatimg)
' Initialize
'   ptr_fatimg: pointer to FAT sector buffer
    _ptr_fatimg := ptr_fatimg

PUB DeInit{}

    _ptr_fatimg := 0

PUB ReadPart{}
' Synchronize partition start offset with pointer in currently active
'   sector buffer (currently hardcoded for partition #1)
    bytemove(@_part_st, _ptr_fatimg+PART1ENT+PART_START, 4)

PUB ReadBPB{}   ' xxx validate signatures before syncing?
' Synchronize BIOS Parameter Block data with currently active sector buffer
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

PUB SectSz{}: b
' Number of bytes per sector
'   Returns: word
    return _sect_sz

PUB Clust2Sect(clust_nr): sect
' Starting sector of cluster number
'   Returns: long
    { convert cluster number to sector number, then offset by where the volume's data
        starts }
    return _data_region + (_sect_per_clust * clust_nr)

PUB ClustLastSect{}: sect
' Last sector of cluster
'   Returns: long
    return (clust2sect(_next_clust) + _sect_per_clust)-1

PUB DirEntNeverUsed{}: bool
' Flag indicating directory entry never used
'   Returns: boolean
    { first character of filename is NUL? Directory entry was never used }
    return (_str_fn[0] == $00)

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
    return _file_attr & $3F

PUB FClose{}
' Close currently open file
    bytefill(@_str_fn, 0, 9)
    bytefill(@_str_fext, 0, 4)
    bytefill(@_file_attr, 0, 1)
    bytefill(@_time_cr_ms, 0, 1)
    wordfill(@_time_cr, 0, 1)
    wordfill(@_date_cr, 0, 1)
    wordfill(@_date_lastxs, 0, 1)
    wordfill(@_clust_file_h, 0, 1)
    wordfill(@_time_lastwr, 0, 1)
    wordfill(@_date_lastwr, 0, 1)
    wordfill(@_clust_file_l, 0, 1)
    longfill(@_file_sz, 0, 1)
    longfill(@_next_clust, 0, 2)

PUB FDateAcc{}: d
' Date file was last accessed
'   Returns: bitmap
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    return _date_lastxs

PUB FDateCreated{}: d
' Date file was created
'   Returns: bitmap
'       bit 15..9: year from 1980 (e.g., 41 = 2021 (80+41-100=21))
'       bit 8..5: month
'       bit 4..0: day
    return _date_cr

PUB FDeleted{}: d
' Flag indicating file is deleted
'   Returns: boolean
    return (_str_fn[0] == FATTR_DEL)            ' FN first char is xE5?

PUB FFirstClust{}: c ' xxx which is faster, the below, or c.word[1] :=, c.word[0] := ... ?
' First cluster of file
'   Returns: long
    return (_clust_file_h << 16) | _clust_file_l

PUB FFirstSect{}: s
' First sector of file
'   Returns: long
    return clust2sect(ffirstclust{})

PUB FEnd{}: p
' Last position in file
    return (_file_sz - 1)

PUB FIsDir{}: bool
' Flag indicating file is a (sub)directory
'   Returns: boolean
    return ((fattrs{} & FATTR_SUBDIR) <> 0)

PUB FIsVolNm{}: bool
' Flag indicating file is the volume name
'   Returns: boolean
    return ((fattrs{} & FATTR_VOL_NM) <> 0)

PUB FName{}: ptr_str
' File name
'   Returns: pointer to string
    return @_str_fn

PUB FNameExt{}: ptr_str
' File name extension
'   Returns: pointer to string
    return @_str_fext

PUB FNextClust{}: c
' Next cluster used by file
    return _next_clust

PUB FNum{}: f_no
' File number within directory
'   Returns: integer
    return _file_nr

PUB FPrevClust{}: c
' Previous cluster used by file
    return _prev_clust

PUB FSize{}: sz
' File size, in bytes
'   Returns: long
    return _file_sz

PUB FModDate{}: d
' Date file was last modified
'   Returns: bitmap
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    return _date_lastwr

PUB FModTime{}: t
' Time file was last modified
'   Returns: bitmap
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    return _time_lastwr

PUB FOpenEnt(fnum)
' Open file by dirent #
'   NOTE: No validation is performed on data in sector buffer
    _file_nr := fnum
    fnum := _ptr_fatimg + (fnum * DIRENT_LEN)   ' calc offset for this file
    bytefill(@_str_fn, 0, 9)                    ' clear string buffers
    bytefill(@_str_fext, 0, 4)

    bytemove(@_str_fn, fnum+DIRENT_FN, 8)
    bytemove(@_str_fext, fnum+DIRENT_EXT, 3)
    bytemove(@_file_attr, fnum+DIRENT_ATTRS, 1)
    bytemove(@_time_cr_ms, fnum+DIRENT_TSC_MS, 1)
    bytemove(@_time_cr, fnum+DIRENT_TSC, 2)
    bytemove(@_date_cr, fnum+DIRENT_DSC, 2)
    bytemove(@_date_lastxs, fnum+DIRENT_DSXS, 2)
    bytemove(@_clust_file_h, fnum+DIRENT_FCLUST_H, 2)
    bytemove(@_time_lastwr, fnum+DIRENT_TSM, 2)
    bytemove(@_date_lastwr, fnum+DIRENT_DSM, 2)
    bytemove(@_clust_file_l, fnum+DIRENT_FCLUST_L, 2)
    bytemove(@_file_sz, fnum+DIRENT_SZ, 4)

    { when opening the file, initialize next and prev cluster numbers with
        the file's first cluster number }
    _next_clust := (_clust_file_h << 16) | (_clust_file_l)
    _prev_clust := _next_clust

PUB FTimeCreated{}: t
' Time file was created
'   Returns: bitmap
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    return _time_cr

PUB FTimeCreated_ms{}: ms
' Timestamp of file creation, in milliseconds
'   Returns: byte
    return _time_cr_ms

PUB FTotalClust{}: c
' Total number of clusters occupied by file
'   Returns: long
'   NOTE: This value is inferred from known file size, bytes per sector,
'       and sectors per cluster
    return 1 #> (_file_sz / (_sect_sz * _sect_per_clust))

PUB FTotalSect{}: s
' Total number of sectors occupied by file
'   Returns: long
'   NOTE: This value is inferred from known file size and bytes per sector
    return filesize{} / _sect_sz

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

PUB SectsPerClust{}: spc
' Sectors per cluster (usually 32)
'   Returns: byte
'   NOTE: Values returned should be powers of 2 only
    return _sect_per_clust

PUB SectsPerFAT{}: spf
' Sectors per FAT
'   Returns: long
    return _sect_per_fat

PUB SetFModDate(date_word)
' Update file modified datestamp
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    _date_lastwr := date_word

PUB SetFModTime(time_word)
' Update file modified timestamp
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    _time_lastwr := time_word

PUB SetFSize(new_sz)
' Set file size (cached copy)
    _file_sz := new_sz

PUB Sig0x29Valid{}: bool
' Flag indicating signature byte 0x29 is valid
'   Returns: boolean
    return _sigx29 == $29

PUB Sig0xAA55Valid{}: bool
' Flag indicating signature word 0xAA55 is valid
'   Returns: boolean
    return (_sigxaa55 == SIG)

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

