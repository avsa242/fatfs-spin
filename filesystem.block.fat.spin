{
    --------------------------------------------
    Filename: filesystem.block.fat.spin
    Author: Jesse Burt
    Description: FAT filesystem engine
    Copyright (c) 2021
    Started Aug 1, 2021
    Updated Sep 19, 2021
    See end of file for terms of use.
    --------------------------------------------
}

CON

' FAT32 structure on disk

    ' offsets within FS
    BOOT_LEN        = 446
    MBR             = 0                         ' these two refer to the same
    BOOTRECORD      = 0
    JMPBOOT         = BOOTRECORD+$00            ' 3 bytes
    FATOEMNAME      = BOOTRECORD+$03            ' 8
    FATOEMNAME_LEN  = 8                         ' text + NUL
    BYTESPERLSECT   = BOOTRECORD+$0B            ' 2
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
    FATMIRROR       = 15
    ACTVFAT         = 8                         ' LOWER 7 BITS
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

    MBRSIG          = $1FE
    SIG             = $AA55

    ' partition boot record

    FSTYPE_FAT16    = $36                       ' "FAT16"
    FSTYPE_FAT32    = $52                       ' "FAT32"

    ' FS information sector
    FSINFOSIG1      = $000
    FSISIG1         = $41_61_52_52              ' RRaA
    FSINFOSIG2      = $1E4
    FSISIG2         = $61_41_72_72              ' rrAa
    LSTKNWNFRCLUST  = $1E8
    LSTALLOCCLUST   = $1EC
    FSINFOSIG3      = $1FC
    FSISIG3         = $AA550000

    MRKR_EOC        = $0FFF_FFFF                ' end of chain (last cluster)

VAR

    long _ptr_fatimg

    ' BIOS parameter block (BPB)
    long _clust_shf
    long _clust_total
    long _clust_rtdir_st
    long _clust_sz

    long _data_region
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
    word _sect_sz
    word _sect_fsinfo
    word _sect_rsvd

'    word _sect_sz 'xxx some method to compare this to requested sect size on Init()?
    word _sect_per_part
    word _sect_per_trk
    word _sigxaa55

    byte _drv_num
    byte _fat_medtyp
    byte _nr_fats
    byte _sect_per_clust 'xxx check is a power of 2
    byte _sigx29
    byte _fstype[8] 'xxx just one byte and store 0 or 1?    'XXX not used
    byte _twofats 'XXX not used

    byte _boot_code[BOOTCODE_LEN]
    byte _str_fatnm[FATNAME_LEN+1]
    byte _str_oem_nm[FATOEMNAME_LEN+1]
    byte _str_vol_nm[VOLNAME_LEN+1]

CON
' Directory entry ("dirent")
    ' offsets within root dir sector
    DIRENT_LEN      = 32
    FNAME           = $00                       ' filename
    FNEXT           = $08                       ' filename extension
    FATTR           = $0B                       ' file attributes
    FCREATE_TMS     = $0D                       ' creation timestamp (ms)
    FCREATE_T       = $0E                       ' creation timestamp
    FCREATE_D       = $10                       ' creation datestamp
    FLSTXS_D        = $12                       ' last accessed datestamp
    FCLUST_H        = $14                       ' first cluster (MSW, FAT32)
    FLSTWR_T        = $16                       ' last written timestamp
    FLSTWR_D        = $18                       ' last written datestamp
    FCLUST_L        = $1A                       ' first cluster (LSW)
    FSZ             = $1C                       ' size

    ' file attributes
    FARC            = 1 << 5                    ' is an archive
    FSUBDIR         = 1 << 4                    ' is a sub-directory
    FVOL_NM         = 1 << 3                    ' is the volume name
    FSYSFIL         = 1 << 2                    ' is a system file
    FHIDNFIL        = 1 << 1                    ' is hidden
    FWRPROT         = 1                         ' is write-protected
    FDEL_MARKER     = $E5                       ' FN first char, if deleted

VAR
' Directory entry
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

    ' current file
    '   is open
    '   current seek position
    '   bytes remaining from seek to end
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

PUB SyncPart{}
' Synchronize partition start offset with pointer in currently active
'   sector buffer (currently hardcoded for partition #1)
    bytemove(@_part_st, _ptr_fatimg+PART1ENT+PART_START, 4)

PUB SyncBPB{}   ' xxx validate signatures before syncing?
' Synchronize BIOS Parameter Block data with currently active sector buffer
    _fat_actv := (byte[_ptr_fatimg][FLAGS] >> ACTVFAT) & $7F
    bytemove(@_sect_bkupboot, _ptr_fatimg+BKUPBOOTSECT, 2)
    bytemove(@_boot_code, _ptr_fatimg+BOOTCODE, BOOTCODE_LEN)
    bytemove(@_str_fatnm, _ptr_fatimg+FATNAME, FATNAME_LEN)
    bytemove(@_fat_ver, _ptr_fatimg+FAT32VERS, 2)

    ' updates FATFlags() and FATMirroring()
    bytemove(@_fat_flags, _ptr_fatimg+FLAGS, 2)
    bytemove(@_sect_fsinfo, _ptr_fatimg+FSINFOSECT, 2)
    bytemove(@_sig_fsi1, _ptr_fatimg+FSINFOSIG1, 4)
    bytemove(@_sig_fsi2, _ptr_fatimg+FSINFOSIG2, 4)
    bytemove(@_sig_fsi3, _ptr_fatimg+FSINFOSIG3, 4)
    bytemove(@_nr_heads, _ptr_fatimg+NRHEADS, 2)
    bytemove(@_sect_hidn, _ptr_fatimg+NRHIDDENSECT, 4)
    _drv_num := byte[_ptr_fatimg][PARTLOGICLDN]
    bytemove(@_sect_sz, _ptr_fatimg+BYTESPERLSECT, 2)
    _fat_medtyp := byte[_ptr_fatimg][MEDIADESC]
    _nr_fats := byte[_ptr_fatimg][FATCOPIES]
    bytefill(@_str_oem_nm, 0, FATOEMNAME_LEN+1)
     bytemove(@_str_oem_nm, _ptr_fatimg+FATOEMNAME, FATOEMNAME_LEN+1)
    bytemove(@_sect_per_part, _ptr_fatimg+SECTPERPART, 4)
    bytemove(@_part_sn, _ptr_fatimg+PART_SN, 4)
    bytemove(@_sect_rsvd, _ptr_fatimg+RSVDSECTS, 2)
    bytemove(@_clust_rtdir_st, _ptr_fatimg+ROOTDIRCLUST, 4)
    _sect_per_clust := byte[_ptr_fatimg][SECPERCLUST]
    bytemove(@_sect_per_fat, _ptr_fatimg+SECTPERFAT, 4)
    bytemove(@_sect_per_trk, _ptr_fatimg+SECTPERTRK, 2)
    _sigx29 := byte[_ptr_fatimg][SIGX29]
    bytemove(@_sigxaa55, _ptr_fatimg+MBRSIG, 2)
    bytefill(@_str_vol_nm, 0, VOLNAME_LEN)      ' clear string buffer
    ' copy volume name string from boot record to string buffer
    bytemove(@_str_vol_nm, _ptr_fatimg+VOLNAME, VOLNAME_LEN)

    _sect_fat1_st := _part_st + _sect_rsvd
    _clust_shf := math.log2(_sect_per_clust)

    _root_ents := 16 << _clust_shf 'xxx where's 16 come from?
    _data_region := (_sect_fat1_st + 2 * _sect_per_fat) - 2 * _sect_per_clust
    _rootdir := (_data_region << _clust_rtdir_st << _clust_shf) << math.log2(_sect_sz)
    _rootdirend := _rootdir + (_root_ents << math.log2(DIRENT_LEN))
    _clust_total := ((_sect_per_part - _data_region + _part_st) >> _clust_shf)
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
'   Returns: pointer to buffer containing boot code 'XXX indicate how many bytes it is
    return @_boot_code

PUB BytesPerClust{}: b
' Number of bytes per cluster:
'   Returns: long
    return _sect_per_clust * _sect_sz

PUB BytesPerSect{}: b
' Number of bytes per sector
'   Returns: word
    return _sect_sz

PUB Clust2Sect(clust_nr): sect
' Starting sector of cluster number
'   Returns: long
    return _data_region + (_sect_per_clust * clust_nr)

PUB ClustLastSect{}: sect
' Last sector of cluster
'   Returns: long
    return (clust2sect(_next_clust) + _sect_per_clust)-1

PUB DirEntNeverUsed{}: bool
' Flag indicating directory never used
'   Returns: boolean
    return _str_fn[0] == $00

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

PUB FATFlags{}: f
' Flags
'   Returns: word
    return _fat_flags

PUB FATMirroring{}: state
' Flag indicating FAT mirroring is enabled
'   Returns: boolean
    return ((_fat_flags >> FATMIRROR) & 1) == 0

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
    'xxx _prev_clust, _next_clust

PUB FileAttrs{}: a
' File attributes
'   Returns: bitmap
'       bit 5: is an archive
'           4: is a subdirectory
'           3: is the volume name
'           2: is a system file
'           1: is a hidden file
'           0: is write-protected
    return _file_attr & $3F

PUB FileFirstClust{}: c ' xxx which is faster, the below, or c.word[1] :=, c.word[0] := ... ?
' First cluster of file
'   Returns: long
    return (_clust_file_h << 16) | _clust_file_l

PUB FileFirstSect{}: s
' First sector of file
'   Returns: long
    return clust2sect(filefirstclust{})

PUB FileCreateMS{}: ms
' Timestamp of file creation, in milliseconds
'   Returns: byte
    return _time_cr_ms

PUB FileCreateDate{}: d
' Date file was created
'   Returns: bitmap
'       bit 15..9: year from 1980 (e.g., 41 = 2021 (80+41-100=21))
'       bit 8..5: month
'       bit 4..0: day
    return _date_cr

PUB FileCreateTime{}: t
' Time file was created
'   Returns: bitmap
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    return _time_cr

PUB FileEnd{}: p
' Last byte number of file
    return (_file_sz - 1)

PUB FileIsDeleted{}: d
' Flag indicating file is deleted
'   Returns: boolean
    return _str_fn[0] == FDEL_MARKER            ' FN first char is xE5?

PUB FileName{}: ptr_str
' File name
'   Returns: pointer to string
    return @_str_fn

PUB FileNameExt{}: ptr_str
' File name extension
'   Returns: pointer to string
    return @_str_fext

PUB FileNextClust{}: c
' Next cluster used by file
    return _next_clust

PUB FilePrevClust{}: c
' Previous cluster used by file
    return _prev_clust

PUB FileSize{}: sz
' File size, in bytes
'   Returns: long
    return _file_sz

PUB FSizeUpdate(new_sz)
' Update file size
    _file_sz := new_sz

PUB FileTotalClust{}: c
' Total number of clusters occupied by file
'   Returns: long
'   NOTE: This value is inferred from known file size, bytes per sector,
'       and sectors per cluster
    return 1 #> (_file_sz / (_sect_sz * _sect_per_clust))

PUB FileTotalSect{}: s
' Total number of sectors occupied by file
'   Returns: long
'   NOTE: This value is inferred from known file size and bytes per sector
    return filesize{} / _sect_sz

PUB FLastAccDate{}: d
' Date file was last accessed
'   Returns: bitmap
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    return _date_lastxs

PUB FLastModDate{}: d
' Date file was last modified
'   Returns: bitmap
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    return _date_lastwr

PUB FLastModTime{}: t
' Time file was last modified
'   Returns: bitmap
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    return _time_lastwr

PUB FModDate(date_word)
' Update file modified datestamp
'       bit 15..9: year from 1980
'       bit 8..5: month
'       bit 4..0: day
    _date_lastwr := date_word

PUB FModTime(time_word)
' Update file modified timestamp
'       bit 15..11: hours
'       bit 10..5: minutes
'       bit 4..0: 2 second intervals
    _time_lastwr := time_word

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

PUB FOpen(fnum)
' Open file
'   NOTE: No validation is performed on data in sector buffer
    fnum := _ptr_fatimg + (fnum * DIRENT_LEN)   ' calc offset for this file
    bytefill(@_str_fn, 0, 9)                    ' clear string buffers
    bytefill(@_str_fext, 0, 4)

    bytemove(@_str_fn, fnum+FNAME, 8)
    bytemove(@_str_fext, fnum+FNEXT, 3)
    bytemove(@_file_attr, fnum+FATTR, 1)
    bytemove(@_time_cr_ms, fnum+FCREATE_TMS, 1)
    bytemove(@_time_cr, fnum+FCREATE_T, 2)
    bytemove(@_date_cr, fnum+FCREATE_D, 2)
    bytemove(@_date_lastxs, fnum+FLSTXS_D, 2)
    bytemove(@_clust_file_h, fnum+FCLUST_H, 2)
    bytemove(@_time_lastwr, fnum+FLSTWR_T, 2)
    bytemove(@_date_lastwr, fnum+FLSTWR_D, 2)
    bytemove(@_clust_file_l, fnum+FCLUST_L, 2)
    bytemove(@_file_sz, fnum+FSZ, 4)

    ' when opening the file, initialize next and prev cluster numbers with
    '   the file's first cluster number
    _next_clust := (_clust_file_h << 16) | (_clust_file_l)
    _prev_clust := _next_clust

PUB Heads{}: h
' Number of heads
'   Returns: word
    return _nr_heads

PUB HiddenSectors{}: s
' Number of hidden sectors in partition
'   Returns: long
    return _sect_hidn

PUB IsDirectory{}: bool
' Flag indicating file is a (sub)directory
'   Returns: boolean
    return (fileattrs{} & FSUBDIR) <> 0

PUB LogicalDrvNum{}: n
' Logical drive number of partition
'   Returns: byte
    return _drv_num

PUB LogicalSectorBytes{}: b
' Size of logical sector, in bytes
'   Returns: word
'   NOTE: Values returned should be powers of 2 only
    return _sect_sz

PUB MediaType{}: t
' Media type of FAT
    return _fat_medtyp

PUB NextCluster{}: c
' Get next cluster number in chain
'   Returns: long
    c := 0
    ' update the next and prev cluster pointers, by following the chain
    '   read from the FAT
    _next_clust := fatent2clust(_prev_clust & $7f)
    _prev_clust := _next_clust
    if _next_clust == MRKR_EOC                  ' End-of-Chain marker reached
        return -1                               '   no more clusters
    return _next_clust

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

PUB Sect2Clust(sect): clust
' Map given sector number to a cluster number
'   Returns: long
    clust := ((sect - _data_region) / _sect_per_clust)

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

