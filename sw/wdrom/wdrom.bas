' to do

' PUIS flag
' confirm DCM order
' if ROYL not found, look for non-ROYL modules
' confirm byte order in preamp values and microjogs
' add Types for ROYL modules
' rewrite code for verifying and saving MOD files
' identify ROM resident modules by their Type byte (= 4 for flash)


#include once "vbcompat.bi"

Dim bVerbose As uByte                   ' flag which selects verbose output mode, 1 = verbose
Dim dwROMsize As uLong                  ' ROM file size in bytes
Dim offset As uLong                     ' 

Dim wdModID As uShort                   ' ROYL module ID

Dim bCompressFlg As uByte               ' = 1 if PCMB is compressed
Dim sROMstrg As String                  ' string which holds entire ROM file
Dim dwROYLpos0 As uLong                 ' offset to "ROYL" text in ROM string (0 base)
Dim dwROYLpos1 As uLong                 ' offset to "ROYL" text in ROM string (1 base)
Dim result As uLong                     ' result of file operation (0 = success)

Dim bHdrSiz As uByte                    ' header size in module 0x0B / 0x20B
Dim bRecSiz As uByte                    ' size of each directory record in module 0x0B / 0x20B
Dim wdModSiz As uShort                  ' size of module in directory record in module 0x0B / 0x20B (bytes or sectors)
Dim dwModulSiz As uLong                 ' actual size of module in bytes
Dim dwModAdd As uLong                   ' address of module in directory record in module 0x0B / 0x20B (ROM / SA)
Dim bExtraByts As uByte                 ' number of extra bytes past last dword boundary in ROM module
Dim dwSiz As uLong                      ' size of ROM module in bytes (from ROYL header)
Dim dwRegDatLoc As uLong                ' offset to region data in module 0x0B / 0x20B
Dim bRegDatSiz As uByte                 ' 
Dim bNumSAregs As uByte                 ' number of SA regions in module 0x0B / 0x20B
Dim dwRegnLoc As uLong                  ' location of SA region
Dim dwRegnSiz As uLong                  ' size of SA region
Dim bRegn As uByte                      ' region counter

' first index identifies directory, second index identifies ROM module

'  module_parameter( 0 = dir_0B / 1 = dir_20B,   0 -> 6 = 0A 0B 0D 30 47 4F 20B )

Dim wdModSize( 0 To 1, 0 To 6 ) As uShort               ' size of ROM module in 0x0B / 0x20B directory
Dim dwModAddr( 0 To 1, 0 To 6 ) As uLong                ' address of ROM module in 0x0B / 0x20B directory
Dim dwModCksm( 0 To 1, 0 To 6 ) As uLong                ' 32-bit checksum of ROM module in 0x0B / 0x20B directory

Dim bNumMods As uByte                                   ' number of ROYL modules in 0x0B / 0x20B directory
Dim dwDirOffst( 0 To 1 ) As uLong = {0, 0}              ' offset to 0x0B / 0x20B directories in ROM
Dim wdDirID( 0 To 1 ) As uShort = {0, 0}                ' IDs of 0x0B / 0x20B directories in ROM
Dim bActiveDir As uByte                                 ' = 0 if 0x0B is active, = 1 if 0x20B is active

' bDirFlag appears to be an unsigned byte value, eg 02 is active, FE is inactive

Dim bDirFlag( 0 To 1 ) As Byte = {0, 0}                 ' flag byte denoting whether 0x0B / 0x20B directory is active

Dim sFwVer As String * 8                                ' firmware version in module 0x0D
Dim sModelNum As String * 40                            ' model number in module 0x0D
Dim sSerNum As String * 20                              ' serial number in module 0x0D

Dim sROMfwver As String * 8                             ' ROM firmware version in module 0x4F

Dim wdReadChOfst As uShort                              ' offset to read channel parameters in module 0x47
Dim wdPreampOfst As uShort                              ' offset to preamp parameters in module 0x47
Dim wdMjogOfst As uShort                                ' offset to microjogs in module 0x47
Dim bNumRecs As uByte                                   ' number of preamp and microjog records in module 0x47
Dim sHM As String * 2                                   ' Head/Media DCM in module 0x47
Dim dwPreampVal As uLong                                ' preamp value in module 0x47
Dim wdMjog As uShort                                    ' microjog value in module 0x47

Dim sArg1 As String                                     ' first argument on command line
Dim sROYL As Const String = "ROYL"                      ' "ROYL" text
Dim sCRLF As Const String = !"\r\n"                     ' <CR> <LF>
Dim sBAD As Const String = "BAD"                        ' "BAD" text
Dim sOK As Const String = "OK"                          ' "OK" text
Dim sPrintLin As String                                 ' text for line output
Dim sPrintLinA As String                                ' text for line output of Palmer ROM checksum
Dim sPrintCksm As String                                ' text for checksum output (not digitally signed)
Dim sPrintCksmA As String                               ' text for checksum output (digitally signed)
Dim sBuff As String                                     ' string buffer for right justified data
Dim sHexDump As String                                  ' hex dump text in verbose mode
Dim sPCMBsuffix As String                               ' "_bad_CS.bin" or ".bin" suffix to indicate checksum status
Dim sPCMBprefix As String                               ' "rev" for compressed PCMBlock, "PCM" for uncompressed
Dim sCksmStat As String                                 ' checksum status, either BAD or OK

Dim sBadCksm As Const String = "Module has bad checksum -- processing aborted"

Dim dwCsumLen As uLong                                  ' number of checksum bytes, either 1, 2 or 4

Dim bPCMBsum8 As uByte                                  ' 8-bit checksum at end of PCMB
Dim wdPCMBsum16 As uShort                               ' 16-bit checksum at end of PCMB
Dim dwPCMBsum32 As uLong                                ' 32-bit checksum at end of PCMB
Dim dwPCMBsum32A As uLong                               ' 32-bit checksum before start of 0x100-byte signature block

Dim bSum As uByte                                       ' 8-bit sum of bytes
Dim wdSum As uShort                                     ' 16-bit sum of words
Dim dwSum As uLong                                      ' 32-bit sum of dwords
Dim wdSum8 As uShort                                    ' 16-bit sum of bytes
Dim dwSum8 As uLong                                     ' 32-bit sum of bytes
Dim dwSum8A As uLong                                    ' 32-bit sum of bytes excluding 0x100-byte signature block

Dim sAnalysisFil As String                              ' ROManalysis.txt -- log/analysis file
Dim sModFil As String                                   ' ROM module file name
Dim sPCMBfil As String                                  ' PCMBlock file name
Dim sROMfilnam As String                                ' ROM file spec
Dim sFlashdir As String                                 ' name of directory for extracted ROM components

Dim af As Long                                          ' file handle of log/analysis file
Dim mf As Long                                          ' file handle of module file
Dim pf As Long                                          ' file handle of PCMBlock file
Dim rf As Long                                          ' file handle of ROM file

Dim strgROMptr As ZString Ptr                           ' string pointer to string buffer for ROM
Dim ROMptr As Any Ptr                                   ' any pointer to string buffer for ROM
Dim bROMptr As uByte Ptr                                ' byte pointer to string buffer for ROM
Dim wdROMptr As uShort Ptr                              ' word pointer to string buffer for ROM
Dim dwROMptr As uLong Ptr                               ' dword pointer to string buffer for ROM

Dim i As uLong
Dim j As Integer
Dim k As Long
Dim idx As uByte
Dim tempAnyPtr As Any Ptr


' definition of LDSC record for PCMBlock

Type typLDSC Field = 1
    
    As uByte bID                ' PCMBlock ID
    As uByte bAttribs           ' PCMBlock attributes (if bit 0 = 1, then compressed)
    As uShort wdDecSizeHi       ' Decompressed size of PCMBlock (upper 16 bits)
    As uLong dwPCMblklen1       ' PCMBlock size including checksum byte(s)
    As uLong dwPCMblklen2       ' PCMBlock size excluding checksum byte(s)
    As uLong dwPCMblkloc        ' offset of PCMBlock relative to start of ROM section
    As uLong dwRAMaddr1         ' RAM load address of PCMBlock
    As uLong dwRAMaddr2         ' RAM load address #2 ???
    As uLong dwUnknown1         ' unknown function, possible MCU ID ???
    As uShort wdDecSizeLo       ' Decompressed size of PCMBlock (lower 16 bits)
    As uByte bAlwaysZero        ' always 0x00 ???
    As uByte bCksum8            ' checksum of LDSC bytes 0 - 1Eh
    
End Type

Dim LDSC As typLDSC
Dim LDSCptr As typLDSC Ptr      ' define pointer for LDSC UDT


' definition of head map field in module 0x0A

Type typHeadMap Field = 1

    As uByte bHdMapSiz          ' size of head map record in module 0x0A
    As uByte bAPBrev            ' APB revision in header of module 0x0A
    As uByte bAPBflags          ' APB flags in header of module 0x0A
    As uByte bPhyHds            ' number of physical heads in module 0x0A
    As uByte bUsedHds           ' number of used heads in module 0x0A
    As uByte bHeadMap1          ' head map #1 when bHdMapSiz = 0x20 
    As uByte bHeadMap2          ' head map #2 when bHdMapSiz = 0x20 
    As uByte bUnknown1
    As uByte bDCM( 0 To 9 )     ' 10 x DCM characters
    As uLong dwUnknown2
    As uLongInt uliUnknown3
    As uShort wdCksm            ' 16-bit sum of previous bytes

End Type

Dim udtHeadMap As typHeadMap
Dim udtHeadMapPtr As typHeadMap Ptr


' definition of header for ROM resident ROYL modules

' Type typROYLhdr Field = 1
'
'    As uLong dwSig              ' should be "ROYL" - 0x4C594F52
'    As uByte bType              ' = 4 for flash
'    As uByte bReserved1         ' reserved
'    As uShort wdHdrSiz          ' size of header, offset to data area
'    As uShort wdModID           ' module ID
'    As uShort wdModSiz          ' module size in sectors
'    As uLong dwCksm             ' checksum dword -- select so that module sum = 0x00000000
'    As uByte bVersion( 0 To 7 ) ' firmware version - 8 characters
'    
' End Type
'
'Dim udtROYLhdr As typROYLhdr
'Dim udtROYLhdrPtr As typROYLhdr Ptr


Sub Usage

    Print
    Print "This program parses a WD ROM, extracts PCMBlocks and ROYL modules, and "
    Print "tests their integrity. Compressed PCMBlocks will be saved as big-endian."
    Print
    Print "Usage:  WDROMV17 [-v[erbose]] ROM_filename"
    Print
    Print " Example1:  WDROMV17 ROM.bin"
    Print " Example2:  WDROMV17 -v ROM.bin"
    Print
    End
    
End Sub


' Assembly language function to reverse the endian-ness of a 32-bit dword

Function EndianRev32( ByVal dwNum As uLong ) As uLong
    
	ASM
        
	  mov eax, [dwNum]
	  bswap eax
	  mov [Function], eax
      
	End ASM
    
End Function


' Add all the bytes and return a 32-bit checksum

Function bSum32( bFilptr As uByte Ptr, dwBuffsize As uLong ) As uLong

	Dim dwCksum32 As uLong
	Dim i As uLong

	dwCksum32 = 0

	For i = 0  To dwBuffsize - 1
        
		dwCksum32 += bFilptr[ i ]
        
	Next i

	Return dwCksum32
    
End Function


' Add all the words and return a 16-bit checksum

Function wdSum16( wdFilptr As uShort Ptr, dwBuffsize As uLong ) As uShort

	Dim wdCksum16 As uShort
	Dim i As uLong

	wdCksum16 = 0

	For i = 0  To ( dwBuffsize Shr 1 ) - 1
        
		wdCksum16 += wdFilptr[ i ]
        
	Next i

	Return wdCksum16
    
End Function


' Add all the dwords and return a 32-bit checksum

Function dwSum32( dwFilptr As uLong Ptr, dwBuffsize As uLong ) As uLong

	Dim dwCksum32 As uLong
	Dim i As uLong

	dwCksum32 = 0

	For i = 0  To ( dwBuffsize Shr 2 ) - 1
        
		dwCksum32 += dwFilptr[ i ]
        
	Next i

	Return dwCksum32
    
End Function


' Check command line for correct syntax, otherwise display usage information

If Command( 1 ) = "" Then 
    
    Usage
    
ElseIf Command( 3 ) <> "" Then
    
    Print
    Print "Syntax error:  too many arguments"
    Usage
    
End If

' Verbose mode prints the LDSCs as hex dumps

sArg1 = UCase( Command( 1 ) )

If ( sArg1 = "-V" ) OrElse ( sArg1 = "-VERBOSE" ) Then
    
    bVerbose = 1
    sROMfilnam = Command( 2 )
Else
    If Command( 2 ) = "" Then
        
        sROMfilnam = Command( 1 )
    Else
        Usage
        
    End If
    
End If

' Check if ROM file exists

If Not FileExists( sROMfilnam ) Then
    
    Print
    Print "File not found: "; sROMfilnam
    End
    
End If


' Check ROM size for correctness

dwROMsize = FileLen( sROMfilnam )

'If dwROMsize MOD &H10000 <> 0 Then
If ( dwROMsize And &HFFFF ) <> 0 Then

    Print
    Print "ROM size is not a multiple of 64KiB"
    End
    
End If

' Find the next available Flash directory and create it plus subdirectories

For j = 0 To &HFF
    
	sFlashdir = "Flash_" & Hex( j, 2 )

	If Dir( sFlashdir, fbDirectory ) <> "" Then
        
		sFlashdir = ""
		Continue For
	Else
		Mkdir(  sFlashdir )
		Mkdir(  sFlashdir & "\000Bmods" )
		Mkdir(  sFlashdir & "\020Bmods" )
		Mkdir(  sFlashdir & "\PCMBlock" )
		Exit For
        
	End If
    
Next j


If sFlashdir = "" Then
    
    Print
    Print "No spare Flash_nn directory  -  program aborted"
    End
Else
    sAnalysisFil = sFlashdir & "\ROManalysis.txt"
    Print
    Print "ROM modules will be saved to "; sFlashdir ; "\000Bmods and "; sFlashdir ; "\020Bmods"
    Print "PCMBlocks will be saved to "; sFlashdir ; "\PCMBlock"
    Print "ROM analysis will be saved to "; sAnalysisFil
    
End If


rf = FreeFile
result = Open ( sROMfilnam For Binary Access Read As #rf )

If result <> 0 Then

    Print
    Print "Error "; result; " - could not open ROM file:  "; sROMfilnam
    End
    
End If

af = FreeFile
result = Open ( sAnalysisFil For Output As #af )

If result <> 0 Then

    Print
    Print "Error "; result; " - could not open analysis file:  "; sAnalysisFil
    End
    
End If

' read ROM file into a large string

sROMstrg = Space( dwROMsize )
Get #rf, 1, sROMstrg
Close #rf
strgROMptr = StrPtr( sROMstrg )
ROMptr = strgROMptr                                         ' use Any Ptr to avoid compiler warnings

' Search for LDSCs and verify checksums of PCMBlocks

Print
Print "Analysing "; sROMfilnam ; " ... ";

Print #af, "Analysing "; sROMfilnam ; " ..."
Print #af, 
Print #af, "Searching for LDSCs and verifying PCMBlocks ..."
Print #af, 

' Print column headers

Print #af, "LDSC   LDSC    Att   PCMBlock          RAM         size      PCMBlk CS"
Print #af, "Start  ID CS        Start -  End     address     RAM / ROM    Exp/Act"
Print #af, "-----  -- --   --   -----   -----   --------   ------ -----  ---------"

' Assume each LDSC begins on a 16-byte boundary and analyse each group of 32 bytes

For offset = 0 To dwROMsize - 16 Step 16

    LDSCptr = ROMptr + offset
    LDSC = LDSCptr[ 0 ]

' dwCsumLen = number of checksum bytes, either 1, 2 or 4
' All PCMBlocks other than the first have an 8-bit checksum

    If LDSC.bID = &H5A Then
    
        dwCsumLen = LDSC.dwPCMblklen1 - LDSC.dwPCMblklen2
        
        If ( dwCsumLen <> 1 ) AndAlso ( dwCsumLen <> 2 ) AndAlso ( dwCsumLen <> 4 ) Then Continue For End If
        If ( dwCsumLen = 2 ) AndAlso ( LDSC.dwPCMblklen1 Mod 2 <> 0 ) Then Continue For End If
    Else
        dwCsumLen = 1
    
    End If

' Convert PCMblkloc to an absolute address within ROM  -- assumes that addresses are relative to 256KiB boundaries

    LDSC.dwPCMblkloc += ( offset Shr 18 ) * &H40000
    
    If LDSC.dwPCMblkloc < offset + 32 Then Continue For End If
    If LDSC.dwPCMblklen2 + LDSC.dwPCMblkloc > dwROMsize Then Continue For End If

' Byte 0x1E of LDSC should always (?) be 0

    If LDSC.bAlwaysZero <> 0 Then Continue For End If

' Byte 0x1F of LDSC is checksum of bytes 0 - 0x1E of LDSC
'  If checksum is bad, then this is not a valid LDSC

    bROMptr = ROMptr + offset
    bSum = bSum32( bROMptr, 31 ) And &HFF
    
    If bSum <> LDSC.bcksum8 Then Continue For End If

' If verbose mode, then add LDSC contents to hex dump

    If bVerbose = 1 Then
    
        For j = 0 To 31 Step 16
        
            sHexDump &= Hex( offset + j, 6 ) & Space( 2 )

            bROMptr = ROMptr + offset + j
        
            For k = 0 To 15
                
                sHexDump &= Hex( bROMptr[ k ], 2 ) & Space( 1 )
            
            Next k
        
            sHexDump &= sCRLF
        
        Next j		
        
        sHexDump &= sCRLF
        
    End If

' Print absolute start address, ID and checksum of LDSC

    sBuff = Space( 5 )
    Rset sBuff, Hex( offset )
    sPrintLin = sBuff & Space( 2 )
    
    sPrintLin &= Hex( LDSC.bID, 2) & Space( 1 )
    sPrintLin &= Hex( LDSC.bcksum8, 2) & Space( 3 )

' Compute the checksum for the PCMBlock ...

    Select Case As Const dwCsumLen

    Case 1                                              ' Compute the 8-bit checksum for the PCMBlock ...
        
        bROMptr = ROMptr + LDSC.dwPCMblkloc
        bSum = bSum32( bROMptr, LDSC.dwPCMblklen2 ) And &HFF

        bROMptr = ROMptr + LDSC.dwPCMblkloc + LDSC.dwPCMblklen2
        bPCMBsum8 = bROMptr[ 0 ]

    Case 2                                              ' ... or compute the 16-bit checksum for the PCMBlock
        
        wdROMptr = ROMptr + LDSC.dwPCMblkloc
        wdSum = wdSum16( wdROMptr, LDSC.dwPCMblklen2 )

        wdROMptr = ROMptr + LDSC.dwPCMblkloc + LDSC.dwPCMblklen2
        wdPCMBsum16 = wdROMptr[ 0 ]

    Case 4                                              ' ... or compute the 32-bit byte sum for the PCMBlock
            
        bROMptr = ROMptr + LDSC.dwPCMblkloc
        dwSum8 = bSum32( bROMptr, LDSC.dwPCMblklen2 )
        
        dwROMptr = ROMptr + LDSC.dwPCMblkloc + LDSC.dwPCMblklen2
        dwPCMBsum32 = dwROMptr[ 0 ]
        
        dwSum8A = bSum32( bROMptr, LDSC.dwPCMblklen2 - &H100 )              ' account for Palmer ROMs with 0x100-byte signature
        dwROMptr = ROMptr + LDSC.dwPCMblkloc + LDSC.dwPCMblklen2 - &H100
        dwPCMBsum32A = dwROMptr[ 0 ]

    End Select
        
' Print attribute byte

    sPrintLin &= Hex( LDSC.bAttribs, 2 ) & Space( 3 )

' Print PCMBlock Start - End

    sBuff = Space( 5 )
    Rset sBuff, Hex( LDSC.dwPCMblkloc )
    sPrintLin &= sBuff & " - "

    sBuff = Space( 5 )
    Rset sBuff, Hex( LDSC.dwPCMblkloc + LDSC.dwPCMblklen1 - 1 )
    sPrintLin &= sBuff & Space( 3 )

' Print RAM loadpoint address

    sBuff = Space( 8 )
    Rset sBuff, Hex( LDSC.dwRAMaddr1 )
    sPrintLin &= sBuff

' If PCMBlock is compressed, then print "c"

    bCompressFlg = LDSC.bAttribs And &H01
    
    If bCompressFlg = 1 Then
        
        sPrintLin &= " c "
    Else
        sPrintLin &= Space( 3 )

    End If

' Print size of PCMBlock in RAM (decompressed) and ROM

    sBuff = Space(6)
    
    If bCompressFlg = 0 Then
        
        Rset sBuff, Hex( LDSC.dwPCMblklen2 )
    Else
        Rset sBuff, Hex( LDSC.wdDecSizeLo + ( LDSC.wdDecSizeHi Shl 16 ) )
        
    End If
    
    sPrintLin &= sBuff & Space( 1 )

    sBuff = Space( 5 )
    Rset sBuff, Hex( LDSC.dwPCMblklen2 )
    sPrintLin &= sBuff

' Print Actual and Expected checksum and result of comparison (OK or BAD or unknown CRC)

    Select Case As Const dwCsumLen
    
    Case 1          ' 8-bit checksum
        
        sPrintLin &= Space( 4 )
        sPrintLin &= Hex( bPCMBsum8, 2) & Space( 3 )
        sPrintLin &= Hex( bSum, 2) & Space( 3 )

        If bSum <> bPCMBsum8 Then sCksmStat = sBAD Else sCksmStat = sOK End If
        sPrintLin &= sCksmStat
        Print #af, sPrintLin

    Case 2          ' 16-bit checksum
        
        sPrintLin &= Space( 2 )
        sPrintLin &= Hex( wdPCMBsum16, 4) & Space( 1 )
        sPrintLin &= Hex( wdSum, 4) & Space( 3 )
        
        If wdSum <> wdPCMBsum16 Then sCksmStat = sBAD Else sCksmStat = sOK End If
        sPrintLin &= sCksmStat
        Print #af, sPrintLin

    Case 4          ' 32-bit checksum
        
        sPrintCksm = Space( 2 ) & Hex( dwPCMBsum32, 8) & Space( 1 )
        sPrintCksm &= Hex( dwSum8, 8) & Space( 1 )
        
        If dwSum8 <> dwPCMBsum32 Then sCksmStat = sBAD Else sCksmStat = sOK End If
        sPrintCksm &= sCksmStat & " (not digitally signed)"
        
' Account for Palmer ROMs with 0x100-byte signature

        sPrintCksmA = Space( 2 ) & Hex( dwPCMBsum32A, 8) & Space( 1 )
        sPrintCksmA &= Hex( dwSum8A, 8) & Space( 1 )
        
        If dwSum8A <> dwPCMBsum32A Then sCksmStat = sBAD Else sCksmStat = sOK End If
        sPrintCksmA &= sCksmStat & " (digitally signed)"
        
        Print #af, sPrintLin;
    
        If dwSum8 = dwPCMBsum32 Then
            
            Print #af, sPrintCksm
            
        ElseIf dwSum8A = dwPCMBsum32A Then
            
            Print #af, sPrintCksmA
            
        Else
            Print #af, sPrintCksm
            Print #af, Space( 59 ) & sPrintCksmA
            
        End If
    
    End Select

' If PCMBlock is compressed, then byte-reverse each dword (except the first), and write the block to revBlkid_offset.bin. 
' Otherwise, if not compressed, then write the block to PCMBlkid_offset.bin, excluding checksum.
' If checksum is BAD, then append "_bad_CS" to filename. 

    If sCksmStat = sBAD Then
        
        sPCMBsuffix = "_bad_CS.bin"
    Else
        sPCMBsuffix = ".bin"
        
    End If
    

    If bCompressFlg = 1 Then

        sPCMBprefix = "rev"

        dwROMptr = ROMptr + LDSC.dwPCMblkloc
        
        For j = 1 To ( LDSC.dwPCMblklen2 Shr 2 ) - 1

            dwROMptr[ j ] = EndianRev32( dwROMptr[ j ] )
            
        Next j
    Else
        sPCMBprefix = "PCM"
        
    End If
    

    bROMptr = ROMptr + LDSC.dwPCMblkloc

    sPCMBfil = sFlashdir & "\PCMBlock\" & sPCMBprefix & "Blk" & Hex( LDSC.bID, 2 ) & "_" & Hex( LDSC.dwPCMblkloc ) & sPCMBsuffix
    
    pf = Freefile
    Open sPCMBfil For Binary As #pf
    
' If the PCMBlock has a digital signature, then save all bytes. Otherwise omit the trailing checksum.
    
    If ( dwCsumLen = 4 ) And ( dwSum8A = dwPCMBsum32A ) Then
        
        Put #pf, 1, bROMptr[ 0 ], LDSC.dwPCMblklen1
    Else
        Put #pf, 1, bROMptr[ 0 ], LDSC.dwPCMblklen2
        
    End If
        
    Close #pf

Next offset


Print #af,

' Print definition of acronyms

Print #af, "LDSC   = PM Loader Config String (32 bytes)"
Print #af, "ID     = ID byte of LDSC (byte #0)"
Print #af, "CS     = Checksum byte or word"
Print #af, "Att    = Attributes"
Print #af, "PCMBlk = Program Code Memory Block"
Print #af, "Exp    = Expected checksum for PCMBLock"
Print #af, "Act    = Actual checksum for PCMBLock"
Print #af, "c      = compressed PCMBlock"
Print #af, "size   = size of decompressed (in RAM) and compressed (in ROM) PCMBlock in bytes"
Print #af,

' If verbose mode was selected, then print LDSCs as hex dumps

If bVerbose = 1 Then
    
    Print #af,
    Print #af, "LDSC hex dumps ..."
    Print #af,
    Print #af, "Offset  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F"
    Print #af,
    Print #af, sHexDump
    sHexDump = ""
    
End If

' Search forwards for ROYL directory module 0x0B or 0x20B
'  - reverse search has problems with latest ROMs

offset = 1
dwROYLpos1 = 1

Do Until dwROYLpos1 = 0

    dwROYLpos1 = Instr( offset, sROMstrg, sROYL )
    
    If dwROYLpos1 <> 0 Then
        
        dwROYLpos0 = dwROYLpos1 - 1
        wdROMptr = ROMptr + dwROYLpos0 + 8
        wdModID = wdROMptr[ 0 ]
        
        If wdModID = &H0B Then
            
            wdDirID( 0 ) = &H0B
            dwDirOffst( 0 ) = dwROYLpos0
            
            If wdDirID( 1 ) = &H20B Then 
            
                Exit Do
            
            End If
        
        ElseIf wdModID = &H20B Then
            
                wdDirID( 1 ) = &H20B
                dwDirOffst( 1 ) = dwROYLpos0
                
                If wdDirID( 0 ) = &H0B Then 
                    
                    Exit Do
                
                End If
        
        End If
        
        offset = dwROYLpos1 + 4
        
    End If
    
Loop


If ( wdDirID( 0 ) = 0 ) AndAlso ( wdDirID( 1 ) = 0 ) Then 
    
        Print #af, "No ROYL directory modules (0x0B or 0x20B) found in ROM"
        End
    
End If

' Do 2 passes, one for 0x0B and the second for 0x20B

For k = 0 To 1

    If wdDirID( k ) = 0 Then
        
        Continue For
        
    End If


    Print #af,
    Print #af, "ROYL directory module 0x"; Hex( wdDirID( k ), 4 ); " found at 0x"; Hex( dwDirOffst( k ) )
    Print #af,

    offset = dwDirOffst( k ) + &H1B
    bROMptr = ROMptr + offset
    bDirFlag( k ) = bROMptr[ 0 ]
    
    Print #af, "Active directory flag = 0x"; Hex( bDirFlag( k ), 2 )
    Print #af,

/'
Offset(h) 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F

00000000  52 4F 59 4C 04 80 1E 00 0B 02 01 00 71 30 FC A6  ROYL.€......q0ü¦
00000010  30 30 30 33 30 30 30 30 00 00 00 02 00 00 0A 12  00030000........
00000020  02 01 00 40 00 03 18 90 42 1D 59 0C 00 1D 59 0C  ...@....B.Y...Y.
00000030  00 12 01 0A 00 4E 00 00 19 00 00 00 C0 07 00 00  .....N......À...
00000040  00 00 00 12 01 0B 00 29 01 00 19 00 00 9A F1 07  .......).....šñ.
00000050  00 00 00 00 00 12 01 0B 02 29 01 00 19 00 00 9A  .........).....š
00000060  D1 07 00 00 00 00 00 12 01 81 01 00 0C 00 19 00  Ñ...............
00000070  00 00 B4 07 00 00 00 00 00 12 01 30 00 00 04 00  ..´........0....
00000080  19 00 00 00 B0 07 00 00 00 00 00 12 01 47 00 18  ....°........G..
00000090  0B 00 19 00 00 82 C6 07 00 00 00 00 00 12 01 0D  .....‚Æ.........
000000A0  00 08 01 00 19 00 00 4E C0 07 00 00 00 00 00 12  .......NÀ.......
000000B0  01 4F 00 2C 05 00 19 00 00 56 C1 07 00 00 00 00  .O.,.....VÁ.....
000000C0  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
000000D0  00 00 00 56 02 FF 0F 03 00 5C 6A 0C 00 00 00 00  ...V.ÿ...\j.....
000000E0  00 5C 6A 0C 00 00 8A 0C 00 2E 35 06 00 00 14 19  .\j...Š...5.....
000000F0  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
00000100  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
00000110  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
00000120  00 00 00 00 00 00 00 00 00                       .........
'/

' Determine number and size of ROM module records

    offset = dwDirOffst( k ) + &H06
    bROMptr = ROMptr + offset
    bHdrSiz = bROMptr[ 0 ]

    offset = dwDirOffst( k ) + bHdrSiz
    bROMptr  = ROMptr + offset
    bNumMods = bROMptr[ 0 ]
    bRecSiz  = bROMptr[ 1 ]

' Print SA region data

    dwRegDatLoc = bHdrSiz + ( bNumMods * bRecSiz )

    offset = dwDirOffst( k ) + dwRegDatLoc + 1
    bROMptr  = ROMptr + offset
    bRegDatSiz = bROMptr[ 0 ]
    bNumSAregs = bROMptr[ 1 ]
    
    Print #af, "Identifying SA regions ..."
    Print #af,
    Print #af, "Reg#    Reg size     Reg loc"
    Print #af, "----  ----------  ----------"
   
    offset = dwDirOffst( k ) + dwRegDatLoc + 7
    dwROMptr = ROMptr + offset

    For bRegn = 0 To ( bNumSAregs - 1 )
        
        dwRegnSiz = dwROMptr[ 0 ]
        dwRegnLoc = dwROMptr[ 1 ]
        dwROMptr += 2
        
        Print #af, "0x"; Hex( bRegn, 2 ); "  0x"; Hex( dwRegnSiz, 8 ); "  0x"; Hex( dwRegnLoc, 8 )
        
    Next bRegn

' Test integrity of ROM modules

    Print #af,
    Print #af, "Verifying ROYL modules ..."
    Print #af,
    Print #af, " ID          Size (bytes)         Address    Checksum"
    Print #af, " dir   hdr        dir       hdr"
    Print #af, "----  ----   --------  --------   --------   --------"
   
    For i = 1 To bNumMods
			
' Get module ID, size and address

        offset = dwDirOffst( k ) + &H21 + ( i - 1 ) * bRecSiz
        wdROMptr = ROMptr + offset
        wdModID = wdROMptr[ 0 ]
        wdModSiz = wdROMptr[ 1 ]

        offset = dwDirOffst( k ) + &H29 + ( i - 1 ) * bRecSiz
        dwROMptr = ROMptr + offset
        dwModAdd = dwROMptr[ 0 ]
        
        If wdModID = 1 Then

'            Print #af,  "0001  N/A    "; Hex( wdModSiz * &H200, 8 ); "  N/A        "; Hex( dwModAdd, 8 ); Space( 13 ); "N/A"

            Print #af,  "0001  N/A    "; _
                        Hex( wdModSiz * &H200, 8 ); "  N/A        "; _
                        Hex( dwModAdd, 8 ); _
                        Space( 13 ); "N/A"
        
        ElseIf ( wdModID = 0 ) OrElse ( wdModSiz = 0 ) OrElse ( dwModAdd = 0 ) Then 

            Continue For
        
        Else
	
' Calculate module checksum
	
            Select Case As Const wdModID
            
                Case &H0A, &H0B, &H0D, &H30, &H47, &H4F, &H5D, &H181, &H1A2, &H1B0, &H1B6, &H20B, &H303
                    
                    dwModulSiz = wdModSiz
    
                Case Else
                    
                    dwModulSiz = wdModSiz * &H200

            End Select
            

            bExtraByts = dwModulSiz Mod 4 

' Save ROM module to file

            If k = 0 Then
            
                sModFil = sFlashdir & "\000Bmods\" & Hex( wdModID, 4) & ".bin"
            Else
                sModFil = sFlashdir & "\020Bmods\" & Hex( wdModID, 4) & ".bin"
            
            End If

            bROMptr = ROMptr + dwModAdd
            
            mf = FreeFile
            Open sModFil For Binary As #mf
            Put #mf, 1, bROMptr[ 0 ], dwModulSiz
            Close #mf

' calculate 32-bit checksum of ROYL module

            dwROMptr = ROMptr + dwModAdd
            dwSum = dwSum32( dwROMptr, dwModulSiz - bExtraByts )

' position pointer at last dword boundary and account for extra bytes

            tempAnyPtr = ROMptr + dwModAdd + ( dwModulSiz - bExtraByts )

            Select Case As Const bExtraByts

            Case 1
                
                bROMptr = tempAnyPtr
                dwSum += bROMptr[ 0 ]

            Case 2

                wdROMptr = tempAnyPtr
                dwSum += wdROMptr[ 0 ]

            Case 3

                dwROMptr = tempAnyPtr
                dwSum += dwROMptr[ 0 ] And &H00FFFFFF

            End Select

            
            sPrintLin = Hex( wdModID, 4 ) & Space( 2 )

            offset = dwModAdd + 8
            wdROMptr = ROMptr + offset
           
            If wdROMptr[ 0 ] = wdModID Then
                
                sPrintLin &= sOK & Space( 5 )
            Else
                sPrintLin &= Hex( wdROMptr[ 0 ], 4 ) & Space( 3 )
                
            End If
            
            
            sPrintLin &= Hex( dwModulSiz, 8 ) & Space( 2 )

            offset = dwModAdd + &HA
            wdROMptr = ROMptr + offset
            dwSiz = wdROMptr[ 0 ] * &H200
            
            If dwSiz = dwModulSiz Then

                sPrintLin &= sOK & Space( 9 )
            Else
                sPrintLin &= Hex( dwSiz, 8 ) & Space( 3 )
                
            End If

            sPrintLin &= Hex( dwModAdd, 8 ) & Space( 3 ) & Hex( dwSum, 8 ) & Space( 2 )
            
            If dwSum = 0 Then

                sPrintLin &= sOK
            Else
                sPrintLin &= sBAD

            End If
            
            Print #af, sPrintLin

            Select Case As Const wdModID
            
                Case &H0A   :   idx = 0
                Case &H0B   :   idx = 1
                Case &H0D   :   idx = 2
                Case &H30   :   idx = 3
                Case &H47   :   idx = 4
                Case &H4F   :   idx = 5
                Case &H20B  :   idx = 6
                Case Else   :   idx = &HFF
            
            End Select
            
                
            If idx <> &HFF Then 
                
                wdModSize( k, idx ) = wdModSiz
                dwModAddr( k, idx ) = dwModAdd
                dwModCksm( k, idx ) = dwSum
                
            End If
	
        End If
        
    Next i

Next k

' Print explanation of terms

Print #af,
Print #af, "dir  -  Module ID/Size as reported in directory module (0x20B or 0x0B)"
Print #af, "hdr  -  Module ID/Size as reported in module's header"
Print #af, "N/A  -  Not Applicable"
Print #af, "BAD  -  Module has invalid checksum. This may be due to non-existent module."
Print #af,

Print #af, "ROM modules saved to "; sFlashdir; "\000Bmods and "; sFlashdir; "\020Bmods"
Print #af,

' Determine active directory (0x0B or 0x20B)

If ( dwDirOffst( 0 ) = 0 ) And ( dwDirOffst( 1 ) = 0 ) Then

    Goto finished
    
ElseIf dwDirOffst( 1 ) = 0 Then
    
    bActiveDir = 0
    Print #af, "Active directory is 0x0B -- directory 0x20B not present"
    
ElseIf dwDirOffst( 0 ) = 0 Then
    
    bActiveDir = 1
    Print #af, "Active directory is 0x20B -- directory 0x0B not present"
    
ElseIf bDirFlag( 0 ) = bDirFlag( 1 ) Then
    
    Print #af, "Cannot determine active directory"
    Goto finished
    
ElseIf bDirFlag( 0 ) > bDirFlag( 1 ) Then
    
    bActiveDir = 0
    Print #af, "Active directory is 0x0B"
    
Else
    bActiveDir = 1
    Print #af, "Active directory is 0x20B"
    
End If

' Analyse module 0x0A - extract head map, DCM, and calculate checksum

' definition of head map field in module 0x0A

'Type typHeadMap Field = 1
'
'    As uByte bHdMapSiz          ' size of head map record in module 0x0A
'    As uByte bAPBrev            ' APB revision in header of module 0x0A
'    As uByte bAPBflags          ' APB flags in header of module 0x0A
'    As uByte bPhyHds            ' number of physical heads in module 0x0A
'    As uByte bUsedHds           ' number of used heads in module 0x0A
'    As uByte bHeadMap1          ' head map #1 when bHdMapSiz = 0x20 
'    As uByte bHeadMap2          ' head map #2 when bHdMapSiz = 0x20 
'    As uByte bUnknown1
'    As uByte bDCM( 0 To 9 )     ' 10 x DCM characters
'    As uLong dwUnknown2
'    As uLongInt uliUnknown3
'    As uShort wdCksm            ' 16-bit sum of previous bytes
'
'End Type
'
'Dim udtHeadMap As typHeadMap
'Dim udtHeadMapPtr As typHeadMap Ptr

Print #af,
Print #af, "Analysing active 0x0A module ..."
Print #af,

/'
Offset(h) 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F

00000000  52 4F 59 4C 04 80 1E 00 0A 00 01 00 E2 1F A5 71  ROYL.€......â.¥q
00000010  30 30 30 32 30 30 30 30 0A 11 11 00 00 00 30 0D  00020000......0.
00000020  06 0A 09 00 00 05 7C 57 7C 4D 4A 33 44 48 32 43  ......|W|MJ3DH2C
00000030  00 00 00 00 FF FF FF FF FF FF 00 00 91 F6 EF 03  ....ÿÿÿÿÿÿ..‘öï.
00000040  FF 03 00 00 00 00 4E 00 00 00 00 00 00 00        ÿ.....N.......
'/

If dwModCksm( bActiveDir, 0 ) <> 0 Then
    
    Print #af, sBadCksm
Else
    offset = dwModAddr( bActiveDir, 0 ) + &H06
    bROMptr = ROMptr + offset
    bHdrSiz = bROMptr[ 0 ]

' Locate offset of head map block and verify checksum

    offset = dwModAddr( bActiveDir, 0 ) + bHdrSiz
    bROMptr = ROMptr + offset
    wdSum8 = bSum32( bROMptr, &H1E ) And &HFFFF

' read heap map record into Type

    offset = dwModAddr( bActiveDir, 0 ) + bHdrSiz
    udtHeadMapPtr = ROMptr + offset
    udtHeadMap = udtHeadMapPtr[ 0 ]

'    wdROMptr = ROMptr + dwModAddr( bActiveDir, 0 ) + bHdrSiz + &H1E
'    wdSum8 += wdROMptr[ 0 ]
    wdSum8 += udtHeadMap.wdCksm

    Print #af, "Head map checksum (Expected / Actual) = 0x0000 / 0x"; Hex( wdSum8, 4 ); " - ";
    
    If wdSum8 = 0 Then
        
        Print #af, sOK

' Check number of physical/logical heads and head map

'        udtHeadMapPtr = ROMptr + dwModAddr( bActiveDir, 0 ) + bHdrSiz
'        udtHeadMap = udtHeadMapPtr[ 0 ]
        
        Print #af, "Number of heads (physical / in use) = "; udtHeadMap.bPhyHds; "/"; udtHeadMap.bUsedHds

        If udtHeadMap.bHdMapSiz = &H20 Then
            
            Print #af, "Head map #1 = 0x"; Hex( udtHeadMap.bHeadMap1, 2 ); " / 0b"; Bin( udtHeadMap.bHeadMap1, 8 )
            Print #af, "Head map #2 = 0x"; Hex( udtHeadMap.bHeadMap2, 2 ); " / 0b"; Bin( udtHeadMap.bHeadMap2, 8 )
        Else
            offset = dwModAddr( bActiveDir, 0 ) + bHdrSiz + &H20
            wdROMptr = ROMptr + offset

            Print #af, "Head map #1 = 0x"; Hex( wdROMptr[ 0 ], 4 ); " / 0b"; Bin( wdROMptr[ 0 ], 16 )
            Print #af, "Head map #2 = 0x"; Hex( wdROMptr[ 1 ], 4 ); " / 0b"; Bin( wdROMptr[ 1 ], 16 )

        End If

' Check DCM

'        bROMptr = ROMptr + dwModAddr( bActiveDir, 0 ) + bHdrSiz + 8

'        If bROMptr[ 0 ] <> 0 Then
        If udtHeadMap.bDCM( 0 ) <> 0 Then

            Print #af,
            sPrintLin = "DCM = "

            For i = 0 To 9
                
'                sPrintLin &= Chr( bROMptr[ i ] ) & Space( 1 )
                sPrintLin &= Chr( udtHeadMap.bDCM( i ) ) & Space( 1 )
                
            Next i

            Print #af, sPrintLin

' Preamp and bottom VCM appear to be reversed, eg S B L P M H C R V K U
'                                                       ^     ^
            Print #af, Space(6); ": : : : : : : : : :"
            Print #af, Space(6); ": : : : : : : : : unknown"
            Print #af, Space(6); ": : : : : : : : top VCM"
            Print #af, Space(6); ": : : : : : : ACA"
'			Print #af, Space(6); ": : : : : : preamp"
            Print #af, Space(6); ": : : : : : bottom VCM"
            Print #af, Space(6); ": : : : : HSA"
            Print #af, Space(6); ": : : : media"
            Print #af, Space(6); ": : : preamp"
'			Print #af, Space(6); ": : : bottom VCM"
            Print #af, Space(6); ": : latch"
            Print #af, Space(6); ": base"
            Print #af, Space(6); "spindle motor"
            
        End If
	Else
        Print #af, sBAD
        
	End If
    
End If


' Analyse module 0x0D - extract firmware version, WWN, model/serial numbers, PUIS flag

/'
Offset(h) 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F

00000000  52 4F 59 4C 04 80 1E 00 0D 00 01 00 32 DC 68 51  ROYL.€......2ÜhQ
00000010  30 30 30 31 30 30 30 30 00 00 00 00 00 00 00 01  00010000........
00000020  30 31 2E 30 31 41 30 31 01 01 06 02 00 00 50 01  01.01A01......P.
00000030  4E E0 59 C2 60 F5 00 01 FE FF 42 42 42 42 46 01  NàYÂ`õ..þÿBBBBF.
00000040  01 01 00 01 00 00 57 44 43 20 57 44 33 30 4E 4D  ......WDC WD30NM
00000050  5A 57 2D 31 31 47 58 36 53 31 20 20 20 20 20 20  ZW-11GX6S1      
00000060  20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20                  
00000070  20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20                  
00000080  20 20 03 02 00 00 00 00 32 00 00 00 00 01 00 01    ......2.......
00000090  00 00 00 00 00 00 00 00 00 00 00 00 00 00 44 00  ..............D.
000000A0  20 20 20 20 20 20 20 20 4E 4F 5F 50 43 42 41 5F          NO_PCBA_
000000B0  53 4E 5F 50 4F 50 55 4C 41 54 45 44 20 20 20 20  SN_POPULATED    
000000C0  20 20 20 20 00 00 00 00 00 00 00 00 00 00 00 00      ............
000000D0  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
000000E0  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
000000F0  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
00000100  00 00 00 00 00 00 00 00                          ........
'/

' The size of this module varies, so avoid reading beyond end of module

' !!! where is PUIS flag?

'Type udtMod0D
'
'    As uShort wdUnknown1                ' +0   '
'    As uByte bFwVer( 0 To 7 )           ' +2   ' firmware version (Identify Device)
'    As uByte bUnknown2( 0 To 5 )        ' +10  '
'    As uByte bWWN( 0 To 7 )             ' +16  ' World Wide Name
'    As uByte bUnknown3( 0 To 15 )       ' +24  '
'    As uByte bModelNum( 0 To 39 )       ' +40  ' model number
'    As uByte bSerialNum( 0 To 19 )      ' +80  ' serial number
'    As uByte bUnknown4( 0 To 29 )       ' +100 ' 
'    As uByte bUnknown5( 0 To 7 )        ' +130 ' text, usually spaces
'    As uByte bPCBSN( 0 To 19 )          ' +138 ' PCB serial number
'    As uByte bUnknown6( 0 To 7 )        ' +158 ' text, usually spaces
'
'End Type

Print #af,
Print #af, "Analysing active 0x0D module ..."
Print #af,

If dwModCksm( bActiveDir, 2 ) <> 0 Then
    
    Print #af, sBadCksm
Else
    offset = dwModAddr( bActiveDir, 2 ) + &H06
    bROMptr = ROMptr + offset
    bHdrSiz = bROMptr[ 0 ]
    
    offset = dwModAddr( bActiveDir, 2 ) + bHdrSiz + 2
    sFwVer = Mid( sROMstrg, offset + 1, 8 )

    Print #af, "Firmware Version = "; sFwVer

' extract WWN

    sPrintLin = ""
    offset = dwModAddr( bActiveDir, 2 ) + bHdrSiz + &H10
    bROMptr = ROMptr + offset
    
    For i = 0 To 7
        
        sPrintLin &= Hex( bROMptr[ i ], 2 )
        
    Next i
    
    Print #af, "World Wide Name = " & sPrintLin

' extract model number

    If wdModSize( bActiveDir, 2 ) > ( bHdrSiz + &H28 ) Then
        
        offset = dwModAddr( bActiveDir, 2 ) + bHdrSiz + &H28
        sModelNum = Mid( sROMstrg, offset + 1, 40 )
        
        Print #af, "Model Number = "; sModelNum
        
    End If

' extract serial number

    If wdModSize( bActiveDir, 2 ) > ( bHdrSiz + &H50 ) Then
        
        offset = dwModAddr( bActiveDir, 2 ) + bHdrSiz + &H50
        sSerNum = Mid( sROMstrg, offset + 1, 20 )
        
        Print #af, "Serial Number = "; sSerNum
        
    End If
    
    Print #af,
    
End If

' Analyse module 0x4F - extract ROM firmware version

If wdModSize( bActiveDir, 5 ) <> 0 Then
    
    Print #af,
    Print #af, "Analysing active 0x4F module ..."
    Print #af,
    
    If dwModCksm( bActiveDir, 5 ) <> 0 Then
        
        Print #af, sBadCksm
    Else
        offset = dwModAddr( bActiveDir, 5 ) + &H10
        sROMfwver = Mid( sROMstrg, offset + 1, 8 )
        
        Print #af, "ROM version = "; sROMfwver
        
    End If

End If


' Analyse module 0x47 - extract preamp/head/read channel adaptives, DCM

/'
Offset(h) 00   02   04   06   08   0A   0C   0E

00000000  524F 594C 0180 1E00 4700 000C CB77 A83D  ROYL.€..G...Ëw¨=
00000010  4354 2E37 3737 3700 0101 0400 3000 DE01  CT.7777.....0.Þ.
00000020  0602 0000 0000 0000 0000 0000 0000 334A  ..............3J
'/

Print #af,
Print #af, "Analysing active 0x47 module ..."
Print #af,

If dwModCksm( bActiveDir, 4 ) <> 0 Then
    
    Print #af, sBadCksm
    
Else
    offset = dwModAddr( bActiveDir, 4 ) + &H1C
    wdROMptr = ROMptr + offset
    
    wdReadChOfst = wdROMptr[ 0 ]
    wdPreampOfst = wdROMptr[ 1 ]
    wdMjogOfst   = wdROMptr[ 2 ]
    
    bNumRecs = ( wdMjogOfst - wdPreampOfst ) / 4

' extract preamp values

    Print #af, "Preamp values"
    Print #af, String( 11, "-" )

    offset = dwModAddr( bActiveDir, 4 ) + wdPreampOfst
    dwROMptr = ROMptr + offset

    For i = 0 To bNumRecs - 1
        
        dwPreampVal = dwROMptr[ i ]
        
'        Print #af, Hex( i , 1 ); "  "; Hex( dwPreampVal, 8 )
        Print #af, Hex( i ); "  "; Hex( dwPreampVal, 8 )
        
    Next i

' extract microjogs

    Print #af,
    Print #af, "Microjogs"
    Print #af, String( 7, "-" )

    offset = dwModAddr( bActiveDir, 4 ) + wdMjogOfst
    wdROMptr = ROMptr + offset
    
    For i = 0 To bNumRecs - 1

        wdMjog = wdROMptr[ i ]
        
'        Print #af, Hex( i , 1 ); "  "; Hex( wdMjog, 4 )
        Print #af, Hex( i ); "  "; Hex( wdMjog, 4 )
        
    Next i

' extract head/media DCM codes

    offset = dwModAddr( bActiveDir, 4 ) + &H2E
    sHM = Mid( sROMstrg, offset + 1, 2 )
    
    Print #af,
    Print #af, "Head/Media DCM = "; sHM
    
End If

finished:

sROMstrg = ""
Print "done."
End


/'

Palmer ROM

2060-800065 ROM has 0x100-byte signature at end of boot block.
This signature must be excluded from checksum calculations.

Offset(h) 00       04       08       0C

00000000  5A040000 281B0000 241B0000 20000000  <-- LDSC of first PCMBlock
00000010  00100000 00100000 01B60000 F81E00ED

00000020  AF0500FB D04803E0 C1684907 00D58168  <-- start of boot block
........

00001A30  1F000000 08060000 E0FFFFFF 09000000
00001A40  10130000 27A60A00 F1075F58 A75EEA8D
                   ^^^^^^^^ *********
           Actual checksum   Start of signature block
........
00001B30  B772F113 685E00E3 08093FEE 4064B02B
00001B40  93D8D059 C77836D5 00000000 00000000
                   ********
     End of signature block
'/