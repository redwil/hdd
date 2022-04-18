' to do - 

' PUIS flag
' add 4-byte checksum for boot block
' confirm DCM order
' if ROYL not found, look for non-ROYL modules
' confirm byte order in preamp values and microjogs
' add Types for ROYL modules
' rewrite code for verifying and saving MOD files


#include once "vbcompat.bi"

Dim rf As Long
Dim ROMfil As String
Dim bverbose As Byte
Dim ROMsize As Integer
Dim offset As UInteger
Dim m As UByte
Dim dwcsumlen As ULong
Dim j As Integer
Dim i As ULong
Dim k As Long
Dim bytvar As UByte
Dim bsum As UByte
Dim hexdump As String
Dim wdcksum8 As UShort
Dim wdsum As UShort
Dim wdvar As UShort
Dim cksmstat As String
Dim sROYL As Const String = "ROYL"
Dim strgvar As String * 4
Dim modID As UShort
Dim nummods As UByte
Dim dirOffst( 0 To 1 ) As UInteger = {0, 0}
Dim dirID( 0 To 1 ) As UShort = {0, 0}
Dim activeDir As Ubyte
' dirFlag appears to be an unsigned byte value, eg 02 is active, FE is inactive
Dim dirFlag( 0 To 1 ) As Byte = {0, 0}
Dim hdrsiz As UByte
Dim recsiz As UByte
Dim modsiz As UShort
Dim modulsiz As UInteger
Dim modadd As UInteger
Dim dwcksm As UInteger
Dim dwvar As ULong
Dim extrabyts As Ubyte
Dim dwsiz As UInteger
Dim regdatloc As UInteger
Dim regdatsiz As Ubyte
Dim numSAregs As Byte
Dim regnloc As UInteger
Dim regnsiz As UInteger
Dim regn As Byte
' first index identifies directory, second index identifies ROM module
'  module_parameter( 0 = dir_0B / 1 = dir_20B,   0 -> 6 = 0A 0B 0D 30 47 4F 20B )
Dim modsize( 0 To 1, 0 To 6 ) As UShort
Dim modaddr( 0 To 1, 0 To 6 ) As UInteger
Dim modcksm( 0 To 1, 0 To 6 ) As UInteger
Dim idx As UByte
Dim hdmapsiz As UByte
Dim APBflags As UByte
Dim APBrev As UByte
Dim bphyhds As UByte
Dim usedhds As UByte
Dim sDCM As String * 10
Dim fwver As String * 8
Dim modelnum As String * 40
Dim sernum As String * 20
Dim ROMfwver As String * 8
Dim sBadCksm As Const String = "Module has bad checksum -- processing aborted"
Dim ReadChOfst As UShort
Dim PreampOfst As UShort
Dim MjogOfst As UShort
Dim numrecs As UByte
Dim sHM As String * 2
Dim modfil As String
Dim mf As Long
Dim preampval As UInteger
Dim sbuff As String
Dim Flashdir As String
Dim analysisFil As String
Dim af As Long
Dim PCMBsum8 As UByte
Dim PCMBsum16 As UShort
Dim PCMBsum32 As ULong
Dim compressflag As UByte
Dim sROMfil As String
Dim ROYLpos As ULong
Dim pf As Long
Dim PCMBfil As String
Dim sCRLF As Const String = !"\r\n"
Dim sBAD As Const String = "BAD"
Dim sOK As Const String = "OK"


Type typLDSC Field = 1
	As UByte ID					' PCMBlock ID
	As UByte attribs				' PCMBlock attributes (if bit 0 = 1, then compressed)
	As UShort decSizeHi			' Decompressed size of PCMBlock (upper 16 bits)
	As ULong PCMblklen1			' PCMBlock size including checksum byte(s)
	As ULong PCMblklen2			' PCMBlock size excluding checksum byte(s)
	As ULong PCMblkloc			' offset of PCMBlock relative to start of ROM section
	As ULong RAMaddr1				' RAM load address of PCMBlock
	As ULong RAMaddr2				' RAM load address #2 ???
	As ULong unknown1				' unknown function, possible MCU ID ???
	As UShort decSizeLo			' Decompressed size of PCMBlock (lower 16 bits)
	As UByte alwaysZero			' always 0x00 ???
	As UByte bcksum8				' checksum of LDSC bytes 0 - 1Eh
End Type

Dim LDSC As typLDSC

Declare Sub Usage

Sub Usage
	Print "This program parses a WD ROM, extracts PCMBlocks and ROYL modules, and "
	Print "tests their integrity. Compressed PCMBlocks will be saved as big-endian."
	Print
	Print "Usage:  WDROMV12 [-v[erbose]] ROMfilename"
	Print "Example1:  WDROMV12 ROM.bin"
	Print "Example2:  WDROMV12 -v ROM.bin"
	Print
End Sub


' Check command line for correct syntax, otherwise display usage information

If Command(1) = "" Then 
	Print "No file name specified"
	Usage
	End
ElseIf Command(3) <> "" Then
	Print "Too many arguments"
	Usage
	End
End If

' Verbose mode prints the LDSCs as hex dumps

If (Ucase(Command(1)) = "-V") OR (Ucase(Command(1)) = "-VERBOSE") Then
	bverbose = 1
	ROMfil = Command(2)
Else
	If Command(2) = "" Then 
		ROMfil = Command(1)
	Else
		Usage
		End
	End If
End If

' Check if file exists and open it for reading

If Not FileExists( ROMfil ) Then
	Print "File not found: "; ROMfil
	End
End If

' Find the next available Flash directory and create it plus subdirectories

For j = 0 To &HFF
	Flashdir = "Flash_" & Hex( j, 2 )

	If FileExists( Flashdir & "\NUL" ) Then 
		Flashdir = ""
		Continue For
	Else
		Mkdir(  Flashdir )
		Mkdir(  Flashdir & "\000Bmods" )
		Mkdir(  Flashdir & "\020Bmods" )
		Mkdir(  Flashdir & "\PCMBlock" )
		Exit For
	End If
Next j

If Flashdir = "" Then
	Print "No spare Flash_nn directory  -  program aborted"
	End
Else
	analysisFil = Flashdir & "\ROManalysis.txt"
	Print "ROM modules will be saved to "; Flashdir ; "\000Bmods and "; Flashdir ; "\020Bmods"
	Print "PCMBlocks will be saved to "; Flashdir ; "\PCMBlock"
	Print "ROM analysis will be saved to "; analysisFil
End If


rf = FreeFile
Open ROMfil For Binary Access Read As #rf

' Check ROM size for correctness

ROMsize = FileLen( ROMfil )

If ROMsize MOD &H10000 <> 0 Then
	Print "ROM size is not a multiple of 64KB"
	Close #rf
	End
End If

af = FreeFile
Open analysisFil For Output As #af

' Position byte pointer at beginning of file

Seek #rf,1

' Search for LDSCs and verify checksums of PCMBlocks

Print
Print "Analysing "; ROMfil ; " ... ";

Print #af, "Analysing "; ROMfil ; " ..."
Print #af, 
Print #af, "Searching for LDSCs and verifying PCMBlocks ..."
Print #af, 

' Print column headers

Print #af, "LDSC   LDSC    Att   PCMBlock          RAM         size      PCMBlk CS"
Print #af, "Start  ID CS        Start -  End     address     RAM / ROM    Exp/Act"
Print #af, String( 75, "-" )


/' Assume each LDSC begins on a 16-byte boundary
   and analyse each group of 32 bytes '/

   For offset = 0 To ROMsize - 16 Step 16

	Get #rf, offset + 1, LDSC

' dwcsumlen = number of checksum bytes, either 1, 2 or 4

	If LDSC.ID = &H5A Then

		dwcsumlen = LDSC.PCMblklen1 - LDSC.PCMblklen2

		If (dwcsumlen <> 1) And (dwcsumlen <> 2) And (dwcsumlen <> 4) Then Continue For End If
		If (dwcsumlen = 2) AND (LDSC.PCMblklen1 Mod 2 <> 0) Then Continue For End If
	Else
		dwcsumlen = 1
	End If

' Convert PCMblkloc to an absolute address within ROM

	LDSC.PCMblkloc += (offset Shr 18) * &H40000

	If LDSC.PCMblkloc < offset + 32 Then Continue For End If
	If LDSC.PCMblklen2 + LDSC.PCMblkloc > ROMsize Then Continue For End If

' Byte 0x1E of LDSC should always (?) be 0

	If LDSC.alwaysZero <> 0 Then Continue For End If

/' Byte 0x1F of LDSC is checksum of bytes 0 - 0x1E of LDSC
   If checksum is bad, then this is not a valid LDSC '/

	bsum = 0
	Seek #rf, offset + 1

	For j = 1 to 31
		Get #rf, , bytvar
		bsum += bytvar
	Next j

	If bsum <> LDSC.bcksum8 Then Continue For End If

' If verbose mode, then add LDSC contents to hex dump

	If bverbose = 1 Then

		For j = 0 To 31 Step 16

			hexdump = hexdump + Hex( CUint( offset + j ), 6) + "  "
			Seek #rf, offset + j + 1

			For k = 1 To 16

				Get #rf, , bytvar
        	            hexdump = hexdump + Hex( bytvar, 2 ) + " "

			Next k

	            hexdump = hexdump + sCRLF
	
		Next j		

		hexdump = hexdump + sCRLF
	End If

' Print absolute start address, ID and checksum of LDSC

	sbuff = Space(5)
	Rset sbuff, Hex( CUint( offset ))
	Print #af, sbuff;  "  ";

	Print #af, Hex( LDSC.ID, 2) + " ";
	Print #af, Hex( LDSC.bcksum8, 2) + Space( 3 );

' Compute the checksum byte for the PCMBlock ...

	If dwcsumlen = 1 Then

		Get #rf, LDSC.PCMblkloc + LDSC.PCMblklen2 + 1, PCMBsum8
		bsum = 0
		Seek #rf, LDSC.PCMblkloc + 1

		For j = 1 To LDSC.PCMblklen2

			Get #rf, , bytvar
			bsum += bytvar

		Next j

' ... or compute the checksum word for the PCMBlock

	ElseIf dwcsumlen = 2 Then


		Get #rf, LDSC.PCMblkloc + LDSC.PCMblklen2 + 1, PCMBsum16
		wdsum = 0
		Seek #rf, LDSC.PCMblkloc + 1

		For j = 1 To LDSC.PCMblklen2 Step 2

			Get #rf, , wdvar
			wdsum += wdvar

		Next j

	Else
		Get #rf, LDSC.PCMblkloc + LDSC.PCMblklen2 + 1, PCMBsum32
	End If

' Print attribute byte

	Print #af, Hex( LDSC.attribs, 2 ); Space( 3 );

' Print PCMBlock Start - End

	sbuff = Space(5)
	Rset sbuff, Hex( CUint( LDSC.PCMblkloc ))
	Print #af, sbuff; " - ";

	sbuff = Space(5)
	Rset sbuff, Hex( CUint( LDSC.PCMblkloc + LDSC.PCMblklen1 - 1 ))
	Print #af, sbuff; Space( 3 );

' Print RAM loadpoint address

	sbuff = Space(8)
	Rset sbuff, Hex( CUint( LDSC.RAMaddr1 ))
	Print #af, sbuff;

' If PCMBlock is compressed, then print "c"

	compressflag = LDSC.attribs And &H01

	If compressflag = 1 Then
		Print #af, " c ";
	Else
		Print #af, Space( 3 );
	End If

' Print size of PCMBlock in RAM (decompressed) and ROM

	sbuff = Space(6)

	If compressflag = 0 Then
		Rset sbuff, Hex( CUint( LDSC.PCMblklen2 ))
	Else
		Rset sbuff, Hex( CUint( LDSC.decSizeLo + ( LDSC.decSizeHi Shl 16 ) ))
	End If

	Print #af, sbuff; " ";

	sbuff = Space(5)
	Rset sbuff, Hex( CUint( LDSC.PCMblklen2 ))
	Print #af, sbuff;

' Print Actual and Expected checksum and result of comparison (OK or BAD or unknown CRC)

	If dwcsumlen = 1 Then 

		Print #af, Space( 4 ) ;
		Print #af, Hex$( PCMBsum8, 2); Space( 3 );
		Print #af, Hex$( bsum, 2); Space( 3 ); 
		If bsum <> PCMBsum8 Then cksmstat = sBAD Else cksmstat = sOK End If

	ElseIf dwcsumlen = 2 Then

		Print #af, Space( 2 ); 
		Print #af, Hex$( PCMBsum16, 4); " ";
		Print #af, Hex$( wdsum, 4); Space( 3 );
		If wdsum <> PCMBsum16 Then cksmstat = sBAD Else cksmstat = sOK End If
	Else
		Print #af, Space( 3 ); Hex$( CUint( PCMBsum32 ), 8); " ";
		cksmstat = "unknown CRC"
	End If

	Print #af, cksmstat
' End If


/' If checksum is OK and if PCMBlock is compressed, then byte-reverse each dword
 (except the first), and write the block to revBlkid_offset.bin. 
 Otherwise, if not compressed, then write the block to PCMBlkid_offset.bin, excluding checksum
'/
	If cksmstat = sBAD Then Continue For End If

	pf = Freefile
	Seek #rf, LDSC.PCMblkloc + 1

	If compressflag = 1 Then

		PCMBfil = Flashdir & "\PCMBlock\revBlk" & Hex( LDSC.ID, 2 ) & "_" & Hex( CUint( LDSC.PCMblkloc ) ) & ".bin"
		Open PCMBfil For Binary As #pf

		Get #rf, , dwvar
		Put #pf, 1, dwvar

		For j = 1 To LDSC.PCMblklen2 Step 4

			Get #rf, , dwvar
			dwvar = 	( ( dwvar And &H000000FF ) Shl 24 ) Or _
					( ( dwvar And &H0000FF00 ) Shl 8 ) Or _
					( ( dwvar And &H00FF0000 ) Shr 8 ) Or _
					( ( dwvar And &HFF000000 ) Shr 24 )
			Put #pf, , dwvar
		Next j
	Else
		PCMBfil = Flashdir & "\PCMBlock\PCMBlk" & Hex( LDSC.ID, 2 ) & "_" & Hex( CUint( LDSC.PCMblkloc ) ) & ".bin"
		Open PCMBfil For Binary As #pf

		For j = 1 To LDSC.PCMblklen2

			Get #rf, , bytvar
			Put #pf, , bytvar
		Next j
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

If bverbose = 1 Then
	Print #af,
	Print #af, "LDSC hex dumps ..."
	Print #af,
	Print #af, "Offset  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F"
	Print #af,
	Print #af, hexdump
	hexdump = ""
End If

' Search forwards for ROYL directory module 0x0B or 0x20B
'  - reverse search has problems with latest ROMs

sROMfil = Space( ROMsize )
Get #rf, 1, sROMfil

offset = 1
ROYLpos = 1

Do Until ROYLpos = 0

	ROYLpos = Instr( offset, sROMfil, sROYL )

	If ROYLpos <> 0 Then 
		Get #rf, ROYLpos + 8, modID

		If modID = &H0B Then
			dirID( 0 ) = &H0B
			dirOffst( 0 ) = ROYLpos

			If dirID( 1 ) = &H20B Then 
				Exit Do
			End If

		ElseIf modID = &H20B Then
			dirID( 1 ) = &H20B
			dirOffst( 1 ) = ROYLpos

			If dirID( 0 ) = &H0B Then 
				Exit Do
			End If

		End If

		offset = ROYLpos + 4
	End If
Loop


If (dirID( 0 ) = 0) And (dirID( 1 ) = 0) Then 
	Print #af, "No ROYL directory modules (0x0B or 0x20B) found in ROM"
	Close #rf
	End
End If

' Do 2 passes, one for 0x0B and the second for 0x20B

For k = 0 To 1

	If dirID( k ) = 0 Then
		Continue For
	End If

	offset = dirOffst( k )

	Print #af,
	Print #af, "ROYL directory module 0x"; Hex( dirID( k ), 4 ); " found at 0x"; Hex( offset - 1 )
	Print #af,
	
	Get #rf, offset + &H1B, dirFlag( k )
	Print #af, "Active directory flag = 0x"; Hex( dirFlag( k ), 2 )
	Print #af,

' Determine number and size of ROM module records

	Get #rf, offset + &H06, hdrsiz
	Get #rf, offset + hdrsiz, nummods
	Get #rf, offset + hdrsiz + 1, recsiz

' Print SA region data

	regdatloc = hdrsiz + (nummods * recsiz)
	Get #rf, offset + regdatloc + 1, regdatsiz
	Get #rf, offset + regdatloc + 2, numSAregs
	Seek #rf, offset + regdatloc + 7

	Print #af, "Identifying SA regions ..."
	Print #af,
	Print #af, "Reg#  Reg size    Reg loc"
	Print #af, String( 28, "-" )

	For regn = 0 To (numSAregs - 1)
		Get #rf, , regnsiz
		Get #rf, , regnloc
		Print #af, "0x"; Hex( regn, 2 ); "  0x"; Hex( regnsiz, 8 ); "  0x"; Hex( regnloc, 8 )
	Next regn

' Test integrity of ROM modules

	Print #af,
	Print #af, "Verifying ROYL modules ..."
	Print #af,
	Print #af, " ID          Size (bytes)         Address    Checksum"
	Print #af, "dir   hdr    dir       hdr"
	Print #af, String( 53, "-" )
	
	For i = 1 To nummods
			
' Get module ID, size and address
	
		Get #rf, offset + &H21 + (i - 1) * recsiz, modID
		Get #rf, offset + &H23 + (i - 1) * recsiz, modsiz
		Get #rf, offset + &H29 + (i - 1) * recsiz, modadd
	
		If modID = 1 Then
			Print #af, "0001  ";_
				"N/A    ";_
				Hex( modsiz * &H200, 8 ); "  ";_
				"N/A        ";_
				Hex( modadd, 8 );_
				Space( 11 ); "  N/A"
		ElseIf (modID = 0) Or (modsiz = 0) Or (modadd = 0) Then 
			Continue For
		Else
	
' Calculate module checksum

	
			If (modID <> &H0A) And _
			   (modID <> &H0B) And _
			   (modID <> &H0D) And _
			   (modID <> &H30) And _
			   (modID <> &H47) And _
			   (modID <> &H4F) And _
			   (modID <> &H5D) And _
			   (modID <> &H181) And _
			   (modID <> &H1A2) And _
			   (modID <> &H1B6) And _
			   (modID <> &H1B0) And _
			   (modID <> &H20B) Then
				modulsiz = modsiz * &H200
			Else
				modulsiz = modsiz
			End If
	
			dwcksm = 0
			extrabyts = modulsiz Mod 4 
			Seek #rf, modadd + 1

' Save ROM module to file

			If k = 0 Then
				modfil = Flashdir & "\000Bmods\" & Hex( modID, 4) & ".bin"
			Else
				modfil = Flashdir & "\020Bmods\" & Hex( modID, 4) & ".bin"
			End If

			mf = FreeFile
			Open modfil For Binary As #mf

			For j = 1 To (modulsiz - extrabyts) Step 4
				Get #rf, , dwvar
				dwcksm += dwvar
				Put #mf, , dwvar
			Next j
	
			If extrabyts = 1 Then
				Get #rf, modadd + j, bytvar
				dwcksm += bytvar
				Put #mf, , bytvar
			ElseIf extrabyts = 2 Then
				Get #rf, modadd + j, wdvar
				dwcksm += wdvar
				Put #mf, , wdvar
			ElseIf extrabyts = 3 Then
				Get #rf, modadd + j, dwvar
				dwcksm += dwvar And &H00FFFFFF
				Put #mf, , dwvar And &H00FFFFFF
			End If

			Close #mf

	
			Print #af, Hex( modID, 4 ); "  ";
			Get #rf, modadd + 9, wdvar
	
			If wdvar = modID Then
				Print #af, sOK; Space( 5 );
			Else
				Print #af, Hex( wdvar, 4 ); "   ";
			End If
	
			Print #af, Hex( modulsiz, 8 ); "  ";
			Get #rf, modadd + &HB, wdvar
			dwsiz = wdvar * &H200
	
			If dwsiz = modulsiz Then
				Print #af, sOK; Space( 9 );
			Else
				Print #af, Hex( dwsiz, 8 ); "   ";
			End If
	
			Print #af, Hex( modadd, 8 ); "   ";_
				Hex( dwcksm, 8 ); "  ";
	
			If dwcksm = 0 Then
				Print #af, sOK
			Else
				Print #af, sBAD
			End If

			If     modID =  &H0A Then 
				idx = 0
			ElseIf modID =  &H0B Then 
				idx = 1
			ElseIf modID =  &H0D Then 
				idx = 2
			ElseIf modID =  &H30 Then 
				idx = 3
			ElseIf modID =  &H47 Then 
				idx = 4
			ElseIf modID =  &H4F Then 
				idx = 5
			ElseIf modID = &H20B Then 
				idx = 6
			Else 
				idx = &HFF
			End If

			If idx <> &HFF Then 
				modsize( k, idx ) = modsiz
				modaddr( k, idx ) = modadd
				modcksm( k, idx ) = dwcksm
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

Print #af, "ROM modules saved to "; Flashdir; "\000Bmods and "; Flashdir; "\020Bmods"
Print #af,

' Determine active directory (0x0B or 0x20B)

If (dirOffst( 0 ) = 0) And (dirOffst( 1 ) = 0) Then
	Close #rf
	End
ElseIf (dirOffst( 1 ) = 0) Then
	activeDir = 0
	Print #af, "Active directory is 0x0B -- directory 0x20B not present"
ElseIf (dirOffst( 0 ) = 0) Then
	activeDir = 1
	Print #af, "Active directory is 0x20B -- directory 0x0B not present"
ElseIf dirFlag( 0 ) = dirFlag( 1 ) Then
	Print #af, "Cannot determine active directory"
	Close #rf
	End
ElseIf dirFlag( 0 ) > dirFlag( 1 ) Then
	activeDir = 0
	Print #af, "Active directory is 0x0B"
Else
	activeDir = 1
	Print #af, "Active directory is 0x20B"
End If

' Analyse module 0x0A - extract head map, DCM, and calculate checksum

Print #af,
Print #af, "Analysing active 0x0A module ..."
Print #af,

If modcksm( activeDir, 0 ) <> 0 Then
	Print #af, sBadCksm
Else
	offset = modaddr( activeDir, 0 ) + 1
	Get #rf, offset + &H06, hdrsiz

' Locate offset of head map block

	Seek #rf, offset + hdrsiz

' Verify checksum

	wdcksum8 = 0

	For i = 1 To &H1E
		Get #rf, , bytvar
		wdcksum8 += bytvar
	Next i

	Get #rf, , wdvar
	wdcksum8 += wdvar
	Print #af, "Head map checksum (Expected / Actual) = 0x0000 / 0x"; Hex( wdcksum8, 4 ); " - ";

	If wdcksum8 = 0 Then
		Print #af, sOK

' Check number of physical/logical heads and head map

		Seek #rf, offset + hdrsiz
		Get #rf, , hdmapsiz
		Get #rf, , APBrev
		Get #rf, , APBflags
		Get #rf, , bphyhds
		Get #rf, , usedhds
		Print #af, "Number of heads (physical / in use) = "; bphyhds; "/"; usedhds

		If hdmapsiz = &H20 Then
			Get #rf, , bytvar
			Print #af, "Head map #1 = 0x"; Hex( bytvar, 2 ); " / 0b"; Bin( bytvar, 8 )
			Get #rf, , bytvar
			Print #af, "Head map #2 = 0x"; Hex( bytvar, 2 ); " / 0b"; Bin( bytvar, 8 )
		Else
			Get #rf, offset + hdrsiz + &H20, wdvar
			Print #af, "Head map #1 = 0x"; Hex( wdvar, 4 ); " / 0b"; Bin( wdvar, 16 )
			Get #rf, , wdvar
			Print #af, "Head map #2 = 0x"; Hex( wdvar, 4 ); " / 0b"; Bin( wdvar, 16 )
		End If

' Check DCM

		Get #rf, offset + hdrsiz + 8, bytvar

		If bytvar <> 0 Then
			Get #rf, offset + hdrsiz + 8, sDCM

			Print #af,
			Print #af, "DCM = ";

			For i = 1 To 10
				Print #af, Mid( sDCM, i, 1 ); " ";
			Next i


' Preamp and bottom VCM appear to be reversed, eg S B L P M H C R V K U
'                                                       ^     ^
			Print #af,
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

Print #af,
Print #af, "Analysing active 0x0D module ..."
Print #af,

If modcksm( activeDir, 2 ) <> 0 Then
	Print #af, sBadCksm
Else
	offset = modaddr( activeDir, 2 ) + 1
	Get #rf, offset + &H06, hdrsiz
	Get #rf, offset + hdrsiz + 2, fwver
	Print #af, "Firmware Version = "; fwver

	Print #af, "World Wide Name = ";

	For i = 0 To 7
		Get #rf, offset + hdrsiz + &H10 + i, bytvar
		Print #af, Hex( bytvar, 2);
	Next i

	Print #af,

	If modsize( activeDir, 2 ) > ( hdrsiz + &H28 ) Then
		Get #rf, offset + hdrsiz + &H28, modelnum
		Print #af, "Model Number = "; modelnum
	End If

	If modsize( activeDir, 2 ) > ( hdrsiz + &H50 ) Then
		Get #rf, offset + hdrsiz + &H50, sernum
		Print #af, "Serial Number = "; sernum
	End If

	Print #af,
End If

' Analyse module 0x4F - extract ROM firmware version

If modsize( activeDir, 5 ) <> 0 Then
	Print #af,
	Print #af, "Analysing active 0x4F module ..."
	Print #af,

	If modcksm( activeDir, 5 ) <> 0 Then
		Print #af, sBadCksm
	Else
		offset = modaddr( activeDir, 5 ) + 1
		Get #rf, offset + &H10, ROMfwver
		Print #af, "ROM version = "; ROMfwver
	End If
End If


' Analyse module 0x47 - extract preamp/head/read channel adaptives, DCM

Print #af,
Print #af, "Analysing active 0x47 module ..."
Print #af,

If modcksm( activeDir, 4 ) <> 0 Then
	Print #af, sBadCksm
Else
	offset = modaddr( activeDir, 4 ) + 1
	Get #rf, offset + &H1C, ReadChOfst
	Get #rf, , PreampOfst
	Get #rf, , MjogOfst
	numrecs = ( MjogOfst - PreampOfst ) / 4

	Seek #rf, offset + PreampOfst
	Print #af, "Preamp values"
	Print #af, String( 11, "-" )

	For i = 1 To numrecs
		Get #rf, , preampval
		Print #af, Hex( i - 1 , 1 ); "  "; Hex( preampval, 8 )
	Next i

	Seek #rf, offset + MjogOfst
	Print #af,
	Print #af, "Microjogs"
	Print #af, String( 7, "-" )

	For i = 1 To numrecs
		Get #rf, , wdvar
		Print #af, Hex( i - 1 , 1 ); "  "; Hex( wdvar, 4 )
	Next i

	Get #rf, offset + &H2E, sHM
	Print #af,
	Print #af, "Head/Media DCM = "; sHM
End If

Close #rf
Print "done."
End
