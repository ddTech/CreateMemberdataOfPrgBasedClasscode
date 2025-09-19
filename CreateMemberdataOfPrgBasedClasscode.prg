*-- By Toni Köhler
*--
*-- DD changed on 18.09.25
*-- from VFPX 
*-- https://github.com/ToniKoehler/CreateMemberdataOfPrgBasedClasscode
*-
*--
*-- 2025-09-18	- Frank Dietrich modified for in-editor use.
*-- 			  Added getMemberDataFromEditor(), getClassName(), insertMemberData()
*-- 		 	  loadFoxTools(), stripIndent()
*--
*-- 2025-0-19	- modified the writing of the memberdata property in order to prevent
*--				  error 18 - 'line is too long' on classes with lots of PEMs
*--				- Also removed the unnecessarily printed out "+ CHR(23) +" statements
*--				- added xml processing instruction to _memberdata
*-- 			  

#DEFINE CON_VfpCrLf 			CHR(13) + CHR(10)

#DEFINE CON_TRIGGER_KEY 		ALT+M				&& trigger key for in editor behavior. By default ALT+M
													&& Modify, if You prefer a different key
										
#DEFINE CON_FOXTOOLS_PATH  		""					&& directory of foxtools.fll location if not home()
													&& --> loadFoxTools().
													&& needed for in-editor work only
#DEFINE CON_MB_CAPTION			"_MemberData - creator"												
#DEFINE CON_VFP_MAXLINELENGTH	8192				&& VFPs maximum line length
#DEFINE CON_XML_ENCODING		["Windows-1252"]	&& Code page of the xml processing instruction - double quotes needed here	


LPARAMETERS tcPrgPath, tcClassName


*-- DD changed on 18.09.25
*==============================================================================================
*  call this program without parameters in order to prepare                                    
*  its procedures for in-editor functionality  
*
*  Type ALT-M from within the editor to insert memberData at current position
*
*  Call this program again in order to switch the functionality off again
*==============================================================================================
if pCount() = 0 
	*--------------------------------------------------------------------------------------
	*  prepare for in-editor / on-key behavior                                             
	*--------------------------------------------------------------------------------------
	LOCAL lcThisFile
	m.lcThisFile = sys(16)
	if atc(justStem(m.lcThisFile), set("Procedure")) > 0
		*-- already set ... unload
		release Procedure (m.lcThisFile)
		on Key Label CON_TRIGGER_KEY
		
		wait window "In-editor _MemberData tool OFF" time 3
		
	else	
		set procedure to (m.lcThisFile) additive
		
		on Key Label CON_TRIGGER_KEY getMemberDataFromEditor()
		
		wait window "In-editor _MemberData tool loaded" time 3
		
	endif
	
else
	*--------------------------------------------------------------------------------------
	*  Default behavior                                                                    
	*--------------------------------------------------------------------------------------

	doCreateMemberData(m.tcPrgPath, m.tcClassName)
	
endif


PROCEDURE doCreateMemberData(tcPrgPath, tcClassName, tlPrgPathIsStream)

*!*  -------------------------------- TONI KÖHLER 2025-07-14 15:50:09 --------------------------------------
*!*  This program creates the memberdata (Methods and Properties) of your referenced program.PRG:ClassName
*!*  Pass the Fullpath of your .PRG and the contained classname.
*!*  As displayname the program use the same capitalization like written in your code.
*!*  
*!*  Just call this program like this:
*!*  _cliptext = CreateVfpMemberdataOfPrgBasedClass("C:\Dev\myTestProgram.prg", "myTestClassName")
*!*  
*!*  As return value you receive the MEMBERDATA block
*!*	_MEMBERDATA =														 ;
*!*		+ [<VFPData>] + CHR(13)											 ;
*!*		+ [<memberdata name="..." type="..." display="..."/>] + CHR(13)	 ;
*!*		+ [</VFPData>]
*!*  -------------------------------------------------------------------------------------------------------

LOCAL lcClassCode, lcClassName, lcCodeLine, lcEndTag, lcFileExt, lcFileStream, lcLCodeLine
LOCAL lcMethodName, lcNewPropertyName, lcPrgPath, lcPropertyName, lcReturn, lcSearchForDim
LOCAL lcSearchForEqualSign, lcStartTag, llProperiesFinished, lnArrayStartPos, lnElement, lnEnd
LOCAL lnLine, lnLines, lnPEMs, lnTry

LOCAL ARRAY laCodeLines[1]
LOCAL ARRAY laPropertiesList[1, 3]

lcReturn = "It's nothing todo!"

m.lcFileStream = ""

*-- DD changed on 18.09.25
*-- Content of prg file provided?
if pCount() > 2 and m.tlPrgPathIsStream and VARTYPE(tcPrgPath) = "C" and ! empty(m.tcPrgPath)
	*==========================================================================================
	*  Content provided directly                                                               
	*==========================================================================================
	m.lcFileStream = m.tcPrgPath

else && pCount() > 2 and m.tlPrgPathIsStream...	
	*==========================================================================================
	*  filename given                                                                          
	*==========================================================================================

	IF VARTYPE(tcPrgPath) != "C" OR EMPTY(tcPrgPath)
		DEBUGOUT "Parameter tcPrgPath is not optional!"
		RETURN lcReturn
	ENDIF
	lcPrgPath	= tcPrgPath
	
	*-- DD changed on 18.09.25
	*-- moved down
	*--
	*-- IF VARTYPE(tcClassName) != "C" OR EMPTY(tcClassName)
	*-- 	DEBUGOUT "Parameter tcClassName is not optional!"
	*-- 	RETURN lcReturn
	*-- ENDIF	
	*-- lcClassName	= tcClassName

	lcFileExt = UPPER(JUSTEXT(lcPrgPath))
	IF !lcFileExt == "PRG"
		DEBUGOUT "Allowed fileextension is PRG, your extension is " + lcFileExt
		RETURN lcReturn
	ENDIF
	IF !FILE(lcPrgPath)
		DEBUGOUT 'File "' + lcPrgPath + '" not found'
		RETURN lcReturn
	ENDIF

	lcFileStream = FILETOSTR(lcPrgPath)

endif && pCount() > 2 and m.tlPrgPathIsStream...

IF VARTYPE(tcClassName) != "C" OR EMPTY(tcClassName)
	DEBUGOUT "Parameter tcClassName is not optional!"
	RETURN lcReturn
ENDIF

lcClassName	= tcClassName

*!*  -------------------------------- TONI KOEHLER 2025-07-14 14:56:21 --------------------------------------
*!*  Begin with work.
*!*  Extracting class code.
*!*  -------------------------------------------------------------------------------------------------------

lcStartTag	 = "DEFINE CLASS " + lcClassName + " AS "
lcEndTag	 = "ENDDEFINE"

*-- DD changed on 18.09.25
*-- moved up
*--
*-- lcFileStream = FILETOSTR(lcPrgPath)

IF !EMPTY(lcFileStream) AND ATC(lcStartTag, lcFileStream) > 0
	DEBUGOUT "Class is found"
	lcClassCode	= STREXTRACT(lcFileStream, lcStartTag, lcEndTag, 1, 1 + 4)
	lcClassCode	= STRTRAN(lcClassCode, CHR(9), "")
	IF !EMPTY(lcClassCode)
		DEBUGOUT "ClassCode is not empty"
		lnLines = ALINES(laCodeLines, lcClassCode, 1 + 4, CON_VfpCrLf)
		DEBUGOUT "ClassCode contains count of rows: " + ALLTRIM(STR(lnLines, 15, 0))
		IF lnLines > 0
			llProperiesFinished = .F.
			FOR lnLine = 1 TO lnLines
				lcCodeLine	= laCodeLines(lnLine)
				*!*  TONI KOEHLER 2025-07-14 15:00:17
				*!*  >>  Removing some unwanted words
				lcCodeLine	= STRTRAN(lcCodeLine, "protected ", "", -1, -1, 1)
				lcCodeLine	= STRTRAN(lcCodeLine, "hidden ", "", -1, -1, 1)
				lcLCodeLine	= LOWER(lcCodeLine)
				IF LEFT(lcLCodeLine, 10) == "procedure " OR LEFT(lcLCodeLine, 9) == "function "
					DEBUGOUT "Properties finished"
					llProperiesFinished = .T.
				ENDIF
				IF !llProperiesFinished
					IF !LEFT(lcCodeLine, 1) == "_"
						lcSearchForEqualSign = "="
						lcSearchForDim		 = "dimension "
						lcNewPropertyName	 = ""
						DO CASE
							CASE ATC(lcSearchForEqualSign, lcCodeLine) > 0	&& Regular Property
								lcNewPropertyName = ALLTRIM(STREXTRACT(lcCodeLine, "", lcSearchForEqualSign, 1))

							CASE LEFT(LOWER(lcCodeLine), LEN(lcSearchForDim)) == lcSearchForDim	&& Array als Property
								lnArrayStartPos	= ATC(lcSearchForDim, lcCodeLine, 1)
								lnArrayStartPos	= lnArrayStartPos + LEN(lcSearchForDim)
								IF lnArrayStartPos > 0
									lnEnd = 0
									*!*  TONI KOEHLER 2025-07-14 15:03:36
									*!*  >>  Check Array-Subscription part "("
									IF ATC("(", lcCodeLine) > 0
										lnEnd = ATC("(", lcCodeLine)
									ENDIF

									*!*  TONI KOEHLER 2025-07-14 15:03:36
									*!*  >>  Check Array-Subscription part "["
									DO CASE
										CASE lnEnd = 0
											lnEnd = ATC("[", lcCodeLine)
										CASE lnEnd > 0 AND ATC("[", lcCodeLine) < lnEnd
											lnEnd = ATC("[", lcCodeLine)
									ENDCASE

									*!*  TONI KOEHLER 2025-07-14 15:04:54
									*!*  >>  Extract Property-Arrayname
									lnEnd = lnEnd - lnArrayStartPos
									IF lnEnd > 0
										lcNewPropertyName = SUBSTR(lcCodeLine, lnArrayStartPos, lnEnd)
									ENDIF
								ENDIF
						ENDCASE

						lcPropertyName = lcNewPropertyName

						IF !EMPTY(lcPropertyName)
							*!*  TONI KOEHLER 2025-07-14 15:05:23
							*!*  >>  Check for PEMName_COMATTRIB of OlePublic Classes
							IF !"_COMATTRIB" $ UPPER(lcPropertyName)
								IF !EMPTY(lcPropertyName) AND ISALPHA(LEFT(lcPropertyName, 1))
									DEBUGOUT "Adding property: " + lcPropertyName
									EXTERNAL ARRAY taArray
									= fnAddToArray(@laPropertiesList, lcPropertyName, "property")
								ENDIF
							ENDIF
						ENDIF
					ENDIF
				ELSE	&& Methods
					lcMethodName = ""
					DO CASE
						CASE LEFT(lcLCodeLine, 10) == "procedure "
							DEBUGOUT "PROCEDURE line found"
							lcMethodName = STREXTRACT(lcCodeLine, "procedure ", "(", 1, 1 + 2)
							DEBUGOUT "PROCEDURE name is: " + lcMethodName
						CASE LEFT(lcLCodeLine, 9) == "function "
							DEBUGOUT "FUNCTION line found"
							lcMethodName = STREXTRACT(lcCodeLine, "function ", "(", 1, 1 + 2)
							DEBUGOUT "FUNCTION name is: " + lcMethodName
					ENDCASE

					IF !EMPTY(lcMethodName)
						DEBUGOUT "Adding method: " + lcMethodName
						EXTERNAL ARRAY taArray
						= fnAddToArray(@laPropertiesList, lcMethodName, "method")
					ENDIF
				ENDIF
			NEXT
		ENDIF
	ENDIF
ENDIF


*!*  -------------------------------- TONI KOEHLER 2025-07-14 15:09:14 --------------------------------------
*!*  Merge Properties and Methods to Memberdata-Codeblock
*!*  -------------------------------------------------------------------------------------------------------

lcReturn  = ""
lnElement = 0
lnPEMs	  = ALEN(laPropertiesList, 1)

*-- DD changed on 19.09.25
*-- - removed the trailing + CHR(13) in each row
*-- - added xml processing instruction
*-- - check resulting line length and switch to a more 
*--   compact variant when exceeded.

*-- TEXT TO lcReturn ADDITIVE TEXTMERGE NOSHOW FLAGS 1 PRETEXT 4
*-- 	_MEMBERDATA = ;
*-- 		+ [<VFPData>] + CHR(13) ;
*-- ENDTEXT
*-- 
*-- FOR lnElement = 1 TO lnPEMs
*-- 	TEXT TO lcReturn ADDITIVE TEXTMERGE NOSHOW FLAGS 1&& PRETEXT 4
*-- 	
*-- 		+ [<memberdata name="<<laPropertiesList(lnElement, 1)>>" type="<<laPropertiesList(lnElement, 3)>>" display="<<laPropertiesList(lnElement, 2)>>"/>] + CHR(13) ;
*-- 	ENDTEXT
*-- NEXT
*TEXT TO lcReturn ADDITIVE TEXTMERGE NOSHOW FLAGS 1 + 2
*		+ [</VFPData>]
		
*ENDTEXT


*--------------------------------------------------------------------------------------
*  If the writeout exceeds VFP's maximum line length switch to a more compact version  
*  but try standard with line per PEM first
*--------------------------------------------------------------------------------------
for m.lnTry = 1 to 2

	set textmerge on 
	set Textmerge to MEMVAR m.lcReturn NOSHOW

	if m.lnTry = 2
		\_MEMBERDATA = [<?xml version="1.0" encoding=<<CON_XML_ENCODING>> standalone="yes"?><VFPData>] ;

	else
		\_MEMBERDATA = [<?xml version="1.0" encoding=<<CON_XML_ENCODING>> standalone="yes"?>];
		\+	[<VFPData>] ;
	endif

	FOR lnElement = 1 TO lnPEMs
	
		if m.lnTry = 2
			*----------------------------------------------------------------------------------
			*  compact version                                                                 
			*----------------------------------------------------------------------------------
			if mod(m.lnElement, 2) = 1
				\+ [<memberdata name="<<laPropertiesList(lnElement, 1)>>" type="<<laPropertiesList(lnElement, 3)>>" display="<<laPropertiesList(lnElement, 2)>>"/>
				if m.lnElement = m.lnPEMs
					*-- close row when last element
					\\</VFPData>]
				endif	
			else 
				*-- continue on same line
				\\<memberdata name="<<laPropertiesList(lnElement, 1)>>" type="<<laPropertiesList(lnElement, 3)>>" display="<<laPropertiesList(lnElement, 2)>>"/><<iif(m.lnElement = m.lnPEMs, "</VFPData>]", "];")>>
			endif
		else					
			*----------------------------------------------------------------------------------
			*  standard version                                                                
			*----------------------------------------------------------------------------------
			\+		[<memberdata name="<<laPropertiesList(lnElement, 1)>>" type="<<laPropertiesList(lnElement, 3)>>" display="<<laPropertiesList(lnElement, 2)>>"/>] ;
		endif
		
	NEXT

	if m.lnTry = 1
		\+	[</VFPData>]
	endif
	
	set Textmerge to
	set Textmerge OFF
	
	*--------------------------------------------------------------------------------------
	*  exit when OK on first run                                                           
	*--------------------------------------------------------------------------------------
	*debugout len(m.lcReturn)
	
	if m.lnTry = 1 
		if len(m.lcReturn) < CON_VFP_MAXLINELENGTH or ;
		  MessageBox( ;
		  +	"_MemberData statement exceeds VFP's maximum line length. ";
		  +	"You might run into a 'line too long' error upon compile."+ CON_VfpCrLf ;
		  +	"Try a more compact version?", 32 + 4, CON_MB_CAPTION ;
		  ) # 6
			
			exit
			
		endif

	endif 

endfor &&* m.lnTry = 1 to 2


DEBUGOUT "Memberdata finished - length:", len(m.lcReturn)

*-----------------------------------------------------------------------------------------
*  Notify user that he/she might run into problems upon save, when the line got too long  
*-----------------------------------------------------------------------------------------
if len(m.lcReturn) > CON_VFP_MAXLINELENGTH
	=MessageBox(;
		"_MemberData exceeds VPF's maximum line length. You might have to remove some of the PEMs from the XML.", ;
		64, CON_MB_CAPTION)
endif

RETURN lcReturn

ENDPROC &&  doCreateMemberData(...


PROCEDURE fnAddToArray(taArray, tcPropertyName, tcType)

	*!*  -------------------------------- TONI KOEHLER 2025-07-14 15:10:26 --------------------------------------
	*!*  Adding Element to referenced array
	*!*  -------------------------------------------------------------------------------------------------------

	LOCAL lnRows

	EXTERNAL ARRAY taArray
	lnRows			   = fnReDimArray(@taArray)
	taArray(lnRows, 1) = LOWER(tcPropertyName)	&& name
	taArray(lnRows, 2) = tcPropertyName			&& display
	taArray(lnRows, 3) = tcType					&& type
ENDPROC

FUNCTION fnReDimArray(taArray)

	*!*  -------------------------------- TONI KOEHLER 2025-07-14 15:10:51 --------------------------------------
	*!*  ReDim referenced array
	*!*  -------------------------------------------------------------------------------------------------------

	LOCAL lnCols, lnRows

	lnRows = ALEN(taArray, 1)
	lnCols = ALEN(taArray, 2)
	IF TYPE("taArray(lnRows, 1)") != "L"	&& Check for ReDim
		lnRows = lnRows + 1
		DIMENSION taArray(lnRows, lnCols)
	ENDIF
	RETURN lnRows
ENDFUNC


*==============================================================================================
* Program....:  CREATEMEMBERDATAOFPRGBASEDCLASSCODE.PRG
*				
* Version....:  1.0
* Author.....:  Frank Dietrich
* Date.......:  18.September 2025 
* Notice.....:  Copyright (c) 2025 
*               Frank Dietrich Datentechnik
*               Leo-Baeck-Str. 22
*               14165 Berlin
*  			    CIS   :	100322,333
*				e-mail: frank.dietrich@dd-tech.de, All Rights Reserved.
* Compiler...:  Visual FoxPro 09.00.0000.7423 for Windows
*
* Abstract...:  Section with addtional procedures for in-editor functionality
* Changes....:



*==============================================================================================
*
*         PROCEDURE: getClassName             
*
*==============================================================================================
PROCEDURE getClassName             
*  Created...........:  18.September 2025, 11:52 Uhr
*  Changed...........:   
*  Description.......:  return the class name of the class definition the cursor is currently in 
*						called by getMemberDataFromEditor()
*
*  Calling Samples...:  ?getClassName(<@ExpA>, <ExpnN>)
*  Parameters........:  @taLines, tnCurLine
*  Returns...........:  ClassName or empty string
lparameters	taLines, tnCurLine

LOCAL lcClassName, lcCurrentLine, lcLine, llFound, llHaveDefinition, lnLine, ;
      lnToLine, loLocalInfo

EXTERNAL ARRAY taLines

*-- if .T. and .F.
*-- 	dimension taLines[1]
*-- endif	

m.lcClassName = ""

for m.lnLine = m.tnCurLine to 1 step -1
	m.lcLine = stripIndent(m.taLines[m.lnLine])

	if empty(m.lcLine) or left(m.lcLine,1) = "*" 
		*-- ignore comments and empty lines
		*-- loop
	else		
		m.llHaveDefinition = atc("DEFINE CLASS ", m.lcLine)> 0
		if m.llHaveDefinition 
			m.lcClassName = strExtract(m.lcLine, "DEFINE CLASS ", " AS",1,1)	
			if empty(m.lcClassName)
				*-- something's wrong, but stop here
				wait window "empty Class definition found" time 5
				debugout m.lcLine
			endif
			exit
		endif		
	endif
endfor &&* m.lnLine = m.tnCurLine to 1 step -1

return m.lcClassName



*==============================================================================================
*
*         PROCEDURE: getMemberDataFromEditor             
*
*==============================================================================================
PROCEDURE getMemberDataFromEditor             
*  Created...........:  18.September 2025, 13:32 Uhr
*  Changed...........:   
*  Description.......:  Extract member data from within the editor
*						triggered from within the editor via on key command
*
*  Calling Samples...:  getMemberDataFromEditor()
*  Parameters........:  
*  Returns...........:  boolean - allways .T.
LOCAL lcClassName, lcProgText, lnHandle, lnResult, lnCurLine, lnLines, lcMemberData
LOCAL array laEnv[ 25 ], laLines[1]

*-- check if foxTools library is loaded
if loadFoxTools()

	m.lnHandle = _WonTop()
	m.lnResult = _EdGetEnv( lnHandle, @laEnv )

	IF ( lnResult = 0 ) OR;
		( laEnv[ 2 ] = 0 ) OR ;
		( laEnv[ 12 ] > 0 ) OR ;
		( laEnv[ 17 ] = 0 ) OR ;
		( NOT INLIST( laEnv[ 25 ], 1, 8, 10, 12 ) )
		RETURN
	ENDIF

	m.lnSelStart = laEnv[ 17 ] 

	*--------------------------------------------------------------------------------------
	*  Get the contents of the editing window into an array                                
	*  beware! if you have more than 65,000 lines of code,                                 
	*  this will crash                                                                     
	*--------------------------------------------------------------------------------------
	m.lcProgText = _EdGetStr( m.lnHandle, 0, m.laEnv[ 2 ] - 1 )
	m.lnLines 	 = aLines( m.laLines, m.lcProgText )

	*--------------------------------------------------------------------------------------
	*  Get the line number ( the number returned is 0-based )                              
	*  in which the cursor is currently positioned                                         
	*--------------------------------------------------------------------------------------
	m.lnCurLine = _EdGetLNum( m.lnHandle, m.lnSelStart )
	
	*--------------------------------------------------------------------------------------
	*  Detect the name of the class definition the cursor                                  
	*  is currently located in                                                             
	*--------------------------------------------------------------------------------------		
	m.lcClassName = getClassName( @laLines, m.lnCurLine )

	if ! empty(m.lcClassName)
		*--------------------------------------------------------------------------------------
		*  if successful call Toni's ogitinal procedure but provide                            
		*  the code content directly                                                           
		*--------------------------------------------------------------------------------------
		debugout "Class Name: ", m.lcClassName
		m.lcMemberData = doCreateMemberData(m.lcProgText, m.lcClassName, .T.)
	
		*--------------------------------------------------------------------------------------
		*  and if this succeeded, insert the pepares memberdata property                       
		*  at current cursor position. otherwise display result to debuger                                                          
		*--------------------------------------------------------------------------------------
		if upper(left(stripIndent(m.lcMemberData), 11)) = "_MEMBERDATA" 
			insertMemberData(m.lnHandle, m.lnSelStart, m.lnCurLine, m.lcMemberData)
		else
			*acti screen
			*? left(m.lcMemberdata, 400)
			debugout allt(left(m.lcMemberdata, 400))
		endif
	endif	

endif && loadFoxTools()

return .T.
* eop getMemberDataFromEditor
*==============================================================================================

*==============================================================================================
*
*         PROCEDURE: insertMemberData             
*
*==============================================================================================
PROCEDURE insertMemberData             
*  Created...........:  18.September 2025, 13:02 Uhr
*  Changed...........:   
*  Description.......:  insert created member data at current editor position
*
*  Calling Samples...:  insertMemberData(<ExpN1>, <ExpN2>, <ExpN3>, <ExpC>)
*  Parameters........:  tnHandle, tnSelStart, tnLine, tcData
*  Returns...........:  boolean - always .T.
lparameters	tnHandle, tnSelStart, tnLine, tcData
LOCAL lnPos

m.lnPos = _EdGetLPos( m.tnHandle, m.tnLine ) 
_EdSetPos( m.tnHandle, m.lnPos )
m.lnLen = len( m.tcData ) 
_EdInsert( m.tnHandle, m.tcData, m.lnLen )
*** And Add a CR
_EdSendKey( tnHandle, 13 )
*WAIT WINDOW 'Local Declaration Inserted' NOWAIT
*** And reposition the cursor
*lnPos = tnSelStart + lnLen + 1
*_EdSetPos( tnHandle, lnPos )

*-- DD changed on 09.02.02
*_EdStoPos( tnHandle, lnPos, .T. )

return .T.
* eop insertMemberData
*==============================================================================================
ENDPROC


*=========================================================
*
*         FUNCTION: LoadFoxTools             
*
*=========================================================
PROCEDURE loadFoxTools             
*  Created...........:  7.Februar 2002, 10:59 Uhr
*  Changed...........:   
*  Description.......:  Loading Foxtools if not already loaded
*  Calling Samples...:  ? LoadFoxtools()
*  Parameters........:  
*  Returns...........:  .T. or .F.
local lc_Path, ll_RetVal

*-- Is it already loaded?
if atc("foxtools", set("LIBRA")) = 0
	
	*===================================*
	*  No, Foxtools is not loaded. See  *
	*  if we shall use a special path   *
	*  or get it from the default VFP-  *
	*  Directory                        *
	*===================================*
	m.lc_Path = iif(empty(CON_FOXTOOLS_PATH), ;
				addbs(home()),;
				addbs(allt(CON_FOXTOOLS_PATH)))

	*====================================*
	*  If FoxTools is available load it  *
	*  otherwise just return .F.         *
	*====================================*
	if file(m.lc_Path + "foxtools.fll")
		*-- DD changed on 07.11.02
		*-- This still pointed to home()				
		*set library to (home() +"FoxTools") additive
		set library to (m.lc_Path +"FoxTools") additive
		m.ll_RetVal = .T.
	else
		m.ll_RetVal = .F.
	endif				
else
	*-- Yes, it's already loaded
	m.ll_RetVal = .T.	
endif	

return m.ll_RetVal
*-- eof loadFoxTools


*==============================================================================================
*
*         PROCEDURE: stripIndent             
*
*==============================================================================================
PROCEDURE stripIndent             
*  Created...........:  18.September 2025, 12:06 Uhr
*  Changed...........:   
*  Description.......:  remove leading spaces or tabs from given line
*  Calling Samples...:  stripIndent(<ExpC>)
*  Parameters........:  tcLine
*  Returns...........:  string - line without indentation
lparameters	tcLine
LOCAL lcRetVal, lnLLength, lnChar
m.tcLine 	= iif(vartype(m.tcLine)="C", m.tcLine, "")

m.lcRetVal 	= m.tcLine
m.lnLLength = len(m.tcLine)

if m.lnLLength > 0
	for m.lnChar = 1 to m.lnLLength
		if inlist(left(m.lcRetVal,1), " ", chr(9))
			m.lcRetVal = substr(m.lcRetVal, 2)
		else
			exit
		endif
	endfor &&* m.lnChar = 1 to m.lnLLength
endif

return m.lcRetVal
* eop stripIndent
*==============================================================================================
ENDPROC
