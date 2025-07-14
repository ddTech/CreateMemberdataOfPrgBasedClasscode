LPARAMETERS tcPrgPath, tcClassName

#DEFINE CON_VfpCrLf CHR(13) + CHR(10)

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
LOCAL lnLine, lnLines, lnPEMs

LOCAL ARRAY laCodeLines[1]
LOCAL ARRAY laPropertiesList[1, 3]

lcReturn = "It's nothing todo!"

IF VARTYPE(tcPrgPath) != "C" OR EMPTY(tcPrgPath)
	DEBUGOUT "Parameter tcPrgPath is not optional!"
	RETURN lcReturn
ENDIF
IF VARTYPE(tcClassName) != "C" OR EMPTY(tcClassName)
	DEBUGOUT "Parameter tcClassName is not optional!"
	RETURN lcReturn
ENDIF

lcPrgPath	= tcPrgPath
lcClassName	= tcClassName


lcFileExt = UPPER(JUSTEXT(lcPrgPath))
IF !lcFileExt == "PRG"
	DEBUGOUT "Allowed fileextension is PRG, your extension is " + lcFileExt
	RETURN lcReturn
ENDIF
IF !FILE(lcPrgPath)
	DEBUGOUT 'File "' + lcPrgPath + '" not found'
	RETURN lcReturn
ENDIF

*!*  -------------------------------- TONI KOEHLER 2025-07-14 14:56:21 --------------------------------------
*!*  Begin with work.
*!*  Extracting class code.
*!*  -------------------------------------------------------------------------------------------------------

lcStartTag	 = "DEFINE CLASS " + lcClassName + " AS "
lcEndTag	 = "ENDDEFINE"
lcFileStream = FILETOSTR(lcPrgPath)
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
TEXT TO lcReturn ADDITIVE TEXTMERGE NOSHOW FLAGS 1 PRETEXT 4
	_MEMBERDATA = ;
		+ [<VFPData>] + CHR(13) ;
ENDTEXT

FOR lnElement = 1 TO lnPEMs
	TEXT TO lcReturn ADDITIVE TEXTMERGE NOSHOW FLAGS 1&& PRETEXT 4
	
		+ [<memberdata name="<<laPropertiesList(lnElement, 1)>>" type="<<laPropertiesList(lnElement, 3)>>" display="<<laPropertiesList(lnElement, 2)>>"/>] + CHR(13) ;
	ENDTEXT
NEXT

TEXT TO lcReturn ADDITIVE TEXTMERGE NOSHOW FLAGS 1 + 2
		+ [</VFPData>]
		
ENDTEXT

DEBUGOUT "Memberdata finished"
RETURN lcReturn


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


