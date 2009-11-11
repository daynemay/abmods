/* *************************************************************************************************
Program      : Flist.p
Version      : 1.0
Date         : August of 2001
Platform     : Progress V9.1B 
Purpose      : This is a plugin for the Progress Procedure Editor allowing the programmer a quick
               look at the database fields associated with a table

Authors      : Cherian George &
               Alex Cupiers @ Exel Logistics Ireland Limited.               
Usage        : This program is called as Persistent Trigger from /gui/adecomm/_adeevnt.p                

************************************************************************************************** */

DEFINE VARIABLE hdlSList  AS WIDGET-HANDLE. /* Selection List Handle*/
DEFINE VARIABLE hdlFrame  AS WIDGET-HANDLE. /* Dialog-Box Handle */
DEFINE VARIABLE hdlEdit   AS WIDGET-HANDLE. /* Editor Handle */


DEFINE VARIABLE chTemp AS CHARACTER NO-UNDO. /*Temporary Character Variable */
DEFINE VARIABLE l_ok   AS LOGICAL   NO-UNDO. /* Temporary Logical Variable*/

DEFINE VARIABLE c_line       AS CHARACTER  NO-UNDO. /*Line in the Editor that is being edited*/

DEFINE VARIABLE c_word       AS CHARACTER  NO-UNDO.     
DEFINE VARIABLE c_word1      AS CHARACTER  NO-UNDO.

DEFINE VARIABLE i_WordStart  AS INTEGER    NO-UNDO. /* Word Starting Position */
DEFINE VARIABLE i_Wordlength AS INTEGER    NO-UNDO. /* Word Length in Characters*/
DEFINE VARIABLE i_WordEnd    AS INTEGER    NO-UNDO. /* End of the current word */

DEFINE VARIABLE c_tempdir    AS CHARACTER  NO-UNDO. /* a temporary directory */
DEFINE VARIABLE c_tempfile   AS CHARACTER  NO-UNDO. /* a temporary file */

DEFINE VARIABLE l_modified   AS LOGICAL    NO-UNDO. /* Changed editor contents? */
DEFINE VARIABLE l_period     AS LOGICAL    NO-UNDO. /* Did the user include "." */

DEFINE VARIABLE i            AS INTEGER    NO-UNDO.

/*If this is not a ADE program such as the Editor or Section-Editor then return */
IF NOT ENTRY(NUM-ENTRIES(PROGRAM-NAME(2)), PROGRAM-NAME(2), "/") BEGINS "ADE" THEN RETURN. 

IF NUM-DBS EQ 0  THEN RETURN.
IF SELF:TYPE NE "EDITOR" THEN RETURN.

IF SELF:LENGTH = 0 THEN RETURN.

/* Determine where we we should drop our temporary file.  Look for */
/* this program, and make a temporary file in the same directory.  */

c_tempdir = RIGHT-TRIM( SEARCH( PROGRAM-NAME (1) ), PROGRAM-NAME (1) ).
c_tempdir = REPLACE ( c_tempdir, "~\", "/").
c_tempdir = RIGHT-TRIM ( c_tempdir, "/" ).

c_tempfile = c_tempdir + "/tempfile.p".

/* Write the contents of the procedure window out to a temporary file.  */
/* We then read the file line by line until we get to where the cursor  */
/* is positioned.  It has to be done this way, since we can't deal with */
/* the EDITOR:SCREEN-VALUE if there is over 32K in the editor window.   */
l_modified = SELF:MODIFIED.
IF NOT SELF:SAVE-FILE(c_tempfile) THEN RETURN.
SELF:MODIFIED = l_modified. /* Because SAVE-FILE() sets MODIFIED to FALSE */

INPUT FROM VALUE ( c_tempfile ).
DO i = 1 TO SELF:CURSOR-LINE:

  IMPORT UNFORMATTED c_line NO-ERROR.

END.
INPUT CLOSE.

IF OPSYS = "WIN32" THEN
  c_tempfile = REPLACE ( c_tempfile, "/", "~\" ).

/* Delete the temporary file */
OS-DELETE VALUE ( c_tempfile ).

/* Trouble deleting the temporary file? */
IF OS-ERROR NE 0 THEN
    MESSAGE 
        "Error #"  
        STRING(OS-ERROR,"99")
        " occurred while deleting temporary file."
        VIEW-AS ALERT-BOX.

/* Blank line.  Get out of here. */
IF TRIM(c_line) = "" THEN RETURN.

/* Look for the next space/EOL to the right of the cursor */
i_WordEnd = INDEX ( c_line, " ", SELF:CURSOR-CHAR ).

IF i_WordEnd = 0 THEN i_WordEnd = LENGTH ( c_line ) + 1.

/* Substring up to the end of the current word  */
c_line       = TRIM(SUBSTRING(c_line, 1, i_WordEnd)).

i_WordStart  = R-INDEX(c_line, " ") + 1. /* Look for a space position from the right end */
i_WordLength = i_WordEnd - i_WordStart.
                
c_word = SUBSTRING(c_Line, i_WordStart, i_WordLength). /* Get the word */


/* Punctuation.  If it ends in a ".", take note of that fact */
l_period = FALSE.
IF c_word MATCHES "*~~." THEN
  l_period = TRUE.

/* Trim punctuation */
c_word = RIGHT-TRIM ( c_word, "."  ).  
c_word = LEFT-TRIM  ( c_word, "~{" ).  /* Allow includes */
c_word = RIGHT-TRIM ( c_word, "~}" ).  /* Allow includes */
c_word = TRIM       ( c_word, '"'  ).  /* Quotes */
c_word = TRIM       ( c_word, "'"  ).  /* Single quotes */
c_word = REPLACE ( c_word, "/*", "").  /* Open comments */
c_word = REPLACE ( c_word, "*/", "").  /* Close comments */



IF TRIM(c_word) = "" THEN RETURN.

/*Search for the Table in all connected DB's*/
RUN linxade/changedb.p ( c_word, OUTPUT l_ok ).

IF NOT l_ok THEN DO:
    APPLY "ENTRY":U TO SELF.
    RETURN.
END.



FIND FIRST dictdb._file 
    WHERE dictdb._file._file-name = c_word 
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE dictdb._file  THEN RETURN. 

hdlEdit = SELF:HANDLE.
/*Create Dialog Box*/
CREATE DIALOG-BOX hdlFrame 
       ASSIGN HIDDEN = TRUE 
              X = 20
              Y = 20
              HEIGHT-CHARS = 12
              WIDTH-CHARS = 30
              BGCOLOR = 15
              FGCOLOR = 1
              TITLE = "Select Database Field".
    
/*If User pressed ESC or F4 */
ON "END-ERROR" OF hdlFrame OR
   "ENDKEY" OF hdlFrame DO :
    APPLY "GO" TO hdlframe.    
    RETURN NO-APPLY.
END.

/*Create the Selection List */
CREATE SELECTION-LIST hdlSlist
       ASSIGN FRAME = hdlFrame
              WIDTH-CHARS = 28
              HEIGHT-CHARS = 10
              BGCOLOR = /* 1  */ 15   /* zzbase */
              FGCOLOR = 4 
              FONT = 6 /* 5 */.
ON 'RETURN':U OF hdlSlist
DO:
    APPLY "GO" TO hdlFrame.
    RETURN NO-APPLY.
END.
ON 'TAB':U OF hdlSlist
DO:
    APPLY "GO" TO hdlFrame.
    RETURN NO-APPLY.
END.

IF AVAILABLE dictdb._file THEN DO :
    /*Add all field Names to the Selection List */
    FOR EACH dictdb._field OF dictdb._file NO-LOCK :
       hdlSlist:ADD-LAST(dictdb._field._field-name).
   END.
   /*Enable the Dialog-Box and Field List */
   ASSIGN   
   hdlFrame:HIDDEN = FALSE
   hdlFrame:SENSITIVE = TRUE
   hdlSlist:SENSITIVE = TRUE.
   /*Set focus to the frame and selection list */
   APPLY "ENTRY" TO hdlFrame.
   APPLY "ENTRY" TO hdlSlist.
   
   WAIT-FOR "GO" OF hdlFrame.
   /*If the user pressed the ESC button then exit with out inserting to the editor */
   IF LASTKEY EQ KEYCODE(KBLABEL("END-ERROR")) OR
      LASTKEY EQ KEYCODE("F4") OR 
      LASTKEY EQ KEYCODE("PF4") 
   THEN chTemp = "". 
   ELSE chTemp = hdlslist:SCREEN-VALUE.
END.
/*Delete the Widget hdlFrame */
DELETE WIDGET hdlFrame.
/*Set Focus to the Editor */
APPLY "ENTRY" TO hdlEdit.
IF chTemp = "" THEN RETURN.

/* Move the cursor to the end of the word */
SELF:CURSOR-CHAR = i_WordEnd.

/*Insert the selected field into the editor */
IF l_period THEN
    hdlEdit:INSERT-STRING(chTemp).
ELSE
    hdlEdit:INSERT-STRING("." + chTemp).

APPLY "ENTRY":u TO SELF.


    


                         



               
