DEFINE INPUT  PARAMETER hEditor         AS HANDLE       NO-UNDO.
DEFINE INPUT  PARAMETER iLineNo         AS INTEGER      NO-UNDO.
DEFINE INPUT  PARAMETER iCursor         AS INTEGER      NO-UNDO.
DEFINE OUTPUT PARAMETER cWord           AS CHARACTER    NO-UNDO.
DEFINE OUTPUT PARAMETER iStartLine      AS INTEGER      NO-UNDO.
DEFINE OUTPUT PARAMETER iWordStart      AS INTEGER      NO-UNDO.
DEFINE OUTPUT PARAMETER iWordLength     AS INTEGER      NO-UNDO.

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE iWordEnd    AS INTEGER    NO-UNDO. /* c_word end    */

DEFINE VARIABLE i            AS INTEGER    NO-UNDO.
/**
DEFINE VARIABLE llFound      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lhWindow     AS HANDLE     NO-UNDO.
   **/

ASSIGN
    cWord = ""
    iWordStart = 0
    iWordLength = 0
    iStartLine = iLineNo.

RUN linxade/gtedline.p
   (INPUT hEditor,
    INPUT iLineNo,
    OUTPUT cLine).

IF TRIM ( cLine ) = "" THEN RETURN.

/* Look for the next space to the right of the cursor */
iWordEnd = INDEX ( cLine, " ", iCursor ).

IF iWordEnd = 0 THEN iWordEnd = LENGTH ( cLine ) + 1.

/* Trim off everything past the end of the current word  */
cLine       = RIGHT-TRIM ( SUBSTRING ( cLine, 1, iWordEnd ) ).

iWordStart  = R-INDEX(cLine, " ") + 1. /* The space before the current word */
iWordLength = iWordEnd - iWordStart.

cWord = SUBSTRING(cLine, iWordStart, iWordLength). /* Get the word */
                                   /****
/* Trim punctuation */
c_word = RIGHT-TRIM ( c_word, "."  ).  
c_word = LEFT-TRIM  ( c_word, "~{" ).  /* Allow includes */
c_word = RIGHT-TRIM ( c_word, "~}" ).  /* Allow includes */
c_word = TRIM       ( c_word, '"'  ).  /* Quotes */
c_word = TRIM       ( c_word, "'"  ).  /* Single quotes */
c_word = REPLACE ( c_word, "/*", "").  /* Open comments */
c_word = REPLACE ( c_word, "*/", "").  /* Close comments */

****/
