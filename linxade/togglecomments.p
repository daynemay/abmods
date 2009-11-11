FUNCTION beginsComment RETURNS LOGICAL
    ( INPUT lcText AS CHARACTER ) FORWARD.

FUNCTION endsComment RETURNS LOGICAL
    ( INPUT lcText AS CHARACTER ) FORWARD.

FUNCTION stripLeadingComment RETURNS CHARACTER
    ( INPUT lcText AS CHARACTER ) FORWARD.

FUNCTION stripTrailingComment RETURNS CHARACTER
    ( INPUT lcText AS CHARACTER ) FORWARD.




DEFINE VARIABLE llIsComment     AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lcSelText       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lii             AS INTEGER      NO-UNDO.
DEFINE VARIABLE lcCommentLine   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lcUncomment     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE liCursorChar    AS INTEGER      NO-UNDO.
DEFINE VARIABLE liCursorLine    AS INTEGER      NO-UNDO.
DEFINE VARIABLE llEndy          AS LOGICAL      NO-UNDO.



IF SELF:TYPE NE "EDITOR" THEN 
DO:
    APPLY "ENTRY":U TO SELF.
    RETURN.
END.

ASSIGN
    liCursorChar = SELF:CURSOR-CHAR
    liCursorLine = SELF:CURSOR-LINE.

IF SELF:SELECTION-TEXT = "" THEN
DO:

    IF SELF:CURSOR-LINE GE NUM-ENTRIES ( SELF:SCREEN-VALUE, CHR(10) ) - 1 THEN
        llEndy = TRUE.

    SELF:CURSOR-CHAR = 1.
    SELF:SOURCE-COMMAND("select_line","").
END.

ASSIGN
    lcSelText   = SELF:SELECTION-TEXT.
    llIsComment = beginsComment ( lcSelText ) AND endsComment ( lcSelText ).

IF llIsComment THEN
DO:

    /* If already a comment, uncomment each line. */
    DO lii = 1 TO NUM-ENTRIES ( lcSelText, CHR ( 13 ) ):

        lcCommentLine = ENTRY ( lii, lcSelText, CHR ( 13 ) ).
        lcCommentLine = REPLACE ( lcCommentLine, CHR(10), "" ).

        IF beginsComment ( lcCommentLine ) THEN
            lcCommentLine = stripLeadingComment ( lcCommentLine ).

        IF endsComment ( lcCommentLine ) THEN
            lcCommentLine = stripTrailingComment ( lcCommentLine ).

        lcUncomment = lcUncomment + CHR ( 13 ) + lcCommentLine. /* now uncommented */

    END.

    lcUncomment = SUBSTRING ( lcUncomment, 2 ). /* Remove that first CHR(13) */

    lcSelText = lcUncomment.

END.
ELSE
DO:
 /* If not already a comment, then comment out each line... */
    lcSelText = "/*" + REPLACE ( lcSelText, CHR(13), "*/" + CHR(13) + "/*" ) + "*/".
    lcSelText = REPLACE ( lcSelText, CHR(10), "" ).

    /* ...but don't bother commenting out blank lines. */
    lcSelText = REPLACE ( lcSelText, "/**/", "" ).
END.

/* Dodginess if we're dealing with the last line in the editor. */
IF llEndy THEN
DO:
    SELF:REPLACE-SELECTION-TEXT ( "" ).
    SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + lcSelText.
END.
ELSE
    SELF:REPLACE-SELECTION-TEXT ( lcSelText ).

APPLY "ENTRY":U TO SELF.


ASSIGN
    SELF:CURSOR-CHAR = liCursorChar
    SELF:CURSOR-LINE = liCursorLine.

RETURN.




FUNCTION beginsComment RETURNS LOGICAL
        ( INPUT lcText AS CHARACTER ).

    RETURN TRIM ( lcText ) MATCHES "/~~**".

END FUNCTION.

FUNCTION endsComment RETURNS LOGICAL
    ( INPUT lcText AS CHARACTER ).

    RETURN TRIM ( lcText ) MATCHES "*~~*/".

END FUNCTION.


FUNCTION stripLeadingComment RETURNS CHARACTER
    ( INPUT lcText AS CHARACTER ).

    DEFINE VARIABLE liOpenIndex AS INTEGER     NO-UNDO.

    liOpenIndex = INDEX ( lcText, "/*" ) .

    IF liOpenIndex GT 0 THEN
    DO:
        lcText = SUBSTRING ( lcText, 1, liOpenIndex - 1 ) + SUBSTRING ( lcText, liOpenIndex + 2 ).
    END.

    RETURN lcText.

END FUNCTION.


FUNCTION stripTrailingComment RETURNS CHARACTER
    ( INPUT lcText AS CHARACTER ).

    DEFINE VARIABLE liCloseIndex AS INTEGER     NO-UNDO.

    liCloseIndex = R-INDEX ( lcText, "*/" ) .

    IF liCloseIndex GT 0 THEN
    DO:
        lcText = SUBSTRING ( lcText, 1, liCloseIndex - 1 ) + SUBSTRING ( lcText, liCloseIndex + 2 ).
    END.

    RETURN lcText.

END FUNCTION.
