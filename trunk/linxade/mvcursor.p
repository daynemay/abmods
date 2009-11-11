DEFINE INPUT PARAMETER c_Direction AS CHARACTER NO-UNDO.

/***************************************************/
/* This isn't working well for large files. This   */
/* is because of limitations in linxade/gtedline.p */
/***************************************************/

DEFINE VARIABLE toMoveVertical   AS INTEGER NO-UNDO.
DEFINE VARIABLE toMoveHorizontal AS INTEGER NO-UNDO.

DEFINE VARIABLE cWord   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLine   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCursor AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLength AS INTEGER   NO-UNDO.

DEFINE VARIABLE lMoveRequired AS LOGICAL NO-UNDO.

DEFINE VARIABLE iCursorChar AS INTEGER NO-UNDO.
DEFINE VARIABLE iCursorLine AS INTEGER NO-UNDO.

IF SELF:TYPE <> "EDITOR" THEN RETURN.

CASE c_Direction:

    WHEN "CTRL-LEFT" THEN 
    DO:

        RUN PreviousWord.

    END.

    WHEN "CTRL-RIGHT" THEN 
    DO: 

        RUN NextWord.

    END.

    WHEN "LEFT" THEN
    DO:
        
        RUN PreviousChar.

    END.

    WHEN "RIGHT" THEN
    DO:

        RUN NextChar.

    END.

END CASE.

APPLY "ENTRY":U TO SELF.





PROCEDURE PreviousChar.

    IF SELF:CURSOR-CHAR = 1 THEN
    DO:

        RUN linxade/gtedline.p
            (INPUT SELF,
             INPUT SELF:CURSOR-LINE - 1,
             OUTPUT cLine ).

        SELF:CURSOR-CHAR = IF SELF:CURSOR-LINE = 1
                           THEN 1
                           ELSE LENGTH ( cLine ) + 1.

        SELF:CURSOR-LINE = MAX ( 1, SELF:CURSOR-LINE - 1 ).

    END.
    ELSE
    DO:
        SELF:CURSOR-CHAR = SELF:CURSOR-CHAR - 1.
    END.

END PROCEDURE.





PROCEDURE NextChar.

    RUN linxade/gtedline.p
        (INPUT SELF,
         INPUT SELF:CURSOR-LINE,
         OUTPUT cLine ).

    IF SELF:CURSOR-CHAR > LENGTH ( cLine ) THEN
    DO:
        IF SELF:CURSOR-LINE = SELF:NUM-LINES THEN
        DO:
            /* Nothing*/
        END.
        ELSE
        DO:
            ASSIGN
                SELF:CURSOR-LINE = SELF:CURSOR-LINE + 1
                SELF:CURSOR-CHAR = 1.
        END.
    END.
    ELSE
    DO:
        SELF:CURSOR-CHAR = SELF:CURSOR-CHAR + 1.
    END.

END PROCEDURE.









PROCEDURE PreviousWord.
        

    ASSIGN
        lMoveRequired = TRUE
        iCursorChar = SELF:CURSOR-CHAR
        iCursorLine = SELF:CURSOR-LINE.

    

    MOVE_BACK_BLOCK:
    DO WHILE lMoveRequired:

        RUN linxade/gtedword.p
            (INPUT SELF,
             INPUT iCursorLine,
             INPUT iCursorChar,
             OUTPUT cWord,
             OUTPUT iLine,
             OUTPUT iCursor,
             OUTPUT iLength).

        /* If we're at (or before) the beginning of a word, move to the previous */
        IF iCursor = iCursorChar OR cWord = "" THEN DO:
            IF iCursorChar > 1 THEN DO:
                iCursorChar = iCursorChar - 1.
                lMoveRequired = TRUE.
            END.
            ELSE DO:
                IF iCursorLine = 1 THEN DO:
                    lMoveRequired = FALSE.
                END.
                ELSE DO:
                    iCursorLine = iCursorLine - 1.
                    RUN linxade/gtedline.p
                        (INPUT SELF,
                         INPUT iCursorLine,
                         OUTPUT cLine).

                    iCursorChar = MAX ( LENGTH ( cLine ), 1 ).
                    lMoveRequired = TRUE.
                END.
            END.
            NEXT MOVE_BACK_BLOCK. 
        END.

        /* If we're in a word, move to the beginning of that word */
        IF cWord <> "" THEN DO:
            ASSIGN
                iCursorLine = iLine
                iCursorChar = iCursor
                lMoveRequired    = FALSE.
            NEXT MOVE_BACK_BLOCK.
        END.

    END. /* DO WHILE lMoveRequired */

    SELF:CURSOR-LINE = iCursorLine.
    SELF:CURSOR-CHAR = iCursorChar NO-ERROR.


END PROCEDURE. /* PreviousWord */






PROCEDURE nextWord.

    DEFINE VARIABLE lSeekingFirstWordOnLine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSeekingNextWord        AS LOGICAL NO-UNDO.
    
    ASSIGN
        lMoveRequired = TRUE
        iCursorChar = SELF:CURSOR-CHAR
        iCursorLine = SELF:CURSOR-LINE.

    MOVE_FORWARD_BLOCK:
    DO WHILE lMoveRequired:

        RUN linxade/gtedline.p
            (INPUT SELF,
             INPUT iCursorLine,
             OUTPUT cLine).

        RUN linxade/gtedword.p
            (INPUT SELF,
             INPUT iCursorLine,
             INPUT iCursorChar,
             OUTPUT cWord,
             OUTPUT iLine,
             OUTPUT iCursor,
             OUTPUT iLength).


        IF lSeekingNextWord THEN DO:
            IF iCursorChar = iCursor THEN DO:
                ASSIGN
                    lSeekingNextWord = FALSE
                    lMoveRequired    = FALSE.
                NEXT MOVE_FORWARD_BLOCK.
            END.
        END.

        /* If we're at (or past) the last word on the line, */
        /* go to the next line, unless it's the last line.  */
        IF ( iCursor + iLength ) >= LENGTH ( cLine )  THEN DO:

            IF iCursorLine = SELF:NUM-LINES THEN DO:
                lMoveRequired = FALSE.
                NEXT MOVE_FORWARD_BLOCK.
            END.

            ASSIGN
                iCursorLine = iCursorLine + 1
                iCursorChar = 1
                lMoveRequired = TRUE
                lSeekingFirstWordOnLine = TRUE.
            NEXT MOVE_FORWARD_BLOCK.
        END.


        /* If we're looking for the first word on the line... */
        IF lSeekingFirstWordOnLine THEN DO:

            /* Blank line, go to next. */
            IF TRIM ( cLine  ) = "" THEN DO:
                ASSIGN
                    iCursorChar = 1
                    iCursorLine = iCursorLine + 1
                    lMoveRequired = TRUE.
                NEXT MOVE_FORWARD_BLOCK.
            END.

            /* Go to beginning of first NON-BLANK character */
            iCursorChar = INDEX ( cLine, TRIM (cLine) ).
            lMoveRequired = FALSE.
            NEXT MOVE_FORWARD_BLOCK.

        END.



        /* If we're before the beginning of a word, move to the beginning */
        IF cWord = "" THEN DO:
            ASSIGN
                lSeekingFirstWordOnLine = TRUE
                lMoveRequired           = TRUE.
            NEXT MOVE_FORWARD_BLOCK.
        END.



        /* Eat blanks */
        IF SUBSTRING ( cLine, iCursorChar, 1 ) = " " THEN DO:
            ASSIGN
                iCursorChar = iCursorChar + 1
                lMoveRequired = TRUE
                lSeekingNextWord = TRUE.
            NEXT MOVE_FORWARD_BLOCK.
        END.

        /* If we're in a word, move to end of the word and keep searching */
        IF iCursorChar >= iCursor THEN DO:
            ASSIGN
                iCursorLine = iLine
                iCursorChar = iCursor + iLength + 1
                lSeekingNextWord = TRUE
                lMoveRequired    = TRUE.
            NEXT MOVE_FORWARD_BLOCK.
        END.
   





    END. /* DO WHILE lMoveRequired */

    SELF:CURSOR-LINE = MIN ( SELF:NUM-LINES, iCursorLine).
    SELF:CURSOR-CHAR = iCursorChar.

END PROCEDURE. /* NextWord */
