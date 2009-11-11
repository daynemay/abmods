DEFINE INPUT PARAMETER pcCompilationType AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcWinTitle AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcInFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutFileName AS CHARACTER NO-UNDO.


IF SELF:TYPE <> "EDITOR" THEN
    RETURN.

IF SELF:WINDOW = ? THEN
    RETURN.

ASSIGN 
    lcOutFileName = "./compoutput.txt"
    lcWinTitle    = SELF:WINDOW:TITLE

    /* lcWinTitle will be something like "Procedure - .\toll-rate\fp-br.p" */
    /* This line will strip out everything after the last SPACE in that.   */
    lcInFileName  = TRIM ( 
                        SUBSTRING ( 
                            lcWinTitle, 
                            R-INDEX ( 
                                lcWinTitle, 
                                " " 
                                    ),
                            LENGTH ( lcWinTitle ) 
                            - R-INDEX ( lcWinTitle, " ") 
                            + 1
                                  )
                         ).

IF SEARCH ( lcInFileName ) = ? THEN
    RETURN.

CASE pcCompilationType:

    WHEN "XREF" THEN DO:
        COMPILE VALUE ( lcInFileName ) XREF VALUE ( lcOutFileName ).
    END.

    WHEN "PREPROCESS" THEN DO:
        COMPILE VALUE ( lcInFileName ) PREPROCESS VALUE ( lcOutFileName ).
    END.

    WHEN "LISTING" THEN DO:
        COMPILE VALUE ( lcInFileName ) LISTING VALUE ( lcOutFileName ).
    END.

    WHEN "DEBUG-LIST" THEN DO:
        COMPILE VALUE ( lcInFileName ) DEBUG-LIST VALUE ( lcOutFileName ).
    END.



    OTHERWISE DO:
    END.

END CASE. /* pcCompilationType */

/* Open it in the Procedure Window  */
RUN adecomm/_pwmain.p (INPUT "_ab.p",       /* AppBuilder as parent */
                       INPUT lcOutFileName, /* The compilation output */
                       INPUT "").           /* Edit mode */

OS-DELETE VALUE ( lcOutFileName ) NO-ERROR.
