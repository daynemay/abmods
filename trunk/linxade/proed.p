DEFINE VARIABLE lcWinTitle AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcFileName  AS CHARACTER NO-UNDO.

IF SELF:TYPE <> "EDITOR" THEN
    RETURN.

IF SELF:WINDOW = ? THEN
    RETURN.

ASSIGN 
    lcWinTitle    = SELF:WINDOW:TITLE

    /* lcWinTitle will be something like "Procedure - .\toll-rate\fp-br.p" */
    /* This line will strip out everything after the last SPACE in that.   */
    lcFileName    = TRIM ( 
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
                         ) NO-ERROR.

IF SEARCH ( lcFileName ) = ? THEN
    RETURN.

/* Open it in the Procedure Window  */
RUN adecomm/_pwmain.p (INPUT "_ab.p",       /* AppBuilder as parent */
                       INPUT lcFileName,  /* The file */
                       INPUT "").           /* Edit mode */
