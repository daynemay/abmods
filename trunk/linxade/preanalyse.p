DEFINE VARIABLE lcWinTitle AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcInFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutFileName AS CHARACTER NO-UNDO.

RUN getCurrentFilename
    ( OUTPUT lcInFileName ).

RUN analyseFile.








PROCEDURE getCurrentFilename.
    DEFINE OUTPUT PARAMETER pcInFileName AS CHARACTER NO-UNDO.

    IF SELF:TYPE <> "EDITOR" THEN
        RETURN.
    
    IF SELF:WINDOW = ? THEN
        RETURN.
    
    ASSIGN 
        lcWinTitle    = SELF:WINDOW:TITLE
    
        /* lcWinTitle will be something like "Procedure - .\toll-rate\fp-br.p" */
        /* This line will strip out everything after the last SPACE in that.   */
        pcInFileName  = TRIM ( 
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
    
    IF SEARCH ( pcInFileName ) = ? THEN
        RETURN.

END PROCEDURE.


DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD ttName AS CHARACTER.

DEFINE TEMP-TABLE ttTempTableField NO-UNDO
    FIELD ttTableName AS CHARACTER
    FIELD ttFieldName AS CHARACTER
    FIELD dataType    AS CHARACTER
    FIELD fieldExtent AS CHARACTER.

PROCEDURE analyseFile.

    COMPILE VALUE ( lcInFileName ) PREPROCESS VALUE ( lcOutFileName ).
    
END PROCEDURE.
    

