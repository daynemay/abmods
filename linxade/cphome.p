DEFINE VARIABLE lcWinTitle   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcFileName   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcDirectory  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lii          AS INTEGER     NO-UNDO.
DEFINE VARIABLE lhWin        AS HANDLE      NO-UNDO.
DEFINE VARIABLE lhFrame      AS HANDLE      NO-UNDO.
DEFINE VARIABLE lhEditor     AS HANDLE      NO-UNDO.
DEFINE VARIABLE llFound      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE llContinue   AS LOGICAL     NO-UNDO.

DEFINE VARIABLE liCursorLine AS INTEGER     NO-UNDO.
DEFINE VARIABLE liCursorChar AS INTEGER     NO-UNDO.

{linxade/nonzeromin.i}

ASSIGN 
    lhWin         = SELF:WINDOW
    lcWinTitle    = lhWin:TITLE

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

IF lcFileName = "" OR lcFileName BEGINS "Untitled:" THEN 
    RETURN.

/* The user's home directory */
lcDirectory = ENTRY ( 1, PROPATH ).

/* Find first \ or / in the filename */
lii = NON-ZERO-MIN (
                    STRING ( INDEX ( lcFileName, "/"  ) ) + "," + 
                    STRING ( INDEX ( lcFileName, "~\" ) )
                   ).

/* Strip out everything before the first / or \ */
lcFileName = SUBSTRING ( lcFileName, lii ).

/* My home directory */
lcFileName = lcDirectory + lcFileName.

IF SEARCH ( lcFileName ) <> ? THEN 
DO:

    MESSAGE 
        "This file is already in your home directory.  Continue?" 
        VIEW-AS ALERT-BOX
        BUTTONS YES-NO UPDATE llContinue.

    IF NOT llContinue THEN 
    DO:

        APPLY "ENTRY" TO SELF.
        RETURN.

    END.

END.

/* Save contents of the file into home directory */
SELF:SAVE-FILE ( lcFileName ).

/* Save cursor location */
ASSIGN
    liCursorLine = SELF:CURSOR-LINE
    liCursorChar = SELF:CURSOR-CHAR.

/* Close the current file */
SELF:MODIFIED = FALSE.
APPLY "WINDOW-CLOSE":U TO lhWin.

/* Open the version we just saved to the user's home directory  */
RUN adecomm/_pwmain.p (INPUT "_ab.p",     /* AppBuilder as parent */
                       INPUT lcFileName,  /* The file */
                       INPUT "").         /* Edit mode */

IF FOCUS:TYPE = "EDITOR" THEN 
DO:

    ASSIGN
        /* Reposition the cursor to where it was */
        FOCUS:CURSOR-LINE = liCursorLine.
        FOCUS:CURSOR-CHAR = liCursorChar.

END.
