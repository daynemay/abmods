DEFINE VARIABLE c_filename   AS CHARACTER  NO-UNDO. /* The file to open */

DEFINE VARIABLE c_line       AS CHARACTER  NO-UNDO. /* Current line in editor */
DEFINE VARIABLE c_word       AS CHARACTER  NO-UNDO. /* Current word in editor */

DEFINE VARIABLE i_WordStart  AS INTEGER    NO-UNDO. /* c_word start  */
DEFINE VARIABLE i_WordLength AS INTEGER    NO-UNDO. /* c_word length */
DEFINE VARIABLE i_WordEnd    AS INTEGER    NO-UNDO. /* c_word end    */

DEFINE VARIABLE c_tempdir    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c_tempfile   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE l_modified   AS LOGICAL    NO-UNDO. /* Changed editor contents? */

DEFINE VARIABLE i            AS INTEGER    NO-UNDO.

DEFINE VARIABLE llCancel     AS LOGICAL    NO-UNDO.

DEFINE VARIABLE llFound      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lhWindow     AS HANDLE     NO-UNDO.

DEFINE STREAM sMru.

{linxade/mru-tt.i}
{linxade/mru.i}

{linxade/nonzeromin.i}
{linxade/winprocs.i}


CREATE WIDGET-POOL "PPOPEN".

IF SELF:WINDOW = fAppBuilderWindow ( ) THEN
DO:

    c_filename = ?.

    DO WHILE SEARCH ( c_filename ) = ? AND
        NOT llCancel:

        RUN userInputFileName
            ( INPUT-OUTPUT c_filename,
              OUTPUT llCancel ).

    END.

    IF llCancel THEN
    DO:
        APPLY "ENTRY":U TO SELF.
        RETURN.
    END.

    RUN openFileForEditing
        ( INPUT c_filename ).

    RETURN.

END.




/*If this is not a ADE program such as the Editor or Section-Editor then return */
IF NOT ENTRY(NUM-ENTRIES(PROGRAM-NAME(2)), PROGRAM-NAME(2), "/") BEGINS "ADE" THEN
    RETURN.


IF SELF:TYPE NE "EDITOR" THEN
    RETURN.


/* Determine where we we should drop our temporary file.  Look for */
/* this program, and make a temporary file in the same directory.  */
c_tempdir = RIGHT-TRIM( SEARCH( PROGRAM-NAME (1) ), PROGRAM-NAME (1) ).
c_tempdir = REPLACE ( c_tempdir, "~\", "/").
c_tempdir = RIGHT-TRIM ( c_tempdir, "/" ).

c_tempfile = c_tempdir + "/tempfile.p".

/* Write the contents of the procedure window out to a temporary file.  */
/* We then read the file line by line until we get to where the cursor  */
/* is positioned.  It has to be done this way, since we can't deal with */
/* the EDITOR:SCREEN-VALUE if there is over 32K in the text area.       */
l_modified = SELF:MODIFIED.
IF NOT SELF:SAVE-FILE ( c_tempfile ) THEN RETURN.
SELF:MODIFIED = l_modified. /* Because SAVE-FILE() sets MODIFIED to FALSE */

INPUT FROM VALUE ( c_tempfile ).
DO i = 1 TO SELF:CURSOR-LINE ON END-KEY UNDO, LEAVE:

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
        " occurred while deleting temporary file." SKIP
        " You will have to manually delete " +
        c_tempfile + "."
        VIEW-AS ALERT-BOX.

/* Look for the next space or punctuation char to the right of the cursor */
i_WordEnd = NON-ZERO-MIN (
                 STRING ( INDEX ( c_line, " ",  SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "~}", SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "~{", SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "'",  SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, '"',  SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "(",  SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, ")",  SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, ",",  SELF:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "~~", SELF:CURSOR-CHAR ) )
                ).

IF i_WordEnd = 0 THEN i_WordEnd = LENGTH ( c_line ) + 1.

/* Trim off everything past the end of the current word  */
c_line       = TRIM(SUBSTRING(c_line, 1, i_WordEnd)).

/* The space or punctuation character before the current word */
i_WordStart  = 1 + MAX (
                        R-INDEX ( c_line, " " , SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "~}", SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "~{", SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "'" , SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, '"' , SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "(" , SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, ")" , SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "," , SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "~~", SELF:CURSOR-CHAR ),
                        R-INDEX ( c_line, "=" , SELF:CURSOR-CHAR )
                       ).

i_WordLength = i_WordEnd - i_WordStart.

c_word = SUBSTRING(c_Line, i_WordStart, i_WordLength). /* Get the word */

/* Trim punctuation */
c_word = RIGHT-TRIM ( c_word, "."  ).
c_word = LEFT-TRIM  ( c_word, "~{" ).       /* Allow includes */
c_word = RIGHT-TRIM ( c_word, "~}" ).       /* Allow includes */
c_word = TRIM       ( c_word, '"'  ).
c_word = TRIM       ( c_word, "'"  ).
c_word = REPLACE ( c_word, "/" + "*", "").  /* Open comments */
c_word = REPLACE ( c_word, "*" + "/", "").  /* Close comments */
c_word = TRIM       ( c_word, '('  ).
c_word = TRIM       ( c_word, ')'  ).

/* Get the full pathname */
c_filename = c_word.

/* Class names */
IF c_filename MATCHES "au~.com~.*" THEN
DO:
    c_filename = REPLACE ( c_filename, ".", "/" ) + ".cls".
END.

DO WHILE SEARCH ( c_filename ) = ? AND NOT llCancel:
    
    RUN userInputFileName
        ( INPUT-OUTPUT c_filename,
          OUTPUT llCancel ).

END.

IF llCancel THEN
DO:
    APPLY "ENTRY":U TO SELF.
    RETURN.
END.

RUN openFileForEditing
    ( INPUT c_filename ).

DELETE WIDGET-POOL "PPOPEN".



/***********************************************************************/
PROCEDURE openFileForEditing.

    DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.

    IF pcFileName = ? THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN.
    END.

    /* Is the file already open in the AppBuilder? */
    RUN searchWidgetTree
        (pcFilename,
         SESSION:FIRST-CHILD,
         OUTPUT lhWindow,
         OUTPUT llFound).

    /* If it is already open, just go to that window. */
    IF llFound THEN
    DO:
        
        lhWindow:WINDOW-STATE = WINDOW-NORMAL.
        APPLY "ENTRY":U TO lhWindow.

    END.
    ELSE
    DO:

        /* Add it to my own private little MRU stash... */
        RUN getMru.
        
        /* Open it in the Procedure Window  */
        RUN adecomm/_pwmain.p (INPUT "_ab.p",     /* AppBuilder as parent */
                               INPUT SEARCH ( pcFilename ),  /* The file! */
                               INPUT "").         /* Edit mode */
    END.

END PROCEDURE. /* openFileForEditing */



/***********************************************************************/
PROCEDURE userInputFileName:
    DEFINE INPUT-OUTPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER plCancel   AS LOGICAL   NO-UNDO.

    RUN linxade/filedialog.w
        ( INPUT-OUTPUT pcFileName,
          OUTPUT plCancel ).

    IF plCancel THEN
        RETURN.

    IF SEARCH ( pcFileName ) = ? THEN
        MESSAGE
            "Could not find"
            pcFileName
            "in the PROPATH."
            VIEW-AS ALERT-BOX.

END PROCEDURE.  /* userInputFileName */

