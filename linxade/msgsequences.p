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

DEFINE VARIABLE liDbNum AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcDbName AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lcSeqName AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcQuery AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhquery AS HANDLE    NO-UNDO.

DEFINE VARIABLE lhSeqBuffer AS HANDLE      NO-UNDO.

{linxade/nonzeromin.i}



CREATE WIDGET-POOL "MSGDATATYPE".

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
/*  c_word = RIGHT-TRIM ( c_word, "."  ). */
c_word = LEFT-TRIM  ( c_word, "~{" ).       /* Allow includes */
c_word = RIGHT-TRIM ( c_word, "~}" ).       /* Allow includes */
c_word = TRIM       ( c_word, '"'  ).       /* Quotes */
c_word = TRIM       ( c_word, "'"  ).       /* Single quotes */
c_word = REPLACE ( c_word, "/" + "*", "").  /* Open comments */
c_word = REPLACE ( c_word, "*" + "/", "").  /* Close comments */

lcSeqName = c_word.

DO liDbNum = 1 TO NUM-DBS:

    lcDbName = LDBNAME ( liDbNum ) .

lcQuery = "FOR EACH " + lcDbName + "._sequence NO-LOCK " +
          "  WHERE _sequence._seq-name matches '*' + " + QUOTER ( lcSeqName ) + " + '*'".

    CREATE BUFFER lhSeqBuffer 
        FOR TABLE ( lcDbName + "._sequence" )
        IN WIDGET-POOL "MSGDATATYPE".
    
    CREATE QUERY lhQuery
        IN WIDGET-POOL "MSGDATATYPE".

    lhQuery:ADD-BUFFER ( lhSeqBuffer ).
    lhQuery:QUERY-PREPARE ( lcQuery ).

    lhQuery:QUERY-OPEN ( ).

    lhQuery:GET-FIRST ( ).

    IF lhQuery:QUERY-OFF-END THEN
        NEXT.

    DO WHILE NOT lhQuery:QUERY-OFF-END:

        MESSAGE lhSeqBuffer:BUFFER-FIELD ("_seq-name"):BUFFER-VALUE VIEW-AS ALERT-BOX.

        lhQuery:GET-NEXT ( ).

    END.

END.
    
    APPLY "ENTRY":U TO SELF.
    
DELETE WIDGET-POOL "MSGDATATYPE".



