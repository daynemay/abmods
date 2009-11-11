
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
DEFINE VARIABLE llValidTable AS LOGICAL     NO-UNDO.

DEFINE VARIABLE lcTableName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFieldName AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcQuery AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhquery AS HANDLE    NO-UNDO.

DEFINE VARIABLE lhFileBuffer AS HANDLE      NO-UNDO.
DEFINE VARIABLE lhFieldBuffer AS HANDLE NO-UNDO.

DEFINE VARIABLE lcMessage  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcThisLine AS CHARACTER   NO-UNDO.

DEFINE VARIABLE liExtent AS INTEGER NO-UNDO.
DEFINE VARIABLE lcOrigTableName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lii AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ttField LIKE _field.

/*
DEFINE VARIABLE llFound      AS LOGICAL    NO-UNDO.
  */
{linxade/nonzeromin.i}

&SCOPED-DEFINE recordsPerMessage 50

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



CASE NUM-ENTRIES ( c_word, "." ):

    WHEN 2 THEN
    DO:
        ASSIGN
            lcTableName = ENTRY ( 1, c_word, "." )
            lcFieldName = ENTRY ( 2, c_word, "." ).
    END.

    WHEN 3 THEN
    DO:
            ASSIGN
            lcTableName = ENTRY ( 2, c_word, "." )
            lcFieldName = ENTRY ( 3, c_word, "." ).
    END.

    OTHERWISE
        RETURN.

END CASE.


RUN utils/db/isdbtable.p
    ( INPUT lcTableName,
      OUTPUT llValidTable,
      OUTPUT liDbNum ).

IF NOT llValidTable THEN
DO:

    /* Check for things like ttwhatever, bwhatever, bttwhatever, b2ttwhatever as well */
    lcOrigTableName = lcTableName.

    DO lii = 2 TO 5
        WHILE NOT llValidTable:

        lcTableName = SUBSTRING ( lcOrigTableName, lii ).    
        
        RUN utils/db/isdbtable.p
            ( INPUT lcTableName,
              OUTPUT llValidTable,
              OUTPUT liDbNum ).
    
    END.

END.
    
IF /* still */ NOT llValidTable THEN
    RETURN.

lcDbName = LDBNAME ( liDbNum ).

lcQuery = "FOR EACH " + lcDbName + "._file NO-LOCK " +
          "  WHERE _file._file-name = " + QUOTER ( lcTableName ) +
          ", EACH " + lcDbName + "._field NO-LOCK " +
          "  OF _file " +
          "  WHERE _field._field-name matches '*' + " + QUOTER ( lcFieldName ) + " + '*' ".


    CREATE BUFFER lhFileBuffer
        FOR TABLE ( lcDbName + "._file" )
        IN WIDGET-POOL "MSGDATATYPE".

    CREATE BUFFER lhFieldBuffer
        FOR TABLE ( lcDbName + "._field" )
        IN WIDGET-POOL "MSGDATATYPE".

    CREATE QUERY lhQuery
        IN WIDGET-POOL "MSGDATATYPE".

    lhQuery:ADD-BUFFER ( lhFileBuffer ).
    lhQuery:ADD-BUFFER ( lhFieldBuffer ).

    lhQuery:QUERY-PREPARE ( lcQuery ).

    lhQuery:QUERY-OPEN ( ).

    lhQuery:GET-FIRST ( ).

    IF lhQuery:QUERY-OFF-END THEN
        RETURN.

    ASSIGN
        i = 0
        lcMessage = "".

    EMPTY TEMP-TABLE ttField.

    DO WHILE NOT lhQuery:QUERY-OFF-END:

        i = i + 1.

        CREATE ttField.
        BUFFER ttField:BUFFER-COPY ( lhFieldBuffer, "", "" ).
        lhQuery:GET-NEXT ( ).

    END.

    DEFINE VARIABLE lcTheField AS CHARACTER   NO-UNDO.
    
    RUN linxade/getfield.w 
        ( INPUT  lcDbName,
          INPUT  lcTableNAme,
          INPUT  TABLE ttField,
          OUTPUT lcTheField).

/* TODO: REPLACE lcFieldName  (thje partial field) with the fiull field  */
    SELF:INSERT-STRING ( TRIM ( lcTheField, lcFieldName )  ).

    APPLY "ENTRY":U TO SELF.

DELETE WIDGET-POOL "MSGDATATYPE".





