{linxade/magic-tt.i}
DEFINE INPUT PARAMETER pcFilename AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcText       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lrMagicRowid AS ROWID       NO-UNDO.

IF SELF:TYPE NE "EDITOR" THEN
    RETURN.

RUN linxade/getselectedtext.p
    ( SELF:WINDOW,
      OUTPUT lcText ).

IF lcText = "" THEN
DO:
    RUN linxade/getcurrentword.p
        ( SELF:WINDOW,
          FALSE,
          OUTPUT lcText ).
END.

IF lcText = "" THEN
DO:
    APPLY "ENTRY":U TO SELF.
    RETURN.
END.

RUN buildMagic
    ( INPUT pcFilename,
      INPUT lcText ).

APPLY "ENTRY":U TO SELF.

DEFINE VARIABLE liOrder AS INTEGER     NO-UNDO.

RUN linxade/showmagic.w
    ( INPUT TABLE ttMagic,
      OUTPUT liOrder ).

RUN applyMagic
    ( INPUT liOrder ).

RETURN.




PROCEDURE buildMagic.
    DEFINE INPUT PARAMETER pcFilename AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcTerm AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttMagic.

    DEFINE VARIABLE liOrder AS INTEGER     NO-UNDO.

    /*Things that Ctrl-. should provide:*/
    
    /* a list of (temp) table names matching pcTerm */
    RUN getTableNames
        ( INPUT pcTerm ).

    FOR EACH ttTable NO-LOCK
        BY ttTable.tablename:
        CREATE ttMagic.
        ASSIGN
            liOrder = liOrder + 1
            ttMagic.action  = {&CompleteTableName}
            ttMagic.keyText = ttTable.tableName
            ttMagic.notes   = "Complete table name '" + ttTable.tablename + "'"
            ttMagic.order   = liOrder.
        
    END.

    /* if pcTerm matches tablename.fieldname or part thereof, a list of matching fields */
    RUN getTableFields
        ( INPUT pcTerm ).

    FOR EACH ttField NO-LOCK
        BY ttField.fieldName:
        
        CREATE ttMagic.
        ASSIGN
            liOrder = liOrder + 1
            ttMagic.action         = {&CompleteFieldName}
            ttMagic.keyText        = ttField.fieldName
            ttMagic.notes          = "Complete field name '" + ttField.fieldName + "'".
            ttMagic.order          = liOrder.
    END.

    /* if pcTerm is a (temp) table name, a list of its indices */

    /* any sequence names matching pcTerm - insert NEXT-VALUE ( ) call */
    RUN getSequences
        ( INPUT pcTerm ).

    FOR EACH ttSequence NO-LOCK
        BY ttSequence.sequenceName:
        
        CREATE ttMagic.
        ASSIGN
            liOrder = liOrder + 1
            ttMagic.action         = {&NextValueSequence}
            ttMagic.keyText        = ttSequence.sequenceName
            ttMagic.notes          = "Insert 'NEXT-VALUE (" + ttSequence.sequenceName + ")'".
            ttMagic.order          = liOrder.
    END.

    /* variable names matching pcTerm */

    /* a list of instances of pcTerm in the current editor - see linxade/showinstances.p */
    RUN getInstances
        ( INPUT pcTerm,
          INPUT pcFilename ).

    FOR EACH ttInstance NO-LOCK
        BY ttInstance.lineNo:

        CREATE ttMagic.
        ASSIGN
            liOrder         = liOrder + 1
            ttMagic.action  = {&ShowAllInstances} + " '" + pcTerm + "'"
            ttMagic.keyText = TRIM ( ttInstance.lineContent )
            ttMagic.lineNo  = ttInstance.lineNo
            ttMagic.notes   = "Go to line " + STRING ( ttInstance.lineNo ) + ": " + ttInstance.lineContent
            ttMagic.order   = liOrder.
    
    END.
    

    /* "shortcuts" - Ctrl-Alt-S style (date, username, other) */


END PROCEDURE.




PROCEDURE getTableNames.
    DEFINE INPUT PARAMETER pcTablename AS CHARACTER NO-UNDO.

    /* TODO: Temp-table names as well as table names? */

    DEFINE VARIABLE lii AS INTEGER     NO-UNDO.

    DEFINE VARIABLE lcDbName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcQuery AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lhFileBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE lhQuery AS HANDLE      NO-UNDO.

    DO lii = 1 TO NUM-DBS:
    
        lcDbName = LDBNAME ( lii ).
    
        lcQuery = 
            "FOR EACH " + lcDbName + "._file NO-LOCK      " +
            "   WHERE " + lcDbName + "._file._file-name BEGINS " + QUOTER ( pcTableName ).
    
        CREATE BUFFER lhFileBuffer 
            FOR TABLE ( lcDbName + "._file" ).
    
        CREATE QUERY lhQuery.
    
        lhQuery:ADD-BUFFER ( lhFileBuffer ).
    
        lhQuery:QUERY-PREPARE ( lcQuery ).
    
        lhQuery:QUERY-OPEN ( ).
    
        lhQuery:GET-FIRST ( ).

        DO WHILE NOT lhQuery:QUERY-OFF-END:

            CREATE ttTable.
            ASSIGN
                ttTable.tablename = lhFileBuffer::_file-name.

            lhQuery:GET-NEXT().

        END.
    
    END.

END PROCEDURE.





PROCEDURE getTableFields.
    DEFINE INPUT PARAMETER pcTerm AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcTableName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcFieldName AS CHARACTER   NO-UNDO.

    CASE NUM-ENTRIES ( pcTerm, "." ):

        WHEN 2 THEN
        DO:
            ASSIGN
                lcTableName = ENTRY ( 1, pcTerm, "." )
                lcFieldName = ENTRY ( 2, pcTerm, "." ).
        END.
    
        WHEN 3 THEN
        DO:
                ASSIGN
                lcTableName = ENTRY ( 2, pcTerm, "." )
                lcFieldName = ENTRY ( 3, pcTerm, "." ).
        END.
    
        OTHERWISE
            RETURN.
    
    END CASE.

    DEFINE VARIABLE llValidTable AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE liDbNum AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lcDbName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcQuery AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lhFileBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE lhFieldBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE lhQuery  AS HANDLE      NO-UNDO.



    RUN utils/db/isdbtable.p
        ( INPUT lcTableName,
          OUTPUT llValidTable,
          OUTPUT liDbNum ).

    IF NOT llValidTable THEN
        RETURN.

    lcDbName = LDBNAME ( liDbNum ).
    
    lcQuery = "FOR EACH " + lcDbName + "._file NO-LOCK " +
              "  WHERE _file._file-name = " + QUOTER ( lcTableName ) +
              ", EACH " + lcDbName + "._field NO-LOCK " +
              "  OF _file " +
              "  WHERE _field._field-name matches '*' + " + QUOTER ( lcFieldName ) + " + '*' ".


    CREATE BUFFER lhFileBuffer
        FOR TABLE ( lcDbName + "._file" ).

    CREATE BUFFER lhFieldBuffer
        FOR TABLE ( lcDbName + "._field" ).

    CREATE QUERY lhQuery.

    lhQuery:ADD-BUFFER ( lhFileBuffer ).
    lhQuery:ADD-BUFFER ( lhFieldBuffer ).

    lhQuery:QUERY-PREPARE ( lcQuery ).

    lhQuery:QUERY-OPEN ( ).

    lhQuery:GET-FIRST ( ).

    IF lhQuery:QUERY-OFF-END THEN
        RETURN.

    EMPTY TEMP-TABLE ttField.

    DO WHILE NOT lhQuery:QUERY-OFF-END:

        CREATE ttField.
        ttField.fieldName = lhFieldBuffer::_field-name.
        lhQuery:GET-NEXT ( ).

    END.

END PROCEDURE.





PROCEDURE getSequences.
    DEFINE INPUT PARAMETER pcSequenceName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lii AS INTEGER     NO-UNDO.

    DEFINE VARIABLE lcDbName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcQuery AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lhSequenceBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE lhQuery AS HANDLE      NO-UNDO.

    DO lii = 1 TO NUM-DBS:
    
        lcDbName = LDBNAME ( lii ).
    
        lcQuery = 
            "FOR EACH " + lcDbName + "._sequence NO-LOCK      " +
            "   WHERE " + lcDbName + "._sequence._seq-name MATCHES '*' + " + QUOTER ( pcSequenceName ) + " + '*' ".
    
        CREATE BUFFER lhSequenceBuffer 
            FOR TABLE ( lcDbName + "._sequence" ).
    
        CREATE QUERY lhQuery.
    
        lhQuery:ADD-BUFFER ( lhSequenceBuffer ).
    
        lhQuery:QUERY-PREPARE ( lcQuery ).
    
        lhQuery:QUERY-OPEN ( ).
    
        lhQuery:GET-FIRST ( ).

        DO WHILE NOT lhQuery:QUERY-OFF-END:

            CREATE ttSequence.
            ASSIGN
                ttSequence.SequenceName = lhSequenceBuffer::_seq-name.

            lhQuery:GET-NEXT().

        END.
    
    END.


END PROCEDURE.






DEFINE STREAM sInstances.
PROCEDURE getInstances.
    DEFINE INPUT PARAMETER pcTerm AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcFilename AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lii     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lcLine  AS CHARACTER   NO-UNDO.

    INPUT STREAM sInstances FROM VALUE ( pcFilename ).
    
    DO WHILE TRUE 
        ON ENDKEY UNDO, LEAVE 
        ON ERROR  UNDO, LEAVE :
        
        IMPORT STREAM sInstances UNFORMATTED lcLine.
        lii = lii + 1.
    
        IF lcLine MATCHES "* " + pcTerm + " *"  OR 
           lcLine MATCHES "* " + pcTerm         OR
           lcLine MATCHES pcTerm + " *"         OR
           lcLine MATCHES "*~." + pcTerm + " *" OR
           lcLine MATCHES "*~." + pcTerm + ".*" OR
           lcLine MATCHES "*:" + pcTerm + " *"  OR
           TRIM ( lcLine ) MATCHES pcTerm          THEN
        DO:
        
            CREATE ttInstance.
            ASSIGN
                ttInstance.lineNo = lii
                ttInstance.lineContent = lcLine.
        END.
    
    END.
    
    INPUT STREAM sInstances CLOSE.

END PROCEDURE.





PROCEDURE applyMagic.
    DEFINE INPUT PARAMETER piOrder AS INTEGER NO-UNDO.

    DEFINE VARIABLE lcCurrentLine AS CHARACTER NO-UNDO.

    FOR FIRST ttMagic NO-LOCK
        WHERE ttMagic.order = piOrder :

        CASE TRUE :
            WHEN ttMagic.action = {&CompleteTableName} THEN
            DO:
                SELF:INSERT-STRING ( SUBSTRING ( ttMagic.keyText, LENGTH ( lcText ) + 1 ) ).
            END.

            WHEN ttMagic.action = {&CompleteFieldName} THEN
            DO:
                SELF:INSERT-STRING ( SUBSTRING ( ttMagic.keyText, LENGTH ( lcText ) - R-INDEX ( lcText, "." ) + 1 ) ).
            END.

            WHEN ttMagic.action = {&NextValueSequence} THEN
            DO:
                RUN linxade/gtedline.p
                    ( INPUT SELF,
                      INPUT SELF:CURSOR-LINE,
                      OUTPUT lcCurrentLine ).

                lcCurrentLine = CHR (10) + SUBSTRING ( lcCurrentLine, 1, SELF:CURSOR-CHAR - LENGTH ( lcText ) - 1 ) 
                                + "NEXT-VALUE ( " + ttMagic.keyText + " )."
                                + SUBSTRING ( lcCurrentLine, SELF:CURSOR-CHAR ).

                SELF:CURSOR-CHAR = 1.
                SELF:SOURCE-COMMAND ( "select-line", "" ).
                SELF:REPLACE-SELECTION-TEXT ( lcCurrentLine ).

            END.

            WHEN ttMagic.action BEGINS {&ShowAllInstances} THEN
            DO:

                RUN linxade/gtedline.p
                    ( INPUT  SELF,
                      INPUT  ttMagic.lineNo,
                      OUTPUT lcCurrentLine ).

                ASSIGN
                    SELF:CURSOR-LINE = ttMagic.lineNo
                    SELF:CURSOR-CHAR = INDEX ( lcCurrentLine, lcText ) 
                    NO-ERROR. 

            END.

        END CASE.

    END.

    APPLY "ENTRY":U TO SELF.

END PROCEDURE.
