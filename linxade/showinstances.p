DEFINE INPUT  PARAMETER pcFilename    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER phWindow      AS HANDLE    NO-UNDO.

{linxade/winprocs.i}

DEFINE VARIABLE lhEditor AS HANDLE NO-UNDO.

DEFINE VARIABLE lcTerm        AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLineNo      AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttToDo NO-UNDO 
    FIELD lineNo AS INTEGER
    FIELD taskDescription AS CHARACTER  .

DEFINE STREAM sInstances.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE lii    AS INTEGER   NO-UNDO.

lhEditor = SELF.

IF lhEditor:TYPE NE "EDITOR" THEN
    RETURN.

RUN linxade/getselectedtext.p
    ( INPUT phWindow,
      OUTPUT lcTerm ).

IF lcTerm = "" THEN
DO:
    RUN linxade/getcurrentword.p
        ( INPUT SELF:WINDOW, 
          OUTPUT lcTerm ).
END.

IF lcTerm = "" THEN
    RETURN.

INPUT STREAM sInstances FROM VALUE ( pcFilename ).

DO WHILE TRUE 
    ON ENDKEY UNDO, LEAVE 
    ON ERROR  UNDO, LEAVE :
    
    IMPORT STREAM sInstances UNFORMATTED lcLine.
    lii = lii + 1.

    IF lcLine MATCHES "* " + lcTerm + " *"  OR 
       lcLine MATCHES "* " + lcTerm         OR
       lcLine MATCHES lcTerm + " *"         OR
       lcLine MATCHES "*~." + lcTerm + " *" OR
       lcLine MATCHES "*~." + lcTerm + ".*" OR
       lcLine MATCHES "*:" + lcTerm + " *"  OR
       TRIM ( lcLine ) MATCHES lcTerm          THEN
    DO:
    
        CREATE ttToDo.
        ASSIGN
            ttToDo.lineNo = lii
            ttToDo.taskDescription = lcLine.
    END.

END.

INPUT STREAM sInstances CLOSE.

IF NOT CAN-FIND ( FIRST ttToDo ) THEN
DO:
    liLineNo = ?.
    RETURN.
END.

RUN linxade/todolist.w
    ( INPUT  TABLE ttToDo,
      INPUT  phWindow,
      INPUT  500,
      INPUT  SUBSTITUTE ( "Instances of '&1':", lcTerm ),
      OUTPUT liLineNo ). 

IF liLineNo NE ? AND liLineNo NE 0 THEN
DO:
    lhEditor:CURSOR-LINE = liLineNo.
    lhEditor:SOURCE-COMMAND ( "deselect", "" ).
    lhEditor:CURSOR-CHAR = INDEX ( ENTRY ( liLineNo, lhEditor:SCREEN-VALUE, CHR (10 ) ), lcTerm ).
    /* TODO: Highlight selected text? */
    APPLY "ENTRY" TO lhEditor.
END.
