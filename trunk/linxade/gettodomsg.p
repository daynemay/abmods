DEFINE INPUT  PARAMETER pcFilename    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER phParent      AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER piLineNo      AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttToDo NO-UNDO 
    FIELD lineNo AS INTEGER
    FIELD taskDescription AS CHARACTER  .

DEFINE STREAM sToDo.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE lii    AS INTEGER   NO-UNDO.

INPUT STREAM sToDo FROM VALUE ( pcFilename ).

DO WHILE TRUE 
    ON ENDKEY UNDO, LEAVE 
    ON ERROR  UNDO, LEAVE :
    
    IMPORT STREAM sToDo UNFORMATTED lcLine.
    lii = lii + 1.

    IF lcLine MATCHES "*TODO: *" THEN
    DO:
        CREATE ttToDo.
        ASSIGN
            ttToDo.lineNo = lii
            ttToDo.taskDescription = SUBSTRING ( lcLine, INDEX ( lcLine, "TODO:" ) + 5 ).
        ttToDo.taskDescription = TRIM ( ttToDo.taskDescription, "*/" ).
    END.
    
END.

INPUT STREAM sToDo CLOSE.

IF NOT CAN-FIND ( FIRST ttToDo ) THEN
DO:
    piLineNo = ?.
    RETURN.
        
END.

RUN linxade/todolist.w
    ( INPUT  TABLE ttToDo,
      INPUT  phParent,
      INPUT  0,
      INPUT  "TODO:",
      OUTPUT pilineNo ). 
