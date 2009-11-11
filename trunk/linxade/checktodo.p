DEFINE VARIABLE lcWinTitle    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcFilename    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcToDoMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lilineno AS INTEGER     NO-UNDO.

{linxade/winprocs.i}

DEFINE TEMP-TABLE ttToDo NO-UNDO
    FIELD lineNo          AS INTEGER 
    FIELD taskDescription AS CHARACTER.

lcFileName = fGetCurrentProcedure ( ).

IF NOT SELF:TYPE = "EDITOR" THEN
    RETURN.

IF SEARCH ( lcFilename ) = ? THEN
    RETURN.

IF SELF:MODIFIED THEN
DO:
    MESSAGE "Cannot check TODO: on a modified file.  Save first."
        VIEW-AS ALERT-BOX.
    APPLY "ENTRY" TO SELF.
    RETURN.
END.

RUN linxade/gettodomsg.p
    ( INPUT  lcFilename,
      INPUT  SELF:WINDOW,
      OUTPUT liLineNo ).


IF liLineNo NE 0 AND liLineNo NE ? THEN
DO:
    SELF:CURSOR-LINE = liLineNo.
END.

APPLY "ENTRY" TO SELF.
