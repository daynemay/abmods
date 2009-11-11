/* Close the current file, just like hitting that big X up there on the right */

DEFINE VARIABLE lhNextWin AS HANDLE  NO-UNDO.
DEFINE VARIABLE llFound   AS LOGICAL NO-UNDO.
DEFINE VARIABLE hWin AS WIDGET-HANDLE NO-UNDO.


IF SELF:TYPE <> "EDITOR" THEN
    RETURN.


hWin = SELF:WINDOW.


/* If there's another editor window open, get a */ 
/* handle to it, and give focus to that window. */ 
RUN searchWidgetTree
    ( INPUT "whatever",
      INPUT SESSION:LAST-CHILD,
      OUTPUT lhNextWin,
      OUTPUT llFound ).

APPLY "WINDOW-CLOSE":U TO hWin.

IF VALID-HANDLE ( lhNextWin ) THEN
    APPLY "ENTRY" TO lhNextWin.




/********************************************************************/
PROCEDURE searchWidgetTree.
DEFINE INPUT  PARAMETER icFileName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihParent   AS WIDGET-HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER ohChild    AS WIDGET-HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER olFound    AS LOGICAL NO-UNDO.

  ohChild = ?.

  IF NOT VALID-HANDLE ( ihParent ) THEN RETURN.

  IF ihParent:TYPE <> "Window"  THEN RETURN.

  /* For now, any old Procedure window will do.  We might get tricky */
  /* later on.  But it can't be the window that we're closing... */
  IF ihParent:TITLE MATCHES "*Procedure*" AND 
     ihParent <> hWin                        THEN DO:

    olFound = TRUE.
    OhChild = ihParent.
    RETURN.
  END.

  RUN searchWidgetTree ( icFileName, 
                         ihParent:LAST-CHILD, 
                         OUTPUT ohChild, 
                         OUTPUT olFound ).

  IF NOT olFound THEN

    RUN searchWidgetTree ( icFileName,
                           ihParent:PREV-SIBLING, 
                           OUTPUT ohChild, 
                           OUTPUT olFound ).

END PROCEDURE.
