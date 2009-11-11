&IF DEFINED ( fAppBuilderWindow ) = 0 &THEN

&GLOBAL-DEFINE fAppBuilderWindow TRUE
FUNCTION fAppBuilderWindow RETURNS HANDLE.

    DEFINE VARIABLE lhWindow AS HANDLE      NO-UNDO.
    DEFINE VARIABLE llFound AS LOGICAL     NO-UNDO.

    RUN searchWidgetTree
        ( "AppBuilder",
          SESSION:FIRST-CHILD,
          OUTPUT lhWindow,
          OUTPUT llFound).

    IF llFound AND VALID-HANDLE ( lhWindow ) THEN
        RETURN lhWindow.

END FUNCTION.
&ENDIF

FUNCTION fileMatch RETURNS LOGICAL
    ( INPUT lcTitle    AS CHARACTER,
      INPUT lcFileName AS CHARACTER ).

  
  ASSIGN
      lcTitle    = REPLACE ( lcTitle, "\", "#slash#" ).
      lcTitle    = REPLACE ( lcTitle, "/", "#slash#" ).
      
      lcFilename = REPLACE ( lcFileName, "\", "#slash#" ).
      lcFilename = REPLACE ( lcFileName, "/", "#slash#" ).

  IF lcTitle MATCHES "*#slash#" + lcFileName + "*" OR 
     lcTitle MATCHES "* " + lcFileName + "*" OR
     lcTitle = lcFileName  THEN
     RETURN TRUE.

END FUNCTION.


&IF DEFINED ( SearchWidgetTree ) = 0 &THEN

&GLOBAL-DEFINE SearchWidgetTree TRUE
PROCEDURE searchWidgetTree.

    DEFINE INPUT  PARAMETER icFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ihParent   AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER ohChild    AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER olFound    AS LOGICAL NO-UNDO.

  ohChild = ?.

  IF NOT VALID-HANDLE ( ihParent ) THEN RETURN.

  IF ihParent:TYPE <> "Window"  THEN RETURN.

  IF fileMatch ( ihParent:TITLE, icFileName ) THEN 
  DO:
  /* MESSAGE ihParent:TITLE "matches" "*" + icFileName + "*" VIEW-AS ALERT-BOX. */
    olFound = TRUE.
    OhChild = ihParent.
    RETURN.
  END.

  RUN searchWidgetTree ( icFileName,
                         ihParent:FIRST-CHILD,
                         OUTPUT ohChild,
                         OUTPUT olFound ).

  IF NOT olFound THEN

    RUN searchWidgetTree ( icFileName,
                           ihParent:NEXT-SIBLING,
                           OUTPUT ohChild,
                           OUTPUT olFound ).

END PROCEDURE. /* searchWidgetTree */

&ENDIF

FUNCTION fGetCurrentProcedure RETURNS CHARACTER.

    DEFINE VARIABLE lcWinTitle AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcFileName AS CHARACTER   NO-UNDO.
    

    ASSIGN 
        lcWinTitle    = SELF:WINDOW:TITLE
    
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

    RETURN lcFilename.

END FUNCTION.








PROCEDURE FindWidgetOfType.

    DEFINE INPUT  PARAMETER ihParent   AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER pcType     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ohChild    AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER olFound    AS LOGICAL NO-UNDO.

    ohChild = ?.

    IF NOT VALID-HANDLE ( ihParent ) THEN RETURN.

    IF ihParent:TYPE = pcType THEN 
    DO:
        /* MESSAGE ihParent:TITLE "matches" "*" + icFileName + "*" VIEW-AS ALERT-BOX. */
        olFound = TRUE.
        OhChild = ihParent.
        RETURN.
    END.

    IF ihParent:TYPE <> "Window" AND 
       ihParent:TYPE <> "Frame"  AND 
       ihParent:TYPE <> "FIELD-GROUP" THEN 
        RETURN.


  RUN FindWidgetOfType ( ihParent:FIRST-CHILD,
                         pcType,
                         OUTPUT ohChild,
                         OUTPUT olFound ).

  IF NOT olFound THEN

    RUN FindWidgetOfType ( ihParent:NEXT-SIBLING,
                           pcType,
                           OUTPUT ohChild,
                           OUTPUT olFound ).

END PROCEDURE. /* FindWidget */


