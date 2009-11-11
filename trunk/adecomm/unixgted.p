DEFINE INPUT  PARAMETER hWidget AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER hEditor AS HANDLE NO-UNDO.

DEFINE VARIABLE llFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lhFrame AS HANDLE      NO-UNDO.

IF hWidget = ? THEN
    RETURN.



RUN searchWidgetTree IN THIS-PROCEDURE
    ("EDITOR",
     hWidget:WINDOW,
     OUTPUT hEditor ).







PROCEDURE searchWidgetTree.
DEFINE INPUT  PARAMETER icType   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihParent AS WIDGET-HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER ohChild  AS WIDGET-HANDLE NO-UNDO.

  ohChild = ?.

  IF NOT VALID-HANDLE ( ihParent ) THEN 
    RETURN.

  IF CAN-QUERY (ihParent, "TYPE" ) AND 
     ihParent:TYPE = icType            THEN 
  DO:

    ohChild = ihParent.
    RETURN.

  END.




  IF CAN-QUERY ( ihParent, "FIRST-CHILD" ) THEN
  DO:

    RUN searchWidgetTree
      ( icType,
        ihParent:FIRST-CHILD,
        OUTPUT ohChild ).

    IF ohChild <> ? AND ohChild:TYPE = icType THEN
      RETURN.

  END.




  IF CAN-QUERY ( ihParent, "NEXT-SIBLING" ) THEN
  DO:

    RUN searchWidgetTree
      ( icType,
        ihParent:NEXT-SIBLING,
        OUTPUT ohChild ).

    IF ohChild <> ? AND ohChild:TYPE = icType THEN
      RETURN.

  END.

END PROCEDURE.
    
