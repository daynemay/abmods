{linxade/widgettree.i}
{linxade/winprocs.i}

DEFINE INPUT  PARAMETER icType AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttWidget.

RUN getChildren ( fAppBuilderWindow() ).

PROCEDURE getChildren.
    DEFINE INPUT PARAMETER ihParent AS HANDLE NO-UNDO.

    IF NOT CAN-DO ( icType, ihParent:TYPE ) THEN
        RETURN.

    RUN addCurrent
        ( INPUT ihParent ).

    IF CAN-QUERY ( ihParent, "FIRST-CHILD" ) AND 
       VALID-HANDLE ( ihParent:FIRST-CHILD ) THEN
        RUN getChildren ( ihParent:FIRST-CHILD ).
        
    IF CAN-QUERY ( ihParent, "NEXT-SIBLING" ) AND 
       VALID-HANDLE ( ihParent:NEXT-SIBLING ) THEN
        RUN getChildren ( ihParent:NEXT-SIBLING ).

END PROCEDURE.


PROCEDURE addCurrent.
    DEFINE INPUT PARAMETER ihWidget AS HANDLE NO-UNDO.

    FIND FIRST ttWidget
        WHERE ttWidget.widget-handle = ihWidget
        NO-LOCK NO-ERROR.

    IF AVAILABLE ttWidget THEN
        RETURN.

    CREATE ttWidget.
    ASSIGN 
        ttWidget.widget-handle = ihWidget
        ttWidget.widget-type   = ihWidget:TYPE.

END PROCEDURE.
