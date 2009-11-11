DEFINE VARIABLE lcString AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttShortcut NO-UNDO
    FIELD shortcutID        AS CHARACTER
    FIELD shortcutProcedure AS CHARACTER.

/*If this is not a ADE program such as the Editor or Section-Editor then return */
IF NOT ENTRY(NUM-ENTRIES(PROGRAM-NAME(2)), PROGRAM-NAME(2), "/") BEGINS "ADE" THEN
    RETURN.

IF SELF:TYPE NE "EDITOR" THEN
    RETURN.

RUN setupShortcuts.

RUN getStringToInsert
    ( OUTPUT lcString ).

RUN insertString
    ( INPUT lcString ).

APPLY "ENTRY":u TO SELF.

PROCEDURE setupShortcuts.

    CREATE ttShortcut.
    ASSIGN
        ttShortcut.shortcutId = "Date"
        ttShortcut.shortcutProcedure = "shortcutDate".

    CREATE ttShortcut.
    ASSIGN
        ttShortcut.shortcutId = "Username"
        ttShortcut.shortcutProcedure = "shortcutUsername".

END PROCEDURE.


PROCEDURE getStringToInsert.
    DEFINE OUTPUT PARAMETER pcString AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcSelected AS CHARACTER   NO-UNDO.

    lcList = "".

    FOR EACH ttShortcut NO-LOCK:
    
        lcList = IF lcList = ""
                 THEN ttShortcut.shortcutId
                 ELSE ( lcList + "," + ttShortcut.shortcutId ).
    
    END.
    
    RUN linxade/listdialog.w
        ( INPUT lcList,
          INPUT "Select shortcut",
          OUTPUT lcSelected ).

    FIND FIRST ttShortcut
        WHERE ttShortcut.shortcutId = lcSelected
        NO-LOCK NO-ERROR.
        
    IF NOT AVAILABLE ttShortcut THEN
        RETURN.

    RUN VALUE ( ttShortcut.shortcutProcedure )
        ( OUTPUT pcString ).    

END PROCEDURE.


PROCEDURE insertString.
    DEFINE INPUT PARAMETER pcString AS CHARACTER NO-UNDO.
    
    SELF:INSERT-STRING ( pcString ).
    
END PROCEDURE.






PROCEDURE shortcutDate.
    DEFINE OUTPUT PARAMETER pcShortcut AS CHARACTER NO-UNDO.

    pcShortcut = STRING ( TODAY, "99/99/99" ).

END PROCEDURE.


PROCEDURE shortcutUsername.
    DEFINE OUTPUT PARAMETER pcShortcut AS CHARACTER NO-UNDO.

    IF OS-GETENV ( "USERNAME" ) = "" THEN
        pcShortcut = USERID ( LDBNAME ( 1 ) ).
    ELSE 
        pcShortcut = OS-GETENV ( "USERNAME" ).

        
END PROCEDURE.


/* to add more shortcuts, add shortcut procedures here, and add ttShortcut
   records above  */
