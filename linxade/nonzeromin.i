FUNCTION NON-ZERO-MIN RETURNS INTEGER
    (INPUT lcString AS CHARACTER).

    &SCOP HIGH-VALUE   9999999

    DEFINE VARIABLE lii        AS INTEGER NO-UNDO.
    DEFINE VARIABLE liMinSoFar AS INTEGER NO-UNDO.
    DEFINE VARIABLE liEntry    AS INTEGER NO-UNDO.

    liMinSoFar = {&HIGH-VALUE}.

    /* PROGRESS doesn't allow for user-defined functions with unspecified */
    /* number of inputs (e.g. the PROGRESS function MIN() ).  So we take  */
    /* a single character input that is a comma-separated list of inputs  */
    DO lii = 1 TO NUM-ENTRIES ( lcString ):

        liEntry = INTEGER ( ENTRY ( lii, lcString ) ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN 
            NEXT.

        IF liEntry = 0 THEN
            NEXT.

        IF liEntry < liMinSoFar THEN
            liMinSoFar = liEntry.

    END.

    IF liMinSoFar = {&HIGH-VALUE} THEN
        liMinSoFar = 0.

    RETURN liMinSoFar.

END FUNCTION. /* NON-ZERO-MIN */
