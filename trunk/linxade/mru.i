&GLOBAL-DEFINE maxHistoryLength 14
DEFINE STREAM sMru.

PROCEDURE checkMruRecord.
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

    FILE-INFO:FILENAME = "C:\DJMDLC\".
    IF FILE-INFO:FILE-TYPE = ? THEN
        OS-CREATE-DIR "C:\DJMDLC".

    FILE-INFO:FILENAME = "C:\DJMDLC\ppopen.mru".
    IF FILE-INFO:FILE-TYPE = ? THEN
    DO:
        OUTPUT TO "C:\DJMDLC\ppopen.mru".
        OUTPUT CLOSE.
    END.

END PROCEDURE.



PROCEDURE addToMru.
    DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.

    FIND FIRST ttMru
        WHERE SEARCH ( ttMru.fileName ) = SEARCH ( pcFileName )
        NO-ERROR.

    IF NOT AVAILABLE ttMru THEN
    DO:
        CREATE ttMru.
        ttMru.fileName  = pcFileName.
    END.

    ttMru.order = 1.

    /* Make sure the MRU file exists. */
    RUN checkMruRecord.

    OUTPUT STREAM sMru TO C:\DJMDLC\ppopen.mru.

    FOR EACH ttMru 
        WHERE ttMru.order <= {&maxHistoryLength} + 1
        BY ttMru.order:

        PUT STREAM sMru UNFORMATTED ttMru.fileName SKIP.

    END.

    OUTPUT STREAM sMru CLOSE.

END PROCEDURE. /* addToMru */



PROCEDURE getMru.

    DEFINE VARIABLE lcMruName  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE liOrder AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttMru.
    liOrder = 1.

    /* Make sure the MRU directory exists. */
    RUN checkMruRecord.

    INPUT STREAM sMru FROM C:\DJMDLC\ppopen.mru.

    DO WHILE TRUE ON END-KEY UNDO, LEAVE:

        IF liOrder > {&maxHistoryLength} + 1 THEN
            LEAVE.

        IMPORT STREAM sMru UNFORMATTED lcMruName.

        IF SEARCH ( lcMruName ) = ? THEN
            NEXT.

        /* Note that this will mean that ttMru will start at 2. */
        liOrder = liOrder + 1.

        CREATE ttMru.
        ASSIGN
            ttMru.fileName  = lcMruName
            ttMru.order     = liOrder.

    END.

    INPUT STREAM sMru CLOSE.

END PROCEDURE. /* getMru */






PROCEDURE absoluteToPropathFilename.
    DEFINE INPUT-OUTPUT PARAMETER pcFilename AS CHARACTER NO-UNDO.
    
    /* Pre: pcFilename is an absolute pathname to a file.                   */
    /* Post: pcFilename is the shortest possible version of the filename    */
    /*       that will still be picked up by the PROPATH.                   */
    /* e.g. if PROPATH = ".,./tollworks,./tollworks/toll-rate,..." and      */
    /*         pcFilename = "./tollworks/toll-rate/something.p"             */
    /*      then pcfileName will be "something.p" after this procedure.     */
    /* e.g. if PROPATH = ".,./tollworks,./tollworks/toll-rate,..." and      */
    /*         pcFilename = "./tollworks/hw/something.p"                    */
    /*      then pcfileName will be "hw/something.p" after this procedure.  */

    DEFINE VARIABLE lii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lcTempFilename AS CHARACTER   NO-UNDO.

    pcFilename = REPLACE ( pcFilename, "\", "/" ).


    /* An e.g. of how the following loop would process              */
    /*          "J:/tollworks/hw/something.p"                       */
    /*   1. Is "something.p" in the PROPATH?  If so, terminate.     */
    /*   2. Is "hw/something.p" in the PROPATH?  If so, terminate.  */
    /*   3. Is "tollworks/hw/something.p" in the PROPATH? ... etc   */

    DO lii = R-INDEX ( pcFilename, "/" ) TO 1 BY -1:

        IF SUBSTRING ( pcFilename, lii, 1 ) <> "/" THEN
            NEXT.

        /* Try everything after the slash */
        lcTempFilename = SUBSTRING ( pcFilename, lii + 1 ).
        
        IF SEARCH ( lcTempFilename ) <> ? THEN
        DO:
        
            pcFilename = lcTempFilename.
            RETURN.

        END.

    END.

END PROCEDURE.
