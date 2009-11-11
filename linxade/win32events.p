DEFINE INPUT        PARAMETER pcProduct  AS CHAR    NO-UNDO.
DEFINE INPUT        PARAMETER pcEvent    AS CHAR    NO-UNDO.
DEFINE INPUT        PARAMETER pcContext  AS CHAR    NO-UNDO.
DEFINE INPUT        PARAMETER pcOther    AS CHAR    NO-UNDO.

DEFINE VARIABLE lcToDoMessage AS CHARACTER   NO-UNDO.

{linxade/mru-tt.i}
{linxade/mru.i}
{linxade/winprocs.i}

/* ON "ALT-I" ANYWHERE PERSISTENT RUN linxade/select-code-for-context.p.*/

ON "CTRL-K" ANYWHERE PERSISTENT RUN linxade/showhelp.p.

/* Triggers for use while editing */

/* Perform a COMPILE PREPROCESS */
ON 'CTRL-ALT-P':U ANYWHERE PERSISTENT RUN linxade/compilation.p
                                                 ( INPUT "PREPROCESS").

/* Perform a COMPILE XREF */
ON 'CTRL-X':U ANYWHERE PERSISTENT RUN linxade/compilation.p
                                                 ( INPUT "XREF").

/* Perform a COMPILE DEBUG-LIST */
ON 'CTRL-ALT-D':U ANYWHERE PERSISTENT RUN linxade/compilation.p
                                                 ( INPUT "DEBUG-LIST").

/* Perform a COMPILE LISTING */
ON 'CTRL-ALT-L':U ANYWHERE PERSISTENT RUN linxade/compilation.p
                                                 ( INPUT "LISTING").

ON 'CTRL-ALT-X':U ANYWHERE PERSISTENT 
    RUN proxylog-inq.w.

/* That's really a CTRL-. but whatever*/
/* I want CTRL-. to be "anything", similar to Eclipse. */
ON "CTRL-¾":U ANYWHERE PERSISTENT
    RUN linxade/magic.p
        ( INPUT pcOther ).

/* That's really a CTRL-SLASH like in Eclipse.  But AppBuilder is convinced */
/* that it's that strange reverse question mark. */
 ON "CTRL-¿" ANYWHERE PERSISTENT RUN linxade/togglecomments.p. 
/* ON "CTRL-¿" ANYWHERE PERSISTENT RUN linxade/showsel.p. */

/* A "better" incremental find - i.e. "find-as-you-type" */
/* ON 'CTRL-S':U ANYWHERE PERSISTENT
    RUN linxade/incfind.p. */


ON 'CTRL-D':U ANYWHERE PERSISTENT
    RUN linxade/checktodo.p.

ON 'CTRL-ALT-CURSOR-RIGHT':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "HARD-RIGHT" ).

ON 'CTRL-ALT-CURSOR-LEFT':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "HARD-LEFT" ).

ON 'CTRL-ALT-CURSOR-UP':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "HARD-UP" ).

ON 'CTRL-ALT-CURSOR-DOWN':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "HARD-DOWN" ).


ON 'CTRL-SHIFT-O':U ANYWHERE PERSISTENT
    RUN linxade/showinstances.p
        ( INPUT  pcOther,
          INPUT  CURRENT-WINDOW  ).

ON 'ALT-CURSOR-UP':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "UP" ).

ON 'ALT-CURSOR-DOWN':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "DOWN" ).

ON 'ALT-CURSOR-LEFT':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "LEFT" ).

ON 'ALT-CURSOR-RIGHT':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "RIGHT" ).

ON 'CTRL-SHIFT-ALT-CURSOR-UP':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "LITTLE-UP" ).

ON 'CTRL-SHIFT-ALT-CURSOR-DOWN':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "LITTLE-DOWN" ).

ON 'CTRL-SHIFT-ALT-CURSOR-LEFT':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "LITTLE-LEFT" ).

ON 'CTRL-SHIFT-ALT-CURSOR-RIGHT':U ANYWHERE PERSISTENT
    RUN linxade/movewin.p
        ( INPUT "LITTLE-RIGHT" ).


ON 'CTRL-T':U ANYWHERE PERSISTENT RUN linxade/msgdatatype.p.
ON 'CTRL-N':U ANYWHERE PERSISTENT RUN linxade/msgtableidx.p.
ON 'CTRL-Q':U ANYWHERE PERSISTENT RUN linxade/msgsequences.p.

ON 'CTRL-ALT-S':U ANYWHERE PERSISTENT RUN linxade/shortcuts.p.

/* Open current file in PROCEDURE EDITOR (e.g. if you're in SECTION EDITOR) */
ON 'SHIFT-F5':U ANYWHERE PERSISTENT RUN linxade/proed.p.

/* Copy the current file (with edits) to home area and continue work on it */
ON 'SHIFT-F7':U ANYWHERE PERSISTENT RUN linxade/cphome.p.

/* F1 is "Help" (in GUI) */
/* F2 is "Run"  (in GUI) */
/* F3 is "Open" */
ON 'F5'  ANYWHERE PERSISTENT RUN linxade/ppopen.p.
/* F6 is "Save" */
ON 'F8'  ANYWHERE PERSISTENT RUN linxade/shutfile.p.
/* F9 is "Find Next" */
ON 'F10' ANYWHERE PERSISTENT RUN linxade/flist.p.

/* start BECOM configuration setup program on CTRL-F12 */
ON 'CTRL-F12' ANYWHERE PERSISTENT RUN becom/becomcfg.w.
/* start BECOM on F12 */
ON 'F12' ANYWHERE PERSISTENT RUN becom/becom.p.

/* start multycopy */
ON 'CTRL-F11':U ANYWHERE PERSISTENT RUN becom/multicopy.p.
/* start multipaste  */
ON 'F11':U ANYWHERE PERSISTENT RUN becom/multipaste.w.







DEFINE VARIABLE lcWinTitle    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcFilename    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lhTheWindow AS HANDLE      NO-UNDO.
DEFINE VARIABLE llFound AS LOGICAL     NO-UNDO.

DEFINE VARIABLE lii AS INTEGER     NO-UNDO.
DEFINE VARIABLE lhTestWidget AS HANDLE      NO-UNDO.
DEFINE VARIABLE liLineNo AS INTEGER     NO-UNDO.


/* Whenever we open a file, add it to (my personal) MRU list. */
IF pcEvent = "Open" THEN
DO:

    /* Get the window we just opened if any. */
    RUN searchWidgetTree
        ( INPUT pcOther, 
          INPUT fAppBuilderWindow(),
          OUTPUT lhTheWindow,
          OUTPUT llFound ).
    
    RUN linxade/gettodomsg.p
        ( INPUT  pcOther,
          INPUT  IF llFound THEN lhTheWindow ELSE fAppBuilderWindow(),
          OUTPUT liLineNo ).

    IF liLineNo NE ? AND liLineNo NE 0 THEN
    DO:

        IF VALID-HANDLE ( lhTheWindow ) THEN
            APPLY "ENTRY" TO lhTheWindow.
 
        lhTestWidget = lhTheWindow:FIRST-CHILD.

        RUN FindWidgetOfType
            ( lhTheWindow, "EDITOR", OUTPUT lhTestWidget, OUTPUT llFound ).
    
        APPLY "ENTRY" TO lhTestWidget.
        lhTestWidget:CURSOR-LINE = liLineNo.

    END.

    RUN absoluteToPropathFilename IN THIS-PROCEDURE
        ( INPUT-OUTPUT pcOther ).

    RUN getMru.

    RUN addToMru
        ( INPUT pcOther ).

END.
