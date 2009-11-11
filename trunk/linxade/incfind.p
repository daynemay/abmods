DEFINE VARIABLE lhEditor            AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhFrame             AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhFindString        AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhFindLabel         AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhWindow            AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhAppBuilderWindow  AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhCloseButton       AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhDirectionRadioSet AS HANDLE   NO-UNDO.

DEFINE VARIABLE liWindowWidth       AS INTEGER  NO-UNDO.
DEFINE VARIABLE liWindowHeight      AS INTEGER  NO-UNDO.
DEFINE VARIABLE llForward           AS LOGICAL  NO-UNDO.


{linxade/winprocs.i}

/* Stolen from Firefox: if the string can't be found, make the fill-in red */
&SCOPED-DEFINE NOT-FOUND-BGCOLOR    12 /* RED */
&SCOPED-DEFINE NORMAL-BGCOLOR       15 /* WHITE */
&SCOPED-DEFINE FIND-FRAME-BGCOLOR   8  /* GREY */ 

&SCOPED-DEFINE SCROLLBAR-WIDTH      21 /* Used for geometry */

&SCOPED-DEFINE MAX-LENGTH-OF-LINE   200

/* Make sure we're in a code editor before we try anything */
IF SELF:TYPE <> "EDITOR" THEN
    RETURN.

ASSIGN
    lhEditor            = SELF:HANDLE           /* Code editor */
    lhWindow            = lhEditor:WINDOW       /* Window containing code */
    lhAppBuilderWindow  = fAppBuilderWindow (). /* AppBuilder window */ 


CREATE WIDGET-POOL "INCFIND".

/* A frame at the bottom of the code window */
CREATE FRAME lhFrame 
    IN WIDGET-POOL "INCFIND"
    ASSIGN
        BGCOLOR         = {&FIND-FRAME-BGCOLOR}
        SCROLLABLE      = FALSE
        SIDE-LABELS     = TRUE
        TOP-ONLY        = TRUE
        OVERLAY         = TRUE 
        BOX             = TRUE
        PARENT          = lhWindow
        WIDTH-PIXELS    = lhWindow:WIDTH-PIXELS - 
                            {&SCROLLBAR-WIDTH}
        HEIGHT-PIXELS   = 30
        Y               = lhWindow:HEIGHT-PIXELS  - 
                            lhFrame:HEIGHT-PIXELS - 
                            {&SCROLLBAR-WIDTH} + 2
        X               = 2
    TRIGGERS:

        ON GO DO:
            APPLY "U1" TO lhFindString.
            RETURN NO-APPLY.
        END.

        ON END-ERROR 
        DO:
            APPLY "U1" TO lhFindString.
            RETURN NO-APPLY.
        END.

    END TRIGGERS. /* for lhFrame */




CREATE BUTTON  lhCloseButton
    IN WIDGET-POOL "INCFIND"
    ASSIGN
        FRAME = lhFrame
        X = 8
        Y = 8 
        HEIGHT-PIXELS = 12
        WIDTH-PIXELS = 12
        VISIBLE = TRUE
        SENSITIVE = TRUE
        FONT = 6
        BGCOLOR = 12
        LABEL = "x"
    TRIGGERS:
    
        ON CHOOSE
        DO:
            APPLY "U1" TO lhFindString.
        END.
    
    END TRIGGERS.




/* A label for our fill-in (below) */
CREATE TEXT lhFindLabel 
    IN WIDGET-POOL "INCFIND"
    ASSIGN
        FRAME           = lhFrame
        FONT            = lhEditor:FONT
        VISIBLE         = TRUE
        WIDTH-PIXELS    = 35
        X               = 30.

CREATE RADIO-SET lhDirectionRadioSet
    IN WIDGET-POOL "INCFIND"
    ASSIGN
        FRAME           = lhFrame
        FONT            = lhEditor:FONT
        X               = lhFindLabel:X + lhFindLabel:WIDTH-PIXELS
        Y               = 1
        HEIGHT-PIXELS   = lhFrame:HEIGHT-PIXELS - 3
        WIDTH-PIXELS    = 95
        HORIZONTAL      = FALSE
        RADIO-BUTTONS   = "Next,Next,Previous,Previous"
        VISIBLE         = TRUE
        SENSITIVE       = TRUE

    TRIGGERS:

        ON VALUE-CHANGED 
        DO:
            RUN toggleDirection.
            APPLY "ENTRY" TO lhFindString.
        END.

        ON CURSOR-UP
        DO:
            SELF:SCREEN-VALUE = "Next".
            APPLY "VALUE-CHANGED" TO SELF.
        END.

        ON CURSOR-DOWN
        DO:
            SELF:SCREEN-VALUE = "Previous".
            APPLY "VALUE-CHANGED" TO SELF.
        END.


    END TRIGGERS.


/* A fill-in to type in our find string. */
CREATE FILL-IN lhFindString 
    IN WIDGET-POOL "INCFIND"
    ASSIGN 
        FRAME               = lhFrame
        HEIGHT-PIXELS       = 21
        Y                   = 3
        FONT                = lhEditor:FONT /* Match the font to what's in the editor */
        RESIZABLE           = TRUE
        BGCOLOR             = {&NORMAL-BGCOLOR}
        SENSITIVE           = TRUE
        VISIBLE             = TRUE
        SIDE-LABEL-HANDLE   = lhFindLabel
        LABEL               = "Find"
        X                   = lhDirectionRadioSet:X + lhDirectionRadioSet:WIDTH-PIXELS
    
    TRIGGERS:

        ON VALUE-CHANGED 
        DO:
            RUN searchForString IN THIS-PROCEDURE
                ( INPUT SELF:SCREEN-VALUE,
                  INPUT FALSE ).
        END.

        ON RETURN
        DO:
            RUN searchForString IN THIS-PROCEDURE
                ( INPUT SELF:SCREEN-VALUE,
                  INPUT TRUE ).
            RETURN NO-APPLY.
        END. 

        ON END-ERROR 
        DO:
            APPLY "U1" TO SELF.
            RETURN NO-APPLY.
        END.

    END TRIGGERS. /* for lhFindString */

/* If the user starts typing outside of the search string field,  */
/* close this program down.                                       */
ON ANY-KEY ANYWHERE 
DO:
    IF CAN-QUERY ( SELF, "FRAME" ) AND 
       SELF:FRAME <> lhFrame     THEN
    DO:
        APPLY "U1" TO lhFindString.
        RETURN.
    END.
END.

ON WINDOW-RESIZED OF lhWindow
DO:
    RUN resizeWindow IN THIS-PROCEDURE.
END.

ASSIGN
    liWindowWidth = lhWindow:WIDTH-PIXELS
    liWindowHeight = lhWindow:HEIGHT-PIXELS

    llForward = TRUE.

/* Fit the fill-in into the window. */
lhFindString:WIDTH-PIXELS  = lhFrame:WIDTH-PIXELS - lhFindString:X - 5 NO-ERROR.
lhFindString:FORMAT = "X(" + STRING ( TRUNCATE ( lhFindString:WIDTH-CHARS, 0 ) - 1 ) + ")" NO-ERROR.

/* If the user has highlighted something, then default to that. */
IF LENGTH ( lhEditor:SELECTION-TEXT ) <= lhFindString:WIDTH-CHARS - 1 THEN
    lhFindString:SCREEN-VALUE = lhEditor:SELECTION-TEXT.    

APPLY "ENTRY":U TO lhFindString.

/* AppBuilder keeps taking focus on the WAIT-FOR.  If there's a better */
/* way to stop this from happening, let me know.                       */
lhAppBuilderWindow:HIDDEN = TRUE.


/* Wait until the user is done searching */
WAIT-FOR "U1" OF lhFindString.


/* Restore the AppBuilder window. */
lhAppBuilderWindow:HIDDEN = FALSE.

/* Clean up after ourselves */
DELETE WIDGET lhFindString.
DELETE WIDGET lhFrame.
DELETE WIDGET-POOL "INCFIND".

/* Put the user back in the code editor */
APPLY "ENTRY" TO lhEditor.






PROCEDURE searchForString.
    DEFINE INPUT PARAMETER pcFindString AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER plGetNext    AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE lcCurrentLine AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lcSelectedPlusNext AS CHARACTER NO-UNDO.
    DEFINE VARIABLE llFound AS LOGICAL     NO-UNDO.

    lhFindString:BGCOLOR  = {&NORMAL-BGCOLOR}.        

    IF pcFindString = "" THEN
    DO:
        lhEditor:CLEAR-SELECTION ().
        RETURN.
    END.

    /* Get the current line of the editor */
    RUN linxade/gtedline.p 
        ( INPUT lhEditor, 
          INPUT lhEditor:CURSOR-LINE, 
          OUTPUT lcCurrentLine ).

    /* If we're searching for something that we're already in (e.g. if the 
       user has previously type "somethi" and we're currently got the "somethi"
       highlighted from within the word, "something", then if the next letter
       typed is an "N", we don't want to find the NEXT "somethin", we want to
       highlight the "somethin" in our CURRENT "something" */ 
    lcSelectedPlusNext = SUBSTRING ( lcCurrentLine, 
                                     lhEditor:CURSOR-CHAR - LENGTH ( lhEditor:SELECTION-TEXT ), 
                                     LENGTH ( lhEditor:SELECTION-TEXT ) + 1 ) NO-ERROR.
    
    IF NOT plGetNext AND pcFindString = lcSelectedPlusNext THEN DO:
        lhEditor:CURSOR-CHAR = MAX ( 1, lhEditor:CURSOR-CHAR - LENGTH ( lhEditor:SELECTION-TEXT ) ).
    END. 

    /* If the user has just hit BACKSPACE, then they don't want to find the */
    /* next "someth", they want the current one highlighted.  Since the     */
    /* cursor is at the END of the highlight, the "current" one is actually */
    /* the PREVIOUS one.                                                    */
    IF KEY-FUNCTION ( LAST-KEY ) = "BACKSPACE" THEN
        llFound = lhEditor:SEARCH ( pcFindString, FIND-PREV-OCCURRENCE + FIND-WRAP-AROUND + FIND-SELECT).
    ELSE
        IF llForward THEN
            llFound = lhEditor:SEARCH ( pcFindString, FIND-NEXT-OCCURRENCE + FIND-WRAP-AROUND + FIND-SELECT).
        ELSE
        DO:
            
            lhEditor:CURSOR-CHAR = MAX ( 1, lhEditor:CURSOR-CHAR - LENGTH ( lhEditor:SELECTION-TEXT ) ).
            
            IF lhEditor:CURSOR-CHAR = 1 THEN
                ASSIGN
                    lhEditor:CURSOR-LINE = MAX ( 1, lhEditor:CURSOR-LINE - 1 )
                    lhEditor:CURSOR-CHAR = {&MAX-LENGTH-OF-LINE}.
            
            llFound = lhEditor:SEARCH ( pcFindString, FIND-PREV-OCCURRENCE + FIND-WRAP-AROUND + FIND-SELECT).
        END.
        
    IF NOT llFound THEN
        lhFindString:BGCOLOR  = {&NOT-FOUND-BGCOLOR}.

END PROCEDURE.






PROCEDURE resizeWindow.

    DEFINE VARIABLE liDeltaWidth  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE liDeltaHeight AS INTEGER     NO-UNDO.

    ASSIGN
        liDeltaWidth   = lhWindow:WIDTH-PIXELS - liWindowWidth
        liDeltaHeight  = lhWindow:HEIGHT-PIXELS - liWindowHeight
        
        liWindowWidth  = lhWindow:WIDTH-PIXELS
        liWindowHeight = lhWindow:HEIGHT-PIXELS.
    
    /* Order is important. If the window is getting smaller, then we need to shrink the */
    /* fill-in before the frame (so it will always fit in the frame), but if it's       */
    /* getting bigger, then we need to make the frame bigger first (same reason).       */
    /* The same logic also applies to the code editor as to our fill-in.                */
    IF liDeltaWidth < 0 THEN
        ASSIGN
            lhFindString:WIDTH-PIXELS   = lhFindString:WIDTH-PIXELS + liDeltaWidth
            lhFrame:WIDTH-PIXELS        = lhFrame:WIDTH-PIXELS + liDeltaWidth
            
            lhEditor:WIDTH-PIXELS       = lhEditor:WIDTH-PIXELS + liDeltaWidth
            lhEditor:FRAME:WIDTH-PIXELS = lhEditor:FRAME:WIDTH-PIXELS + liDeltaWidth
            .
    ELSE
        ASSIGN
            lhFrame:WIDTH-PIXELS        = lhFrame:WIDTH-PIXELS + liDeltaWidth
            lhFindString:WIDTH-PIXELS   = lhFindString:WIDTH-PIXELS + liDeltaWidth
        
            lhEditor:FRAME:WIDTH-PIXELS = lhEditor:FRAME:WIDTH-PIXELS + liDeltaWidth
            lhEditor:WIDTH-PIXELS       = lhEditor:WIDTH-PIXELS + liDeltaWidth
            .
 
    /* Changes of height are a bit different, since our frame stays the same height, */
    /* and just moves to the bottom of the window after a resize.                    */
    lhFrame:Y               = lhWindow:HEIGHT-PIXELS - lhFrame:HEIGHT-PIXELS - 20.

    /* But the code editor still needs to resize to fit the window, and again,       */
    /* the order is important.                                                       */
    IF liDeltaHeight < 0 THEN
        ASSIGN
            lhEditor:HEIGHT-PIXELS = lhEditor:HEIGHT-PIXELS + liDeltaHeight
            lhEditor:FRAME:HEIGHT-PIXELS = lhEditor:FRAME:HEIGHT-PIXELS + liDeltaHeight
            .
    ELSE
        ASSIGN
            lhEditor:FRAME:HEIGHT-PIXELS = lhEditor:FRAME:HEIGHT-PIXELS + liDeltaHeight
            lhEditor:HEIGHT-PIXELS = lhEditor:HEIGHT-PIXELS + liDeltaHeight
            .

END PROCEDURE.


PROCEDURE toggleDirection.

    llForward = NOT llForward.

END PROCEDURE.
