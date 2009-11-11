DEFINE VARIABLE lhWindow            AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhEditor            AS HANDLE   NO-UNDO.

DEFINE VARIABLE lhFrame             AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhCloseButton       AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhDescription       AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhOpenFiles         AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhBrowseButton      AS HANDLE   NO-UNDO.
DEFINE VARIABLE lhPerformComparison AS HANDLE   NO-UNDO.


DEFINE VARIABLE lcThisFile AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lhAppBuilderWindow  AS HANDLE   NO-UNDO.


DEFINE VARIABLE liWindowWidth       AS INTEGER  NO-UNDO.
DEFINE VARIABLE liWindowHeight      AS INTEGER  NO-UNDO.

{linxade/winprocs.i}
{linxade/widgettree.i}

&SCOPED-DEFINE DIFF-FRAME-BGCOLOR   8  /* GREY */ 
&SCOPED-DEFINE SCROLLBAR-WIDTH      21 /* Used for geometry */
&SCOPED-DEFINE FILE-SELECT-MESSAGE "<select file or type filename here>"


DEFINE TEMP-TABLE ttOpenFile NO-UNDO
    FIELD editor-window AS HANDLE
    FIELD file-name     AS CHARACTER.

/* Make sure we're in a code editor before we try anything */
IF SELF:TYPE <> "EDITOR" THEN
    RETURN.

IF SELF:MODIFIED THEN 
DO:
    MESSAGE "Save this file before attempting to run a DIFF against it."
        VIEW-AS ALERT-BOX.
    APPLY "ENTRY" TO SELF.  
    RETURN.
END.  



CREATE WIDGET-POOL "RUNDIFF".

ASSIGN
    lhEditor            = SELF:HANDLE           /* Code editor */
    lhWindow            = lhEditor:WINDOW       /* Window containing code */
    lhAppBuilderWindow  = fAppBuilderWindow ()  /* AppBuilder window */ 
    liWindowWidth       = lhWindow:WIDTH-PIXELS
    liWindowHeight      = lhWindow:HEIGHT-PIXELS.


/* A frame at the bottom of the code window */
CREATE FRAME lhFrame 
    IN WIDGET-POOL "RUNDIFF"
    ASSIGN
        PARENT          = lhWindow
        BGCOLOR         = {&DIFF-FRAME-BGCOLOR}
        SCROLLABLE      = FALSE
        SIDE-LABELS     = TRUE
        TOP-ONLY        = TRUE
        OVERLAY         = TRUE 
        BOX             = TRUE
        WIDTH-PIXELS    = lhWindow:WIDTH-PIXELS - 
                            {&SCROLLBAR-WIDTH}
        HEIGHT-PIXELS   = 100
        Y               = lhWindow:HEIGHT-PIXELS  - 
                            lhFrame:HEIGHT-PIXELS - 
                            {&SCROLLBAR-WIDTH} + 2
        X               = 2
        TRIGGERS:
            ON END-ERROR
            DO:
                APPLY "U1" TO lhFrame.
                RETURN NO-APPLY.
            END.
        END TRIGGERS.

CREATE BUTTON  lhCloseButton
    IN WIDGET-POOL "RUNDIFF"
    ASSIGN
        FRAME           = lhFrame
        X               = 8
        Y               = 8 
        FONT            = 6
        HEIGHT-PIXELS   = 12
        WIDTH-PIXELS    = 12
        SENSITIVE       = TRUE
        LABEL           = "x"
    
    TRIGGERS:
    
        ON CHOOSE
        DO:
            APPLY "U1" TO lhFrame.
        END.
    
    END TRIGGERS.




CREATE TEXT lhDescription 
    IN WIDGET-POOL "RUNDIFF"
    ASSIGN
        FRAME           = lhFrame
        FONT            = lhEditor:FONT
        FORMAT          = "X(45)"
        SCREEN-VALUE    = "Run Araxis diff between this program and..."
        X               = 30
        WIDTH-PIXELS    = lhFrame:WIDTH-PIXELS - lhDescription:X - 10
        HEIGHT-PIXELS   = 20
        VISIBLE         = TRUE
        .

/* A combo-box for the file to compare against. */
CREATE COMBO-BOX lhOpenFiles 
    IN WIDGET-POOL "RUNDIFF"
    ASSIGN 
        FRAME               = lhFrame
        DATA-TYPE           = "CHARACTER"
        FORMAT              = "X(40)"
        SUBTYPE             = "DROP-DOWN"
        Y                   = lhDescription:Y + lhDescription:HEIGHT-PIXELS
        X                   = lhDescription:X
        SENSITIVE           = TRUE
        VISIBLE             = TRUE
        SIDE-LABEL-HANDLE = lhDescription
        INNER-LINES         = 3
        FONT                = lhEditor:FONT /* Match the font to what's in the editor */
        BGCOLOR             = 15
        
        TRIGGERS:

            ON ANY-PRINTABLE 
            DO:
                APPLY "VALUE-CHANGED" TO SELF.
            END.
        
            ON VALUE-CHANGED
            DO:
                RUN handleChangeOfComparisonFile.
            END.
        
        END TRIGGERS.
        
        
        
        
        
        

CREATE BUTTON  lhBrowseButton
    IN WIDGET-POOL "RUNDIFF"
    ASSIGN
        FRAME           = lhFrame
        X               = lhOpenFiles:X + lhOpenFiles:WIDTH-PIXELS + 3
        Y               = lhOpenFiles:Y + 1
        FONT            = 6
        HEIGHT-PIXELS   = lhOpenFiles:HEIGHT-PIXELS - 1
        WIDTH-PIXELS    = 20
        SENSITIVE       = TRUE
        VISIBLE = TRUE
        LABEL           = "..."
    
    TRIGGERS:
    
        ON CHOOSE
        DO:
            DEFINE VARIABLE lcFileName AS CHARACTER   NO-UNDO.
        
            RUN browseForFile
                ( OUTPUT lcFileName ).

            IF lcFileName <> ? THEN
            DO:
                lhOpenFiles:SCREEN-VALUE = lcFileName. 
                APPLY "ENTRY" TO lhOpenFiles.
                APPLY "VALUE-CHANGED" TO lhOpenFiles.
            END.

        END.
    
    END TRIGGERS.



CREATE BUTTON lhPerformComparison
    IN WIDGET-POOL "RUNDIFF"
    ASSIGN
        FRAME           = lhFrame
        X               = lhOpenFiles:X 
        Y               = lhOpenFiles:Y + lhOpenFiles:HEIGHT-PIXELS + 3
        FONT            = lhEditor:FONT
        HEIGHT-PIXELS   = 20
        WIDTH-PIXELS    = lhBrowseButton:X - lhOpenFiles:X + lhBrowseButton:WIDTH-PIXELS
        SENSITIVE       = FALSE
        VISIBLE         = TRUE
        LABEL           = "Compare..."
    
    TRIGGERS:
    
        ON CHOOSE 
        DO:
            RUN performComparison.
            APPLY "U1" TO lhFrame.
        END.
        
    END TRIGGERS.













/* If the user starts typing outside of the search string field,  */
/* close this program down.                                       */
ON ANY-KEY ANYWHERE 
DO:
    IF CAN-QUERY ( SELF, "FRAME" ) AND 
       SELF:FRAME <> lhFrame     THEN
    DO:
        APPLY "U1" TO lhFrame.
        RETURN.
    END.
END.

ON WINDOW-RESIZED OF lhWindow
DO:
    RUN handleWindowResize IN THIS-PROCEDURE.
END.


/* AppBuilder keeps taking focus on the WAIT-FOR.  If there's a better */
/* way to stop this from happening, let me know.                       */
lhAppBuilderWindow:HIDDEN = TRUE.

RUN populateOpenFiles.

APPLY "ENTRY" TO lhOpenFiles.

WAIT-FOR U1 OF lhFrame.

/* Restore the AppBuilder window. */
lhAppBuilderWindow:HIDDEN = FALSE.


DELETE WIDGET-POOL "RUNDIFF".

/* Put the user back in the code editor */
APPLY "ENTRY" TO lhEditor.







PROCEDURE performComparison.

    DEFINE VARIABLE lcDiffExecutable        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcProgressWindowsFuck   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcCommandLine           AS CHARACTER NO-UNDO. 
    
    lcDiffExecutable = "C:\Program Files\Araxis\Araxis Merge 2001 Professional Evaluation\compare.exe".
    lcProgressWindowsFuck = "C:\Progra~~1\Araxis\Araxis~~1\compare.exe".

    IF SEARCH ( lcDiffExecutable ) = ? THEN
        MESSAGE "Could not find the Araxis compare.exe file." VIEW-AS ALERT-BOX.

    lcCommandLine = lcProgressWindowsFuck + " " + SEARCH ( lcThisFile ) + " " + lhOpenFiles:SCREEN-VALUE.

    OS-COMMAND SILENT VALUE ( lcCommandLine ).

END PROCEDURE.






PROCEDURE browseForFile.
    DEFINE OUTPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE llOK AS LOGICAL NO-UNDO.

    SYSTEM-DIALOG GET-FILE pcFileName
        TITLE      "Choose file to compare against " + lcThisFile
        /* FILTERS    "All Files"   "*" */
        MUST-EXIST
        USE-FILENAME
        UPDATE llOk
        IN WINDOW lhWindow.

    IF NOT llOK OR
       SEARCH ( pcFileName ) = ? THEN
        pcFileName = ?.

END PROCEDURE. 







PROCEDURE handleWindowResize.

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
            lhDescription:WIDTH-PIXELS   = lhDescription:WIDTH-PIXELS + liDeltaWidth
            lhFrame:WIDTH-PIXELS        = lhFrame:WIDTH-PIXELS + liDeltaWidth
            
            lhEditor:WIDTH-PIXELS       = lhEditor:WIDTH-PIXELS + liDeltaWidth
            lhEditor:FRAME:WIDTH-PIXELS = lhEditor:FRAME:WIDTH-PIXELS + liDeltaWidth
            .
    ELSE
        ASSIGN
            lhFrame:WIDTH-PIXELS        = lhFrame:WIDTH-PIXELS + liDeltaWidth
            lhDescription:WIDTH-PIXELS   = lhDescription:WIDTH-PIXELS + liDeltaWidth
        
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












PROCEDURE populateOpenFiles.

    DEFINE VARIABLE lcCurrentFile AS CHARACTER NO-UNDO.

    RUN linxade/getwidgets.p
        ( INPUT "*",
          OUTPUT TABLE ttWidget ).

    EMPTY TEMP-TABLE ttOpenFile.

    FOR EACH ttWidget NO-LOCK
        WHERE ttWidget.widget-type = "WINDOW":
        

        IF ttWidget.widget-handle:NAME <> "Procedure Window" THEN 
            NEXT.

        lcCurrentFile = TRIM ( REPLACE ( ttWidget.widget-handle:TITLE, "Procedure - ", "" ) ).

        IF ttWidget.widget-handle = lhWindow THEN
        DO:
            lcThisFile = lcCurrentFile.
            NEXT.
        END.

        IF SEARCH ( lcCurrentFile ) = ? THEN
            NEXT.

        FIND FIRST ttOpenFile
            WHERE ttOpenFile.file-name = lcCurrentFile
            NO-LOCK NO-ERROR.

        IF AVAILABLE ttOpenFile THEN
            NEXT.

        CREATE ttOpenFile.
        ASSIGN
            ttOpenFile.editor-window = ttWidget.widget-handle
            ttOpenFile.file-name     = lcCurrentFile.
    
    END.

    FOR EACH ttOpenFile NO-LOCK:

        lhOpenFiles:ADD-LAST ( ttOpenFile.file-name ).

    END.

    lhOpenFiles:SCREEN-VALUE = {&FILE-SELECT-MESSAGE} .

END PROCEDURE.



PROCEDURE handleChangeOfComparisonFile.

    IF SEARCH ( lhOpenFiles:SCREEN-VALUE ) = ? THEN
        lhPerformComparison:SENSITIVE = FALSE.
    ELSE
        lhPerformComparison:SENSITIVE = TRUE.

END PROCEDURE.
