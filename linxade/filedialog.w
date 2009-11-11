&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
DEFINE INPUT-OUTPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER plCancel   AS LOGICAL   NO-UNDO.

{linxade/mru-tt.i}
{linxade/mru.i}
{linxade/winprocs.i}

DEFINE STREAM sMru.

DEFINE VARIABLE llSort AS LOGICAL     NO-UNDO.

CREATE WIDGET-POOL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br_mru

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttMru

/* Definitions for BROWSE br_mru                                        */
&Scoped-define FIELDS-IN-QUERY-br_mru ttMru.filename NO-LABEL   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_mru   
&Scoped-define SELF-NAME br_mru
&Scoped-define OPEN-QUERY-br_mru IF llSort THEN DO:      OPEN QUERY {&SELF-NAME} FOR EACH ttMru         WHERE ttMru.filename MATCHES "*" + fiFilename:SCREEN-VALUE + "*"         BY ttMru.filename.  END. ELSE DO:      OPEN QUERY {&SELF-NAME} FOR EACH ttMru         WHERE ttMru.filename MATCHES "*" + fiFilename:SCREEN-VALUE + "*"         BY ttMru.order.  END.
&Scoped-define TABLES-IN-QUERY-br_mru ttMru
&Scoped-define FIRST-TABLE-IN-QUERY-br_mru ttMru


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-br_mru}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiFilename br_mru btSort 
&Scoped-Define DISPLAYED-OBJECTS fiFilename fi-will-open 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btSort 
     LABEL "Sort alphabetically" 
     SIZE 79 BY 1.

DEFINE VARIABLE fi-will-open AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 89 BY .62 NO-UNDO.

DEFINE VARIABLE fiFilename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN 
     SIZE 78.2 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_mru FOR 
      ttMru SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_mru
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_mru Dialog-Frame _FREEFORM
  QUERY br_mru DISPLAY
      ttMru.filename FORMAT "X(80)" NO-LABEL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING NO-SCROLLBAR-VERTICAL SIZE 78 BY 12.28 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiFilename AT ROW 1.24 COL 10 COLON-ALIGNED
     br_mru AT ROW 2.33 COL 12 WIDGET-ID 100
     btSort AT ROW 14.71 COL 11.6
     fi-will-open AT ROW 15.91 COL 2 NO-LABEL WIDGET-ID 2
     SPACE(0.59) SKIP(0.17)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Open File".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br_mru fiFilename Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-will-open IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fi-will-open:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_mru
/* Query rebuild information for BROWSE br_mru
     _START_FREEFORM
IF llSort THEN
DO:

    OPEN QUERY {&SELF-NAME} FOR EACH ttMru
        WHERE ttMru.filename MATCHES "*" + fiFilename:SCREEN-VALUE + "*"
        BY ttMru.filename.

END.
ELSE
DO:

    OPEN QUERY {&SELF-NAME} FOR EACH ttMru
        WHERE ttMru.filename MATCHES "*" + fiFilename:SCREEN-VALUE + "*"
        BY ttMru.order.

END.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_mru */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Open File */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Open File */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_mru
&Scoped-define SELF-NAME br_mru
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_mru Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF br_mru IN FRAME Dialog-Frame
DO:
    IF AVAILABLE ttMru THEN
        APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_mru Dialog-Frame
ON RETURN OF br_mru IN FRAME Dialog-Frame
DO:

    IF AVAILABLE ttMru THEN
        APPLY "GO" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_mru Dialog-Frame
ON VALUE-CHANGED OF br_mru IN FRAME Dialog-Frame
DO:

    IF AVAILABLE ttMru THEN
    DO:
        fiFilename:SCREEN-VALUE = ttMru.filename.
        RUN updateWillOpen (   fiFilename:SCREEN-VALUE ).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSort Dialog-Frame
ON CHOOSE OF btSort IN FRAME Dialog-Frame /* Sort alphabetically */
DO:

    RUN sortFilenames IN THIS-PROCEDURE.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFilename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilename Dialog-Frame
ON ANY-PRINTABLE OF fiFilename IN FRAME Dialog-Frame /* Filename */
DO:
  
    RUN updateWillOpen
        ( INPUT SELF:SCREEN-VALUE + KEY-FUNCTION ( LASTKEY ) ).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilename Dialog-Frame
ON CURSOR-DOWN OF fiFilename IN FRAME Dialog-Frame /* Filename */
DO:

    APPLY "ENTRY":U TO br_mru.

    APPLY "CURSOR-DOWN":U TO br_mru.

    APPLY "VALUE-CHANGED":U TO br_mru.
    
    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilename Dialog-Frame
ON RETURN OF fiFilename IN FRAME Dialog-Frame /* Filename */
DO:

    APPLY "GO" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilename Dialog-Frame
ON VALUE-CHANGED OF fiFilename IN FRAME Dialog-Frame /* Filename */
DO:
  
    RUN updateWillOpen
        ( INPUT SELF:SCREEN-VALUE ).

    {&OPEN-QUERY-br_mru}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON 'F5':U ANYWHERE 
DO:
    RETURN NO-APPLY.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* We will override this below, after the GO event */
    plCancel = TRUE.
    
    llSort = FALSE.

    FRAME {&FRAME-NAME}:PARENT = fAppBuilderWindow ().

    RUN enable_UI.

    RUN populateMru IN THIS-PROCEDURE.

     IF pcFileName NE ? THEN
         fiFilename:SCREEN-VALUE = pcFileName.

    fiFilename:MOVE-TO-TOP().


    WAIT-FOR GO OF FRAME {&FRAME-NAME}.

    plCancel = FALSE.

    pcFileName = fiFilename:SCREEN-VALUE.

    IF AVAILABLE ttMru THEN
    DO:
        pcFileName = ttMru.filename.
    END.

END.
RUN disable_UI.

DELETE WIDGET-POOL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiFilename fi-will-open 
      WITH FRAME Dialog-Frame.
  ENABLE fiFilename br_mru btSort 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateMru Dialog-Frame 
PROCEDURE populateMru :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttMru.

    RUN getMru IN THIS-PROCEDURE.

    DO WITH FRAME {&FRAME-NAME}:
        {&OPEN-QUERY-br_mru}
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortFilenames Dialog-Frame 
PROCEDURE sortFilenames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lcOriginalList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liI             AS INTEGER     NO-UNDO.

    DEFINE VARIABLE lcCurrentSelection AS CHARACTER   NO-UNDO.

    llSort = NOT llSort.


    DO WITH FRAME {&FRAME-NAME}:
        fiFilename:SCREEN-VALUE = "".

        IF llSort THEN
        DO:
            btSort:LABEL = "Sort by recency".
        END.
        ELSE
        DO:
            btSort:LABEL = "Sort alphabetically".
        END.

    END.
    
    {&OPEN-QUERY-br_mru}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateWillOpen Dialog-Frame 
PROCEDURE updateWillOpen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcSearchFor AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF INDEX ( pcSearchFor, "." ) > 0 AND SEARCH ( pcSearchFor ) NE ? THEN
            ASSIGN
                fi-will-open:SCREEN-VALUE = "Will open: " + SEARCH ( pcSearchFor )
                fi-will-open:VISIBLE = TRUE.
        ELSE
            ASSIGN
                fi-will-open:VISIBLE = FALSE.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

