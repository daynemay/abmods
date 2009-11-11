&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/******************************************************************************
  PROCEDURE            : utils/db/fieldsel.w
  DESCRIPTION          : User interface to select a subset of a table's fields
                         for whatever devious purpose you may have in mind.
  AUTHOR               : LINX-daynem
  DATE CREATED         : 11/08/06
  CALLED FROM          : Menu

  CHANGES MADE AND DATE:
  Date      Version      Description

  11/08/06  LINX#daynem  Initial Version.
******************************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT        PARAMETER pcTableName       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER pcFieldList       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER pcWindowTitle     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER pcSelectedFields  AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS sl-TableFields btnAddFields ~
sl-SelectedFields btnRemoveFields btnMoveUp btnMoveDown btnOK 
&Scoped-Define DISPLAYED-OBJECTS sl-TableFields sl-SelectedFields ~
fiFromTitle fiToTitle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddFields 
     LABEL "Add >>" 
     SIZE 10 BY 1.

DEFINE BUTTON btnMoveDown 
     LABEL "Move Down" 
     SIZE 10 BY 1.

DEFINE BUTTON btnMoveUp 
     LABEL "Move Up" 
     SIZE 10 BY 1.

DEFINE BUTTON btnOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON btnRemoveFields 
     LABEL "<< Remove" 
     SIZE 10 BY 1.

DEFINE VARIABLE fiFromTitle AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 25 BY .63 NO-UNDO.

DEFINE VARIABLE fiToTitle AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 25 BY .63 NO-UNDO.

DEFINE VARIABLE sl-SelectedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 25 BY 11.25 NO-UNDO.

DEFINE VARIABLE sl-TableFields AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 25 BY 11.25 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sl-TableFields AT ROW 2.25 COL 4 NO-LABEL
     btnAddFields AT ROW 2.25 COL 29.5
     sl-SelectedFields AT ROW 2.25 COL 40 NO-LABEL
     btnRemoveFields AT ROW 3.5 COL 29.5
     btnMoveUp AT ROW 6.25 COL 66
     btnMoveDown AT ROW 7.5 COL 66
     btnOK AT ROW 12.5 COL 66
     fiFromTitle AT ROW 1.5 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiToTitle AT ROW 1.5 COL 37.5 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.88 BY 13.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Field Selector"
         HEIGHT             = 13.21
         WIDTH              = 76.13
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiFromTitle IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiFromTitle:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiToTitle IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Field Selector */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Field Selector */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddFields C-Win
ON CHOOSE OF btnAddFields IN FRAME DEFAULT-FRAME /* Add >> */
DO:

    RUN addFields IN THIS-PROCEDURE
        ( INPUT sl-TableFields:SCREEN-VALUE ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown C-Win
ON CHOOSE OF btnMoveDown IN FRAME DEFAULT-FRAME /* Move Down */
DO:

    RUN moveDown IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp C-Win
ON CHOOSE OF btnMoveUp IN FRAME DEFAULT-FRAME /* Move Up */
DO:

    RUN moveUp IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:

    APPLY "CLOSE" TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveFields C-Win
ON CHOOSE OF btnRemoveFields IN FRAME DEFAULT-FRAME /* << Remove */
DO:

    RUN removeFields IN THIS-PROCEDURE
        ( INPUT sl-SelectedFields:SCREEN-VALUE ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl-SelectedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-SelectedFields C-Win
ON MOUSE-SELECT-DBLCLICK OF sl-SelectedFields IN FRAME DEFAULT-FRAME
DO:

    APPLY "CHOOSE" TO btnRemoveFields.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-SelectedFields C-Win
ON RETURN OF sl-SelectedFields IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE" TO btnRemoveFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl-TableFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-TableFields C-Win
ON MOUSE-SELECT-DBLCLICK OF sl-TableFields IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE" TO btnAddFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-TableFields C-Win
ON RETURN OF sl-TableFields IN FRAME DEFAULT-FRAME
DO:

    APPLY "CHOOSE" TO btnAddFields.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN getResults IN THIS-PROCEDURE.
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN initialise IN THIS-PROCEDURE.

    RUN enable_UI.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddFields C-Win 
PROCEDURE AddFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcFields AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lcThisEntry AS CHARACTER NO-UNDO.

    DO i = 1 TO NUM-ENTRIES ( pcFields ) WITH FRAME {&FRAME-NAME}:

        lcThisEntry = ENTRY ( i, pcFields ).

        IF sl-SelectedFields:LOOKUP ( lcThisEntry ) = 0 THEN
        DO:

            sl-SelectedFields:ADD-LAST ( ENTRY ( i, pcFields ) ) IN FRAME {&FRAME-NAME}.

        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY sl-TableFields sl-SelectedFields fiFromTitle fiToTitle 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE sl-TableFields btnAddFields sl-SelectedFields btnRemoveFields 
         btnMoveUp btnMoveDown btnOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getResults C-Win 
PROCEDURE getResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        pcSelectedFields = ( IF sl-SelectedFields:LIST-ITEMS EQ ?
                             THEN ""
                             ELSE sl-SelectedFields:LIST-ITEMS ).

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE llIsTable       AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE liDBnum         AS INTEGER      NO-UNDO.

    DEFINE VARIABLE lcQuery         AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE lhQuery         AS HANDLE       NO-UNDO.

    DEFINE VARIABLE lhFileBuffer    AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lhFieldBuffer   AS HANDLE       NO-UNDO.

    DEFINE VARIABLE lcFieldName     AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE liX             AS INTEGER      NO-UNDO.
    DEFINE VARIABLE liY             AS INTEGER      NO-UNDO.

    IF pcFieldList = "" OR 
       pcFieldList = ?      THEN
        pcFieldList = "*".

    /* Centre the window in the screen */
    RUN centrew.p
        ( INPUT {&WINDOW-NAME},
          OUTPUT liX,
          OUTPUT liY ).

    ASSIGN
        {&WINDOW-NAME}:X = liX
        {&WINDOW-NAME}:Y = liY.

    /* Validate the table, and determine which database it is in */
    RUN utils/db/isdbtable.p
        ( INPUT  pcTableName,
          OUTPUT llIsTable,
          OUTPUT liDBnum ).

    IF NOT llIsTable THEN
    DO:

        MESSAGE pcTableName "is not a valid database table." VIEW-AS ALERT-BOX.
        APPLY "CLOSE" TO THIS-PROCEDURE.

    END.

    DO WITH FRAME {&FRAME-NAME}:

        ASSIGN
            fiFromTitle:SCREEN-VALUE    = "Fields in " + pcTableNAme + ":"
            fiToTitle:SCREEN-VALUE      = "Selected Fields:"

            {&WINDOW-NAME}:TITLE        = pcWindowTitle

            sl-SelectedFields:LIST-ITEMS = pcSelectedFields.

    END.

    /* Dynamic query in the appropriate database */    
    lcQuery = "FOR EACH " + LDBNAME ( liDBnum ) + "._file NO-LOCK " + 
              "   WHERE _file._file-name = " + QUOTER ( pcTableName ) + 
              "   , EACH " + STRING ( LDBNAME ( liDBnum ) ) + "._field NO-LOCK " +
              "     OF _file WHERE _field._extent = 0 " + /* Not catering for arrays at the moment. */
              "                AND CAN-DO ( " + QUOTER ( pcFieldList ) + ", _field._field-name )".

    CREATE BUFFER lhFileBuffer FOR TABLE ( LDBNAME ( liDBnum ) + "._file" ).
    CREATE BUFFER lhFieldBuffer FOR TABLE ( LDBNAME ( liDBnum ) + "._field" ).

    CREATE QUERY lhQuery.

    lhQuery:ADD-BUFFER ( lhFileBuffer ).
    lhQuery:ADD-BUFFER ( lhFieldBuffer ).

    lhQuery:QUERY-PREPARE ( lcQuery ).
    lhQuery:QUERY-OPEN ( ).
    lhQuery:GET-FIRST ().

    DO WHILE NOT lhQuery:QUERY-OFF-END:
    
        lcFieldName = lhFieldBuffer:BUFFER-FIELD ("_field-name"):BUFFER-VALUE.

        /* Show the field in the selection list.  */
        sl-TableFields:ADD-LAST ( lcFieldName ).

        lhQuery:GET-NEXT ( ).

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveDown C-Win 
PROCEDURE moveDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE liCnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE lcThisEntry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcPriorEntry AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcScreenValue AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        /* Nothing selected. */
        IF sl-SelectedFields:SCREEN-VALUE = ? THEN
            RETURN.

        /* Can't move last item down. */
        IF sl-SelectedFields:IS-SELECTED ( sl-SelectedFields:NUM-ITEMS ) THEN
            RETURN.

        lcScreenValue = sl-SelectedFields:SCREEN-VALUE.

        DO liCnt = sl-SelectedFields:NUM-ITEMS - 1 TO 1 BY -1:

            IF NOT sl-SelectedFields:IS-SELECTED ( liCnt ) THEN
                NEXT.

            lcThisEntry = sl-SelectedFields:ENTRY ( liCnt ).

            sl-SelectedFields:DELETE ( lcThisEntry ).

            sl-SelectedFields:INSERT ( lcThisEntry, liCnt + 1).

        END.

        sl-SelectedFields:SCREEN-VALUE = lcScreenValue.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveUp C-Win 
PROCEDURE moveUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE liCnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE lcThisEntry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcPriorEntry AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcScreenValue AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        /* Nothing selected to move. */
        IF sl-SelectedFields:SCREEN-VALUE = ? THEN
            RETURN.

        /* Can't move top item up. */
        IF sl-SelectedFields:IS-SELECTED ( 1 )  THEN
            RETURN.

        lcScreenValue = sl-SelectedFields:SCREEN-VALUE.

        DO liCnt = 2 TO sl-SelectedFields:NUM-ITEMS:

            IF NOT sl-SelectedFields:IS-SELECTED ( liCnt ) THEN
                NEXT.

            lcThisEntry = sl-SelectedFields:ENTRY ( liCnt ).

            sl-SelectedFields:DELETE ( lcThisEntry ).

            sl-SelectedFields:INSERT ( lcThisEntry, liCnt - 1 ).

        END.

        sl-SelectedFields:SCREEN-VALUE = lcScreenValue.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveFields C-Win 
PROCEDURE RemoveFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcFields AS CHARACTER NO-UNDO.

    sl-SelectedFields:DELETE ( pcFields ) IN FRAME {&FRAME-NAME} NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

