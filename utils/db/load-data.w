&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 Character
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*------------------------------------------------------------------------

  File: load-data.w 

  Description: To load a tollworks database running against multiple processes
               and load balancing the data load to improve performance

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Daniel Skerry

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

/*************************************************************
Procedure process flow

Load-Data
    get-dump-file-names
    start processes (Loop)
        CheckRunningProcesses
        SpawnLoad
            GenerateLoadProgram
            RunLoadProgram
    CheckForProcesses (Loop)

*************************************************************/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-tables NO-UNDO
    FIELD tablename    AS CHAR
    FIELD dumpname     AS CHAR
    FIELD databasename AS CHAR
    FIELD datatoload   AS LOG
    FIELD datafile     AS CHAR
    FIELD processed    AS LOG
    INDEX idx1 AS PRIMARY tablename
    INDEX idx2 databasename
    INDEX idx3 dumpname
    INDEX idx4 datatoload
    INDEX idx5 datafile
    .

DEF TEMP-TABLE tt-databases NO-UNDO
    FIELD databasename  AS CHAR
    FIELD numprocesses  AS INT
    INDEX idx1 AS PRIMARY databasename
    .

DEF TEMP-TABLE tt-running NO-UNDO
    FIELD datafile     AS CHAR
    FIELD databasename AS CHAR
    INDEX idx1 AS PRIMARY datafile
    INDEX idx2 databasename
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME b-master

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-databases

/* Definitions for BROWSE b-master                                      */
&Scoped-define FIELDS-IN-QUERY-b-master tt-databases.databasename tt-databases.numprocesses   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-master tt-databases.numprocesses   
&Scoped-define ENABLED-TABLES-IN-QUERY-b-master tt-databases
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-b-master tt-databases
&Scoped-define SELF-NAME b-master
&Scoped-define QUERY-STRING-b-master FOR EACH tt-databases NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-b-master OPEN QUERY {&SELF-NAME} FOR EACH tt-databases NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-b-master tt-databases
&Scoped-define FIRST-TABLE-IN-QUERY-b-master tt-databases


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME     ~{&OPEN-QUERY-b-master}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-pause fi-dir fi-out-dir fi-pf 
&Scoped-Define DISPLAYED-OBJECTS fi-pause fi-dir fi-out-dir fi-pf 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-dir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data DIR" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
     &ELSE SIZE 60 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-out-dir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Output DIR" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
     &ELSE SIZE 60 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-pause AS INTEGER FORMAT ">>9":U INITIAL 5 
     LABEL "Wait Pause" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
     &ELSE SIZE 4 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-pf AS CHARACTER FORMAT "X(256)":U 
     LABEL "PF file" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
     &ELSE SIZE 60 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY b-master FOR 
      tt-databases SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE b-master
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-master TERMINAL-SIMULATION _FREEFORM
  QUERY b-master NO-LOCK DISPLAY
      tt-databases.databasename COLUMN-LABEL "DB Name" FORMAT "x(12)":U
      tt-databases.numprocesses COLUMN-LABEL "Processes" FORMAT ">>9":U
  ENABLE
      tt-databases.numprocesses
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 30 BY 10 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     b-master
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 6
          &ELSE AT ROW 2 COL 6 &ENDIF
     fi-pause
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 13 COLON-ALIGNED
          &ELSE AT ROW 12.01 COL 13 COLON-ALIGNED &ENDIF
     fi-dir
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 13 COLON-ALIGNED
          &ELSE AT ROW 13 COL 13 COLON-ALIGNED &ENDIF
     fi-out-dir
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 13 COLON-ALIGNED
          &ELSE AT ROW 13.99 COL 13 COLON-ALIGNED &ENDIF
     fi-pf
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 13 COLON-ALIGNED
          &ELSE AT ROW 15.01 COL 13 COLON-ALIGNED &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21
        TITLE "Archive Data Load".


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
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = "Archive Data load"
         HEIGHT             = 18
         WIDTH              = 75.43
         MAX-HEIGHT         = 18
         MAX-WIDTH          = 75.43
         VIRTUAL-HEIGHT     = 18
         VIRTUAL-WIDTH      = 75.43
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB b-master 1 DEFAULT-FRAME */
/* SETTINGS FOR BROWSE b-master IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-master
/* Query rebuild information for BROWSE b-master
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-databases NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE b-master */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fi-pf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-pf TERMINAL-SIMULATION
ON GO OF fi-pf IN FRAME DEFAULT-FRAME /* PF file */
DO:
  RUN Load-data IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME b-master
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

ON RETURN OF fi-pf
DO:
    APPLY "GO" TO SELF.
    RETURN NO-APPLY.
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
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckForProcesses TERMINAL-SIMULATION 
PROCEDURE CheckForProcesses :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAM piNumProcesses    AS INT NO-UNDO.

    DEF VAR lcfile AS CHAR NO-UNDO.

    INPUT FROM OS-DIR(fi-dir) NO-ATTR-LIST.

    REPEAT:

        IMPORT UNFORMATTED lcfile NO-ERROR.
        lcfile = TRIM(SUBSTR(lcfile,INDEX(lcfile,'" "') + 3),'"').
        /* is this a .loading file */
        IF INDEX(lcfile,".loading") NE 0 THEN
            piNumProcesses = piNumProcesses + 1.

    END. /* REPEAT */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckRunningProcesses TERMINAL-SIMULATION 
PROCEDURE CheckRunningProcesses :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAM TABLE FOR tt-running.    
    
    DEF VAR lcfile     AS CHAR NO-UNDO.
    DEF VAR lcdatafile AS CHAR NO-UNDO.

    INPUT FROM OS-DIR(fi-dir) NO-ATTR-LIST.

    REPEAT:

        IMPORT UNFORMATTED lcfile NO-ERROR.
        lcfile = SUBSTR(lcfile,INDEX(lcfile,'" "') + 3).
        /* is this a .loading file */
        IF INDEX(lcfile,".loading") NE 0 THEN
        DO:
            lcdatafile = REPLACE(lcfile,".loading",".d").
            lcdatafile = REPLACE(lcdatafile,'"',"").
            FIND FIRST tt-running NO-LOCK
                WHERE tt-running.datafile EQ lcdatafile NO-ERROR.
            IF NOT AVAIL tt-running THEN
            DO:
                CREATE tt-running.
                ASSIGN tt-running.datafile = lcdatafile.
                FIND FIRST tt-tables EXCLUSIVE-LOCK
                    WHERE tt-tables.datafile EQ tt-running.datafile NO-ERROR.
                IF AVAIL tt-tables THEN
                    ASSIGN tt-running.databasename = tt-tables.databasename
                           tt-tables.datatoload = FALSE.
            END.
        END.

    END. /* REPEAT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI TERMINAL-SIMULATION  _DEFAULT-DISABLE
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
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI TERMINAL-SIMULATION  _DEFAULT-ENABLE
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
  DISPLAY fi-pause fi-dir fi-out-dir fi-pf 
      WITH FRAME DEFAULT-FRAME IN WINDOW TERMINAL-SIMULATION.
  ENABLE fi-pause fi-dir fi-out-dir fi-pf 
      WITH FRAME DEFAULT-FRAME IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateLoadProgram TERMINAL-SIMULATION 
PROCEDURE GenerateLoadProgram :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM pcDatabaseName   AS CHAR NO-UNDO.
    DEF INPUT PARAM pctablename      AS CHAR NO-UNDO.
    DEF INPUT PARAM pcdatafile       AS CHAR NO-UNDO.
    DEF OUTPUT PARAM pcLoadProgram   AS CHAR NO-UNDO.

    DEF VAR lcErrorFile        AS CHAR NO-UNDO.
    DEF VAR lcLoadingFile      AS CHAR NO-UNDO.
    DEF VAR lcProcessedFile    AS CHAR NO-UNDO.
    DEF VAR lcdatafile         AS CHAR NO-UNDO.
    DEF VAR lcLoadProgram      AS CHAR NO-UNDO.
    DEF VAR lcErrorFileOut     AS CHAR NO-UNDO.
    DEF VAR lcProcessedFileOut AS CHAR NO-UNDO.
    DEF VAR lcdatafileout      AS CHAR NO-UNDO.
    DEF VAR lcLog              AS CHAR NO-UNDO.
    DEF VAR lcLogOut           AS CHAR NO-UNDO.

    ASSIGN lcErrorFile     = TRIM(REPLACE(pcdatafile,".d",".e"))
           lcLoadingFile   = TRIM(REPLACE(pcdatafile,".d",".loading"))
           lcProcessedFile = TRIM(REPLACE(pcdatafile,".d",".processed"))
           lcLoadProgram   = TRIM(REPLACE(pcdatafile,".d",".p"))
           lcLog           = lcLoadProgram + ".log".

    ASSIGN lcErrorFileOut     = REPLACE(lcErrorFile,fi-dir,fi-out-dir)
           lcProcessedFileOut = REPLACE(lcProcessedFile,fi-dir,fi-out-dir)
           lcdatafileOut      = REPLACE(pcdatafile,fi-dir,fi-out-dir)
           lcLogOut           = REPLACE(lcLog,fi-dir,fi-out-dir).

    OUTPUT TO VALUE(lcLoadProgram).
    PUT UNFORMATTED "DISABLE TRIGGERS FOR LOAD OF " pctablename "." SKIP
                    "DEF VAR i      AS INT NO-UNDO."  SKIP
                    "DEF VAR litime AS INT NO-UNDO."  SKIP
                    "DEF STREAM lsLoading."           SKIP
                    "OUTPUT STREAM lsLoading TO " lcLoadingFile "."  SKIP
                    "litime = TIME."                  SKIP
                    "PUT STREAM lsLoading UNFORMATTED 'Loading: ' STRING(TIME,'HH:MM:SS')." SKIP
                    "INPUT FROM " pcdatafile "."      SKIP
                    "REPEAT : "                       SKIP
                    "    CREATE " pctablename "."     SKIP
                    "    IMPORT " pctablename "."     SKIP
                    "    i = i + 1."                  SKIP
                    "    IF i MODULO 1000 = 0 THEN"   SKIP
                    "        PUT STREAM lsLoading UNFORMATTED 'RECORDS LOADED: ' STRING(i) SKIP." SKIP
                    "END."                            SKIP
                    "OUTPUT STREAM lsLoading CLOSE."  SKIP
                    "INPUT CLOSE."                    SKIP
                    "OUTPUT TO " lcProcessedFile "."  SKIP
                    "PUT UNFORMATTED 'Load Complete: ' STRING(TIME,'HH:MM:SS') SKIP." SKIP
                    "PUT UNFORMATTED 'Load Duration: ' STRING(TIME - litime,'HH:MM:SS') SKIP." SKIP
                    "PUT UNFORMATTED 'Total Records Loaded: ' STRING(i) SKIP." SKIP
                    "OUTPUT CLOSE." SKIP
                    "OS-DELETE " lcLoadingFile "." SKIP
                    "OS-COPY " lcErrorFile " " lcErrorFileOut "." SKIP
                    "OS-COPY " lcProcessedFile " " lcProcessedFileOut "." SKIP
                    "OS-COPY " pcdatafile " " lcdatafileOut "." SKIP
                    "OS-COPY " lcLog " " lcLogOut "." SKIP
                    "OS-DELETE " lcErrorFile "." SKIP
                    "OS-DELETE " lcProcessedFile "." SKIP
                    "OS-DELETE " pcdatafile "." SKIP
                    "OS-DELETE " lcLog "." SKIP
                    "OS-DELETE " lcLoadProgram "." SKIP
                    "QUIT." SKIP.
    
    OUTPUT CLOSE.
    pcLoadProgram = lcLoadProgram.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-dump-file-names TERMINAL-SIMULATION 
PROCEDURE get-dump-file-names :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* find all .d files and attach the file names to the tables     */
    /* if multiple .d files exist, create another tt record for each */
DEF VAR lcfile AS CHAR NO-UNDO.

DEF BUFFER btt-tables FOR tt-tables.

INPUT FROM OS-DIR(fi-dir) NO-ATTR-LIST.

REPEAT:

    IMPORT UNFORMATTED lcfile NO-ERROR.
    lcfile = TRIM(SUBSTR(lcfile,INDEX(lcfile,'" "') + 3),'"').
    lcfile = REPLACE(lcfile,'"',"").
    /* is this a .d file */
    IF ENTRY(NUM-ENTRIES(lcfile,"."),lcfile,".") EQ "d" THEN
    DO:
        /* This find is not perfect, but should be ok. There may be a problem
           is we have a dump name of say 'cust' and 'custb' for example */
        FIND FIRST btt-tables NO-LOCK
            WHERE INDEX(lcfile,btt-tables.dumpname + ".") NE 0 NO-ERROR.
        IF AVAIL btt-tables THEN
        DO:
            IF btt-tables.datafile NE "" THEN
            DO:
                CREATE tt-tables.
                BUFFER-COPY btt-tables EXCEPT datafile TO tt-tables NO-ERROR.
            END.
            ELSE
                FIND FIRST tt-tables EXCLUSIVE-LOCK
                    WHERE ROWID(tt-tables) EQ ROWID(btt-tables) NO-ERROR.

            IF AVAIL tt-tables THEN
            DO:
                ASSIGN tt-tables.datafile   = TRIM(lcfile)
                       tt-tables.datatoload = TRUE.
            END.
        END.
    END.

END. /* REPEAT */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-table-names TERMINAL-SIMULATION 
PROCEDURE get-table-names :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lhquery   AS HANDLE NO-UNDO.
DEF VAR lhbuffer  AS HANDLE NO-UNDO.
DEF VAR lcquery   AS CHAR   NO-UNDO.
DEF VAR liloop    AS INT    NO-UNDO.

EMPTY TEMP-TABLE tt-tables.
EMPTY TEMP-TABLE tt-databases.

DO liloop = 1 TO NUM-DBS:
    
    ASSIGN lcquery = "FOR EACH " + LDBNAME(liloop) + "._file NO-LOCK " +
                     " WHERE " + LDBNAME(liloop) + "._file._hidden EQ FALSE".
    CREATE QUERY lhQuery.
    CREATE BUFFER lhbuffer FOR TABLE(LDBNAME(liloop) + "._file").
    lhQuery:SET-BUFFERS (lhbuffer).
    lhQuery:QUERY-PREPARE(lcQuery).
    lhQuery:QUERY-OPEN().
    lhQuery:GET-FIRST().
    REPEAT:
        IF lhQuery:QUERY-OFF-END THEN 
           LEAVE.
        CREATE tt-tables.
        ASSIGN tt-tables.tablename     = lhbuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE   
               tt-tables.dumpname      = lhbuffer:BUFFER-FIELD("_dump-name"):BUFFER-VALUE
               tt-tables.databasename  = LDBNAME(liloop)
               tt-tables.datatoload    = ?
               tt-tables.datafile      = ?
               tt-tables.processed     = NO.
        lhQuery:GET-NEXT().
    END.
    lhQuery:QUERY-CLOSE. 
    DELETE OBJECT lhQuery.

END.

FOR EACH tt-tables NO-LOCK
    BREAK BY tt-tables.databasename:
    IF FIRST-OF(tt-tables.databasename) THEN
    DO:
        CREATE tt-databases.
        ASSIGN tt-databases.databasename = tt-tables.databasename
               tt-databases.numprocesses = 1.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise TERMINAL-SIMULATION 
PROCEDURE Initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN get-table-names IN THIS-PROCEDURE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-data TERMINAL-SIMULATION 
PROCEDURE load-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR liNumProcesses   AS INT  NO-UNDO.
    DEF VAR lcErrMsg         AS CHAR NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN fi-dir fi-out-dir fi-pf fi-pause.
    END.
    
    fi-dir = RIGHT-TRIM(fi-dir,"/").
    fi-dir = fi-dir + "/".
    fi-out-dir = RIGHT-TRIM(fi-out-dir,"/").
    fi-out-dir = fi-out-dir + "/".

    RUN ValidateParameters IN THIS-PROCEDURE
        (OUTPUT lcErrMsg).
    IF lcErrMsg NE "" THEN
    DO:
        MESSAGE lcErrMsg VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    FRAME {&FRAME-NAME}:SENSITIVE = FALSE.

    /* get all connected database table names */
    RUN get-table-names IN THIS-PROCEDURE.

    /* search the input directory for all .d files */
    RUN get-dump-file-names IN THIS-PROCEDURE.
    
    /* check the number of load processes against each db */
    REPEAT:
        RUN StartProcesses IN THIS-PROCEDURE.
        IF NOT CAN-FIND(FIRST tt-tables
                        WHERE tt-tables.datatoload EQ TRUE) THEN LEAVE.
        PAUSE fi-pause no-message.
    END.
    
    MESSAGE "Waiting for final processes to complete". PAUSE 0.
    
    REPEAT:
        RUN CheckForProcesses IN THIS-PROCEDURE
            (OUTPUT liNumProcesses).
        IF liNumProcesses EQ 0 THEN LEAVE.
        PAUSE fi-pause.
    END.
    FRAME {&FRAME-NAME}:SENSITIVE = TRUE.
    MESSAGE "PROCESS COMPLETE" VIEW-AS ALERT-BOX INFO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunLoadProgram TERMINAL-SIMULATION 
PROCEDURE RunLoadProgram :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM pcLoadProgram   AS CHAR NO-UNDO.

    DEF VAR lcCommand    AS CHAR NO-UNDO.

    lcCommand = "mpro -b -pf " + fi-pf + " -p " + pcLoadProgram +
                " >> " + pcLoadProgram + ".log &".

   OS-COMMAND VALUE(lcCommand) NO-WAIT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SpawnLoad TERMINAL-SIMULATION 
PROCEDURE SpawnLoad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM pcDatabaseName   AS CHAR NO-UNDO.

    DEF VAR lcLoadProgram    AS CHAR NO-UNDO.

    FIND FIRST tt-tables EXCLUSIVE-LOCK
        WHERE tt-tables.databasename EQ pcdatabasename
          AND tt-tables.datatoload   EQ TRUE
          AND tt-tables.processed    EQ FALSE NO-ERROR.
    IF AVAIL tt-tables THEN
    DO:
        RUN GenerateLoadProgram IN THIS-PROCEDURE
            (INPUT pcDatabaseName,
             INPUT tt-tables.tablename,
             INPUT tt-tables.datafile,
             OUTPUT lcloadprogram).
        RUN RunLoadProgram IN THIS-PROCEDURE
            (INPUT lcLoadProgram).
        tt-tables.datatoload = FALSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartProcesses TERMINAL-SIMULATION 
PROCEDURE StartProcesses :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR li      AS INT  NO-UNDO.
    DEF VAR liloop  AS INT  NO-UNDO.
    DEF VAR lcdb    AS CHAR NO-UNDO.

    DEF BUFFER btt-databases FOR tt-databases.

    /* Get the number of Processes running for each database */
    EMPTY TEMP-TABLE tt-running.
    RUN CheckRunningProcesses IN THIS-PROCEDURE
        (OUTPUT TABLE tt-running).
      
    /* Check that the maximum number of processes are running for
       each database. If not then spawn more processes for the 
       appropriate databases */
    FOR EACH btt-databases NO-LOCK:
        ASSIGN lcdb = btt-databases.databasename
               li   = 0.
        FOR EACH tt-running NO-LOCK
            WHERE tt-running.databasename EQ btt-databases.databasename:
            li = li + 1.
        END.
        IF li LT btt-databases.numprocesses THEN
        DO:
            DO liloop = 1 TO (btt-databases.numprocesses - li):
                RUN SpawnLoad IN THIS-PROCEDURE
                    (INPUT lcdb).
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateParameters TERMINAL-SIMULATION 
PROCEDURE ValidateParameters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAM pcErrMsg   AS CHAR NO-UNDO.

    DEF VAR lierror    AS INT NO-UNDO.

    /* validate that the pf file exists */
    IF SEARCH(fi-pf) EQ ? THEN
    DO:
        pcErrMsg = "Invalid pf file specified '" + fi-pf + "'.".
        LEAVE.
    END.

    /* validate the directory path of data directory */
    OS-CREATE-DIR VALUE(fi-dir).
    lierror = OS-ERROR.
    IF lierror NE 0 THEN
    DO:
        pcErrMsg = "Data directory not created. System Error #" + STRING(lierror).
        LEAVE.
    END.

    /* validate the directory path of the output directory */
    OS-CREATE-DIR VALUE(fi-out-dir).
    lierror = OS-ERROR.
    IF lierror NE 0 THEN
    DO:
        pcErrMsg = "Output directory not created. System Error #" + STRING(lierror).
        LEAVE.
    END.

    /* validate that the pause is not less than 5 seconds or greater than 1 hour */
    IF fi-pause LT 5 OR
       fi-pause GT 3600 THEN
    DO:
        pcErrMsg = "Wait Pause valid range is 5 to 999 inclusive".
        LEAVE.
    END.

    IF fi-dir EQ fi-out-dir THEN
    DO:
        pcErrMsg = "Data and Output directories cannot be the same directory".
        LEAVE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

