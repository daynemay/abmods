&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 Character
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
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

CREATE WIDGET-POOL.

&GLOBAL-DEFINE except-tables "audit-header,audit-line,custom-compare,custom-dup,customer-d-snapshot,images,interface-log,mnferror,msglog,oldcalls,olditems,pltxn,route-log,rtaudit,sonic-job-msg,sonic-job-msgdtl,upd-pend,webpage,websession,exhd,exln,proxylog,whse-inv,sapb,x_job,zzrthdr,zzrtdet,smrg,smrgval,x_mlog,biexportlog"
&GLOBAL-DEFINE except-manf "manfhdr,mnf-equip,ibtlnk,bumanf-xref,equipmove,manifest,mnf-item,mnf-costs,trip,trip-lane,drivmove"
&GLOBAL-DEFINE except-ar "ar-open-item,ar-open-tr,cash-jrnl,ar-gl-distr,slspost"
&GLOBAL-DEFINE except-pallets "aemvthd,aemvtref,aemvtln,aevact,aerecln"
&GLOBAL-DEFINE except-cn "probill,ks,ksdet,shiphst,pro-status,pro-po,pro-ibt,pro-xref,proitem,prochrg,bflag,pro-dim,apptinfo,pro-ibt,pro-cost,smtxn,pro-deliv,cost-error"
&GLOBAL-DEFINE except-gl "mnfgl,glhdr,gltxn,old-gltxn"


/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&scoped-define top-box-fields fi-bus,fi-cn-from-date,fi-cn-to-date,fi-out-dir,fi-dump-type,fiLetter, fiSeq, fi-max-count
&scoped-define top-box-fields1 fi-bus fi-cn-from-date fi-cn-to-date fi-out-dir fi-dump-type fiLetter fiSeq fi-max-count

def stream s-out.
def stream s-out2.
def stream s-out3.
def stream s-out4.
def stream s-out5.
def stream s-out6.
def stream s-out7.
DEF STREAM s-out8.
DEF STREAM s-out9.
DEF STREAM s-out10.
DEF STREAM s-out11.
DEF STREAM s-out12.
DEF STREAM s-out13.
DEF STREAM s-out14.
DEF STREAM s-out15.
DEF STREAM s-out16.
DEF STREAM s-out17.
DEF STREAM s-out18.
DEF STREAM s-out19.
DEF STREAM s-out20.
def stream s-log.
DEF STREAM s-gl.


def var v-rowid as rowid no-undo.
def var v-count as int no-undo.
def var v-max-count as int no-undo.
def var v-ok as log no-undo.
def var v-field-nbr as int no-undo.
def var v-extent-nbr as int no-undo.
def var h-field as handle no-undo.
def var v-path as char no-undo.
def var v-dump-name as char no-undo.

def var v-table-name as char no-undo.
def var hq-file as widget-handle.
def var hb-file AS widget-handle.
def var hf-file AS widget-handle.
def var v-qs-file as char no-undo.
def var v-error as int no-undo.
def var v-ks-index as char no-undo.
def var i as int no-undo.
def var v-exported as log init false no-undo.
DEF VAR leexported AS DEC NO-UNDO.
DEF VAR ledumped   AS DEC NO-UNDO.


DEF TEMP-TABLE tt-file NO-UNDO
    FIELD db-name   AS CHAR 
    FIELD file-name AS CHAR FORMAT "x(25)"
    FIELD dump-name AS char FORMAT "x(15)"
    FIELD dump-table AS LOGICAL INIT TRUE
    INDEX idx1 AS PRIMARY file-name.

def var v-handle as handle no-undo.
def buffer b-probill for probill.

DEF TEMP-TABLE tt-trip-id NO-UNDO
    FIELD trip-id LIKE trip.trip-id
    INDEX idx1 AS PRIMARY UNIQUE trip-id.
DEF TEMP-TABLE tt-ar-open-tr NO-UNDO
    FIELD r-rowid AS ROWID
    FIELD document-a LIKE ar-open-tr.document-a
    FIELD ar-entity  LIKE ar-open-tr.ar-entity
    FIELD jrnl-no    LIKE ar-open-tr.jrnl-no
    INDEX idx1 AS PRIMARY UNIQUE r-rowid
    INDEX idx2 document-a
    INDEX idx3 ar-entity jrnl-no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-file

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tt-file.db-name tt-file.FILE-NAME tt-file.dump-name   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-file
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tt-file.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-file
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-file


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-bus AS CHARACTER FORMAT "X(4)":U 
     LABEL "B/U's" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
     &ELSE SIZE 4 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-cn-from-date AS DATE FORMAT "99/99/99":U 
     LABEL "From" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-cn-to-date AS DATE FORMAT "99/99/99":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-dump-type AS CHARACTER FORMAT "X(1)":U INITIAL "C" 
     LABEL "Dump Type" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 1 BY 1
     &ELSE SIZE 1 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-max-count AS INTEGER FORMAT ">>>>>>9":U INITIAL 5000 
     LABEL "Max Count" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fi-out-dir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Output Directory" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 1
     &ELSE SIZE 50 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fiLetter AS CHARACTER FORMAT "X(1)":U INITIAL "A" 
     LABEL "Letter" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 1 BY 1
     &ELSE SIZE 1 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fiSeq AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Sequence" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 1 BY 1
     &ELSE SIZE 1 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tt-file SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 TERMINAL-SIMULATION _FREEFORM
  QUERY BROWSE-3 DISPLAY
      tt-file.db-name
tt-file.FILE-NAME
tt-file.dump-name
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 80 BY 15
        TITLE "Table List" NO-EMPTY-SPACE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 1
          &ELSE AT ROW 1 COL 1 &ENDIF
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

DEFINE FRAME frame-param
     fi-bus
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 7 COLON-ALIGNED
          &ELSE AT ROW 1 COL 7 COLON-ALIGNED &ENDIF
     fi-cn-from-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 51 COLON-ALIGNED
          &ELSE AT ROW 1 COL 51 COLON-ALIGNED &ENDIF
     fi-cn-to-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 66 COLON-ALIGNED
          &ELSE AT ROW 1 COL 66 COLON-ALIGNED &ENDIF
     fi-out-dir
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 17 COLON-ALIGNED
          &ELSE AT ROW 2 COL 17 COLON-ALIGNED &ENDIF
     fi-dump-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 10 COLON-ALIGNED
          &ELSE AT ROW 3 COL 10 COLON-ALIGNED &ENDIF HELP
          "The(L)ot (T)ables (C)onnotes (M)anifests (A)R (P)allets"
     fiLetter
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 24 COLON-ALIGNED
          &ELSE AT ROW 3 COL 24 COLON-ALIGNED &ENDIF HELP
          "Start from Letter"
     fiSeq
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 37 COLON-ALIGNED
          &ELSE AT ROW 3 COL 37 COLON-ALIGNED &ENDIF HELP
          "Sequence to append to dump name when using multiple dumps"
     fi-max-count
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 67 COLON-ALIGNED
          &ELSE AT ROW 3 COL 67 COLON-ALIGNED &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16
         SIZE 80 BY 5
        TITLE "Other parameters".


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
         TITLE              = "Dump Tables"
         HEIGHT             = 21.5
         WIDTH              = 80.43
         MAX-HEIGHT         = 21.5
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 21.5
         VIRTUAL-WIDTH      = 80.43
         RESIZE             = yes
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
/* REPARENT FRAME */
ASSIGN FRAME frame-param:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frame-param:MOVE-AFTER-TAB-ITEM (BROWSE-3:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BROWSE-3 1 DEFAULT-FRAME */
/* SETTINGS FOR FRAME frame-param
   Custom                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-file.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME frame-param
&Scoped-define SELF-NAME fi-out-dir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-out-dir TERMINAL-SIMULATION
ON ENTER OF fi-out-dir IN FRAME frame-param /* Output Directory */
DO:
  apply "leave" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-3
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

ON leave OF {&top-box-fields} in frame frame-param
DO:
  v-ok = false.
  run validate-fields(input self:name, output v-ok).
  if not v-ok then
     return no-apply.

  if entry(num-entries("{&top-box-fields}"),"{&top-box-fields}") = self:name then
  do:
     run export-files.
  end.
END.

ON go of {&top-box-fields} in frame frame-param
DO:
  def var v-err as log no-undo.

  v-err = false.
  v-ok = false.
  do i = 1 to num-entries("{&top-box-fields}"):
    run validate-fields(input entry(i,"{&top-box-fields}"),output v-ok).
    if not v-ok then
    do:
       v-err = true.
       leave.
    end.
  end.

  if v-err = true then
     return no-apply.

  run export-files.

END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  run init.
  RUN enable_UI.
  assign fi-bus:screen-value in frame frame-param = "*"
         fi-cn-from-date:screen-value in frame frame-param = string(today)
         fi-cn-to-date:screen-value in frame frame-param = string(today)
         fi-out-dir:screen-value in frame frame-param = "../tmp/"
  .

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF this-procedure .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildDumpFileList TERMINAL-SIMULATION 
PROCEDURE BuildDumpFileList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tt-file.
   FOR EACH fmscont._file NO-LOCK
       WHERE fmscont._file._tbl-type = "T" :
       CREATE tt-file.
       ASSIGN tt-file.FILE-NAME = fmscont._file._file-name
              tt-file.dump-name = fmscont._file._dump-name
              tt-file.db-name   = "fmscont".
   END.
   FOR EACH fmsbigt._file NO-LOCK
       WHERE fmsbigt._file._tbl-type = "T" :
       CREATE tt-file.
       ASSIGN tt-file.FILE-NAME = fmsbigt._file._file-name
              tt-file.dump-name = fmsbigt._file._dump-name
              tt-file.db-name   = "fmsbigt".
   END.
   FOR EACH shrstat._file NO-LOCK
       WHERE shrstat._file._tbl-type = "T":
       CREATE tt-file.
       ASSIGN tt-file.FILE-NAME = shrstat._file._file-name
              tt-file.dump-name = shrstat._file._dump-name
              tt-file.db-name   = "shrstat".
   END.
   FOR EACH gemini._file NO-LOCK
       WHERE gemini._file._tbl-type = "T":
       CREATE tt-file.
       ASSIGN tt-file.FILE-NAME = gemini._file._file-name
              tt-file.dump-name = gemini._file._dump-name
              tt-file.db-name   = "gemini".
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-export-seq TERMINAL-SIMULATION 
PROCEDURE create-export-seq :
def input parameter p-ldbname as char no-undo.
def input parameter p-dump-name as char no-undo.
def input parameter p-dir as char no-undo.
def input parameter p-max-count as int no-undo.

output stream s-out to export-seq.p unbuffered.

put stream s-out unformatted "def stream s-out." skip.
put stream s-out unformatted "def stream s-log." skip.

put stream s-out unformatted "output stream s-log to " + p-dir + "dump-tables.log unbuffered append." skip.
put stream s-out unformatted "output stream s-out to " + p-dir + p-dump-name + "-" +
 p-ldbname + ".d." skip.
put stream s-out unformatted "def var i as int no-undo." skip.

put stream s-out unformatted "for each " + p-ldbname + "._sequence no-lock:" skip.
put stream s-out unformatted 'export stream s-out ' + p-ldbname + '._sequence._Seq-Num' + 
' ' + p-ldbname + '._sequence._Seq-Name' + ' dynamic-current-value(' +
p-ldbname + '._sequence._Seq-Name,' + '"' + p-ldbname '"' + ').' skip.
put stream s-out unformatted "i = i + 1." skip.
put stream s-out unformatted "if i mod " + string(p-max-count) + " = 0 then do:" skip.

put stream s-out unformatted 'put screen row 22 col 1 string(today) + ' + '" " +
  string(time,"hh:mm:ss") ' + ' + " ' + p-ldbname + ' " + string(i) + "                   ".' skip.

put stream s-out unformatted 'put stream s-log unformatted string(today) + ' + '" " +
 string(time,"hh:mm:ss") ' + ' + " ' + p-ldbname + ' " + string(i) skip.' skip.

put stream s-out unformatted "end." skip.
put stream s-out unformatted "end." skip.

put stream s-out unformatted 'put screen row 22 col 1 string(today) + ' + '" " +
 string(time,"hh:mm:ss") ' + ' + " ' + p-ldbname + ' " + string(i) + "                    ".' skip.

put stream s-out unformatted 'put stream s-log unformatted string(today) + ' + '" " +
 string(time,"hh:mm:ss") ' + ' + " ' + p-ldbname + ' " + string(i) skip.' skip.

put stream s-out unformatted "output stream s-out close." skip.
put stream s-out unformatted "output stream s-log close." skip.

output stream s-out close.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-export-table TERMINAL-SIMULATION 
PROCEDURE create-export-table :
def input parameter p-table-name as char no-undo.
def input parameter p-dump-name as char no-undo.
def input parameter p-dir as char no-undo.
def input parameter p-max-count as int no-undo.
DEF INPUT PARAM piFileCount AS INT NO-UNDO.

output stream s-out to export-table.p.

put stream s-out unformatted "def stream s-out." skip.
put stream s-out unformatted "def stream s-log." skip.
put stream s-out unformatted "output stream s-log to " + p-dir + "dump-tables.log unbuffered append." skip.
put stream s-out unformatted "output stream s-out to " + p-dir + p-dump-name + ".d." skip.

put stream s-out unformatted "def var i as int no-undo." skip.

PUT STREAM s-out UNFORMATTED "put stream s-log unformatt 'File Name: " p-table-name "' skip." SKIP. 
put stream s-out unformatted "for each " + p-table-name + " no-lock:" skip.
put stream s-out unformatted "export stream s-out " + p-table-name + "." skip.
put stream s-out unformatted "i = i + 1." skip.
put stream s-out unformatted "if i mod " + string(p-max-count) + " = 0 then do:" skip.
put stream s-out unformatted 'put screen row 22 col 1 string(today) + " " +
 string(time,"hh:mm:ss") + " 'p-table-name + ' " + string(i) + "                    ".' skip.
put stream s-out unformatted 'put stream s-log unformatted string(today) + " " +
 string(time,"hh:mm:ss") + " 'p-table-name + ' " + string(i) skip.' skip.
put stream s-out unformatted "end." skip.
put stream s-out unformatted "end." skip.
put stream s-out unformatted 'put screen row 22 col 1 string(today) + " " +
 string(time,"hh:mm:ss") + " 'p-table-name + ' " + string(i) + "                    ".' skip.
put stream s-out unformatted 'put stream s-log unformatted string(today) + " " +
 string(time,"hh:mm:ss") + " 'p-table-name + ' " + string(i) skip.' skip.
put stream s-out unformatted "output stream s-out close." skip.
put stream s-out unformatted "output stream s-log close." skip.

output stream s-out close.

/* this pause is important to allow the disk to catch up and write everything */
PAUSE 1 NO-MESSAGE.

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
  HIDE FRAME frame-param.
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
  ENABLE BROWSE-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-bus fi-cn-from-date fi-cn-to-date fi-out-dir fi-dump-type fiLetter 
          fiSeq fi-max-count 
      WITH FRAME frame-param IN WINDOW TERMINAL-SIMULATION.
  ENABLE fi-bus fi-cn-from-date fi-cn-to-date fi-out-dir fi-dump-type fiLetter 
         fiSeq fi-max-count 
      WITH FRAME frame-param IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-frame-param}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-ar TERMINAL-SIMULATION 
PROCEDURE export-ar :
/* output ar-open-item s-out */
RUN get-dump-name(INPUT "ar-open-item"). 
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out TO VALUE (v-path).
/* output ar-open-tr s-out2 */
RUN get-dump-name(INPUT "ar-open-tr").
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out2 TO VALUE (v-path).
/* output cash-jrnl s-out3 */
RUN get-dump-name(INPUT "cash-jrnl").
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out3 TO VALUE (v-path).
/* output ar-gl-distr s-out4 */
RUN get-dump-name(INPUT "ar-gl-distr").
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out4 TO VALUE (v-path).
/* output slspost s-out5 */
RUN get-dump-name(INPUT "slspost").
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out5 TO VALUE (v-path).

output stream s-log to value(fi-out-dir + "dump-tables.log") unbuffered append.

ledumped = 0.

FOR EACH ar-open-item NO-LOCK
   WHERE ar-open-item.activefl:

  ledumped = ledumped + 1.
  EXPORT STREAM s-out ar-open-item.

  FOR EACH ar-open-tr NO-LOCK
      WHERE ar-open-tr.Ar-entity  EQ ar-open-item.Ar-entity
        AND ar-open-tr.Cust-no    EQ ar-open-item.Cust-no
        AND ar-open-tr.Reference  EQ ar-open-item.Reference
        AND ar-open-tr.Ar-account EQ ar-open-item.Ar-account
        AND ar-open-tr.busunit    EQ ar-open-item.busunit:

     FIND FIRST tt-ar-open-tr NO-LOCK
         WHERE tt-ar-open-tr.r-rowid EQ ROWID(ar-open-tr) NO-ERROR.
     IF NOT AVAIL tt-ar-open-tr THEN
     DO:
         CREATE tt-ar-open-tr.
         ASSIGN tt-ar-open-tr.r-rowid = ROWID(ar-open-tr)
                tt-ar-open-tr.document-a = ar-open-tr.document-a
                tt-ar-open-tr.ar-entity  = ar-open-tr.ar-entity
                tt-ar-open-tr.jrnl-no    = ar-open-tr.jrnl-no.
         EXPORT STREAM s-out2 ar-open-tr.
    
     END.
  END.

  IF ledumped MOD 1000 EQ 0 THEN
  DO:
      PUT SCREEN ROW 22 COL 1 STRING(TODAY) + " " +
          STRING(TIME,"hh:mm:ss") + " Exported AR1 " + STRING(ledumped) +
           "                    ".
      PUT STREAM s-log UNFORMATTED STRING(TODAY) + " " +
          STRING(TIME,"hh:mm:ss") + " Exported AR1 " + STRING(ledumped) SKIP.
  END.

END.

ledumped = 0.
FOR EACH tt-ar-open-tr
    BREAK BY tt-ar-open-tr.document-a:
    IF  FIRST-OF(tt-ar-open-tr.document-a) THEN
    DO:
        FOR EACH cash-jrnl NO-LOCK
            WHERE cash-jrnl.document-a = tt-ar-open-tr.document-a:
            EXPORT STREAM s-out3 cash-jrnl.
        END.
    END.
    ledumped = ledumped + 1.

    IF ledumped MOD 1000 EQ 0 THEN
    DO:
        PUT SCREEN ROW 22 COL 1 STRING(TODAY) + " " +
            STRING(TIME,"hh:mm:ss") + " Exported AR2 " + STRING(ledumped) +
             "                    ".
        PUT STREAM s-log UNFORMATTED STRING(TODAY) + " " +
            STRING(TIME,"hh:mm:ss") + " Exported AR2 " + STRING(ledumped) SKIP.
    END.
END.

ledumped = 0.
FOR EACH tt-ar-open-tr
    BREAK BY tt-ar-open-tr.ar-entity
          BY tt-ar-open-tr.jrnl-no:
    IF  FIRST-OF(tt-ar-open-tr.ar-entity) OR
        FIRST-OF(tt-ar-open-tr.jrnl-no) THEN
    DO:
        ledumped = ledumped + 1.
        FOR EACH ar-gl-distr NO-LOCK
            WHERE ar-gl-distr.ar-entity EQ tt-ar-open-tr.ar-entity
              AND ar-gl-distr.jrnl-no   EQ tt-ar-open-tr.jrnl-no:
            EXPORT STREAM s-out4 ar-gl-distr.
        END.
        FOR EACH slspost NO-LOCK
            WHERE slspost.ar-entity EQ tt-ar-open-tr.ar-entity
              AND slspost.jrnl-no   EQ tt-ar-open-tr.jrnl-no:
            EXPORT STREAM s-out5 slspost.
        END.

        IF ledumped MOD 1000 EQ 0 THEN
        DO:
            PUT SCREEN ROW 22 COL 1 STRING(TODAY) + " " +
                STRING(TIME,"hh:mm:ss") + " Exported AR3 " + STRING(ledumped) +
                 "                    ".
            PUT STREAM s-log UNFORMATTED STRING(TODAY) + " " +
                STRING(TIME,"hh:mm:ss") + " Exported AR3 " + STRING(ledumped) SKIP.
        END.
    END.

END.

OUTPUT STREAM s-out  CLOSE.
OUTPUT STREAM s-out2 CLOSE.
OUTPUT STREAM s-out3 CLOSE.
OUTPUT STREAM s-out4 CLOSE.
OUTPUT STREAM s-out5 CLOSE.

OUTPUT STREAM s-log CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-by-pronumb TERMINAL-SIMULATION 
PROCEDURE export-by-pronumb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter p-pronumb like probill.pronumb no-undo.


/*    def buffer b1-probill for probill.        */
/*                                              */
/*    find first b1-probill no-lock             */
/*         where b1-probill.pronumb = p-pronumb */
/*           and b1-probill.trn = 0             */
/*         no-error.                            */
/*                                              */
/*    if not avail b1-probill then return.      */

    /* export by pronumb */
    
/* dump shiphst */
FOR EACH shiphst NO-LOCK
    WHERE shiphst.pronumb EQ p-pronumb:
    EXPORT STREAM s-out6 shiphst.
END.

/* dump pro-status */
FOR EACH pro-status NO-LOCK
    WHERE pro-status.pronumb EQ p-pronumb:
    EXPORT STREAM s-out7 pro-status.
END.
    
/* dump pro-po */
FOR EACH pro-po NO-LOCK
    WHERE pro-po.pronumb EQ p-pronumb:
    EXPORT STREAM s-out8 pro-po.
END.
    
/* dump pro-ibt */
FOR EACH pro-ibt NO-LOCK
    WHERE pro-ibt.pronumb EQ p-pronumb:
    EXPORT STREAM s-out9 pro-ibt.
END.
    
/* dump pro-xref */
FOR EACH pro-xref NO-LOCK
    WHERE pro-xref.pronumb EQ p-pronumb:
    EXPORT STREAM s-out10 pro-xref.
END.
    
/* dump proitem */
FOR EACH proitem NO-LOCK
    WHERE proitem.pronumb EQ p-pronumb:
    EXPORT STREAM s-out11 proitem.
END.
    
/* dump prochrg */
FOR EACH prochrg NO-LOCK
    WHERE prochrg.pronumb EQ p-pronumb:
    EXPORT STREAM s-out12 prochrg.
END.
    
/* bflag dump */
FOR EACH bflag NO-LOCK
    WHERE bflag.pronumb EQ p-pronumb:
    EXPORT STREAM s-out13 bflag.
END.
    
/* pro-dim dump */
FOR EACH pro-dim NO-LOCK
    WHERE pro-dim.pronumb EQ p-pronumb:
    EXPORT STREAM s-out14 pro-dim.
END.

/* apptinfo dump */
FOR EACH apptinfo NO-LOCK
    WHERE apptinfo.pronumb EQ p-pronumb:
    EXPORT STREAM s-out15 apptinfo.
END.

/* pro-cost dump */
FOR EACH pro-cost NO-LOCK
    WHERE pro-cost.pronumb EQ p-pronumb:
    EXPORT STREAM s-out17 pro-cost.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-connotes TERMINAL-SIMULATION 
PROCEDURE export-connotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* dump by connotes */

RUN get-dump-name(INPUT "cost-error").
IF RETURN-VALUE <> "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out2 TO VALUE (v-path).

run get-dump-name(input "probill").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out3 TO VALUE (v-path).

run get-dump-name(input "ks").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out4 TO VALUE (v-path).

run get-dump-name(input "ksdet").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out5 TO VALUE (v-path).

RUN get-dump-name(INPUT "shiphst").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out6 TO VALUE (v-path).

RUN get-dump-name(INPUT "pro-status").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out7 TO VALUE (v-path).

/*   */
RUN get-dump-name(INPUT "pro-po").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out8 TO VALUE (v-path).

RUN get-dump-name(INPUT "pro-ibt").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out9 TO VALUE (v-path).

RUN get-dump-name(INPUT "pro-xref").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out10 TO VALUE (v-path).

RUN get-dump-name(INPUT "proitem").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out11 TO VALUE (v-path).

RUN get-dump-name(INPUT "prochrg").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out12 TO VALUE (v-path).

RUN get-dump-name(INPUT "bflag").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out13 TO VALUE (v-path).

RUN get-dump-name(INPUT "pro-dim").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out14 TO VALUE (v-path).

RUN get-dump-name(INPUT "apptinfo").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out15 TO VALUE (v-path).

RUN get-dump-name(INPUT "pro-cost").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out17 TO VALUE (v-path).

RUN get-dump-name(INPUT "smtxn").
if return-value <> "" then return.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out18 TO VALUE (v-path).

output stream s-log to value(fi-out-dir + "dump-tables" + STRING(fiSeq,"9") + ".log") unbuffered append.

RUN get-dump-name(INPUT "pro-deliv").
IF RETURN-VALUE <> "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out TO VALUE (v-path).

FOR EACH pro-deliv NO-LOCK
    where pro-deliv.pod-date >= fi-cn-from-date
      AND pro-deliv.pod-date <= fi-cn-to-date:
    EXPORT STREAM s-out pro-deliv.
END.
OUTPUT STREAM s-out  CLOSE.

for each zzbu no-lock
    where can-do(fi-bus,zzbu.busunit),
    each probill no-lock
        where probill.busunit = zzbu.busunit
          and probill.pro-date >= fi-cn-from-date
          and probill.pro-date <= fi-cn-to-date
          and probill.trn = 0
          and probill.trntyp = "":

    IF probill.pronumb EQ 0 THEN NEXT.
 
    run export-by-pronumb(input probill.pronumb).
    /* export by pronumb,trn */

    for each b-probill no-lock
        where b-probill.pronumb = probill.pronumb:

        /* dump smtxn */
        for each smtxn no-lock
           where smtxn.busunit = probill.busunit
             and smtxn.entity = probill.rev-term-code
             and smtxn.pronumb = probill.pronumb:
            EXPORT STREAM s-out18 smtxn.
        end.

        FOR EACH cost-error NO-LOCK
            where cost-error.pronumb = b-probill.pronumb
              AND cost-error.trn     = b-probill.trn:
            EXPORT STREAM s-out2 cost-error.
        END.

        EXPORT STREAM s-out3 b-probill.

        v-ks-index = "PROBILL|" + string(b-probill.pronumb) + "," + string(b-probill.trn).
        find first ks no-lock
             where ks.ks-index = v-ks-index
             no-error.
        if avail ks then
        do:
           EXPORT STREAM s-out4 ks.

           for each ksdet no-lock
               where ksdet.ks-index = v-ks-index:
               EXPORT STREAM s-out5 ksdet.
           end.
        end.

        leexported = leexported + 1.
        IF leexported MOD 1000 EQ 0 THEN
        DO:
            put screen row 22 col 1 string(today) + " " +
                string(time,"hh:mm:ss") + " Exported Pronumb " + string(leexported) +
                 "                    ".
            put STREAM s-log unformatt string(today) + " " +
                string(time,"hh:mm:ss") + " Exported Pronumb " + string(leexported) SKIP.

        END.
    end. /* end for each b-probill */
end.

OUTPUT STREAM s-out2 CLOSE.
OUTPUT STREAM s-out3 CLOSE.
OUTPUT STREAM s-out4 CLOSE.
OUTPUT STREAM s-out5 CLOSE.
OUTPUT STREAM s-out6 CLOSE.
OUTPUT STREAM s-out7 CLOSE.
OUTPUT STREAM s-out8 CLOSE.
OUTPUT STREAM s-out9 CLOSE.
OUTPUT STREAM s-out10 CLOSE.
OUTPUT STREAM s-out11 CLOSE.
OUTPUT STREAM s-out12 CLOSE.
OUTPUT STREAM s-out13 CLOSE.
OUTPUT STREAM s-out14 CLOSE.
OUTPUT STREAM s-out15 CLOSE.
OUTPUT STREAM s-out17 CLOSE.
OUTPUT STREAM s-out18 CLOSE.

output stream s-log close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-files TERMINAL-SIMULATION 
PROCEDURE export-files :
DEF VAR liInt AS INT NO-UNDO.
DEF VAR liFileCount AS INT NO-UNDO.
    
do with frame frame-param:

   ASSIGN fi-dump-type fiLetter fi-out-dir fi-bus fi-cn-from-date fi-cn-to-date fiSeq.

   output stream s-log to value(fi-out-dir + "dump-tables.log") unbuffered append.
   put stream s-log unformatted string(today) + " " + string(time,"hh:mm:ss") + " Dump Started" skip.
   output stream s-log close.

    /* dump by tables */
    if fi-dump-type = "L" or
       fi-dump-type = "T" then
    do:
        FOR EACH tt-file NO-LOCK
            WHERE tt-file.FILE-NAME >= fiLetter
              AND tt-file.dump-table = TRUE:
            ASSIGN liFileCount = liFileCount + 1.
        END.
        
        output stream s-log to value(fi-out-dir + "dump-tables.log") unbuffered append.
        PUT STREAM s-log UNFORMATT "Total No Tables: " liFileCount SKIP.
        OUTPUT STREAM s-log CLOSE.

        ASSIGN liFileCount = 0.
        for each tt-file no-lock
            WHERE tt-file.FILE-NAME >= fiLetter
              AND tt-file.dump-table = TRUE
            BY tt-file.FILE-NAME:
            ASSIGN liFileCount = liFileCount + 1.
            OUTPUT STREAM s-log TO VALUE(fi-out-dir + "dump-count.log") UNBUFFERED APPEND.
            PUT STREAM s-log UNFORMATT "File Name: " tt-file.FILE-NAME " - Count: " liFileCount SKIP.
            OUTPUT STREAM s-log CLOSE.

            run create-export-table(input tt-file.file-name,
                                    input tt-file.dump-name,
                                    input fi-out-dir,
                                    input integer(fi-max-count),
                                    INPUT liFileCount).
            run export-table.p.
            os-delete export-table.p.
        end.

        /* dump specific tables */

        /* smrg and smrgval */
        /* only dump texp for smrg */

        put screen row 22 col 1 string(today) + " " + string(time,"hh:mm:ss") + " - smrg".

        RUN get-dump-name(INPUT "smrg").
        IF RETURN-VALUE <> "" THEN RETURN.
        v-path = fi-out-dir + v-dump-name + ".d".
        OUTPUT STREAM s-out TO VALUE (v-path).

        RUN get-dump-name(INPUT "smrgval").
        IF RETURN-VALUE <> "" THEN RETURN.
        v-path = fi-out-dir + v-dump-name + ".d".
        OUTPUT STREAM s-out2 TO VALUE (v-path).

        FOR EACH smrg NO-LOCK
            WHERE smrg.busunit = "TEXP":
            EXPORT STREAM s-out smrg.
            FOR EACH smrgval NO-LOCK
                WHERE smrgval.smrgid = smrg.smrgid:
                EXPORT STREAM s-out2 smrgval.
            END.
        END.
        OUTPUT STREAM s-out CLOSE.
        OUTPUT STREAM s-out2 CLOSE.

        /* sapb */
        put screen row 22 col 1 string(today) + " " + string(time,"hh:mm:ss") + " - sapb".
        RUN get-dump-name(INPUT "sapb").
        IF RETURN-VALUE <> "" THEN RETURN.
        v-path = fi-out-dir + v-dump-name + ".d".
        OUTPUT STREAM s-out TO VALUE (v-path).
        for each sapb no-lock:
            assign liInt = int(sapb.reportnm) no-error.
            if  error-status:error then
                export STREAM s-out sapb.
        end.
        OUTPUT STREAM s-out CLOSE.

        /* x_job */
        put screen row 22 col 1 string(today) + " " + string(time,"hh:mm:ss") + " - x_job".
        RUN get-dump-name(INPUT "x_job").
        IF RETURN-VALUE <> "" THEN RETURN.
        v-path = fi-out-dir + v-dump-name + ".d".
        OUTPUT STREAM s-out TO VALUE (v-path).
        for each x_job where jb_runtype = "R" no-lock:
            export STREAM s-out x_job.
        end.
        OUTPUT STREAM s-out CLOSE.

        /* rates */
        put screen row 22 col 1 string(today) + " " + string(time,"hh:mm:ss") + " - zzrthdr".
        RUN get-dump-name(INPUT "zzrthdr").
        IF RETURN-VALUE <> "" THEN RETURN.
        v-path = fi-out-dir + v-dump-name + ".d".
        OUTPUT STREAM s-out TO VALUE (v-path).

        RUN get-dump-name(INPUT "zzrtdet").
        IF RETURN-VALUE <> "" THEN RETURN.
        v-path = fi-out-dir + v-dump-name + ".d".
        OUTPUT STREAM s-out2 TO VALUE (v-path).

        for each zzrthdr no-lock
            where zzrthdr.end-date >= fi-cn-from-date:
            export stream s-out zzrthdr.
        
            for each zzrtdet of zzrthdr NO-LOCK:
                export stream s-out2 zzrtdet.
            end.
        end.
        OUTPUT STREAM s-out CLOSE.
        OUTPUT STREAM s-out2 CLOSE.

        /* dump _user */
        run create-export-table(input "_user",
                                input "_user",
                                input fi-out-dir,
                                input integer(fi-max-count)).
        run export-table.p.
        os-delete export-table.p.
        
        /* dump sequence */
        do i = 1 to num-dbs:
           run create-export-seq(input ldbname(i),
                                   input "_seqvals",
                                   input fi-out-dir,
                                   input integer(fi-max-count)).
           run export-seq.p.
           os-delete export-seq.p.
        end.
    end.

    IF  CAN-DO("L,C",fi-dump-type) THEN
        RUN export-connotes IN THIS-PROCEDURE.
    ELSE IF CAN-DO("L,M",fi-dump-type) THEN
        RUN export-manifests IN THIS-PROCEDURE.
    ELSE IF CAN-DO("L,A",fi-dump-type) THEN
        RUN export-ar IN THIS-PROCEDURE.
    ELSE IF CAN-DO("L,P",fi-dump-type) THEN
        RUN export-pallets IN THIS-PROCEDURE.
  
    output stream s-log to value(fi-out-dir + "dump-tables.log") unbuffered append.
    put stream s-log unformatted string(today) + " " + string(time,"hh:mm:ss") + " Dump Completed" skip.
    output stream s-log close.
    MESSAGE "Dump Completed " VIEW-AS ALERT-BOX TITLE "DEBUG".

    return.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-gl TERMINAL-SIMULATION 
PROCEDURE export-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM piseq-no   AS INT NO-UNDO.
    DEF INPUT PARAM pitrip-no  AS INT NO-UNDO.

    FOR EACH mnfgl NO-LOCK
        WHERE mnfgl.seq-no EQ piseq-no:
        EXPORT STREAM s-out13 mnfgl.
    END.

    FOR EACH glhdr NO-LOCK
        WHERE glhdr.seq-no EQ piseq-no:
        EXPORT STREAM s-out14 glhdr.
    END.

    FOR EACH old-gltxn NO-LOCK
        WHERE old-gltxn.seq-no EQ piseq-no:
        EXPORT STREAM s-out15 old-gltxn.
    END.

    FOR EACH gltxn NO-LOCK
        WHERE gltxn.seq-no EQ piseq-no:
        EXPORT STREAM s-out16 gltxn.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-manifests TERMINAL-SIMULATION 
PROCEDURE export-manifests :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lltripped   AS LOG NO-UNDO.

    ledumped = 0.
    EMPTY TEMP-TABLE tt-trip-id.

    run get-dump-name(input "manfhdr").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out TO VALUE (v-path).

    /* dump manifest info */
    run get-dump-name(input "manifest").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out2 TO VALUE (v-path).
    /* dump mnf-item info */
    run get-dump-name(input "mnf-item").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out3 TO VALUE (v-path).
    /* dump mnf-costs info */
    run get-dump-name(input "mnf-costs").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out4 TO VALUE (v-path).
    /* dump trip info */
    run get-dump-name(input "trip").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out5 TO VALUE (v-path).
    /* dump trip-lane info */
    run get-dump-name(input "trip-lane").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out6 TO VALUE (v-path).
    /* dump drivmove info */
    run get-dump-name(input "drivmove").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out7 TO VALUE (v-path).

    run get-dump-name(input "mnf-equip").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out8 TO VALUE (v-path).

    run get-dump-name(input "ibtlnk").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out9 TO VALUE (v-path).

    run get-dump-name(input "bumanf-xref").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out10 TO VALUE (v-path).

    run get-dump-name(input "equipmove").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out11 TO VALUE (v-path).


    run get-dump-name(input "mnfgl").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out13 TO VALUE (v-path).

    run get-dump-name(input "glhdr").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out14 TO VALUE (v-path).

    run get-dump-name(input "old-gltxn").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out15 TO VALUE (v-path).

    run get-dump-name(input "gltxn").
    if return-value <> "" then return.
    v-path = fi-out-dir + v-dump-name + ".d".
    OUTPUT STREAM s-out16 TO VALUE (v-path).

    output stream s-log to value(fi-out-dir + "dump-tables" + STRING(fiSeq,"9") + ".log") unbuffered append.

    FOR EACH manfhdr NO-LOCK
        WHERE manfhdr.dept-date GE fi-cn-from-date
          AND manfhdr.dept-date LE fi-cn-to-date
          AND CAN-DO(fi-bus,manfhdr.busunit):

        lltripped = NO.
        ledumped = ledumped + 1.

        EXPORT STREAM s-out manfhdr.

        /* manf equip */
        FOR EACH mnf-equip NO-LOCK
            WHERE mnf-equip.manifest-id EQ manfhdr.manifest-id:
            EXPORT STREAM s-out8 mnf-equip.
        END.

        /* dump ibtlnk info */
        FOR EACH ibtlnk NO-LOCK
            WHERE ibtlnk.manifest-id EQ manfhdr.manifest-id:
            EXPORT STREAM s-out9 ibtlnk.
        END.

        /* dump bumanf-xref info */
        FOR EACH bumanf-xref NO-LOCK
            WHERE bumanf-xref.busunit EQ manfhdr.busunit
              AND bumanf-xref.manf-num EQ manfhdr.manf-num:
            EXPORT STREAM s-out10 bumanf-xref.
        END.

        /* dump equipmove info */
        FOR EACH equipmove NO-LOCK
            WHERE equipmove.manifest-id EQ manfhdr.manifest-id:
            EXPORT STREAM s-out11 equipmove.
        END.

        /* all manifest info */
        FOR EACH manifest NO-LOCK
            WHERE manifest.manifest-id EQ manfhdr.manifest-id:
            IF manifest.trip-id GT 1 THEN
                lltripped = TRUE.

            EXPORT STREAM s-out2 manifest.

            FOR EACH mnf-item NO-LOCK
                WHERE mnf-item.busunit EQ manifest.busunit
                  AND mnf-item.manf-num EQ manifest.manf-num
                  AND mnf-item.load-no  EQ manifest.load-no:
                EXPORT STREAM s-out3 mnf-item.
            END.

            FOR EACH mnf-costs NO-LOCK
                WHERE mnf-costs.manf-num EQ manifest.manf-num
                  AND mnf-costs.busunit  EQ manifest.busunit
                  AND mnf-costs.load-no  EQ manifest.load-no:
                EXPORT STREAM s-out4 mnf-costs.
                RUN export-gl IN THIS-PROCEDURE
                    (INPUT mnf-costs.seq-no,
                     INPUT manifest.trip-no).
            END.
            
            IF manifest.trip-id GT 1 THEN
            DO:
                IF NOT CAN-FIND(FIRST tt-trip-id
                                WHERE tt-trip-id.trip-id EQ manifest.trip-id) THEN
                DO:
                    CREATE tt-trip-id.
                    ASSIGN tt-trip-id.trip-id = manifest.trip-id.


                    FIND FIRST trip NO-LOCK
                        WHERE trip.trip-id EQ manifest.trip-id NO-ERROR.
                    IF AVAIL trip THEN
                    DO:
                        EXPORT STREAM s-out5 trip.

                        FOR EACH trip-lane NO-LOCK
                            WHERE trip-lane.trip-id EQ trip.trip-id:
                            EXPORT STREAM s-out6 trip-lane.
                        END.

                        FOR EACH drivmove NO-LOCK
                            WHERE drivmove.trip-id EQ trip.trip-id:
                            EXPORT STREAM s-out7 drivmove.
                        END.

                    END.
                END.
            END.
        END.
        IF ledumped MOD 1000 EQ 0 THEN
        DO:
            put screen row 22 col 1 string(today) + " " +
                string(time,"hh:mm:ss") + " Exported Manifest " + string(ledumped) +
                 "                    ".
            put stream s-log unformatted string(today) + " " +
                string(time,"hh:mm:ss") + " Exported Manifest " + string(ledumped) skip.
        END.

    END.

    ledumped = 0.
    FOR EACH zzbu NO-LOCK
        WHERE CAN-DO(fi-bus,zzbu.busunit),
        EACH trip NO-LOCK
        WHERE trip.busunit    EQ zzbu.busunit
          AND trip.begin-date GE fi-cn-from-date
          AND trip.begin-date LE fi-cn-to-date:

        IF NOT CAN-FIND(FIRST tt-trip-id
                        WHERE tt-trip-id.trip-id EQ trip.trip-id) THEN
        DO:
            ledumped = ledumped + 1.

            CREATE tt-trip-id.
            ASSIGN tt-trip-id.trip-id = trip.trip-id.

            EXPORT STREAM s-out5 trip.

            FOR EACH trip-lane NO-LOCK
                WHERE trip-lane.trip-id EQ trip.trip-id:
                EXPORT STREAM s-out6 trip-lane.
            END.

            FOR EACH drivmove NO-LOCK
                WHERE drivmove.trip-id EQ trip.trip-id:
                EXPORT STREAM s-out7 drivmove.
            END.

            FOR EACH mnf-costs NO-LOCK
                WHERE mnf-costs.trip-no  EQ trip.trip-no
                  AND mnf-costs.busunit  EQ trip.busunit:
                RUN export-gl IN THIS-PROCEDURE
                    (INPUT mnf-costs.seq-no,
                     INPUT mnf-costs.trip-no).
            END.

        END.
        IF ledumped MOD 1000 EQ 0 THEN
        DO:
            put screen row 22 col 1 string(today) + " " +
                string(time,"hh:mm:ss") + " Exported Trip " + string(ledumped) +
                 "                    ".
            put stream s-log unformatted string(today) + " " +
                string(time,"hh:mm:ss") + " Exported Trip " + string(ledumped) skip.
        END.
    END.

    OUTPUT STREAM s-out CLOSE.
    OUTPUT STREAM s-out2 CLOSE.
    OUTPUT STREAM s-out3 CLOSE.
    OUTPUT STREAM s-out4 CLOSE.
    OUTPUT STREAM s-out5 CLOSE.
    OUTPUT STREAM s-out6 CLOSE.
    OUTPUT STREAM s-out7 CLOSE.
    OUTPUT STREAM s-out8 CLOSE.
    OUTPUT STREAM s-out9 CLOSE.
    OUTPUT STREAM s-out10 CLOSE.
    OUTPUT STREAM s-out11 CLOSE.

    OUTPUT STREAM s-out13 CLOSE.
    OUTPUT STREAM s-out14 CLOSE.
    OUTPUT STREAM s-out15 CLOSE.
    OUTPUT STREAM s-out16 CLOSE.

    OUTPUT STREAM s-log CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-pallets TERMINAL-SIMULATION 
PROCEDURE export-pallets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* output aemvthd s-out */
RUN get-dump-name(INPUT "aemvthd"). 
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out TO VALUE (v-path).
/* output aemvtref s-out2 */
RUN get-dump-name(INPUT "aemvtref"). 
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out2 TO VALUE (v-path).
/* stream for aemvtln s-out3 */
RUN get-dump-name(INPUT "aemvtln"). 
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out3 TO VALUE (v-path).
/* stream for aevact s-out4 */
RUN get-dump-name(INPUT "aevact"). 
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out4 TO VALUE (v-path).
/* stream for aerecln s-out5 */
RUN get-dump-name(INPUT "aerecln"). 
IF RETURN-VALUE NE "" THEN RETURN.
v-path = fi-out-dir + v-dump-name + ".d".
OUTPUT STREAM s-out5 TO VALUE (v-path).

output stream s-log to value(fi-out-dir + "dump-tables" + STRING(fiSeq,"9") + ".log") unbuffered append.

ledumped = 0.

FOR EACH aemvthd NO-LOCK
    WHERE aemvthd.mvtdt GE fi-cn-from-date
      AND aemvthd.mvtdt LE fi-cn-to-date:

    EXPORT STREAM s-out aemvthd.
    ledumped = ledumped + 1.

    FOR EACH aemvtref NO-LOCK
        WHERE aemvtref.aemvthdid EQ aemvthd.aemvthdid:
        EXPORT STREAM s-out2 aemvtref.
    END.

    FOR EACH aemvtln NO-LOCK
        WHERE aemvtln.aemvthdid EQ aemvthd.aemvthdid:

        EXPORT STREAM s-out3 aemvtln.
        FIND FIRST aevact NO-LOCK
            WHERE aevact.aemvtlnid EQ aemvtln.aemvtlnid NO-ERROR.
        IF AVAIL aevact THEN
        DO:
            EXPORT STREAM s-out4 aevact.
            FOR EACH aerecln NO-LOCK
                WHERE aerecln.aevactid EQ aevact.aevactid:
                EXPORT STREAM s-out5 aerecln.
            END.
        END.
    END.
    IF ledumped MOD 1000 EQ 0 THEN
    DO:
        PUT SCREEN ROW 22 COL 1 STRING(TODAY) + " " +
            STRING(TIME,"hh:mm:ss") + " Exported Pallets " + STRING(ledumped) +
             "                    ".
        PUT STREAM s-log UNFORMATTED STRING(TODAY) + " " +
            STRING(TIME,"hh:mm:ss") + " Exported Pallet " + STRING(ledumped) skip.
    END.
    
END.

OUTPUT STREAM s-out  CLOSE.
OUTPUT STREAM s-out2 CLOSE.
OUTPUT STREAM s-out3 CLOSE.
OUTPUT STREAM s-out4 CLOSE.
OUTPUT STREAM s-out5 CLOSE.
OUTPUT STREAM s-log CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-dump-name TERMINAL-SIMULATION 
PROCEDURE get-dump-name :
def input parameter p-table-name as char no-undo.

FIND FIRST tt-file NO-LOCK
    WHERE tt-file.file-name EQ p-table-name NO-ERROR.

if not avail tt-file then
do:
    output stream s-log to value(fi-out-dir + "dump-tables.log") unbuffered append.
    put stream s-log unformatted p-table-name + " missing from table list" skip.
    OUTPUT STREAM s-log CLOSE.
    return p-table-name + " missing from table list".
end.

v-dump-name = tt-file.dump-name + "." + STRING(fiSeq,"9").
return "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init TERMINAL-SIMULATION 
PROCEDURE init :
empty temp-table tt-file.


RUN BuildDumpFileList IN THIS-PROCEDURE.

for each tt-file exclusive-lock
    where can-do({&except-tables},tt-file.FILE-NAME)
       OR CAN-DO({&except-ar},tt-file.FILE-NAME) 
       OR CAN-DO({&except-manf},tt-file.FILE-NAME)
       OR CAN-DO({&except-pallets},tt-file.FILE-NAME)
       OR CAN-DO({&except-cn},tt-file.FILE-NAME)
       OR CAN-DO({&except-gl},tt-file.FILE-NAME):
    ASSIGN tt-file.dump-table = FALSE.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-fields TERMINAL-SIMULATION 
PROCEDURE validate-fields :
def input parameter p-field as char no-undo.
def output parameter p-ok as log no-undo.

do with frame frame-param:

   if keylabel(lastkey) = "pf4" or keylabel(lastkey) = "f4" then
   do:
      apply "close" to this-procedure.
      return.
   end.

   case p-field :
      when "fi-bus" then
      do:
         if fi-bus:screen-value = "" then
         do:
            message "Blank Busunit List is Invalid".
            apply "entry" to fi-bus.
            p-ok = false.
            return.
         end.

         if fi-bus:screen-value <> "*" then
         do i = 1 to num-entries(fi-bus:screen-value):
            if not can-find(first zzbu no-lock
                   where zzbu.busunit = entry(i,fi-bus:screen-value)) then
            do:
               message "Invalid Busunit " + entry(i,fi-bus:screen-value).
               apply "entry" to fi-bus.
               p-ok = false.
               return.
            end.
         end.

         p-ok = true.
         hide message no-pause.
         return.
       
      end.

      when "fi-cn-from-date" then
      do:
         if fi-cn-from-date:screen-value > string(today) then
         do:
            message "From Date must be <= today".
            apply "entry" to fi-cn-from-date.
            p-ok = false.
            return.
         end.

         p-ok = true.
         hide message no-pause.
         return.
       
      end.

      when "fi-cn-to-date" then
      do:
         if fi-cn-to-date:screen-value < fi-cn-from-date:screen-value then
         do:
            message "To Date must be >= From Date".
            apply "entry" to fi-cn-to-date.
            p-ok = false.
            return.
         end.

         p-ok = true.
         hide message no-pause.
         return.
       
      end.

      when "fi-out-dir" then
      do:
         if fi-out-dir:screen-value = "" then
         do:
            message "Please specify output directory".
            apply "entry" to fi-out-dir.
            p-ok = false.
            return.
         end.

         os-create-dir value(fi-out-dir:screen-value).
         v-error = os-error.
         if v-error <> 0 then
         do:
            message "Directory not created. System Error #" v-error.
            apply "entry" to fi-out-dir.
            p-ok = false.
            return.
         end.

         p-ok = true.
         hide message no-pause.
         return.
       
      end.

      when "fi-dump-type" then
      do:
         IF NOT CAN-DO("L,T,C,M,A,P",fi-dump-type:SCREEN-VALUE) THEN
/*          if fi-dump-type:screen-value <> "B" and  */
/*             fi-dump-type:screen-value <> "T" and  */
/*             fi-dump-type:screen-value <> "C" then */
         do:
            message "Dump Type must be L/T/C/M/A/P".
            apply "entry" to fi-dump-type.
            p-ok = false.
            return.
         end.

         p-ok = true.
         hide message no-pause.
         return.
       
      end.

      when "fi-max-count" then
      do:
         if integer(fi-max-count:screen-value) = 0 then
            v-max-count = 1000.
         else
            v-max-count = integer(fi-max-count:screen-value).

         p-ok = true.
         hide message no-pause.
         return.
       
      end.

   end case.

   p-ok = true.
   hide message no-pause.
   return.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

