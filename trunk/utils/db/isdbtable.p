/******************************************************************************
  PROCEDURE            : utils/db/isdbtable.p
  DESCRIPTION          : Determine whether an input string is the name of a 
                         table in ANY database (not just the current working,
                         which is all that a static FIND FIRST _file tells us.
  AUTHOR               : LINX-daynem
  DATE CREATED         : 11/08/06
  CALLED FROM          : Menu

  CHANGES MADE AND DATE:
  Date      Version      Description

  11/08/06  LINX#daynem  Initial Version.
  03/10/06  daynem       Rework to use dynamic query rather than change DB.
******************************************************************************/

DEFINE INPUT  PARAMETER icTableName         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER olIsTable           AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oiDatabaseNumber    AS INTEGER NO-UNDO.

DEFINE VARIABLE lhFileBuffer    AS HANDLE NO-UNDO.
DEFINE VARIABLE lhQuery         AS HANDLE NO-UNDO.

DEFINE VARIABLE lcQuery         AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcDbName        AS CHARACTER NO-UNDO.

CREATE WIDGET-POOL "ISDBTABLE".

DB_LOOP:
DO oiDatabaseNumber = 1 TO NUM-DBS:

    lcDbName = LDBNAME ( oiDatabaseNumber ).

    lcQuery = 
        "FOR EACH " + lcDbName + "._file NO-LOCK      " +
        "   WHERE " + lcDbName + "._file._file-name = " + QUOTER ( icTableName ).

    CREATE BUFFER lhFileBuffer 
        FOR TABLE ( lcDbName + "._file" )
        IN WIDGET-POOL "ISDBTABLE".

    CREATE QUERY lhQuery
        IN WIDGET-POOL "ISDBTABLE".

    lhQuery:ADD-BUFFER ( lhFileBuffer ).

    lhQuery:QUERY-PREPARE ( lcQuery ).

    lhQuery:QUERY-OPEN ( ).

    lhQuery:GET-FIRST ( ).

    IF NOT lhQuery:QUERY-OFF-END THEN
    DO:
        olIsTable = TRUE.
        LEAVE DB_LOOP.
    END.

END.

IF NOT olIsTable THEN 
    oiDatabaseNumber = ?.

DELETE WIDGET-POOL "ISDBTABLE".
