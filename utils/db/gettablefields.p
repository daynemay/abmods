/******************************************************************************
  PROCEDURE            : utils/db/gettablefields.p
  DESCRIPTION          : Return a comma-separated list of the fields of a given table
  AUTHOR               : LINX-daynem
  DATE CREATED         : 30/08/06
  CALLED FROM          : Menu

  CHANGES MADE AND DATE:
  Date      Version      Description

  30/08/06  LINX#daynem  Initial Version.
******************************************************************************/
{errors-tt.i}
{error-preproc.i}

DEFINE INPUT  PARAMETER pcTableName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcTableFields AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt-errors.

DEFINE VARIABLE llIsTable       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE liDBnum         AS INTEGER      NO-UNDO.

DEFINE VARIABLE lcQuery         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lhQuery         AS HANDLE       NO-UNDO.

DEFINE VARIABLE lhFieldBuffer   AS HANDLE       NO-UNDO.
DEFINE VARIABLE lhFileBuffer    AS HANDLE       NO-UNDO.

DEFINE VARIABLE lcFieldName     AS CHARACTER    NO-UNDO.

CREATE WIDGET-POOL "GETTABLEFIELDS".

EMPTY TEMP-TABLE tt-errors.

RUN utils/db/isdbtable.p
    ( INPUT  pcTableName,
      OUTPUT llIsTable,
      OUTPUT liDBnum ).

IF NOT llIsTable THEN 
DO:

    {crt-tt-error.i
        "'*'"
        "{&ERROR_ERROR}"
        "pcTableName + 'is not a valid database table.'" 
        }

    RETURN.

END.

lcQuery = "FOR EACH " + LDBNAME ( liDBnum ) + "._file NO-LOCK " + 
          "   WHERE _file._file-name = " + QUOTER ( pcTableName ) + 
          "   , EACH " + STRING ( LDBNAME ( liDBnum ) ) + "._field NO-LOCK " +
          "     OF _file " + 
          "     BY _field._field-name ".

CREATE BUFFER lhFileBuffer 
    FOR TABLE ( LDBNAME ( liDBnum ) + "._file" )
    IN WIDGET-POOL "GETTABLEFIELDS".

CREATE BUFFER lhFieldBuffer 
    FOR TABLE ( LDBNAME ( liDBnum ) + "._field" )
    IN WIDGET-POOL "GETTABLEFIELDS".



CREATE QUERY lhQuery    
    IN WIDGET-POOL "GETTABLEFIELDS".

lhQuery:ADD-BUFFER ( lhFileBuffer  ).
lhQuery:ADD-BUFFER ( lhFieldBuffer ).

lhQuery:QUERY-PREPARE ( lcQuery ).
lhQuery:QUERY-OPEN ( ).
lhQuery:GET-FIRST ().

DO WHILE NOT lhQuery:QUERY-OFF-END:

    lcFieldName = lhFieldBuffer:BUFFER-FIELD ("_field-name"):BUFFER-VALUE.

    IF LOOKUP ( lcFieldName, pcTableFields ) = 0 THEN
        pcTableFields = IF pcTableFields = ""
                        THEN lcFieldName
                        ELSE ( pcTableFields + "," + lcFieldName ).

    lhQuery:GET-NEXT ( ).

END.

DELETE WIDGET-POOL "GETTABLEFIELDS".
