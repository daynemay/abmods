/******************************************************************************
  PROCEDURE            : utils/db/tablefields.i
  DESCRIPTION          : A temp table for fields.  Similar to the _field table.
  AUTHOR               : LINX-daynem
  DATE CREATED         : 11/08/06
  CALLED FROM          : Menu

  CHANGES MADE AND DATE:
  Date      Version      Description

  11/08/06  LINX#daynem  Initial Version.
******************************************************************************/

DEFINE TEMP-TABLE ttTableField NO-UNDO
    FIELD databaseName  AS CHARACTER
    FIELD tableName     AS CHARACTER
    FIELD fieldName     AS CHARACTER
    FIELD dataType      AS CHARACTER
    FIELD fieldFormat   AS CHARACTER
    FIELD fieldLabel    AS CHARACTER
    FIELD fieldHelp     AS CHARACTER.
