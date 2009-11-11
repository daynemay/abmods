
/* Actions */
&GLOBAL-DEFINE CompleteTableName    "Complete table name"
&GLOBAL-DEFINE CompleteFieldName    "Complete field name"
&GLOBAL-DEFINE ShowAllInstances     "Show all instances of"
&GLOBAL-DEFINE NextValueSequence    "Insert NEXT-VALUE call for "
&GLOBAL-DEFINE InsertShortcutText   "Insert shortcut text"
&GLOBAL-DEFINE ShowAllIndices       "Show indices"

DEFINE TEMP-TABLE ttMagic NO-UNDO
    FIELD action    AS CHARACTER
    FIELD keyText   AS CHARACTER
    FIELD notes     AS CHARACTER
    FIELD order     AS INTEGER
    FIELD lineNo    AS INTEGER
    INDEX order order.

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD tablename AS CHARACTER.

DEFINE TEMP-TABLE ttField NO-UNDO 
    FIELD fieldName AS CHARACTER.

DEFINE TEMP-TABLE ttSequence NO-UNDO
    FIELD sequenceName AS CHARACTER.

DEFINE TEMP-TABLE ttInstance NO-UNDO
    FIELD lineContent AS CHARACTER
    FIELD lineNo      AS INTEGER.
