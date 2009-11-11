
DEFINE INPUT PARAMETER c_tblname AS CHARACTER.
DEFINE OUTPUT PARAMETER l_ok AS LOGICAL.

FIND FIRST dictdb._file 
    WHERE dictdb._file._file-name = c_tblname
    NO-LOCK NO-ERROR.

l_ok = AVAILABLE dictdb._file.
