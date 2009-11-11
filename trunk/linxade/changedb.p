
DEFINE INPUT PARAMETER c_tblname AS CHARACTER.
DEFINE OUTPUT PARAMETER l_ok AS LOGICAL.

DEFINE VARIABLE intemp AS INTEGER NO-UNDO.

l_ok = FALSE.

DO intemp = 1 TO NUM-DBS WHILE NOT l_ok:

      CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(intemp)).      

      /* Because CREATE ALIAS statement doesn't take affect */
      /* for the current compilation, split out this logic. */
      RUN linxade/changedb2.p
        (INPUT  c_tblname,
         OUTPUT l_ok).

END.

