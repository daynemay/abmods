/* Nothing yet */
DEFINE VARIABLE hEditor AS HANDLE NO-UNDO.

/**
MESSAGE "UNIX EVENT"
  1 p_product  SKIP
  2 p_event    SKIP
  3 p_context  SKIP
  4 p_other    SKIP
  5 p_ok       SKIP
  VIEW-AS ALERT-BOX.
**/ 

IF SELF <> ? THEN
DO:
    RUN adecomm/unixgted.p
        ( INPUT  SELF:HANDLE,
          OUTPUT hEditor ).
END.

/* I'm having trouble overriding the F5 trigger in UNIX... */
ON F5 ANYWHERE PERSISTENT RUN misc/message.p ("Go, man").
