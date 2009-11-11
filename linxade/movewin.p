DEFINE INPUT PARAMETER pcDirection AS CHARACTER NO-UNDO.

&SCOPED-DEFINE MaxDown  ( SESSION:WORK-AREA-HEIGHT-PIXELS - SELF:WINDOW:HEIGHT-PIXELS )
&SCOPED-DEFINE MaxRight ( SESSION:WORK-AREA-WIDTH-PIXELS  - SELF:WINDOW:WIDTH-PIXELS  )
&SCOPED-DEFINE Step     30
&SCOPED-DEFINE MiniStep 1

CASE pcDirection:

    WHEN "HARD-UP" THEN
        SELF:WINDOW:Y = 0.
    
    WHEN "HARD-DOWN" THEN 
        SELF:WINDOW:Y = {&MaxDown}.

    WHEN "HARD-LEFT" THEN
        SELF:WINDOW:X = 0.
    
    WHEN "HARD-RIGHT" THEN 
        SELF:WINDOW:X = {&MaxRight}.

    WHEN "UP" THEN
        SELF:WINDOW:Y = MAX ( 0, SELF:WINDOW:Y - {&Step} ).

    WHEN "DOWN" THEN
        SELF:WINDOW:Y = MIN ( {&MaxDown}, SELF:WINDOW:Y + {&Step} ).
    
    WHEN "LEFT" THEN
        SELF:WINDOW:X = MAX ( 0, SELF:WINDOW:X - {&Step} ).
    
    WHEN "RIGHT" THEN
        SELF:WINDOW:X = MIN ( {&MaxRight}, SELF:WINDOW:X + {&Step} ).

    WHEN "LITTLE-UP" THEN
        SELF:WINDOW:Y = MAX ( 0, SELF:WINDOW:Y - {&MiniStep} ).

    WHEN "LITTLE-DOWN" THEN
        SELF:WINDOW:Y = MIN ( {&MaxDown}, SELF:WINDOW:Y + {&MiniStep} ).
    
    WHEN "LITTLE-LEFT" THEN
        SELF:WINDOW:X = MAX ( 0, SELF:WINDOW:X - {&MiniStep} ).
    
    WHEN "LITTLE-RIGHT" THEN
        SELF:WINDOW:X = MIN ( {&MaxRight}, SELF:WINDOW:X + {&MiniStep} ).



END CASE.

APPLY "ENTRY":U TO SELF.
