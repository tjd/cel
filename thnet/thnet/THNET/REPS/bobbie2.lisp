; FILE:       reps.feng--FDR/england
; PURPOSE:    testing gulf analogies to WW2
; PROGRAMMER: Bobbie and Keith
; CREATED:    3-13-91
; UPDATED:    thurs


(defun make_Gulf ()

(make_struc 'Gulf
            'sentence
            '(all
              ((country (Iraq) G1)
               (country (US) G2)
               (country (Kuwait) G3)
               (country (Saudi-Arabia) G4)
               (country (England) G5)
               (p_Iraq (Iraq) G6)
               (p_US (US) G7)
               (p_Kuwait (Kuwait) G8)
               (p_Saudi-Arabia (Saudi-Arabia) G9)
               (p_England (England) G10)
               (person (Saddam) G11)
               (person (Bush) G12)
               (person (emir-of-Kuwait) G13)
               (person (sheik-of-SA) G14)
               (person (pm-of-England) G15)
               (leader-of (Saddam Iraq) G16)
               (leader-of (Bush US) G17)
               (leader-of (emir-of-Kuwait Kuwait) G18)
               (leader-of (sheik-of-SA Saudi-Arabia) G19)
               (leader-of (pm-of-England England) G20)
; neighbor-of goes in the other to Iraq direction
               (neighbor-of (Kuwait Iraq) G21)
               (neighbor-of (Saudi-Arabia Iraq) G22)
               (weaker-than (Kuwait Iraq) G23)
               (weaker-than (Saudi-Arabia Iraq) G24)
               (weaker-than (Iraq US) G25)
; defining the UN-allies
                (allies (UN_allies) G26)
                (included-in (England UN_allies) G27)
                (included-in (US UN_allies) G28)
                (included-in (Saudi-Arabia UN_allies) G29)
                (included-in (Kuwait UN_allies) G30)

               (militarize (Saddam Iraq) G33)
               (occupy (Iraq Kuwait) G34)
               (order (Saddam G34) G34a)        ; Saddam orders occupation
; kjh not like
;               (take-first (Iraq Kuwait) G35)
               (enable (G23 G34) G36)           ; K weaker than I enabled occupying
               (enable (G33 G34) G37)           ; I militarization enabled occupying
               (feared-what-would-do-next (Saudi-Arabia Saddam) G38)
               (feared-what-would-do-next (US Saddam) G39)
               (cause (G34 G38) G40)            ; occupying caused fear by SA
               (cause (G34 G39) G41)            ; occupying caused fear by US
               (ask-help (emir-of-Kuwait US) G42)
               (ask-help (sheik-of-SA US) G43)
               (fight-for (US Kuwait) G44)
               (attack (US Iraq) G45)
               (cause (G39 G45) G46)            ; fear causes US to attack
               (cause (G42 G45) G47)            ; ask help Kuwait causes US to attack
               (cause (G43 G45) G48)            ; ask help SA causes US to attack
               (mobilize (Bush US) G49)
               (inspire (Bush US) G50)
                (inspire (Bush UN_allies) G50a)
               (order (Bush G45) G51)           ; bush orders the attack
                (supplies (obj_supplies) G53)
                (provide (US Saudi-Arabia obj_supplies) G54)
                (order (Bush G54) G54a)         ; bush orders US to provide supplies to SA
                (cause (G43 G54) G54b)          ; SA ask-help causes US provide supplies
                (bomb (US Iraq) G55)
                (missile (Iraq Saudi-Arabia) G56)
; leading the forces (Bush commands only US but Schwarzkopf leads them all)
                (forces (US_forces) G57a)
                (forces (UN_forces) G57b)
                (has (US US_forces) G57c)
                (has (UN_allies UN_forces) G57d)
                (command (Bush US_forces) G57e)
                (person (Schwarzkopf) G58)
                (general-of (Schwarzkopf Bush) G59)
                (lead (Schwarzkopf US_forces) G60)
                (lead (Schwarzkopf UN_forces) G60a)
                (directs-from (Schwarzkopf Saudi-Arabia UN_forces) G61)
                (directs-from (Schwarzkopf Saudi-Arabia US_forces) G61a)
                (mass-in (UN_forces Saudi-Arabia) G62)
                (beating (US Iraq) G63) 

; why beating?
                (cause (G25 G63) G64)    ; Iraq weaker than US
                (cause (G49 G63) G65)    ; Bush mobilized US
                (cause (G55 G63) G66)    ; US bomb Iraq
                (cause (G60 G63) G67)    ; Sch. leads US forces
                (cause (G60a G63) G67)   ; Sch. leads UN forces
                (cause (G57e G63) G68)   ; Bush commands US forces
                (cause (G62 G63) G71)    ; UN forces mass in SA

                            )
               )                
) 
)

(defun make_WW2 ()

(make_struc 'WW2
            'sentence
            '(all
              ((country (Germany) W1)
               (country (US) W2)
               (country (England) W3)
               (country (France) W4)
               (country (Austria) W5)
               (country (Czechoslovakia) W6)
               (country (Poland) W7)
               (country (Japan) W8)
               (p_Germany (Germany) W9)
               (p_US (US) W10)
               (p_England (England) W11)
               (p_Czechoslovakia (Czechoslovakia) W12)
               (p_Poland (Poland) W13)
               (p_France (France) W14)
               (p_Austria (Austria) W15)
               (p_Japan (Japan) W16)
               (person (Hitler) W17)
               (person (FDR) W18)
               (person (Chamberlain) W19)
               (person (Churchill) W20)
               (person (leader-of-France) W21)
               (person (leader-of-Czechs) W22)
               (person (leader-of-Poles) W23)
               (person (leader-of-Austria) W24)
               (person (Hirohito) W24a)
               (leader-of (Hitler Germany) W25)
               (leader-of (FDR US) W26)
               (leader-of (Chamberlain England) W27)
               (leader-of (Churchill England) W28)
               (leader-of (leader-of-France France) W29)
               (leader-of (leader-of-Czechs Czechoslovakia) W30)
               (leader-of (leader-of-Poles Poland) W31)
               (leader-of (leader-of-Austria Austria) W32)
               (leader-of (Hirohito Japan) W33)
;neighbor-of goes to Germany
               (neighbor-of (Austria Germany) W35)
               (neighbor-of (Czechoslovakia Germany) W36)
               (neighbor-of (Poland Germany) W37)
               (neighbor-of (France Germany) W38)
               (weaker-than (Austria Germany) W39)
               (weaker-than (Czechoslovakia Germany) W40)
               (weaker-than (Poland Germany) W41)
;defining the allies and axis
                (allies (allies) W42)
                (allies (axis) W43)
                (included-in (France allies) W44)
                (included-in (England allies) W45)
                (included-in (US allies) W46) 
                (included-in (Germany axis) W47)
                (included-in (Japan axis) W48)
;Austria               
                (militarize (Hitler Germany) W50)
                (annex (Germany Austria) W51)
;kjh not like
;               (take-first (Germany Austria) W52)
                (enable (W39 W51) W53)          ; A  weaker than G enabled annex
                (enable (W50 W51) W53a)         ; G militarization enabled annex
                (order (Hitler W51) W54)        ; Hitler ordered annex
;Czechoslovakia
                (annex (Germany Czechoslovakia) W62)
                (enable (W40 W62) W63)          ; Czech weaker than G enabled annex
                (enable (W50 W62) W63a)         ; Germany militarization enabled annex
                (order (Hitler W62) W64)        ; Hitler ordered annex
                (feared-what-would-do-next (England Hitler) W73)
                (feared-what-would-do-next (France Hitler) W74)
                (cause (W62 W73) W75)           ; annexation caused England fear
                (cause (W62 W74) W76)           ; annexation caused France fear
;Poland
                (attack (Germany Poland) W77)
                (order (Hitler W77) W78)        ; Hitler ordered attack
                (conquer (Germany Poland) W79)
                (enable (W41 W79) W80)          ; Poland weaker than G enabled conquer 
                (enable (W50 W79) W81)          ; G militarization enabled conquer
                (fight-for (England Poland) W82)
                (order (Chamberlain W82) W83)   ; Chamb ordered England to fight-for P
                (attack (England Germany) W86) 
                (cause (W77 W86) W87)           ; attack on P caused E to attack G
                (cause (W73 W86) W88)           ; fear of Hitler caused E to attack G
                (fight-for (France Poland) W89)
                (attack (France Germany) W90)
                (cause (W77 W90) W91)           ; attack on P caused F to attack G
                (cause (W74 W90) W92)           ; fear of Hitler caused F to attack G
;France gets beat too
                (attack (Germany France) W100)
                (fight-for (England France) W101)
                (fight-for (France France) W102)
                (conquer (Germany France) W104)
                (enable (W50 W104) W105)        ; G militarization enabled conquer 
;here are the forces
                (forces (allied_forces) W107)           
                (forces (English_forces) W107a)
                (forces (US_forces) W107b)
                (has (allies allied_forces) W107c)
                (has (England English_forces) W107d)
                (has (US US_forces) W107f) 

; here are the minimalist parts for churchill, fdr and us--ON ALWAYS
                (ask-help (Churchill US) W110)
                (attack (Japan US) W117)
                (attack (US Japan) W118)
                (cause (W117 W118) W119)
                (declare-war-on (Germany US) W120)
                (attack (US Germany) W121)
                (cause (W120 W121) W122)
                (mobilize (FDR US) W123)


; CHURCHILL (on for both churchills; off for both roosevelts)
;                (inspire (Churchill England) W108)
;                (militarize (Churchill England) W109)
;                (mobilize (Churchill England) W109a)
;                (command (Churchill English_forces) W111)
;                (command (Churchill allied_forces) W111a)
;                (person (Montgomery) W114)
;                (general-of (Montgomery Churchill) W115)
;                (lead (Montgomery English_forces) W116a) 
;                (lead (Montgomery allied_forces) W116b)
    
; CHURCHILL/ENGLAND ONLY
; why conquer
;                (conquer (England Germany) W124)
;                (cause (W108 W124) W125)        ; churchill inspired england
;                (cause (W109 W124) W126)        ; churchill militarized england
;                (cause (W109a W124) W126a)      ; churchill mobilized england
;                (cause (W116a W124) W127)       ; monty led english forces
;                (cause (W116b W124) W128)       ; monty led allied forces
;                (cause (W111 W124) W130)        ; churchill command english forces
;                (cause (W111a W124) W131)        ; churchill command allied forces
                                 
; CHURCHILL/US ONLY
;                (bomb (Germany England) W150)
;                (missile (Germany England) W151)
;                (militarize (FDR US) W152)
;                (bomb (US Germany) W153)
;                (supplies (obj_supplies) W154)
;                (provide (US England obj_supplies) W155)
;                (cause (W110 W155) W156)        ;Church asks US to help; US provides
;                (mass-in (allied_forces England) W157)
; why conquer?
;                (conquer (US Germany) W124)
;                (cause (W108 W124) W125)    ;churchill inspired england
;                (cause (W109 W124) W125a)   ;churchill militarized england
;                (cause (W109a W124) W125b)  ;churchill mobilized england
;                (cause (W110 W124) W126)    ;churchill asked help
;                (cause (W111 W124) W127)    ;churchill command english forces
;                (cause (W111a W124) W128)   ;churchill command allied forces
;                (cause (W116a W124) W130)   ;monty led english forces 
;                (cause (W116b W124) W131)   ;monty led allied forces
;                (cause (W123 W124) W134)    ;FDR militarize us
;                (cause (W152 W124) W135)    ;FDR mobilize us
;                (cause (W153 W124) W136)    ;US bomb germany
;                (cause (W155 W124) W137)    ;US provide supplies
;                (cause (W157 W124) W138)    ;allied forces massed in England

; ROOSEVELT (on for both roosevelts; off for both churchills)
                (feared-what-would-do-next (Roosevelt Hitler) W160)
                (supplies (obj_supplies) W161)
                (provide (US England obj_supplies) W162)
                (order (FDR W162) W163)   ;FDR order US to provide supplies
                (cause (W160 W163) W164)        ;fear caused FDR to provide supplies
                (militarize (FDR US) W165)
                (cause (W160 W165) W166)        ;fear caused FDR to militarize
                (inspire (FDR US) W167)
                (inspire (FDR allies) W168)
                (person (Eisenhower) W169)
                (general-of (Eisenhower FDR) W170)
                (lead (Eisenhower US_forces) W171)
                (lead (Eisenhower allied_forces) W172) 


; ROOSEVELT/ENGLAND ONLY
                (directs-from (Eisenhower England US_forces) W173)
                (directs-from (Eisenhower England allied_forces) W173c)
                (mobilize (Churchill England) W173a)
                (militarize (Churchill England) W173b)
; why conquer
                (conquer (England Germany) W124)
                (cause (W167 W124) W125)    ;FDR inspired allies
                (cause (W123 W124) W126)    ;FDR militarized
                (cause (W152 W124) W127)    ;FDR mobilized
                (cause (W163 W124) W128)    ;FDR ordered supplies
                (cause (W172 W124) W129)    ;Eisenhower commanded allies
                (cause (W173a W124) W130)   ;Churchill militarized
                (cause (W173b W124) W130)   ;Churchill mobilized

; ROOSEVELT/US ONLY
;                (bomb (Germany England) W180)
;                (missile (Germany England) W181)
;                (bomb (US Germany) W182)
;                (mass-in (US_forces England) W183)
;                (mass-in (allied_forces England) W184)
;why conquer (first five are same as roosevelt/england)
;                (conquer (US Germany) W124)
;                (cause (W167 W124) W125)    ;FDR inspired US
;                (cause (W168 W124) W125a)   ;FDR inspired allies
;                (cause (W123 W124) W126)    ;FDR militarized
;                (cause (W152 W124) W127)    ;FDR mobilized
;                (cause (W163 W124) W128)    ;FDR ordered supplies
;                (cause (W172 W124) W129)    ;Eisenhower led allies
;                (cuase (W171 W124) W129a)   ;Eisenhower led us
;                (cause (W182 W124) W130)    ;US bombed Germany
;                (cause (W155 W124) W131)    ;US provided supplies
;                (cause (W184 W124) W132)    ;allied force massed in england
 

 


              )
            )
)
)
