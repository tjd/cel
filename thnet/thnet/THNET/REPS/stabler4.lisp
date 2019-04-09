;   File   : hfamily.pl
;   Author : E.P. Stabler, Jr
;   Updated: 1988
;   Purpose: This is a first order formulation of Hinton's family trees
;            using has_relation predicates over individuals



(defun make_english_family ()
  (make_struc 'english_family 'family_tree
	      '(family (
			(has_father (arthur christopher) E1)      ;s
			(has_father (victoria christopher) E2)    ;s
			(has_father (colin james) E3)             ;s
			(has_father (charlotte james) E4)         ;s
			(has_father (james andrew) E5)            ;s
			(has_father (jennifer andrew) E6)         ;s
			(has_mother (arthur penelope) E7)         ;s
			(has_mother (victoria penelope) E8)       ;s
			(has_mother (colin victoria) E9)          ;s
			(has_mother (charlotte victoria) E10)     ;s
			(has_mother (james christine) E11)        ;s
			(has_mother (jennifer christine) E12)     ;s
			(has_husband (margaret arthur) E13)       ;s
			(has_husband (penelope christopher) E14)  ;s
			(has_husband (victoria james) E15)        ;s
			(has_husband (christine andrew) E16)      ;s
			(has_husband (jennifer charles) E17)      ;s
			(has_wife (arthur margaret) E18)          ;s
			(has_wife (christopher penelope) E19)     ;s
			(has_wife (james victoria) E20)           ;s
			(has_wife (andrew christine) E21)         ;s
			(has_wife (charles jennifer) E22)         ;s
			(has_son (christopher arthur) E23)        ;s
			(has_son (penelope arthur) E24)           ;s
			(has_son (andrew james) E25)              ;s
			(has_son (christine james) E26)           ;s
			(has_son (james colin) E27)               ;s
			(has_son (victoria colin) E28)            ;s
			(has_daughter (christopher victoria) E29) ;s
			(has_daughter (penelope victoria) E30)    ;s
			(has_daughter (andrew jennifer) E31)      ;s
			(has_daughter (christine jennifer) E32)   ;s
			(has_daughter (victoria charlotte) E33)   ;s
			(has_daughter (james charlotte) E34)      ;s
			(has_brother (victoria arthur) E35)       ;s
			(has_brother (jennifer james) E36)        ;s
			(has_brother (charlotte colin) E37)       ;s
			(has_sister (arthur victoria) E38)        ;s
			(has_sister (james jennifer) E39)         ;s
			(has_sister (colin charlotte) E40)        ;s
			(has_uncle (colin arthur) E41)            ;s
			(has_uncle (charlotte arthur) E42)        ;s
			(has_uncle (colin charles) E43)           ;s
			(has_uncle (charlotte charles) E44)       ;s
			(has_aunt (colin jennifer) E45)           ;s
			(has_aunt (charlotte jennifer) E46)       ;s
			(has_aunt (colin margaret) E47)           ;s
			(has_aunt (charlotte margaret) E48)       ;s
			(has_nephew (arthur colin) E49)           ;s
			(has_nephew (jennifer colin) E50)         ;s
			(has_nephew (charles colin) E51)          ;s
			(has_nephew (margaret colin) E52)         ;s
			(has_niece (arthur charlotte) E53)        ;s
			(has_niece (jennifer charlotte) E54)      ;s
			(has_niece (charles charlotte) E55)       ;s
			(has_niece (margaret charlotte) E56)      ;s
			))))  

(defun make_italian_family ()
  (make_struc 'italian_family 'family_tree
	      '(family (
			(I_has_father (emilio roberto) I1)          ;t
			(I_has_father (lucia roberto) I2)           ;t
			(I_has_father (alfonso marco) I3)           ;t
			(I_has_father (sophia marco) I4)            ;t
			(I_has_father (marco pierro) I5)            ;t
			(I_has_father (angela pierro) I6)           ;t
			(I_has_mother (emilio maria) I7)            ;t
			(I_has_mother (lucia maria) I8)             ;t
			(I_has_mother (alfonso lucia) I9)           ;t
			(I_has_mother (sophia lucia) I10)           ;t
			(I_has_mother (marco francesca) I11)        ;t
			(I_has_mother (angela francesca) I12)       ;t
			(I_has_husband (gina emilio) I13)           ;t
			(I_has_husband (maria roberto) I14)         ;t
			(I_has_husband (lucia marco) I15)           ;t
			(I_has_husband (francesca pierro) I16)      ;t
			(I_has_husband (angela tomaso) I17)         ;t
			(I_has_wife (emilio gina) I18)              ;t
			(I_has_wife (roberto maria) I19)            ;t
			(I_has_wife (marco lucia) I20)              ;t
			(I_has_wife (pierro francesca) I21)         ;t
			(I_has_wife (tomaso angela) I22)            ;t
			(I_has_son (roberto emilio) I23)            ;t
			(I_has_son (maria emilio) I24)              ;t
			(I_has_son (pierro marco) I25)              ;t
			(I_has_son (francesca marco) I26)           ;t
			(I_has_son (marco alfonso) I27)             ;t
			(I_has_son (lucia alfonso) I28)             ;t
			(I_has_daughter (roberto lucia) I29)        ;t
			(I_has_daughter (maria lucia) I30)          ;t
			(I_has_daughter (pierro angela) I31)        ;t
			(I_has_daughter (francesca angela) I32)     ;t
			(I_has_daughter (lucia sophia) I33)         ;t
			(I_has_daughter (marco sophia) I34)         ;t
			(I_has_brother (lucia emilio) I35)          ;t
			(I_has_brother (angela marco) I36)          ;t
			(I_has_brother (sophia alfonso) I37)        ;t
			(I_has_sister (emilio lucia) I38)           ;t
			(I_has_sister (marco angela) I39)           ;t
			(I_has_sister (alfonso sophia) I40)         ;t
			(I_has_uncle (alfonso emilio) I41)          ;t
			(I_has_uncle (sophia emilio) I42)           ;t
			(I_has_uncle (alfonso tomaso) I43)          ;t
			(I_has_uncle (sophia tomaso) I44)           ;t
			(I_has_aunt (alfonso angela) I45)           ;t
			(I_has_aunt (sophia angela) I46)            ;t
			(I_has_aunt (alfonso gina) I47)             ;t
			(I_has_aunt (sophia gina) I48)              ;t
			(I_has_nephew (emilio alfonso) I49)         ;t
			(I_has_nephew (angela alfonso) I50)         ;t
			(I_has_nephew (tomaso alfonso) I51)         ;t
			(I_has_nephew (gina alfonso) I52)           ;t
			(I_has_niece (emilio sophia) I53)           ;t
			(I_has_niece (angela sophia) I54)           ;t
			(I_has_niece (tomaso sophia) I55)           ;t
			(I_has_niece (gina sophia) I56)             ;t
			))))         
