(defun make_band ()

(make_struc 'band
            'problem
            '(start
              (
	       (band-members (obj_members) B1)                       ;s
               (band (obj_band) B2)                                  ;s
               (march-in (obj_members obj_band) B3)                  ;s
               (number-of (obj_members num_total_B) B4)              ;s
               (divide (num_total_B num12 quotient_B1) B5)           ;s
               (remainder-of (B5 num1) B6)                           ;s
               (not-equal (num1 num0) B7)                            ;s
	       (number-left-out (obj_members obj_band num1) B8)      ;s
               (divide (num_total_B num8 quotient_B2) B9)            ;s
               (remainder-of (B9 num1) B10)                          ;s
               (divide (num_total_B num3 quotient_B3) B11)           ;s
               (remainder-of (B11 num1) B12)                         ;s
               (person (obj_director) B13)                           ;s
               (orders (obj_director B5) B14)                        ;s
               (orders (obj_director B9) B15)                        ;s
               (orders (obj_director B11) B16)                       ;s
               (member-of (andrew obj_members) B17)                  ;s
               (person (andrew) B18)                                 ;s
	       (left-out-of (andrew obj_band) B19)                   ;s
               (orders (andrew B21) B20)                             ;s
               (divide (num_total_B num5 quotient_B4) B21)           ;s
               (remainder-of (B21 num0) B22)                         ;s
               (success (B21) B23)                                   ;s
               (cause (B22 B23) B24)                                 ;s
               (number (num_total_B) B25)                            ;s
               (number (num1) B26)                                   ;s
               (number (num0) B27)                                   ;s
               (number (num12) B28)                                  ;s
               (number (num8) B29)                                   ;s
               (number (num3) B30)                                   ;s
               (number (num5) B31)                                   ;s
               (number (quotient_B1) B32)                            ;s
               (number (quotient_B2) B33)                            ;s
               (number (quotient_B3) B34)                            ;s
               (number (quotient_B4) B35)                            ;s
               (zero (num0) B36)                                     ;s
               (one (num1) B37)                                      ;s
               (twelve (num12) B38)                                  ;s
               (eight (num8) B39)                                    ;s
               (three (num3) B40)                                    ;s
               (five (num5) B41)                                     ;s
               (grouping-of (obj_row12 obj_members) B42)             ;s
               (grouping-of (obj_column8 obj_members) B43)           ;s
               (grouping-of (obj_row3 obj_members) B44)              ;s
               (grouping-of (obj_row5 obj_members) B45)              ;s
	       (row-groups (obj_row12) B46)                          ;s
	       (column-groups (obj_column8) B47)                     ;s
	       (row-groups (obj_row3) B48)                           ;s
	       (row-groups (obj_row5) B49)                           ;s
               (number-per-group (obj_row12 num12) B50)              ;s
               (number-per-group (obj_column8 num8) B51)             ;s
               (number-per-group (obj_row3 num3) B52)                ;s
               (number-per-group (obj_row5 num5) B53)                ;s
               (first-try (B5) B54)                                  ;s
               (second-try (B9) B55)                                 ;s
               (third-try (B11) B56)                                 ;s
               (fourth-try (B21) B57)                                ;s
               (first-divisor (num12) B58)                           ;s
               (second-divisor (num8) B59)                           ;s
               (third-divisor (num3) B60)                            ;s
               (fourth-divisor (num5) B61)                           ;s
              )
             )
            '(goals
              (
	       (known (num_total_B) B62)                             ;s
               (divide (num_total_B num_Bx quotient_Bx) B63)         ;s
               (remainder-of (B63 num0) B64)                         ;s
               (equal (num_Bx num5) B65)                             ;s
               (equal (quotient_Bx quotient_B4) B66)                 ;s
               (greater-than (num_total_B num44) B67)                ;s
               (less-than (num_total_B num200) B68)                  ;s
               (number (num_Bx) B69)                                 ;s
               (number (quotient_Bx) B70)                            ;s
               (number (num44) B71)                                  ;s
               (number (num200) B72)                                 ;s
               (forty-four (num44) B73)                              ;s
               (two-hundred (num200) B74)                            ;s
              )
            )
  )
 
)

(defun make_garden ()

(make_struc 'garden
            'problem
            '(start
              (
	       (plants (obj_plants) G1)                              ;t
               (garden (obj_garden) G2)                              ;t
               (number-of (obj_plants num_total_G0) G4)              ;t
               (divide (num_total_G0 num10 quotient_G1) G5)          ;t
               (divide (num_total_G0 num4 quotient_G2) G7)           ;t
               (remainder-of (G7 num0) G8)                           ;t
               (remainder-of (G9 num0) G10)                          ;t
               (person (mr_renshaw) G11)                             ;t
               (suggests (mr_renshaw G5) G13)                        ;t
               (suggests (mrs_renshaw G7) G14)                       ;t
               (suggests (mrs_renshaw G9) G16)                       ;t
               (person (renshaw_daughter) G17)                       ;t
               (points-out (renshaw_daughter G18) G19)               ;t
               (divide (num_total_G1 num10 quotient_G1) G20)         ;t
               (divide (num_total_G1 num4 quotient_G2) G22)          ;t
               (remainder-of (G22 num2) G23)                         ;t
               (remainder-of (G24 num2) G25)                         ;t
               (not-equal (num2 num0) G26)                           ;t
               (points-out (renshaw_daughter G21) G28)               ;t
               (points-out (renshaw_daughter G23) G29)               ;t
               (suggests (renshaw_daughter G32) G31)                 ;t
               (divide (num_total_G1 num6 quotient_G4) G32)          ;t
               (success (G32) G34)                                   ;t
               (cause (G33 G34) G35)                                 ;t
               (number (num_total_G1) G37)                           ;t
               (number (num0) G38)                                   ;t
               (number (num10) G40)                                  ;t
               (number (num4) G41)                                   ;t
               (number (num6) G43)                                   ;t
               (number (quotient_G1) G44)                            ;t
               (number (quotient_G3) G46)                            ;t
               (number (quotient_G4) G47)                            ;t
               (two (num2) G49)                                      ;t
               (ten (num10) G50)                                     ;t
               (five (num5) G52)                                     ;t
               (six (num6) G53)                                      ;t
               (kind-of (obj_kind4 obj_plants) G55)                  ;t
               (kind-of (obj_kind5 obj_plants) G56)                  ;t
               (number-per-kind (obj_kind10 num10) G58)              ;t
               (number-per-kind (obj_kind4 num4) G59)                ;t
               (number-per-kind (obj_kind6 num6) G61)                ;t
               (first-try (G5) G62)                                  ;t
               (third-try (G9) G64)                                  ;t
               (fourth-try (G20) G65)                                ;t
               (sixth-try (G24) G67)                                 ;t
               (seventh-try (G32) G68)                               ;t
               (second-divisor (num4) G70)                           ;t
               (third-divisor (num5) G71)                            ;t
              )
             )
            '(goals
	      (
	       (known (num_total_G1) G73)                            ;t
	       (divide (num_total_G1 num_Gx quotient_Gx) G74)        ;t
	       (subtract (num_total_G1 num0 num_difference_G) G76)   ;t
	       (minimal (num_difference_G) G77)                      ;t
	       (equal (quotient_Gx quotient_G4) G79)                 ;t
	       (number (num_Gx) G80)                                 ;t
	       (number (num_difference_G) G82)                       ;t
	       )
            )
           '(solution
             (
	      (find-lcm! (num10 num4 num5 lcm_G) G83)                ;t
	      (find-multiples! (lcm_G list-of-multiples_G) G85)      ;t
	      (list (list-of-multiples_G) G86)                       ;t
	      (list-plus! (num2 list-of-multiples_G corrected-list_G) G87)   ;t
	      (list (corrected-list_G) G88)                                  ;t
              (find-least-multiple! (num6 corrected-list_G num_tota1_G1) G89);t
              )
           )
  )         
)
