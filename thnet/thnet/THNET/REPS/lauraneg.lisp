; PURPOSE:    testing band/cookie/garden analogies
; PROGRAMMER: Keith Holyoak
; CREATED:    7-03-1989; 
; UPDATED:    9-13-89 by Eric Melz
;             10-18-89 renumbered props and added neg problem                           
;             12-07-90 by Keith Holyoak
;             distinguished num0, num4, num5, num20 in Garden vs Award (in combined rep)


;band problem (target)
(defun make_band ()

(make_struc 'band
            'problem
            '(start
;band members march in the band
              ((band-members (obj_members) B1)
               (band (obj_band) B2)
               (march-in (obj_members obj_band) B3)
;the number of band members is num_total_B
               (number-of (obj_members num_total_B) B4)
;dividing members by 12, 8, or 3 leaves a non-zero remainder of 1
               (divide (num_total_B num12 quotient_B1) B5)
               (remainder-of (B5 num1) B6)
               (not-equal (num1 num0) B7)
	       (number-left-out (obj_members obj_band num1) B8)
               (divide (num_total_B num8 quotient_B2) B9)
               (remainder-of (B9 num1) B10)
               (divide (num_total_B num3 quotient_B3) B11)
               (remainder-of (B11 num1) B12)
;the director orders the above attempts
               (person (obj_director) B13)
               (orders (obj_director B5) B14)
               (orders (obj_director B9) B15)
               (orders (obj_director B11) B16)
;Andrew orders division by 5, which results in 0 remainder
               (member-of (andrew obj_members) B17)
               (person (andrew) B18)
	       (left-out-of (andrew obj_band) B19)
               (orders (andrew B21) B20)
               (divide (num_total_B num5 quotient_B4) B21)
               (remainder-of (B21 num0) B22)
;division by 5 is a success because the remainder is 0
               (success (B21) B23)
               (cause (B22 B23) B24)
;all the numbers mentioned are numbers
               (number (num_total_B) B25)
               (number (num1) B26)
               (number (num0) B27)
               (number (num12) B28)
               (number (num8) B29)
               (number (num3) B30)
               (number (num5) B31)
               (number (quotient_B1) B32)
               (number (quotient_B2) B33)
               (number (quotient_B3) B34)
               (number (quotient_B4) B35)
;various numbers have specific values
               (zero (num0) B36)
               (one (num1) B37)
               (twelve (num12) B38)
               (eight (num8) B39)
               (three (num3) B40)
               (five (num5) B41)
;divisions by 12, 8, 3, and 5 constitute possible selections of groupings
;   of the members 
               (grouping-of (obj_row12 obj_members) B42)
               (grouping-of (obj_column8 obj_members) B43)
               (grouping-of (obj_row3 obj_members) B44)
               (grouping-of (obj_row5 obj_members) B45)
	       (row-groups (obj_row12) B46)
	       (column-groups (obj_column8) B47)
	       (row-groups (obj_row3) B48)
	       (row-groups (obj_row5) B49)
;12, 8, 3, and 5 are possible numbers per group
               (number-per-group (obj_row12 num12) B50)
               (number-per-group (obj_column8 num8) B51)
               (number-per-group (obj_row3 num3) B52)
               (number-per-group (obj_row5 num5) B53)
;dividing by 12, 8, 3, and 5 are the first to fourth solution
;  attempts, respectively
               (first-try (B5) B54)
               (second-try (B9) B55)
               (third-try (B11) B56)
               (fourth-try (B21) B57)
;12, 8, 3, and 5 are the first to fourth divisors considered, respectively
               (first-divisor (num12) B58)
               (second-divisor (num8) B59)
               (third-divisor (num3) B60)
               (fourth-divisor (num5) B61)
              )
             )
            '(goals
;the goal is to determine num_total_B
              ((known (num_total_B) B62)
;which can be divided by num_Bx to give quotient_Bx with 0 remainder
               (divide (num_total_B num_Bx quotient_Bx) B63)
               (remainder-of (B63 num0) B64)                
;where num_Bx = 5 and quotient_Bx = quotient_B4
               (equal (num_Bx num5) B65)
               (equal (quotient_Bx quotient_B4) B66)
;and num_total_B is greater than 45 and less than 200
               (greater-than (num_total_B num44) B67)
               (less-than (num_total_B num200) B68)
               (number (num_Bx) B69)
               (number (quotient_Bx) B70)
               (number (num44) B71)
               (number (num200) B72)
               (forty-four (num44) B73)
               (two-hundred (num200) B74)
              )
            )
  )
 
)

;garden problem (source)
(defun make_garden ()

(make_struc 'garden
            'problem
            '(start
;plants grow in a garden
              ((plants (obj_plants) G1)
               (garden (obj_garden) G2)
               (grow-in (obj_plants obj_garden) G3)
;the number of plants first considered is num_total_G0
               (number-of (obj_plants num_total_G0) G4)
;dividing num_total_G0 by 10, 4, or 5 leaves 0 remainder 
              (divide (num_total_G0 num10 quotient_G1) G5)
               (remainder-of (G5 num0) G6)
               (divide (num_total_G0 num4 quotient_G2) G7)
               (remainder-of (G7 num0) G8)
               (divide (num_total_G0 num5 quotient_G3) G9)
               (remainder-of (G9 num0) G10)
;Mr. Renshaw suggests dividing by 10, Mrs. Renshaw suggests
;   dividing by 4, and both suggest dividing by 5
               (person (mr_renshaw) G11)
               (person (mrs_renshaw) G12)
               (suggests (mr_renshaw G5) G13)
               (suggests (mrs_renshaw G7) G14)
               (suggests (mr_renshaw G9) G15)
               (suggests (mrs_renshaw G9) G16)
;the Renshaws' daughter points out that the total number of plants,
;   num_total_G0, can be increased by 2 to num_total_G1
               (person (renshaw_daughter) G17)
               (plus (num_total_G0 num2 num_total_G1) G18)
               (points-out (renshaw_daughter G18) G19)
;she points out that dividing by 10, 4, or 5 into num_total_G1
;   leaves a non-zero remainder of 2
               (divide (num_total_G1 num10 quotient_G1) G20) 
               (remainder-of (G20 num2) G21)
               (divide (num_total_G1 num4 quotient_G2) G22)
               (remainder-of (G22 num2) G23)
               (divide (num_total_G1 num5 quotient_G3) G24)
               (remainder-of (G24 num2) G25)
               (not-equal (num2 num0) G26)
	       (number-extra-spaces (obj_plants obj_garden num2) G27)
               (points-out (renshaw_daughter G21) G28)
               (points-out (renshaw_daughter G23) G29)
               (points-out (renshaw_daughter G25) G30)
;she suggests dividing num_total_G1 by 6, leaving 0 remainder
               (suggests (renshaw_daughter G32) G31)
               (divide (num_total_G1 num6 quotient_G4) G32)
               (remainder-of (G32 num0) G33)
;division by 6 is a success because it leaves 0 remainder
               (success (G32) G34)
               (cause (G33 G34) G35)
;all the numbers mentioned are numbers
               (number (num_total_G0) G36)
               (number (num_total_G1) G37)
               (number (num0) G38)
               (number (num2) G39)
               (number (num10) G40)
               (number (num4) G41)
               (number (num5) G42)
               (number (num6) G43)
               (number (quotient_G1) G44)
               (number (quotient_G2) G45)
               (number (quotient_G3) G46)
               (number (quotient_G4) G47)
;various numbers have specific values
               (zero (num0) G48)
               (two (num2) G49)
               (ten (num10) G50)
               (four (num4) G51)
               (five (num5) G52)
               (six (num6) G53)
;divisions by 10, 4, 5, and 6 constitute possible selections of
;   kinds of plants 
               (kind-of (obj_kind10 obj_plants) G54)
               (kind-of (obj_kind4 obj_plants) G55)
               (kind-of (obj_kind5 obj_plants) G56)
               (kind-of (obj_kind6 obj_plants) G57)
;10, 4, 5, and 6 are possible numbers per kind
               (number-per-kind (obj_kind10 num10) G58)
               (number-per-kind (obj_kind4 num4) G59)
               (number-per-kind (obj_kind5 num5) G60)
               (number-per-kind (obj_kind6 num6) G61)
;dividing num_total_G0 by 10, 4, and 5, and num_total_G1 by 10, 4, 5
;   and 6 are the first to seventh solution attempts, respectively
               (first-try (G5) G62)
               (second-try (G7) G63)
               (third-try (G9) G64)
               (fourth-try (G20) G65)
               (fifth-try (G22) G66)
               (sixth-try (G24) G67)
               (seventh-try (G32) G68)
;10, 4, 5, and 6 are the first to fourth divisors considered, respectively
               (first-divisor (num10) G69)
               (second-divisor (num4) G70)
               (third-divisor (num5) G71)
               (fourth-divisor (num6) G72)
              )
             )
            '(goals
;the goal is to determine num_total_G1
             ((known (num_total_G1) G73)  
;which can be divided by num_Gx to give quotient_Gx with 0 remainder
               (divide (num_total_G1 num_Gx quotient_Gx) G74)
               (remainder-of (G74 num0) G75)
;and is the smallest value possible
	      (subtract (num_total_G1 num0 num_difference_G) G76)
	      (minimal (num_difference_G) G77)
;where num_Gx = 6 and quotient_Gx = quotient_G4
               (equal (num_Gx num6) G78)
               (equal (quotient_Gx quotient_G4) G79)
               (number (num_Gx) G80)
               (number (quotient_Gx) G81)
;;; Added 9/12
	       (number (num_difference_G) G82)
              )
            )
           '(solution
;find the least common multiple of 10, 4, and 5
             ((find-lcm! (num10 num4 num5 lcm_G) G83)
	      (number (lcm_G) G84)
;find multiples of lcm_G
	      (find-multiples! (lcm_G list-of-multiples_G) G85)
	      (list (list-of-multiples_G) G86)
;add 2 to each number in the resulting list of multiples
	      (list-plus! (num2 list-of-multiples_G corrected-list_G) G87)
	      (list (corrected-list_G) G88)
;num_total_G1 is the lowest multiple of 6 in the corrected list  
              (find-least-multiple! (num6 corrected-list_G num_tota1_G1) G89)
              )
           )
  )         
)


(defun make_single_neg ()

(make_struc 'single_neg
	    'problem
	    '(start
;award recipients sit on the auditorium stage
              ((award-recipients (obj_recipients) A1)
               (stage (obj_stage) A2)
               (sit-on (obj_recipients obj_stage) A3)
;the number of award recipients is num_total_A
               (number-of (obj_recipients num_total_A) A4)
;dividing recipients by 9 or 6 leaves a non-zero remainder of 1
               (divide (num_total_A num9 quotient_A1) A5)
               (remainder-of (A5 num1) A6)
               (not-equal (num1 num0) A7)
               (number-left-out (obj_recipients obj_stage num1) A8)
               (divide (num_total_A num6 quotient_A2) A9)
               (remainder-of (A9 num1) A10)
;dividing recipients by 4 leaves a non-zero remainder of 3
               (divide (num_total_A num4 quotient_A3) A11)
               (remainder-of (A11 num3) A12)
               (not-equal (num3 num0) A13)
               (number-left-out (obj_recipients obj_stage num3) A14)
;assistant dean 1 suggests dividing by 9, assistant dean 2 suggests 6,
;and both suggest 4.
               (person (obj_asst1) A15)
               (person (obj_asst2) A16)
               (suggests (obj_asst1 A5) A17)
               (suggests (obj_asst2 A9) A18)
               (suggests (obj_asst1 A11) A19)
               (suggests (obj_asst2 A11) A20)
;dean orders division by 5, which results in 0 remainder
               (person (obj_dean) A21)
               (orders (obj_dean A23) A22)
               (divide (num_total_A num5 quotient_A4) A23)
               (remainder-of (A23 num0) A24)
;division by 5 is a success because the remainder is 0
               (success (A23) A25)
               (cause (A24 A25) A26)
;all the numbers mentioned are numbers
               (number (num_total_A) A27)
               (number (num9) A28)
               (number (num6) A29)
               (number (num4) A30)
               (number (num5) A31)
               (number (num1) A32)
               (number (num0) A33)
               (number (num3) A34)
               (number (quotient_A1) A35)
               (number (quotient_A2) A36)
               (number (quotient_A3) A37)
               (number (quotient_A4) A38)
;various numbers have specific values
               (nine (num9) A39)
               (six (num6) A40)
               (four (num4) A41)
               (five (num5) A42)
               (one (num1) A43)
               (zero (num0) A44)
               (three (num3) A45)
;divisions by 9, 6, 4, and 5 constitute possible selections of groupings
;of the award recipients
               (grouping-of (obj_row9 obj_recipients) A46)
               (grouping-of (obj_column6 obj_recipients) A47)
               (grouping-of (obj_row4 obj_recipients) A48)
               (grouping-of (obj_row5 obj_recipients) A49)
               (row-groups (obj_row9) A50)
               (column-groups (obj_column6) A51)
               (row-groups (obj_row4) A52)
               (row-groups (obj_row5) A53)
;9, 6, 4, and 5 are possible numbers per group
               (number-per-group (obj_row9 num9) A54)
               (number-per-group (obj_column6 num6) A55)
               (number-per-group (obj_row4 num4) A56)
               (number-per-group (obj_row5 num5) A57)
;groupings by 9 and by 6 result in the same arrangement of award recipients
               (same-arrangement (A46 A47) A58)
;dividing by 9, 6, 4, and 5 are the first to fourth solution
;attempts, respectively
               (first-try (A5) A60)
               (second-try (A9) A61)
               (third-try (A11) A62)
               (fourth-try (A23) A63)
;9, 6, 4, and 5 are the first to fourth divisors considered, respectively
               (first-divisor (num9) A64)
               (second-divisor (num6) A65)
               (third-divisor (num4) A66)
               (fourth-divisor (num5) A67)
              )
	     )
            '(goals
;the goal is to determine num_total_A
               ((known (num_total_A) A68)
;which can be divided by num_Ax to give quotient_Ax with 0 remainder
               (divide (num_total_A num_Ax quotient_Ax) A69)
               (remainder-of (A69 num0a) A70)
;where num_Ax = 5 and quotient_Ax = quotient_A4
               (equal (num_Ax num5) A71)
               (equal (quotient_Ax quotient_A4) A72)
;and num_total_A is greater than 20 and less than 120
               (greater-than (num_total_A num20) A73)
               (less-than (num_total_A num120) A74)
               (number (num_Ax) A75)
               (number (quotient_Ax) A76)
               (number (num20) A77)
               (number (num120) A78)
               (twenty (num20) A79)
               (one-hundred-twenty (num120) A80)
              )
             )
           '(solution
;multiply the numbers per group for the two identical groupings
;then add the remainder for that arrangement
             ((times! (num9 num6 product) A81)
	      (number (product) A82)
              (plus! (product num1 num_total_A) A83)
;this procedure is enabled because divisions by 9 and 6 result in the same
;arrangement of people.
              (enables (A58 A81) A84)
	      (enables (A58 A83) A85)
;check the answer
              (verify! (A12) A86)
              (verify! (A24) A87)
              (verify! (A73) A88)
              (verify! (A74) A89)
             )
            )
  )
)

         

;garden problem combined with award problem 
(defun make_neg_and_garden ()

(make_struc 'neg_and_garden
            'problem
            '(start

;;;;;;;;;;;;;;;;;;;;;;;; AWARD START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;award recipients sit on the auditorium stage
              ((award-recipients (obj_recipients) A1)
               (stage (obj_stage) A2)
               (sit-on (obj_recipients obj_stage) A3)
;the number of award recipients is num_total_A
               (number-of (obj_recipients num_total_A) A4)
;dividing recipients by 9 or 6 leaves a non-zero remainder of 1
               (divide (num_total_A num9 quotient_A1) A5)
               (remainder-of (A5 num1) A6)
               (not-equal (num1 num0a) A7)
               (number-left-out (obj_recipients obj_stage num1) A8)
               (divide (num_total_A num6a quotient_A2) A9)
               (remainder-of (A9 num1) A10)
;dividing recipients by 4 leaves a non-zero remainder of 3
               (divide (num_total_A num4a quotient_A3) A11)
               (remainder-of (A11 num3) A12)
               (not-equal (num3 num0a) A13)
               (number-left-out (obj_recipients obj_stage num3) A14)
;assistant dean 1 suggests dividing by 9, assistant dean 2 suggests 6,
;and both suggest 4.
               (person (obj_asst1) A15)
               (person (obj_asst2) A16)
               (suggests (obj_asst1 A5) A17)
               (suggests (obj_asst2 A9) A18)
               (suggests (obj_asst1 A11) A19)
               (suggests (obj_asst2 A11) A20)
;dean orders division by 5, which results in 0 remainder
               (person (obj_dean) A21)
               (orders (obj_dean A23) A22)
               (divide (num_total_A num5a quotient_A4) A23)
               (remainder-of (A23 num0a) A24)
;division by 5 is a success because the remainder is 0
               (success (A23) A25)
               (cause (A24 A25) A26)
;all the numbers mentioned are numbers
               (number (num_total_A) A27)
               (number (num9) A28)
               (number (num6a) A29)
               (number (num4a) A30)
               (number (num5a) A31)
               (number (num1) A32)
               (number (num0a) A33)
               (number (num3) A34)
               (number (quotient_A1) A35)
               (number (quotient_A2) A36)
               (number (quotient_A3) A37)
               (number (quotient_A4) A38)
;various numbers have specific values
               (nine (num9) A39)
               (six (num6a) A40)
               (four (num4a) A41)
               (five (num5a) A42)
               (one (num1) A43)
               (zero (num0a) A44)
               (three (num3) A45)
;divisions by 9, 6, 4, and 5 constitute possible selections of groupings
;of the award recipients
               (grouping-of (obj_row9 obj_recipients) A46)
               (grouping-of (obj_column6 obj_recipients) A47)
               (grouping-of (obj_row4 obj_recipients) A48)
               (grouping-of (obj_row5 obj_recipients) A49)
               (row-groups (obj_row9) A50)
               (column-groups (obj_column6) A51)
               (row-groups (obj_row4) A52)
               (row-groups (obj_row5) A53)
;9, 6, 4, and 5 are possible numbers per group
               (number-per-group (obj_row9 num9) A54)
               (number-per-group (obj_column6 num6a) A55)
               (number-per-group (obj_row4 num4a) A56)
               (number-per-group (obj_row5 num5a) A57)
;groupings by 9 and by 6 result in the same arrangement of award recipients
               (same-arrangement (A46 A47) A58)
;dividing by 9, 6, 4, and 5 are the first to fourth solution
;attempts, respectively
               (first-try (A5) A60)
               (second-try (A9) A61)
               (third-try (A11) A62)
               (fourth-try (A23) A63)
;9, 6, 4, and 5 are the first to fourth divisors considered, respectively
               (first-divisor (num9) A64)
               (second-divisor (num6a) A65)
               (third-divisor (num4a) A66)
               (fourth-divisor (num5a) A67)

;;;;;;;;;;;;;;;;;;;;;;;; GARDEN START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;plants grow in a garden
               (plants (obj_plants) G1)
               (garden (obj_garden) G2)
               (grow-in (obj_plants obj_garden) G3)
;the number of plants first considered is num_total_G0
               (number-of (obj_plants num_total_G0) G4)
;dividing num_total_G0 by 10, 4, or 5 leaves 0 remainder 
               (divide (num_total_G0 num10 quotient_G1) G5)
               (remainder-of (G5 num0g) G6)
               (divide (num_total_G0 num4g quotient_G2) G7)
               (remainder-of (G7 num0g) G8)
               (divide (num_total_G0 num5g quotient_G3) G9)
               (remainder-of (G9 num0g) G10)
;Mr. Renshaw suggests dividing by 10, Mrs. Renshaw suggests
;   dividing by 4, and both suggest dividing by 5
               (person (mr_renshaw) G11)
               (person (mrs_renshaw) G12)
               (suggests (mr_renshaw G5) G13)
               (suggests (mrs_renshaw G7) G14)
               (suggests (mr_renshaw G9) G15)
               (suggests (mrs_renshaw G9) G16)
;the Renshaws' daughter points out that the total number of plants,
;   num_total_G0, can be increased by 2 to num_total_G1
               (person (renshaw_daughter) G17)
               (plus (num_total_G0 num2 num_total_G1) G18)
               (points-out (renshaw_daughter G18) G19)
;she points out that dividing by 10, 4, or 5 into num_total_G1
;   leaves a non-zero remainder of 2
               (divide (num_total_G1 num10 quotient_G1) G20) 
               (remainder-of (G20 num2) G21)
               (divide (num_total_G1 num4g quotient_G2) G22)
               (remainder-of (G22 num2) G23)
               (divide (num_total_G1 num5g quotient_G3) G24)
               (remainder-of (G24 num2) G25)
               (not-equal (num2 num0g) G26)
	       (number-extra-spaces (obj_plants obj_garden num2) G27)
               (points-out (renshaw_daughter G21) G28)
               (points-out (renshaw_daughter G23) G29)
               (points-out (renshaw_daughter G25) G30)
;she suggests dividing num_total_G1 by 6, leaving 0 remainder
               (suggests (renshaw_daughter G32) G31)
               (divide (num_total_G1 num6g quotient_G4) G32)
               (remainder-of (G32 num0g) G33)
;division by 6 is a success because it leaves 0 remainder
               (success (G32) G34)
               (cause (G33 G34) G35)
;all the numbers mentioned are numbers
               (number (num_total_G0) G36)
               (number (num_total_G1) G37)
               (number (num0g) G38)
               (number (num2) G39)
               (number (num10) G40)
               (number (num4g) G41)
               (number (num5g) G42)
               (number (num6g) G43)
               (number (quotient_G1) G44)
               (number (quotient_G2) G45)
               (number (quotient_G3) G46)
               (number (quotient_G4) G47)
;various numbers have specific values
               (zero (num0g) G48)
               (two (num2) G49)
               (ten (num10) G50)
               (four (num4g) G51)
               (five (num5g) G52)
               (six (num6g) G53)
;divisions by 10, 4, 5, and 6 constitute possible selections of
;   kinds of plants 
               (kind-of (obj_kind10 obj_plants) G54)
               (kind-of (obj_kind4 obj_plants) G55)
               (kind-of (obj_kind5 obj_plants) G56)
               (kind-of (obj_kind6 obj_plants) G57)
;10, 4, 5, and 6 are possible numbers per kind
               (number-per-kind (obj_kind10 num10) G58)
               (number-per-kind (obj_kind4 num4g) G59)
               (number-per-kind (obj_kind5 num5g) G60)
               (number-per-kind (obj_kind6 num6g) G61)
;dividing num_total_G0 by 10, 4, and 5, and num_total_G1 by 10, 4, 5
;   and 6 are the first to seventh solution attempts, respectively
               (first-try (G5) G62)
               (second-try (G7) G63)
               (third-try (G9) G64)
               (fourth-try (G20) G65)
               (fifth-try (G22) G66)
               (sixth-try (G24) G67)
               (seventh-try (G32) G68)
;10, 4, 5, and 6 are the first to fourth divisors considered, respectively
               (first-divisor (num10) G69)
               (second-divisor (num4g) G70)
               (third-divisor (num5g) G71)
               (fourth-divisor (num6g) G72)
             

              )
	     )
            '(goals

;;;;;;;;;;;;;;;;;;;;;;;; AWARD GOALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the goal is to determine num_total_A
               ((known (num_total_A) A68)
;which can be divided by num_Ax to give quotient_Ax with 0 remainder
               (divide (num_total_A num_Ax quotient_Ax) A69)
               (remainder-of (A69 num0a) A70)
;where num_Ax = 5 and quotient_Ax = quotient_A4
               (equal (num_Ax num5a) A71)
               (equal (quotient_Ax quotient_A4) A72)
;and num_total_A is greater than 20 and less than 120
               (greater-than (num_total_A num20) A73)
               (less-than (num_total_A num120) A74)
               (number (num_Ax) A75)
               (number (quotient_Ax) A76)
               (number (num20) A77)
               (number (num120) A78)
               (twenty (num20) A79)
               (one-hundred-twenty (num120) A80)

;;;;;;;;;;;;;;;;;;;;;;;; GARDEN GOALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the goal is to determine num_total_G1
              (known (num_total_G1) G73)  
;which can be divided by num_Gx to give quotient_Gx with 0 remainder
               (divide (num_total_G1 num_Gx quotient_Gx) G74)
               (remainder-of (G74 num0g) G75)
;and is the smallest value possible
	      (subtract (num_total_G1 num0g num_difference_G) G76)
	      (minimal (num_difference_G) G77)
;where num_Gx = 6 and quotient_Gx = quotient_G4
               (equal (num_Gx num6g) G78)
               (equal (quotient_Gx quotient_G4) G79)
               (number (num_Gx) G80)
               (number (quotient_Gx) G81)
;;; Added 9/12
	       (number (num_difference_G) G82)

              )
             )
                          
           '(solution

;;;;;;;;;;;;;;;;;;;;;;;; AWARD SOLUTION ;;;;;;;;;;;;;;;;;;;;;;;;;;

;multiply the numbers per group for the two identical groupings
;then add the remainder for that arrangement
;multiply the numbers per group for the two identical groupings
;then add the remainder for that arrangement
             ((times! (num9 num6 product) A81)
	      (number (product) A82)
              (plus! (product num1 num_total_A) A83)
;this procedure is enabled because divisions by 9 and 6 result in the same
;arrangement of people.
              (enables (A58 A81) A84)
	      (enables (A58 A83) A85)
;check the answer
              (verify! (A12) A86)
              (verify! (A24) A87)
              (verify! (A73) A88)
              (verify! (A74) A89)

;;;;;;;;;;;;;;;;;;;;;;;; GARDEN SOLUTION ;;;;;;;;;;;;;;;;;;;;;;;;;;

;find the least common multiple of 10, 4, and 5
             (find-lcm! (num10 num4g num5g lcm_G) G83)
	      (number (lcm_G) G84)
;find multiples of lcm_G
	      (find-multiples! (lcm_G list-of-multiples_G) G85)
	      (list (list-of-multiples_G) G86)
;add 2 to each number in the resulting list of multiples
	      (list-plus! (num2 list-of-multiples_G corrected-list_G) G87)
	      (list (corrected-list_G) G88)
;num_total_G1 is the lowest multiple of 6 in the corrected list  
              (find-least-multiple! (num6g corrected-list_G num_tota1_G1) G89)

              )
           )
  )
)








