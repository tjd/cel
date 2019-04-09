; PURPOSE:    testing band/cookie/garden analogies
; PROGRAMMER: Keith Holyoak
; CREATED:    7-03-1989; 
; UPDATED:    9-13-89 by Eric Melz
                                 



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
;;; Added 9/12, changed 9/13
	       (number-left-out (obj_members obj_band num1) B7A)
               (divide (num_total_B num8 quotient_B2) B8)
               (remainder-of (B8 num1) B9)
               (divide (num_total_B num3 quotient_B3) B10)
               (remainder-of (B10 num1) B11)
;the director orders the above attempts
               (person (obj_director) B12)
               (orders (obj_director B5) B13)
               (orders (obj_director B8) B14)
               (orders (obj_director B10) B15)
;Andrew orders division by 5, which results in 0 remainder
               (member-of (andrew obj_members) B16)
               (person (andrew) B17)
;;; Added 9/12
	       (left-out-of (andrew obj_band) B17A)
               (orders (andrew B19) B18)
               (divide (num_total_B num5 quotient_B4) B19)
               (remainder-of (B19 num0) B20)
;division by 5 is a success because the remainder is 0
               (success (B19) B21)
               (cause (B20 B21) B22)
;all the numbers mentioned are numbers
               (number (num_total_B) B23)
               (number (num1) B24)
               (number (num0) B25)
               (number (num12) B26)
               (number (num8) B27)
               (number (num3) B28)
               (number (num5) B29)
               (number (quotient_B1) B30)
               (number (quotient_B2) B31)
               (number (quotient_B3) B32)
               (number (quotient_B4) B33)
;various numbers have specific values
               (zero (num0) B34)
               (one (num1) B35)
               (twelve (num12) B36)
               (eight (num8) B37)
               (three (num3) B38)
               (five (num5) B39)
;divisions by 12, 8, 3, and 5 constitute possible selections of groupings
;   of the members 
               (grouping-of (obj_row12 obj_members) B40)
               (grouping-of (obj_column8 obj_members) B41)
               (grouping-of (obj_row3 obj_members) B42)
               (grouping-of (obj_row5 obj_members) B43)
;12, 8, 3, and 5 are possible numbers per group
               (number-per-group (obj_row12 num12) B44)
               (number-per-group (obj_column8 num8) B45)
               (number-per-group (obj_row3 num3) B46)
               (number-per-group (obj_row5 num5) B47)
;dividing by 12, 8, 3, and 5 are the first to fourth solution
;  attempts, respectively
               (first-try (B5) B48)
               (second-try (B8) B49)
               (third-try (B10) B50)
               (fourth-try (B19) B51)
;12, 8, 3, and 5 are the first to fourth divisors considered, respectively
               (first-divisor (num12) B52)
               (second-divisor (num8) B53)
               (third-divisor (num3) B54)
               (fourth-divisor (num5) B55)
              )
             )
            '(goals
;the goal is to determine num_total_B
              ((known (num_total_B) B56)
;which can be divided by num_Bx to give quotient_Bx with 0 remainder
               (divide (num_total_B num_Bx quotient_Bx) B57)
               (remainder-of (B57 num0) B58)                
;where num_Bx = 5 and quotient_Bx = quotient_B4
               (equal (num_Bx num5) B59)
               (equal (quotient_Bx quotient_B4) B60)
;and num_total_B is greater than 45 and less than 200
               (greater-than (num_total_B num44) B61)
               (less-than (num_total_B num200) B62)
               (number (num_Bx) B63)
               (number (quotient_Bx) B64)
               (number (num44) B65)
               (number (num200) B66)
               (forty-four (num44) B67)
               (two-hundred (num200) B68)
              )
            )
  )         
)
 
;cookie problem (target)
(defun make_cookie ()

(make_struc 'cookie
            'problem
            '(start
;cookies go into bags
              ((cookies (obj_cookies) C1)
               (bags (obj_bags) C2)
               (go-into (obj_cookies obj_bags) C3)
;the number of cookies is num_total_C
               (number-of (obj_cookies num_total_C) C4)
;dividing members by 16, 14, or 8 leaves a non-zero remainder of 6
               (divide (num_total_C num16 quotient_C1) C5)
               (remainder-of (C5 num6) C6)
               (not-equal (num6 num0) C7)
;;; Added 9/12; changed 9/13
	       (number-cookies-left (obj_cookies obj_bags num6) C7A)
               (divide (num_total_C num14 quotient_C2) C8)
               (remainder-of (C8 num6) C9)
               (divide (num_total_C num8 quotient_C3) C10)
               (remainder-of (C10 num6) C11)
;Elena makes the above attempts
               (person (elena) C12)
               (tries (elena C5) C13)
               (tries (elena C8) C14)
               (tries (elena C10) C15)
;Cindy suggests division by 9, which results in 0 remainder
               (person (cindy) C16)
               (suggests (cindy C18) C17)
               (divide (num_total_C num9 quotient_C4) C18)
               (remainder-of (C18 num0) C19)
;division by 9 is a success because the remainder is 0
               (success (C18) C20)
;;; typo fix 9/12
               (cause (C19 C20) C21)
;all the numbers mentioned are numbers
               (number (num_total_C) C22)
               (number (num6) C23)
               (number (num0) C24)
               (number (num16) C25)
               (number (num14) C26)
               (number (num8) C27)
               (number (num9) C28)
               (number (quotient_C1) C29)
               (number (quotient_C2) C30)
               (number (quotient_C3) C31)
               (number (quotient_C4) C32)
;various numbers have specific values
               (zero (num0) C33)
               (six (num6) C34)
               (sixteen (num16) C35)
               (fourteen (num14) C36)
               (eight (num8) C37)
               (nine (num9) C38)
;divisions by 16, 14, 8, and 9 constitute possible bags of cookies
               (bags-of (obj_bags16 obj_cookies) C39)
               (bags-of (obj_bags14 obj_cookies) C40)
               (bags-of (obj_bags8 obj_cookies) C41)
               (bags-of (obj_bags9 obj_cookies) C42)
;16, 14, 8, and 9 are possible numbers of cookies per bag
               (number-per-bag (obj_bags16 num16) C43)
               (number-per-bag (obj_bags14 num14) C44)
               (number-per-bag (obj_bags8 num8) C45)
               (number-per-bag (obj_bags9 num9) C46)
;dividing by 16, 14, 8, and 9 are the first to fourth solution
;  attempts, respectively
               (first-try (C5) C47)
               (second-try (C8) C48)
               (third-try (C10) C49)
               (fourth-try (C18) C50)
;16, 14, 8, and 9 are the first to fourth divisors considered, respectively
               (first-divisor (num16) C51)
               (second-divisor (num14) C52)
               (third-divisor (num8) C53)
               (fourth-divisor (num9) C54)
              )
             )
            '(goals
;the goal is to determine num_total_C
              ((known (num_total_C) C55)
;which can be divided by num_Cx to give quotient_Cx with 0 remainder
               (divide (num_total_C num_Cx quotient_Cx) C56)
               (remainder-of (C56 num0) C57)
;;; added 9/12
	       (subtract (num_total_C num0 num_difference_C) C57A)
	       (minimize (num_difference_C) C57B)
	       (number (num_difference_C) C61A)
;where num_Cx = 9 and quotient_Cx = quotient_C4
               (equal (num_Cx num9) C58)
               (equal (quotient_Cx quotient_C4) C59)
               (number (num_Cx) C60)                              
               (number (quotient_Cx) C61)
;;; added 9/12
	       (number (num_difference_C) C61A)
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
;;; added 9/12; changed 9/13
	       (number-extra-spaces (obj_plants obj_garden num2) G18A)
               (points-out (renshaw_daughter G18) G19)
;she points out that dividing by 10, 4, or 5 into num_total_G1
;   leaves a non-zero remainder of 2
;               (divide (num_total_G1 num10 quotient_G1) G20) 
;               (remainder-of (G20 num2) G21)
;               (divide (num_total_G1 num4 quotient_G2) G22)
;               (remainder-of (G22 num2) G23)
;               (divide (num_total_G1 num5 quotient_G3) G24)
;               (remainder-of (G24 num2) G25)
;               (not-equal (num2 num0) G26)
;               (points-out (renshaw_daughter G21) G27)
;               (points-out (renshaw_daughter G23) G28)
;               (points-out (renshaw_daughter G25) G29)
;she suggests dividing num_total_G1 by 6, leaving 0 remainder
               (suggests (renshaw_daughter G31) G30)
               (divide (num_total_G1 num6 quotient_G4) G31)
               (remainder-of (G31 num0) G32)
;division by 6 is a success because it leaves 0 remainder
               (success (G31) G33)
               (cause (G32 G33) G34)
;all the numbers mentioned are numbers
               (number (num_total_G0) G35)
               (number (num_total_G1) G36)
               (number (num0) G37)
               (number (num2) G38)
               (number (num10) G39)
               (number (num4) G40)
               (number (num5) G41)
               (number (num6) G42)
               (number (quotient_G1) G43)
               (number (quotient_G2) G44)
               (number (quotient_G3) G45)
               (number (quotient_G4) G46)
;various numbers have specific values
               (zero (num0) G47)
               (two (num2) G48)
               (ten (num10) G49)
               (four (num4) G50)
               (five (num5) G51)
               (six (num6) G52)
;divisions by 10, 4, 5, and 6 constitute possible selections of
;   kinds of plants 
               (kind-of (obj_kind10 obj_plants) G53)
               (kind-of (obj_kind4 obj_plants) G54)
               (kind-of (obj_kind5 obj_plants) G55)
               (kind-of (obj_kind6 obj_plants) G56)
;10, 4, 5, and 6 are possible numbers per kind
               (number-per-kind (obj_kind10 num10) G57)
               (number-per-kind (obj_kind4 num4) G58)
               (number-per-kind (obj_kind5 num5) G59)
               (number-per-kind (obj_kind6 num6) G60)
;dividing num_total_G0 by 10, 4, and 5, and num_total_G1 by 10, 4, 5
;   and 6 are the first to seventh solution attempts, respectively
               (first-try (G5) G61)
               (second-try (G7) G62)
               (third-try (G9) G63)
               (fourth-try (G20) G64)
               (fifth-try (G22) G65)
               (sixth-try (G24) G66)
               (seventh-try (G31) G67)
;10, 4, 5, and 6 are the first to fourth divisors considered, respectively
               (first-divisor (num10) G68)
               (second-divisor (num4) G69)
               (third-divisor (num5) G70)
               (fourth-divisor (num6) G71)
              )
             )
            '(goals
;the goal is to determine num_total_G1
             ((known (num_total_G1) G72)  
;which can be divided by num_Gx to give quotient_Gx with 0 remainder
               (divide (num_total_G1 num_Gx quotient_Gx) G73)
               (remainder-of (G73 num0) G74)
;;; Added 9/12
	      (subtract (num_total_G1 num0 num_difference_G) G74A)
	      (minimize (num_difference_G) G74B)
;where num_Gx = 6 and quotient_Gx = quotient_G4
               (equal (num_Gx num6) G75)
               (equal (quotient_Gx quotient_G4) G76)
               (number (num_Gx) G77)
               (number (quotient_Gx) G78)
;;; Added 9/12
	       (number (num_difference_G) G78A)
              )
            )
           '(solution
;find the least common multiple of 10, 4, and 5
             ((find-lcm! (num10 num4 num5 lcm_G) G79)
;find multiples of lcm_G
             (find-multiples! (lcm_G list-of-multiples_G) G80)
;add 2 to each number in the resulting list of multiples
              (plus! (num2 list-of-multiples_G corrected-list_G) G81)
;num_total_G1 is the lowest multiple of 6 in the corrected list  
              (find-least-multiple! (num6 corrected-list_G num_tota1_G1) G82)
              )
           )
  )         
)

;schema (source)
(defun make_schema ()

(make_struc 'schema
            'problem
            '(start
;some objects are in a location
              ((objects (obj_things) S1)
               (location (obj_location) S2)
               (at (obj_things obj_location) S3)
;the number of objects is num_total_S
               (number-of (obj_things num_total_S) S4)
;dividing num_total_S by num_a, num_b, or num_c leaves non-zero remainder num_r
               (divide (num_total_S num_a quotient_S1) S5)
               (remainder-of (S5 num_r) S6)
               (not-equal (num_r num0) S7)
;;; Added 9/12
	       (number-left (obj_things obj_location num_r) S7A)
               (divide (num_total_S num_b quotient_S2) S8)
               (remainder-of (S8 num_r) S9)
               (divide (num_total_S num_c quotient_S3) S10)
               (remainder-of (S10 num_r) S11)
;dividing num_total_S by num_d leaves 0 remainder 
               (divide (num_total_S num_d quotient_S4) S12)
               (remainder-of (S12 num0) S13)
;division by num_d is a success because it leaves 0 remainder
               (success (S12) S14) 
               (cause (S13 S14) S15)
;all the numbers mentioned are numbers
               (number (num_total_S) S16)
               (number (num0) S17)
               (number (num_r) S18)
               (number (num_a) S19)
               (number (num_b) S20)
               (number (num_c) S21)
               (number (num_d) S22)
               (number (quotient_S1) S23)
               (number (quotient_S2) S24)
               (number (quotient_S3) S25)
               (number (quotient_S4) S26)
;num0 = 0
               (zero (num0) S26A)
;divisions into obj_set_a, obj_set_b, obj_set_c, and obj_set_d constitute possible
;  selections of subsets of things 
               (subset-of (obj_set_a obj_things) S27)
               (subset-of (obj_set_b obj_things) S28)
               (subset-of (obj_set_c obj_things) S29)
               (subset-of (obj_set_d obj_things) S30)
;num_a, num_b, num_c, and num_d are possible numbers per subset
               (number-per-subset (obj_set_a num_a) S31)
               (number-per-subset (obj_set_b num_b) S32)
               (number-per-subset (obj_set_c num_c) S33)
               (number-per-subset (obj_set_d num_d) S34) 
;num_a, num_b, num_c, and num_d are the first to fourth divisors
;  considered, respectively
               (first-divisor (num_a) S35)
               (second-divisor (num_b) S36)
               (third-divisor (num_c) S37)
               (fourth-divisor (num_d) S38)
              )                    
             )
            '(goals 
;the goal is to determine num_total_S
             ((known (num_total_S) S39)  
;which can be divided by num_Sx to give quotient_Sx with 0 remainder
               (divide (num_total_S num_Sx quotient_Sx) S40)
               (remainder-of (S40 num0) S41)
;where num_Sx = num_d and quotient_Sx = quotient_S4
               (equal (num_Sx num_d) S42)
               (equal (quotient_Sx quotient_S4) S43)
;and num_total_S is greater than num_min and less than num_max
               (greater-than (num_total_S num_min) S44)
               (less-than (num_total_S num_max) S45)
               (number (num_Sx) S46)
               (number (quotient_Sx) S47)
               (number (num_min) S48)
               (number (num_max) S49)
              )
            )
           '(solution 
;find the least common multiple of num_a, num_b, and num_c
             ((find-lcm! (num_a num_b num_c lcm_S) S50)
;find multiples of lcm_S
             (find-multiples! (lcm_S list-of-multiples_S) S51)
;add num_r to each number in the resulting list of multiples
              (plus! (num_r list-of-multiples_S corrected-list_S) S52)
;truncate the corrected list to satisfy any range constraint
              (truncate-to-range! (corrected-list_S new-corrected-list_S) S53)
;num_total_S is the lowest multiple of num_d in the new corrected list
              (find-least-multiple! (num_d new-corrected-list_S num_total_S) S54)
              )
            )
  )         
)


(defun laura1 () 
   (clear_net) 
   (make_band)
   (make_garden)
   (setq experiment '"Mapping band problem to garden problem.")
   (nosimilar)
   (similar 'orders 'suggests .01)
   (constraint_map 'band 'garden)
   (acme-mode)
   (debug_els '())
   (ra)
) 

(defun laura2 () 
   (clear_net) 
   (make_band)
   (make_garden)
   (setq experiment '"Mapping band problem to garden problem.")
   (nosimilar)
   (similar 'orders 'suggests .01)
   (constraint_map 'band 'garden)
   (presumed '(num1=num2 B56=G72 B57=G73 B58=G74
         number-per-group=number-per-kind
         grouping-of=kind-of band-members=plants))     

   (acme-mode)
   (debug_els '())
   (ra)
)


(defun laura3 () 
   (clear_net) 
   (make_band)
   (make_garden)
   (setq experiment '"Mapping band problem to garden problem.")
   (nosimilar)
   (similar 'orders 'suggests .01)
   (constraint_map 'band 'garden)
   (presumed '(num12=num10 num8=num4 num3=num5 num1=num2 num5=num6))
   (acme-mode)
   (debug_els '())
   (ra)
)

(defun laura4 () 
   (clear_net) 
   (make_band)
   (make_schema)
   (setq experiment '"Mapping band problem to schema.")
   (nosimilar)
   (similar 'number-per-group 'number-per-subset .01)
   (similar 'grouping_of 'subset_of .01)
   (constraint_map 'band 'schema)
   (acme-mode)
   (debug_els '())
   (ra)
)

(defun laura1A () 
   (clear_net) 
   (make_cookie)
   (make_garden)
   (setq experiment '"Mapping cookie problem to garden problem.")
   (constraint_map 'cookie 'garden)
   (acme-mode)
   (debug_els '())
   (ra)
) 

(defun laura2A () 
   (clear_net) 
   (make_cookie)
   (make_garden)
   (setq experiment '"Mapping cookie problem to garden problem.")
   (constraint_map 'cookie 'garden)
   (presumed '(num6=num2 C55=G72 C56=G73 C57=G74
         number-per-bag=number-per-kind
         bags-of=kind-of cookies=plants))     

   (acme-mode)
   (debug_els '())
   (ra)
)

              
(defun laura3A () 
   (clear_net) 
   (make_cookie)
   (make_garden)
   (setq experiment '"Mapping cookie problem to garden problem.")
   (constraint_map 'cookie 'garden)
   (presumed '(num16=num10 num14=num4 num8=num5 num6=num2 num9=num6))
   (acme-mode)
   (debug_els '())
   (ra)
)

(defun laura4A () 
   (clear_net) 
   (make_cookie)
   (make_schema)
   (setq experiment '"Mapping cookie problem to schema.")
   (nosimilar)
   (similar 'number-per-bag 'number-per-subset .01)
   (similar 'bags-of 'subset-of .01)
   (constraint_map 'cookie 'schema)
   (acme-mode)
   (debug_els '())
   (ra)
)

