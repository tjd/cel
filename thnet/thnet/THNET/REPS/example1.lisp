; FILE:       acme.example
; PURPOSE:    testing and analogical mapping
; PROGRAMMER: Paul Thagard
; CREATED:    6-15-87
; UPDATED:    5-13-88

(defaults 'acme)

; Problem 1:  use an army to capture a fortress

(defun make_struc1 ()
  (make_struc 'capture-fortress
              'problem
              '(start
                 ((army (obj_army) cf1)
                  (roads (obj_roads) cf2)
                  (fortress (obj_fortress) cf3)
                  (lead_to (obj_roads obj_fortress) cf4)
                  (go_down (obj_army obj_roads) cf5)
                 )
               )
              '(goals
                 ((capture (obj_army obj_fortress) cf6)
                  (safe (obj_army) cf7)
                 )
               )
  )
)

; Problem 2:  use a ray to destroy a tumor 

(defun make_struc2 ()
  (make_struc 'destroy-tumor           ; name
              'problem
              '(start
                 ((ray-source (obj_ray) dt1)
                  (tissue (obj_tissue) dt2)
                  (tumor (obj_tumor) dt3)
                  (surround (obj_tissue obj_tumor) dt4)
                  (outside (obj_ray obj_flesh) dt5)
                 )
               )
              '(goals
                 ((destroy ( obj_ray obj_tumor  ) dt6)
                  (alive (obj_tissue) dt7)
                 )
               )
  )
)

; ********************************************************

(defun acme () (clear_net) 
               (make_struc1)
               (make_struc2)
               (setq experiment '"Army & ray analogy.")
               (constraint_map 'capture-fortress 'destroy-tumor)
               (acme-mode)  
;               (debug_els '(ray-source))
               (graph_maps '(ray-source army tumor))
               (ra)
                 
)




