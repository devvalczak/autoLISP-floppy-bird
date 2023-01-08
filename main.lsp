; ==========================
; start

(setvar "INTELLIGENTUPDATE" 0)

(setq app (vlax-get-acad-object))
(setq active_doc (vla-get-activedocument app))
(setq mspace (vla-get-modelspace active_doc))
(setq blocksrefL NIL)

; ==========================
; referencje

(defun get_blocks ()
  (setq blocksrefL NIL)
  (setq floor_list NIL)

  (vlax-for item mspace
    (if (= (vla-get-objectname item) "AcDbBlockReference")
      (setq blocksrefL (append blocksrefL (list item)))
    )
  )

  (foreach block blocksrefL
    (if (= (vla-get-name block) bird_block_name)
      (setq bird_block_ref block)
    )
    (if (= (vla-get-name block) "points-background")
      (setq point_block_ref block)
    )
    (if (= (vla-get-name block) "floor")
      (setq floor_list (append floor_list (list block)))
    )
  )
)

; ==========================
; zmienne

(setq i 0)
(setq end_game NIL)
(setq was_pressed NIL)
(setq points 0)

; fizyka
(setq jump_speed 10)
(setq vert_speed 0)
(setq fall_speed 0.4)

; rury
(setq pipes_block_name "pipes")
(setq pipes_list NIL)
(setq insertion_point (vlax-3d-point 4000 1500 0))
(setq frame_spawn_counter 0)

; ptak
(setq bird_block_name "ptak")
(setq start_point (vlax-3d-point 1000 1500 0))

; ==========================
; metody
(defun move_bird ()
  (vla-move bird_block_ref (vlax-3d-point '(0 0 0)) (vlax-3d-point (list 0 vert_speed 0)))
  (vla-put-rotation bird_block_ref (* pi (/ (* 2 vert_speed) 180.0)))
)

(defun move_pipe (ref)
  (vla-move ref (vlax-3d-point '(0 0 0)) (vlax-3d-point '(-10 0 0)))
)

(defun LM:rand ( / a c m )
  (setq m   4294967296.0
        a   1664525.0
        c   1013904223.0
        $xn (rem (+ c (* a (cond ($xn) ((getvar 'date))))) m)
  )
  (/ $xn m)
)

(defun LM:vl-setattributevalue ( blk tag val )
  (setq tag (strcase tag))
  (vl-some
      '(lambda ( att )
          (if (= tag (strcase (vla-get-tagstring att)))
              (progn (vla-put-textstring att val) val)
          )
      )
      (vlax-invoke blk 'getattributes)
  )
)

(defun addBlock ()
  (setq insertion_point (vlax-3d-point (list 4000 (+ 1000 (* (LM:rand) 1000)) 0)))
  (vla-insertblock mspace insertion_point pipes_block_name 1 1 1 0)
)

; one time - testy


; koniec testów

; ==========================
; komendy

(defun c:PLAY ()
  (setvar "clayer" "0")
  (setq choice NIL)
  (command "_view" "TOP")
  (command "_-LAYER" "OFF" "game_over" "")
  (command "_-LAYER" "OFF" "game" "")
  (command "_-LAYER" "ON" "menu" "")

  ; todo ustalić jakieś sensowne zmienne pod to:
  (vla-ZoomWindow app (vlax-3d-point '(0 0 0)) (vlax-3d-point '(7000 4000 0)))

  (while (= choice NIL)
    (setq choice (ssget "_+.:E:S"))
    (if (= choice NIL)
      (setq choice NIL)
      (if (and
          (/= (cdr (assoc 2 (entget (ssname choice 0)))) "start-btn")
          (/= (cdr (assoc 2 (entget (ssname choice 0)))) "exit-btn")
        )
        (setq choice NIL)
      )
    )
  )

  (if (= (cdr (assoc 2 (entget (ssname choice 0)))) "start-btn")
    (progn
      (command "_-LAYER" "OFF" "menu" "")
      (C:BIRD)
    )
  )
)

(defun game_over ()
  (foreach pipe pipes_list
    (vla-delete pipe)
  )
  (vla-delete bird_block_ref)

  (setq game_over_choice NIL)
  (command "_-LAYER" "ON" "game_over" "")

  (while (= game_over_choice NIL)
    (setq game_over_choice (ssget "_+.:E:S"))

    (if (/= game_over_choice NIL)
      (if (and
          (/= (cdr (assoc 2 (entget (ssname game_over_choice 0)))) "try-again-btn")
          (/= (cdr (assoc 2 (entget (ssname game_over_choice 0)))) "exit-btn")
        )
        (setq game_over_choice NIL)
      )
    )
  )

  (if (= (cdr (assoc 2 (entget (ssname game_over_choice 0)))) "try-again-btn")
    (progn
      (command "_-LAYER" "OFF" "game_over" "")
      (C:BIRD)
    )
  )
  (if (= (cdr (assoc 2 (entget (ssname game_over_choice 0)))) "exit-btn")
    (progn
      (command "_-LAYER" "OFF" "game_over" "")
      (C:PLAY)
    )
  )
)

(defun c:BIRD ()
  (command "_-LAYER" "ON" "game" "")

  (setq end_game NIL)
  (setq frame_spawn_counter 0)
  (setq points 0)
  (setq vert_speed 0)

  ; dodanie pierwszych rur oraz gracza
  (vla-insertblock mspace start_point bird_block_name 1 1 1 0)
  (setq pipes_list (append (list (addBlock))))

  ; pobranie referencji bloku ptaka
  (get_blocks)

  ; zerowanie punktów na wyświetlaczu
  (LM:vl-setattributevalue point_block_ref "TEXT" 0)

  ; glowna petla gry
  (while (= end_game NIL)
    ; wyjscie z gry przyciskiem Q
    (if (< (acet-sys-keystate 81) 0)
      (setq end_game T)
    )

    ; detekcja spacji
    (if (< (acet-sys-keystate 32) 0)
      (progn
        (if (= was_pressed NIL)
          (progn
            (setq was_pressed T)
            (if (> vert_speed 0)
              (setq vert_speed (+ vert_speed jump_speed))
              (setq vert_speed jump_speed)
            )
          )
        )
      )
      (progn
        (if (= was_pressed T)
          (setq was_pressed NIL)
        )
        (setq vert_speed (- vert_speed fall_speed))
      )
    )

    ; generowanie rur
    (setq frame_spawn_counter (1+ frame_spawn_counter))
    (if (= frame_spawn_counter 100)
      (progn
        (LM:vl-setattributevalue point_block_ref "TEXT" points)
        (setq points (1+ points))
        (setq frame_spawn_counter 0)
        (setq pipes_list (append pipes_list (list (addBlock))))
      )
    )

    ; usuwanie rur spoza mapy
    (if (< (car (vlax-safearray->list (vlax-variant-value (vlax-get-property (nth 0 pipes_list) "InsertionPoint")))) 0)
      (progn
        (vla-delete (nth 0 pipes_list))
        (setq pipes_list (vl-remove (nth 0 pipes_list) pipes_list))
      )
    )

    ; ruch rur oraz detekcja kolizji
    (foreach pipe_block pipes_list
      (move_pipe pipe_block)
      (setq intersect_value (safearray-value (vlax-variant-value (vla-intersectwith bird_block_ref pipe_block acExtendNone))))
      (if (/= intersect_value NIL)
        (progn
          (setq pipe_y_position (cadr (vlax-safearray->list (vlax-variant-value (vlax-get-property pipe_block "InsertionPoint")))))
          (if (or (>= (nth 1 intersect_value) (+ pipe_y_position 355)) (<= (nth 4 intersect_value) (- pipe_y_position 355)))
            (progn
              (setq end_game T)
            )
          )
        )
      )
    )
    (foreach floor floor_list
      (setq floor_intersect_value (safearray-value (vlax-variant-value (vla-intersectwith bird_block_ref floor acExtendNone))))
      (if (/= floor_intersect_value NIL)
        (setq end_game T)
      )
    )

    (move_bird)

    (command "_.DELAY" 10)
  )

  (if (= end_game T)
    (game_over)
  )
)

(c:PLAY)