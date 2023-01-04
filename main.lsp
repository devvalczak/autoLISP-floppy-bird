
; start

; (setvar "INTELLIGENTUPDATE" 0)

(setq app (vlax-get-acad-object))
(setq active_doc (vla-get-activedocument app))

(setq mspace (vla-get-modelspace active_doc))


; referencje

(vlax-for item mspace
  (if (= (vla-get-objectname item) "AcDbBlockReference")
  	(setq blocksrefL (append blocksrefL (list item)))
  )
)

(setq bird_block_ref (nth 0 blocksrefL))

; loop

(defun move ()
  (setq position (vlax-safearray->list (vlax-variant-value (vlax-get-property bird_block_ref "InsertionPoint"))))
  (setq y (- (cadr position) 1))
  (setq new_position (list (car position) y 0))
  (vla-move bird_block_ref (vlax-3d-point position) (vlax-3d-point new_position))
)


(setq i 0)
(setq end_game NIL)

(while (= end_game NIL)
  (setq key (grread nil 13 0))
  (print key)

  (cond (
    (= 113 (cadr key)) (setq end_game T)
  ))

  (move)

  (command "_.delay" 15)
)
