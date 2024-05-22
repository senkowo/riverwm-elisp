
;;; keymap stuff

;;; keymap helper functions

(defun river-keymap--create-map (mode)
  (macroexpand `(river-declare-keymap ,mode)))

(defun river-keymap--bind (mode body-lst)
  (mapcar (lambda (bind-stmnt)
            (macroexpand
             `(river-set
                (map (,mode kbd ,bind-stmnt)
                     (-release ,mode kbd ,(car bind-stmnt)
                               enter-mode normal)))))
          body-lst))

;;; keymap functions

(defmacro river-declare-keymap (keymap)
  (macroexpand `(river-set declare-mode ,(keymap-shorthand keymap))))

(defmacro river-keymap-new (&rest commmands)
  (declare (indent 1))
  (message "DEBUG: commands : %S" commands)
  (let ((main-mode (pop commands))
        (return-lst))
    (dolist (body-lst commands)
      (let ((key (pop body-lst)))
        (pcase key
          ('create-map
           (push (river-keymap--create-map main-mode)
                 return-lst))
          ('bind
           )
          ('exit-to
           (dolist (statement body-lst)
             (let ((dest-mode (car statement))
                   (binds-lst (cadr statement)))
               ;; create binds for exiting to mode
               ))
           )
          ('enter-from
           (dolist (statement body-lst)
             (let ((orig-mode (car statement))
                   (binds-lst (cadr statement)))
               ;; create binds for entering mode
               ))
           ;; 
           )
          (_
           (error "no matches!")))))))

