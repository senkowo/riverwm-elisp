
;; shell stuff

(defun river-shell-run--parse-output (raw)
  (let* ((exit-code
          (string-to-number		; conv to num
           (replace-regexp-in-string
            "^\\([0-9]+\\)[[:space:]].*" "\\1" raw)))
         (output
          (let (orig (replace-regexp-in-string
                      "^[0-9]+[[:space:]]\\(.*\\)" "\\1" raw))
            (if (not (equal "" orig))
                (substring orig 0 -1)
              orig))))
    (cons exit-code output)))

(defun river-shell-run (commands-str)
  "Run COMMANDS-STR in a new shell and log output to *river-log*."
  (unless (stringp commands-str)
    (error "Input must be a string."))
  (cl-flet ((log-to-buf '(lambda (x) (bufr-message "*river-log*" x))))
    (let ((cmd (format "out=$(%s 2>&1) ; echo \"$? $out\"" commands-str))
          (river-exit-code)
          (river-output))
      ;; log before running
      (let ((mesg (format "> %s <" commands-str)))
        (log-to-buf mesg)
        (message "LOG: %s" mesg))
      ;; exec & get output
      (let ((exec-output-parsed (river-shell-run--parse-output
                                 (shell-command-to-string cmd))))
        (setq river-exit-code (car exec-output-parsed))
        (setq river-output (cdr exec-output-parsed)))
      ;; error/log based on outputs
      (pcase river-exit-code
        (0 (when-let ((has-value? (not (string-empty-p river-output)))
                      (mesg (format " (%s)" river-output)))
             (message mesg)
             (log-to-buf mesg)))
        (_ (let ((mesg (format "ERROR: %s" river-output)))
             (message mesg)
             (log-to-buf mesg)))))))
