
;; lib file for init-elisp config

(require 'cl-lib)
(require 'cl-extra)
(require 'cl-macs)

;;; Important variables

;; (defvar river-mod "")
;; (defvar river-alt "")
;; (defvar river-mod-formal "")
;; (defvar river-alt-formal "")


;;; Helper functions


(defun bufr-message (bufname message)
  "Inserts formatted string MESSAGE into the dedicated buffer named BUFNAME."

  (with-current-buffer (get-buffer-create bufname)
    (goto-char (point-max))
    (insert (concat message "\n"))))

(defun lst-to-str-maybe (lst)
  "If LST is a list, concat all elements into a string.
Otherwise return LST without changes."
  (if (listp lst)
      (mapconcat (lambda (elem)
		   (format "%s" elem))
		 lst " ")
    lst))

(defun kbd--split-str-to-lst (kbd-str)
  "Split KBD-STR at each \"-\" unless in <arrow-brackets>, 
return as list."
  (let ((raw-lst (split-string kbd-str "-"))
	(new-lst '()))
    (while-let ((head (pop raw-lst)))
      ;; (message "DEBUG: top head: %s" head)
      (if (cl-search "<" head)
	  (progn
	    (push head new-lst)
	    (while (not (cl-search ">" head))
	      ;; (message "DEBUG: new-lst: %s, car: %s" new-lst (car new-lst))
	      (setq head (pop raw-lst))
	      (setcar new-lst (concat (car new-lst)
				      "-"
				      head))
	      ;; (message "DEBUG: new-lst post %s" new-lst)
	      ))
	(push head new-lst)))
    (reverse new-lst)))

(defvar dvp-numbers-row-sets
  '((nil "$" "dollar")
    ("1" "&" "ampersand")
    ("2" "[" "bracketleft")
    ("3" "{" "braceleft")
    ("4" "}" "braceright")
    ("5" "(" "parenleft")
    ("6" "=" "equal")
    ("7" "*" "asterisk")
    ("8" ")" "parenright")
    ("9" "+" "plus")
    ("0" "]" "bracketright")))

(defun num-to-kbd-real-dvp (n)
  (if (numberp n)
      (setq n (format "%s" n)))
  (unless (stringp n)
    (error "n must be a number or string"))
  (let (ret)
    (mapc (lambda (x)
            (when (equal n (car x))
              (setq ret (nth 1 x))))
          dvp-numbers-row-sets)
    ret))

(defun kbd--key-conv-modifier (c)
  "Return the river-format modifier key corresponding to kbd-form C.
If match not found, error."
  (pcase c
    ("s" "Super")
    ("M" "Alt")
    ("S" "Shift")
    ("C" "Control")
    (_ (error "invalid modifier key: %s" c))))

(defun kbd--key-conv-single (c)
  "Return the river-format non-modifier key corresponding to kbd-form C.
If not found, return C."
  (pcase c
    ("<return>" "Return")
    ("RET" "Return")
    ("SPC" "Space")
    ("<tab>" "Tab")
    ("<escape>" "Escape")
    ("<left>" "Left")
    ("<right>" "Right")
    ("<up>" "Up")
    ("<down>" "Down")
    ("/" "Slash")
    ("," "Comma")
    ("." "Period")
    ("<mouse-left>" "BTN_LEFT")
    ("<mouse-right>" "BTN_RIGHT")
    ("<mouse-up>" "BTN_UP")
    ("<mouse-down>" "BTN_DOWN")
    ("<XF86AudioRaiseVolume>" "XF86AudioRaiseVolume")
    ("<XF86AudioLowerVolume>" "XF86AudioLowerVolume")
    ("<XF86AudioMute>" "XF86AudioMute")
    ("<XF86MonBrightnessUp>" "XF86MonBrightnessUp")
    ("<XF86MonBrightnessDown>" "XF86MonBrightnessDown")
    ("<XF86AudioPlay>" "XF86AudioPlay")
    ("<print>" "Print")
    ("<f12>" "F12")
    ("<f11>" "F11")
    ("<f10>" "F10")
    ("<f9>" "F9")
    ("<f8>" "F8")
    ("<f7>" "F7")
    ("<f6>" "F6")
    ("<f5>" "F5")
    ("<f4>" "F4")
    ("<f3>" "F3")
    ("<f2>" "F2")
    ("<f1>" "F1")
    ("$" "dollar")
    ("&" "ampersand")
    ("[" "bracketleft")
    ("{" "braceleft")
    ("}" "braceright")
    ("(" "parenleft")
    ("=" "equal")
    ("*" "asterisk")
    (")" "parenright")
    ("+" "plus")
    ("]" "bracketright")
    ;; is not a special key, use 'c' without changing
    (_ c)))

(defun kbd-to-shell (kbd)
  "Convert kbd-format keybind KBD to riverctl format and return."
  ;; error if river-mod or river-alt are undefined
  ;; (when (or (string-empty-p 'river-mod)
  ;;           (string-empty-p 'river-alt))
  ;;   (error "river-mod or river-alt is not defined"))
  ;; split KBD by each "-" and process each one
  (let* ((kbd-lst (kbd--split-str-to-lst kbd))
	 (kbd-ele nil)
	 (kbd-orig-length (length kbd-lst))
	 (output ""))
    ;; go through kbd-lst, pop off first elems and concat processed keys
    ;; to output
    (while kbd-lst
      ;; pop off first elem
      (setq kbd-ele (pop kbd-lst))
      ;; if not last elem, is a modifier, else is a single key
      (if kbd-lst
	  (setq output (concat output
			       (kbd--key-conv-modifier kbd-ele)
			       (if (> (length kbd-lst) 1)
				   "+"
				 " ")))
	(setq output (concat output
			     (when (= kbd-orig-length 1)
			       "None ")
			     (kbd--key-conv-single kbd-ele)))))
    output))


;;; Main functions

;; (defun shell-run (command)
;;   "Simlpy run a shell command."
;;   (message "LOG: %s" command)
;;   ;; (start-process-shell-command "shell cmd for river"
;;   ;;       		       nil
;;   ;;       		       (format "%s" command))
;;   (shell-command (format "%s" command))
;;   ;; t
;;   )

(defun river-shell-run (commands-str)
  "Run COMMANDS-STR in a new shell.
Log the output to *river-log*."
  (unless (stringp commands-str)
    (error "Input must be a string."))
  (let (river-output river-exit-code raw-out)
    (cl-flet ((log-to-buffer
                '(lambda (x) (bufr-message "*river-log*" x))))
      ;; log the command to run before running
      (log-to-buffer (concat "> " commands-str " <"))
      (message "LOG: %s" commands-str)
      ;; run command (returns "$exit-code $output")
      (setq raw-out (shell-command-to-string
                     (format "out=$(%s 2>&1)  ; echo \"$? $out\"" commands-str)))
      ;; extract exit code
      (setq river-exit-code (string-to-number
                             (replace-regexp-in-string
                              "^\\([0-9]+\\)[[:space:]].*" "\\1" raw-out)))
      ;; extract output
      (setq river-output
            (let ((orig (replace-regexp-in-string
                         "^[0-9]+[[:space:]]\\(.*\\)" "\\1" raw-out)))
              (if (not (equal "" orig))
                  (substring orig 0 -1)
                orig)))
      ;; print errors/logs as appropriate
      (if (not (equal 0 river-exit-code))
          (progn
            (message "ERROR: %s" river-output)
            (log-to-buffer (concat "ERROR: " river-output)))
        (unless (equal "" river-output)
          (message (concat " (" river-output ")"))
          (log-to-buffer (concat " (" river-output ")"))))
      ;; (if (equal 0 river-exit-code)
      ;;     (unless (equal "" river-output)
      ;;       (log-to-buffer (concat " (" river-output ")")))
      ;;   (message "ERROR: %s" river-output)
      ;;   (log-to-buffer (concat "ERROR: " river-output)))
      
      ))
  )

(defun river-run (commands)
  (if (nlistp commands)
      (setq commands (list commands)))
  (let* ((commands-str-list
          (mapcar (lambda (x) (format "%S" x)) commands))
         (commands-string
          (mapconcat 'identity commands-str-list " "))
         (riverctl-cmd
          (format "riverctl %s" commands-string)))
    (river-shell-run riverctl-cmd)))

(defun river-run-old (commands &optional cmd-prefix)
  "Run the riverctl command.
COMMANDS is a single list of arguments."
  (if (nlistp commands)
      (setq commands (list commands)))
  (let* ((commands ;; make all elements into strings                     
          (mapcar (lambda (x) (format "%S" x)) commands))
         (commands-string
          (mapconcat 'identity commands " "))
         (river-full-cmd (cond ((not cmd-prefix)
                                (format "riverctl %s" commands-string))
                               ((stringp cmd-prefix)
                                (format "%s %s" cmd-prefix commands-string))
                               (t (error "invalid second arg to `river-run'"))))
         (river-output)
         (river-exit-code)
         (raw-out))
    (cl-flet ((log-to-buffer
                '(lambda (x) (bufr-message "*river-log*" x))))
      ;; log the command being run
      (log-to-buffer (concat "> " river-full-cmd " <"))
      (message "LOG: %s" river-full-cmd)
      ;; run command (returns "$exit-code $output")
      (setq raw-out (shell-command-to-string
                     (format "out=$(%s 2>&1)  ; echo \"$? $out\"" river-full-cmd)))
      ;; log output and exit code
      (setq river-exit-code (string-to-number
                             (replace-regexp-in-string
                              "^\\([0-9]+\\)[[:space:]].*" "\\1" raw-out)))
      (setq river-output
            (let ((orig (replace-regexp-in-string
                         "^[0-9]+[[:space:]]\\(.*\\)" "\\1" raw-out)))
              (if (not (equal "" orig))
                  (substring orig 0 -1)
                orig)))
      (if (not (equal 0 river-exit-code))
          (progn
            (message "ERROR: %s" river-output)
            (log-to-buffer (concat "ERROR: " river-output)))
        (unless (equal "" river-output)
          (message (concat " (" river-output ")"))
          (log-to-buffer (concat " (" river-output ")"))))
      ;; (if (equal 0 river-exit-code)
      ;;     (unless (equal "" river-output)
      ;;       (log-to-buffer (concat " (" river-output ")")))
      ;;   (message "ERROR: %s" river-output)
      ;;   (log-to-buffer (concat "ERROR: " river-output)))
      )))

(defun river-set-modifier (var visible formal)
  "Updates modifier key name in output config.
VAR is variable to be changed, like `river-mod'.
VISIBLE is variable to be used in the output config, like \"$mod\".
FORMAL is the formal name for the key, like \"Mod4\"."
  (cond ((string-empty-p var)
	 (error (format "%s is not set" var)))
	((not (symbolp var))
	 (error (format "%s is not a symbol" var)))
	((not (stringp visible))
	 (format "%s is not a string" visible))
	((not (stringp formal))
	 (format "%s is not a string" formal))
	((not (equal "$" (substring visible 0 1)))
	 (format "first char of %s needs to be \"$\"" visible)))
  ;; e.g. (setq river-mod "$mod")
  (set var visible)
  ;; e.g. (setq river-mod-formal "Mod4")
  (set (intern (concat (symbol-name var)
		       "-formal"))
       formal))

(defun river-run-commands (&rest commands)
  "Run every elem in list COMMANDS with \"riverctl\"."
  (mapcar #'river-run commands))

(defmacro river-pairs (&rest setting-pairs)
  "Run \"riverctl\" with every two elems of list SETTING-PAIRS.
If there is one leftover elem, it will be passed to `river-run' without a
second arg."
  `(progn
     ,@(cl-loop for (key val) on setting-pairs by #'cddr collect
		(progn
		  `(river-run ,(format "%s %s"
				       (lst-to-str-maybe key)
				       (lst-to-str-maybe val)))))))

(defmacro river-spawn (&rest command)
  "Run \"riverctl spawn\" with every elem of list COMMAND.
Each elem must be a string."
  `(progn
     ,@(mapcar (lambda (cmd)
		 `(river-run ,(cons 'spawn cmd)))
	       command)))

(defvar river-set--accumilator nil
  "Variable utilized in `river-set--convert' to build commands.")

(defun river-set--convert (root traversed)
  "Recursive function for macro `river-set'.
This function will recursively search through the original list and its 
sublists. When it reaches a list with no sublists, it will push a list of 
the traversed path of elements (variable TRAVERSED) into 
`river-set--accumilator'. ROOT is the parent list and will be updated for 
every recursion down into the list.

If the current element is not a list and the last-traversed element is 
'kbd, the current element must be a kbd-format string. Convert this into 
its corresponding keybind in shell-form with function `kbd-to-shell',
append to TRAVERSED, and remove the kbd element from the traversed list."
  (let ((sublist-found nil))
    ;; iter through root elements
    (dolist (elem root)
      (if (nlistp elem)
          ;; item case
          (progn
            ;; types of item case?
            (cond ((and (equal 'kbd (car (last traversed)))
                        (stringp elem))
                   ;; kbd string case
                   (progn
                     (setq elem (mapcar 'intern (split-string (kbd-to-shell elem) " ")))
                     (setq traversed (append (butlast traversed) elem))))
                  (t
                   ;; regular item elem case
                   (progn
                     (setq traversed (append traversed (list elem))))))
            
            ;; (if (and (equal 'kbd (car (last traversed)))
            ;;          (stringp elem))
            ;;     ;; kbd string case
            ;;     (progn
            ;;       ;; (message "elem should be kbd str: %s" elem)
            ;;       (setq elem (mapcar 'intern (split-string (kbd-to-shell elem) " ")))
            ;;       (setq traversed (append (butlast traversed) elem)))
            ;;   ;; regular item elem case
            ;;   (progn
            ;;     (setq traversed (append traversed (list elem)))))
            
            ;; (if (not (equal 'kbd (car (last traversed))))
            ;;     (setq traversed (append traversed (list elem)))
            ;;   ;; kbd string case!
            ;;   (setq elem (mapcar 'intern (split-string (kbd-to-shell elem) " ")))
            ;;   (setq traversed (append (butlast traversed) elem)))
            )
        ;; elem is list, recurse case
        (progn
          (setq sublist-found t)
          (river-set--convert elem traversed))))
    ;; tail case, accumilate
    (unless sublist-found
      (push traversed river-set--accumilator))))

(defmacro river-set (&rest commands)
  "Constructs and runs commands from all nested arguments.
The variable COMMANDS is a list of commands that will be processed by the
function `river-run'. 
This macro runs the recursive function `river-set--convert' to process 
COMMANDS.
Example input:
(river-set (map (normal (kbd (\"s-Q\" exit)
                             (\"s-R\" spawn \"~/.config/river/init-elisp\")
                (locked (kbd (\"XF86AudioMute spawn 
                                              \"pactl-vol-mute.sh\")))))))"
  ;; (declare (indent (lambda (a b) 11)))
  (declare (indent defun))
  `(progn
     ,@(let (ret)
         (setq river-set--accumilator nil)
         (river-set--convert commands nil)
         ;; (message "DEBUG: convert results: %s" river-set--accumilator)
         ;; set ret to river-set--accumilator so can reset value
         (setq ret (mapcar (lambda (cmd)
                             `(river-run ,(macroexpand `(quote ,cmd))))
                           (reverse river-set--accumilator)))
         (setq river-set--accumilator nil)
         ret)))

;; wrappers around river-set:

(defmacro river-map (&rest commands)
  ;; (declare (indent 0))
  (macroexpand `(river-set ,(cons 'normal commands))))

;; TODO: testing!!!
(cl-defmacro river-map (&rest commands)
  ;; (declare (indent 0))
  (macroexpand `(river-set ,(cons 'normal commands))))

(defmacro river-normal (&rest commands)
  (macroexpand `(river-set (map ,(cons 'normal commands)))))

;; in river-set, if elem is :normal, convert into '(map ,mode kbd)

(defun keymap-shorthand (x)
  (when-let ((sym-as-str (and (symbolp x)
                              (symbol-name x)))
             (found-keymap? (string-match "^:[[:ascii:]]+" sym-as-str))
             (mode (intern (replace-regexp-in-string "^:\\([[:ascii:]]+\\)" "\\1"
                                                     sym-as-str))))
    mode))

(defmacro river-n-bind (&rest commands)
  (macroexpand `(river-set (map (normal ,(cons 'kbd commands))))))

(defmacro river-pointer-n-bind (&rest commands)
  (macroexpand `(river-set (map-pointer (normal ,(cons 'kbd commands))))))

(defmacro river-bind (&rest commands)
  (let ((new-commands nil)
        (curr nil))
    ;; iter through list
    (while (setq curr (and commands (pop commands)))
      (if-let ((mode (keymap-shorthand curr)))
          ;; if curr is :keymap, capture till next :keymap or end
          (let (sub-list)
            (while (and commands
                        (not (keymap-shorthand (car-safe commands)))
                        (setq curr (pop commands)))
              (setq sub-list (append sub-list (list curr))))
            (setq new-commands (append new-commands (list `(map ,mode kbd ,sub-list)))))
        ;; curr is anything else case
        (progn
          (setq new-commands (append new-commands (list curr))))))
    ;; (message "river-bind to-run: %S" new-commands)
    (macroexpand `(river-set ,new-commands))))

;; TODO: to rewrite, i /COULD/ just dolist...

;; (defmacro river-keymap-new (&rest commmands)
;;   (declare (indent 1))
;;   (message "DEBUG: commands : %S" commands)
;;   (let ((main-mode (pop commands))
;;         (return-lst)
;;         (key))
;;     (dolist (action-lst commands)
;;       (setq key (pop action-lst))
;;       (cond ((eq key 'create-map)
;;              ;; immediately river-declare-keymap-new
;;              )
;;             ((eq key 'bind)
;;              (dolist (bind action-lst)
;;                ;; process bind
;;                ))
;;             ((eq key 'exit-to)
;;              (dolist (statement action-lst)
;;                (let ((dest-mode (car statement))
;;                      (binds-lst (cadr statement)))
;;                  ;; create binds for exiting to mode
;;                  ))
;;              )
;;             ((eq key 'enter-from)
;;              (dolist (statement action-lst)
;;                (let ((orig-mode (car statement))
;;                      (binds-lst (cadr statement)))
;;                  ;; create binds for entering mode
;;                  ))
;;              ;; 
;;              )))))

(defmacro river-keymap (&rest commands)
  (declare (indent 1))
  ;; (message "DEBUG: commands: %S" commands)
  (let ((ret-macros nil)
        (mode (keymap-shorthand (pop commands))))
    (unless mode
      (error "first arg must be keymap/mode (e.g. \":normal\""))
    ;; iter through parent list
    (while-let ((loop? (and commands (listp commands)))
                (curr-lst (pop commands)))
      ;; iter through sublist: '(:opt (binds...))
      (if-let ((option (keymap-shorthand (pop curr-lst))))
          (while-let ((loop? (and curr-lst (listp curr-lst)))
                      (curr-item (pop curr-lst)))
            ;; (message "DEBUG: mode: %S, curr-lst: %S, option: %S, curr-item: %S" mode curr-lst option curr-item)
            ;; process based on option type
            (cond ((eq option 'bind)
                   ;; (message "curr-item: %S" curr-item)
                   (push (let ((regular `(,mode kbd ,curr-item))
                               (release `(-release
                                          ,mode kbd ,(car curr-item) enter-mode normal)))
                           ;; (message "DEBUG: reg: %S, rel: %S" regular release)
                           (macroexpand `(river-set (map ,regular
                                                         ,release))))
                         ;; (macroexpand `(river-set (map ,(mapcar (lambda (x)
                         ;;                                          `((,mode kbd ,x)
                         ;;                                            (-release
                         ;;                                             ,mode kbd ,(nth 0 x)
                         ;;                                             enter-mode normal)))
                         ;;                                        curr-item))))
                         ret-macros))
                  ((eq option 'press)
                   (push (macroexpand `(river-set (map ,mode kbd ,curr-item)))
                         ret-macros))
                  ((eq option 'release)
                   (push (macroexpand `(river-set (map -release ,mode kbd ,curr-item)))
                         ret-macros))
                  ((eq option 'exit-to)
                   (when-let ((sub-mode (keymap-shorthand curr-item))
                              (bind-list (and curr-lst (pop curr-lst))))
                     (mapcar (lambda (b)
                               (push (macroexpand
                                      `(river-set (map ,mode kbd
                                                       ,b enter-mode ,sub-mode)))
                                     ret-macros))
                             bind-list)))
                  ((eq option 'enter-from)
                   (when-let ((sub-mode (keymap-shorthand curr-item))
                              (bind-list (and curr-lst (pop curr-lst))))
                     (mapcar (lambda (b)
                               (push (macroexpand
                                      `(river-set (map ,sub-mode kbd
                                                       ,b enter-mode ,mode)))
                                     ret-macros))
                             bind-list)))))
        (progn
          (error "sublist %s must start with format :option" curr-lst))))
    ;; rev list and merge into one progn list
    (cons 'progn
          (mapcan (lambda (p)
                    (cdr p))
                  (reverse ret-macros)))))

;; keymaps

;; (defmacro river-declare-keymap (keymap &rest commands)
;;   (macroexpand `(river-set declare-mode ,keymap))
;;   `(progn
;;      ,(macroexpand `(river-set (map (,keymap ,(cons 'kbd commands)))))
;;      ,(macroexpand `(river-set (map (-release
;; 				     ,keymap ,(cons 'kbd
;; 						    (mapcar (lambda (x)
;; 							      (list (car x)
;; 								    'enter-mode
;; 								    'normal))
;; 							    commands))))))))

(defmacro river-declare-keymap (keymap)
  (macroexpand `(river-set declare-mode ,(keymap-shorthand keymap))))

(defmacro river-n-bind-keymap (&rest commands)
  (macroexpand
   `(river-set (map (normal ,(cons 'kbd
                                   (mapcar (lambda (x)
                                             (list (car x)
                                                   'enter-mode
                                                   (cdr x)))
                                           commands)))))))




