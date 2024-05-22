
;; tags utils

(defun river-tags-range (beg end)
  (cl-loop for x from beg to end
           collect (ash 1 (1- x))))

(defvar river-tags-1-9
  (river-tags-range 1 9))
