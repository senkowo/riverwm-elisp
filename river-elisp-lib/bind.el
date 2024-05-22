
;; river-bind stuff

(river-bind
 (normal
  ("s-<tab>" focus-previous-tags)       ; TODO: implement actual toggling!
  ("s-M-r" focus-previous-tags)
  ("s-M-h" spawn "flow cycle-tags previous")
  ("s-M-s" spawn "flow cycle-tags next")))

;;; bind functions

(defmacro river-bind (&rest commands)
  )
