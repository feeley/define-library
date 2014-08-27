;; this program should be run with the command
;;
;; gsi -e '(load "define-library")' github.com/feeley/digest/digest test.sld

(define-library (test)

  (import (gambit)
          (https://github.com/feeley/digest))

  (begin
    ;; should give "a9993e364706816aba3e25717850c26c9cd0d89d"
    (pp (digest-string "abc" 'sha-1 'hex))))
