;; this program should be run with the command
;;
;; gsi -e '(load "define-library")' github.com/feeley/digest/digest test.sld

(define-library (test)

  (import (gambit)
          (https://github.com/feeley/digest)
          (when-unless))

  (export (rename foo walrus) bar)

  (begin

    (define skip-test #f)

    (unless skip-test

      ;; should give "da39a3ee5e6b4b0d3255bfef95601890afd80709"
      (pp (digest-string "" 'sha-1 'hex))

      ;; should give "a9993e364706816aba3e25717850c26c9cd0d89d"
      (pp (digest-string "abc" 'sha-1 'hex)))))
