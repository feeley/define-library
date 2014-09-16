(define-library (when-unless)

  (import (gambit))

  (export when unless)

  (begin

    (define-syntax when
      (syntax-rules ()
        ((_ test expr expr* ...)
         (if test (begin expr expr* ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((_ test expr expr* ...)
         (if (not test) (begin expr expr* ...)))))))
