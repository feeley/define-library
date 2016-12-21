;; File: when-unless.scm

(define-library (when-unless)

  ;; the following import will be required when syntax-rules is hygienic
#;
  (import (only (gambit)
                if not begin)) ;; required by expansions of when and unless

  (export when unless)

  (begin

    (define-syntax when
      (lambda (x)
        (syntax-case x ()
          ((_ test e e* ...)
           #'(if test (begin e e* ...))))))

    (define-syntax unless
      (syntax-rules ()
        ((_ test expr expr* ...)
         (if (not test) (begin expr expr* ...)))))))
