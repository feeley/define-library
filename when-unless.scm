;; File: when-unless.scm

(define-library (when-unless)

  ;; the following import will be required when syntax-rules is hygienic
#;
  (import (only (gambit)
                if not begin)) ;; required by expansions of when and unless

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
