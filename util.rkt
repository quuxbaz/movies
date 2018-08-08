#lang racket

(define/contract (hash-refs h ks [def #f])
    ((hash? (listof any/c)) (any/c) . ->* . any)
    (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                            [else def]))])
      (for/fold ([h h])
        ([k (in-list ks)])
        (hash-ref h k))))

(require (for-syntax racket/syntax))
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ; If the optional ‘default' is missing, use #f.
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))

(provide hash.refs)
