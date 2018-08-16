#lang racket

(provide 
 ; for json access
 hash.refs
 ; for deleting old log files
 delete-old-files)

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

(define (delete-old-files dir)
  ;; The top down approach
  (for ([f (get-files dir)])
    (when (delete-file? f)
        (delete-file f)))) ; don't really delete it

(define (get-files dir)
  (directory-list dir))

(define (delete-file? path)
  (older-than-days? 30 path))

(define (older-than-days? days path)
  (define timeline-mark (- (current-seconds) (* days 24 3600)))
  (define path-mark 
    (with-handlers ([exn:fail:filesystem? (lambda (e) (displayln e) +inf.0)])
      (file-or-directory-modify-seconds path)))
  (< path-mark timeline-mark))
