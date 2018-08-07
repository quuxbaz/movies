#lang racket

(require net/http-client
         openssl/sha1
         json
         gregor)

;; globals
(define api-host "data.tmsapi.com")
(define api-host-img "movies.tmsimg.com")
(define api-key (make-parameter false))
(define zip-central 43606)
(define radius 50)

(define (api-key-or-fatal-error)
  (or (api-key) (error 'api-key-or-fatal-error "No TMS API set: impossible to proceed.")))

(define (get-file-extension filename)
  (last (string-split filename ".")))

(define (get-http-code bs)
  (string->number (list-ref (string-split (bytes->string/utf-8 bs)) 1)))

(define (fetch-uri host uri [attempt 0])
  (define (sleep-try-again code attempts)
    (when (= attempts 3)
      (begin 
        (printf "Could not fetch [~a].~n" uri)
        (raise 'too-many-attempts)))
    (printf "Code ~a. Sleeping one second before trying again... (attempt ~a)~n" code attempts)
    (sleep 1))
  (define-values (codebs headers port) (http-sendrecv host uri))
  (define code (get-http-code codebs))
  (cond [(= code 200) port]
        [(= code 403) (begin (sleep-try-again code attempt) (fetch-uri host uri (add1 attempt)))]
        [(= code 504) (begin (sleep-try-again code attempt) (fetch-uri host uri (add1 attempt)))]
        [else
         (begin (printf "Could not fetch [~a].~n" uri)
                (printf "Code: ~a.~n" code)
                (printf "Bytes: ~a.~n" (port->bytes port)) 
                'fail)]))

(define (get-asset filename size)
  (file->bytes (if (file-exists? filename) 
                   filename
                   (let ((flag (fetch-image filename size)))
                     (if (symbol=? flag 'okay)
                         filename
                         (format "assets/image-not-available-~a.png" size))))))

(define (fetch-image filename size)
  (define fmt "http://~a/~a?api_key=~a")
  (define uri (format fmt api-host-img filename (api-key-or-fatal-error)))
  ;; a hack to avoid trying forever
  (define retsym 'okay)
  (with-handlers 
    ([(λ (sym) (symbol=? sym 'too-many-attempts))
      ;; giving up after too many attempts
      (λ (x) (set! retsym 'failure))])
    (display-to-file (port->bytes (fetch-uri api-host-img uri))
  filename #:exists 'replace))
  retsym)

(define (fetch-trailers ls-of-root-ids)
  (define fmt "http://~a/v1.1/screenplayTrailers?rootids=~a&player_url=1&api_key=~a")
  (define root-ids (string-join (map number->string ls-of-root-ids) ","))
  (define uri (format fmt api-host root-ids (api-key-or-fatal-error)))
  (define the-date (~t (today) "yyyy-MM-dd"))
  (define shasum (sha1 (open-input-string root-ids)))
  (define local-file (build-path "tmp" (format "./trailers-~a-~a.json" the-date shasum)))
  (if (file-exists? local-file)
      (let ()
        (define local-port (open-input-file local-file))
        (define json (read local-port))
        (close-input-port local-port)
        (hash-ref (hash-ref json 'response) 'trailers))
      (let ()
        (define port (fetch-uri api-host uri))
        (if (port? port)
            (let ()
              (define json (read-json port))
              (close-input-port port)
              (write-to-file #:exists 'replace json local-file)
              (hash-ref (hash-ref json 'response) 'trailers))
            (hasheq)))))

(define (fetch-theaters)
  (define fmt "http://~a/v1.1/theatres?zip=~a&radius=~a&units=~a&api_key=~a")
  (define uri (format fmt api-host zip-central radius "km" (api-key-or-fatal-error)))
  (define the-date (~t (today) "yyyy-MM-dd"))
  (define local-file (build-path "tmp" (format "./theaters-~a-~a-~a.json" zip-central radius the-date)))
  (if (file-exists? local-file)
      (read (open-input-file local-file))
      (let ()
        (define port (fetch-uri api-host uri))
        (if (port? port)
            (let ()
              (define json (read-json port))
              (write-to-file #:exists 'replace json local-file)
              json)
            (hasheq)))))

(define (fetch-by-theater id [arg-date (today)])
  (define fmt "http://~a/v1.1/theatres/~a/showings?imageSize=Sm&imageText=true&startDate=~a&api_key=~a")
  (define the-date (~t arg-date "yyyy-MM-dd"))
  (define todaydate (~t (today) "yyyy-MM-dd"))
  (define uri (format fmt api-host id the-date (api-key-or-fatal-error)))
  (define local-file (build-path "tmp" (format "./theater-~a-~a-~a.json" todaydate id the-date)))
  (if (file-exists? local-file)
      (let () 
        (define local-port (open-input-file local-file))
        (define sexp (read local-port))
        (close-input-port local-port)
        sexp)
      (let ()
        (define port (fetch-uri api-host uri))
        (if (port? port)
            (let ()
              (define json (read-json port))
              (write-to-file #:exists 'replace json local-file)
              (close-input-port port)
              json)
            (hasheq)))))

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
  
(define (by-theater id [the-date (today)])
  (for/list ([m (fetch-by-theater id the-date)])
    `((rootId ,(string->number (hash.refs m.rootId)))
      (tmsId ,(hash.refs m.tmsId))
      (title ,(hash.refs m.title))
      (releaseYear ,(hash.refs m.releaseYear ""))
      (releaseDate ,(hash.refs m.releaseDate ""))
      (genres ,(string-join (hash.refs m.genres '()) ", "))
      (advisories ,(string-join (hash.refs m.advisories '()) ", "))
      (audience ,(hash.refs m.audience ""))
      (ratings ,(string-join (for/list ([r (hash.refs m.ratings '())]) 
                              (hash.refs r.code))))
      (showtimes ,(for/list ([t (hash.refs m.showtimes)])
                   `((tid ,(string->number (hash.refs t.theatre.id)))
                     (name ,(hash.refs t.theatre.name))
                     (datetime ,(parse-datetime (hash.refs t.dateTime) "yyyy-MM-dd'T'HH:mm"))
                     (ticketURI ,(hash.refs t.ticketURI "")))))
      (shortDesc ,(hash.refs m.shortDescription ""))
      (longDesc ,(hash.refs m.longDescription  ""))
      (image-uri ,(hash.refs m.preferredImage.uri))
      (image-caption ,(hash.refs m.preferredImage.caption.content ""))
      (image-category ,(hash.refs m.preferredImage.category ""))
      (image-primary ,(hash.refs m.preferredImage.primary ""))
      (directors ,(string-join (hash.refs m.directors '()) ", "))
      (topCast ,(string-join (hash.refs m.topCast '()) ", "))
      (runTime ,(hash.refs m.runTime "")))))

(provide (all-defined-out)) 
