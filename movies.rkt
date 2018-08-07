#lang racket

(require rackunit
         web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/http/basic-auth
         net/http-client net/url
         gregor gregor/time
         json xml
         (prefix-in tms: "tmsapi.rkt")
         (prefix-in m: "model.rkt")
         (prefix-in html: "html.rkt"))

(define-values (dispatch movie-url)
  (dispatch-rules
   (("") (lambda (r) (redirect-to "index.html" permanently)))
   (("x") admin)
   (("theaters") theaters->response)
   (("theater" (integer-arg)) theater->response)
   (("movies") movies->response)
   (("movies" "popular") movies-popular->response)
   (("movie" (integer-arg)) movie->response)
   (("showtimes" (integer-arg)) showtimes->response)
   (("showtimes" (integer-arg) (string-arg)) showtimes->response)
   (("trailers" (integer-arg)) trailers->response)
   (("tvbanners" "generic" (string-arg) (string-arg)) image-not-available->response)
   (("assets" (string-arg)) asset->response)
   (("assets" (string-arg) (string-arg)) asset->response)
   (("genres") genres->response)
   (("calendar" (integer-arg)) calendar->response)
   (("poptrailers") poptrailers->response)))

(define (poptrailers->response r)
  (string->response (jsexpr->string (m:poptrailers))))

(define (calendar->response r id)
  (string->response (jsexpr->string (m:get-calendar id))))

(define (movie->response r id)
  (string->response (jsexpr->string (m:get-movie-by-id id))))

(define (movies->response r)
  (string->response (jsexpr->string (m:get-movies-all))))

(define (movies-popular->response r)
  (string->response (jsexpr->string (m:get-popular-movies 6))))

(define (genres->response r)
  (define ls-of-hash
    (for/list ([g (m:genres)])
      (hash 'genre g)))
  (string->response (jsexpr->string ls-of-hash)))

(define (theater->response r id)
  (string->response (jsexpr->string (m:get-theater id))))

(define (theaters->response r)
  (string->response (jsexpr->string (m:get-theaters-all))))

(define (showtimes->response r tid [arg-date (~t (today) "yyyy-MM-dd")])
  (define the-date (parse-date arg-date "yyyy-MM-dd"))
  (string->response 
   (jsexpr->string 
    (m:get-movies-by-theater tid the-date))))

(define (trailers->response r id)
  (string->response (jsexpr->string (m:get-other-trailers id))))

(define (bytes->response bs type)
  (response/full 200 #"Okay" (current-seconds) (string->bytes/utf-8 type) (headers) (list bs)))

(define (image-not-available->response r filename [size "small"])
  (bytes->response (file->bytes (format "assets/image-not-available-~a.png" size)) "image/png"))

(define (asset-path filename size)
    (define alist '(("small" "v6") ("medium" "v5") ("large" "v7") ("huge" "v8")))
    (define tagsize (assoc-val size alist))
    (define parts (regexp-split #rx"v[5-8]" filename))
    (format "assets/~a" (string-join (append (take parts 1) (list tagsize) (drop parts 1)) "")))

(define (asset->response r filename [size "small"])
  (bytes->response (tms:get-asset (asset-path filename size) size) "image/jpeg"))

;; key assoc -> value
(define (assoc-val key alist)
  (last (assoc key alist)))

(define (headers)
  ;; Access-Control-Allow-Headers: Content-Type
  ;; Access-Control-Allow-Methods: GET, POST, OPTIONS
  ;; Access-Control-Allow-Origin: *
  (list (make-header #"Access-Control-Allow-Headers" #"Content-Type")
        (make-header #"Access-Control-Allow-Methods" #"GET, POST, OPTIONS")
        (make-header #"Access-Control-Allow-Origin" #"*")))

(define (string->response s)
  (response/full 200 #"Okay" (current-seconds) 
                 TEXT/HTML-MIME-TYPE (headers) (list (string->bytes/utf-8 s))))

(define (admin r)
  (define (reject)
    (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        (format "Movies administration: ~a" (gensym))))
      void))
  (match (request->basic-credentials r)
    [(cons user pass)
     (let ()
       (define the-user (bytes->string/utf-8 user))
       (define the-pass (bytes->string/utf-8 pass))
       (if (not (and (string=? the-user "admin")
                     (string=? the-pass "admin")))
           (reject)
           (response/xexpr (html:show-admin-homepage (m:get-update-logs)))))]
    [else (reject)]))

(define (menu r)
  (response/xexpr 
   `(div (h2 "Examples")
         (ul 
          (li (a ((href "/index.html")) 
                     "The website"))
          (li (a ((href "/api.html")) 
                     "The API"))))))

(define (file-not-found r)
  (response/xexpr "File not found."))

;; Something must've happenned that I'm not using this variable.
;; (define root (path->string (current-directory)))

;; This is used in model:get-connection.  With --demo, we can let
;; other programmers see our work by using a sample database and not
;; updating the database ever.
(define demo? (make-parameter false))

(define (set-things-up)
  (when (not (demo?))
    (m:set-up)
    (m:schedule-daily-update)))

(module+ main
  (file-stream-buffer-mode (current-output-port) 'line)
  (define production? (make-parameter false))
  (command-line 
   #:once-each
   [("-u" "--update") "Updates the database and quits."
    (begin (m:daily-update m:*the-number-of-days* false) (exit))]
   [("-u" "--setup") "Sets things up. (Flag --production does it too.)"
    (begin (m:set-up) (exit))]
   [("-k" "--key") key "Sets the API key with the data provider."
    (tms:api-key key)
    (m:the-database "the-database.db")
    (displayln (format "API key set."))]
   #:once-any
   [("-d" "--demo") "Uses a sample database to demonstrate the system."
    (demo? true)
    (m:the-database "sample.db")
    (displayln (format "Demonstration mode using ~a, which will never be updated." (m:the-database)))]
   [("-p" "--production") "Serves the movies. (Doesn't open browser.)"
    (production? true)])

  (define (main)
    (when (not (demo?))
      ;; Unless it's a demonstration, an API key must be set.
      (tms:api-key-or-fatal-error))

    (set-things-up)
    (displayln "Now serving...")
    (serve/servlet dispatch 
                   #:stateless? true
                   #:log-file (build-path "logs/httpd.log")
                   #:port 8080
                   #:ssl? false
                   #:listen-ip (if (production?) false "127.0.0.1")
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                   #:extra-files-paths (list (build-path "public"))
                   #:server-root-path (build-path "root/")
                   #:file-not-found-responder file-not-found
                   #:command-line? (production?)))
  (main))
