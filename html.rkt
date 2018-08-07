#lang racket

(provide (all-defined-out))

(require xml)         

(define (a href [description href])
  (cons 'a (list `((href ,href)) description)))

(define (js url)
  (cons 'script (list `((src ,url)))))

(define (as ls)
  (map (lambda (href) (a href)) ls))

(define (ul ls) 
  (cons 'ul (for/list ([item ls])
              (list 'li item))))

(define (show-admin-homepage logs)
  (page (cons 'section 
              (list `(button "Update me now!")
                    `(h2 "Logs")
                    (ul (as (map (λ (name) (string-append "logs/" name)) logs)))))))

(define (page body)
  `(html
    (body
     ,(header)
     ,body
     ,(footer))))

(define (header)
  `(section ((id "header")) 
            (h1 "Administration")
            (hr)))

(define (footer)
  `(section ((id "footer")) (hr)
            "Powered by great people!"))
