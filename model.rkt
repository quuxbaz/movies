#lang racket

;; configuration
(define the-database (make-parameter "./the-database.db"))
(define *the-update-hour* 0)
(define *the-update-minute* 0)
(define *the-number-of-days* 14)

(require db
         gregor
         deferred
         (prefix-in tms: "tmsapi.rkt"))

(define db-conn
  (virtual-connection
   (connection-pool
    (lambda () 
      (get-connection)))))

(define (genres)
  (define sql "select movies.mov_genres from movies where length(movies.mov_genres) > 0")
  (define duplicates
    (for/list ([genres (sql-fetch sql)])
      (define ls (hash-ref genres 'mov_genres))
      (for/list ([genre (string-split ls ",")])
        (string-trim genre))))
  (sort (remove-duplicates (flatten duplicates)) string<?))

(define (get-theaters-by-movie-id id)
  (define sql "select theaters.th_id, theaters.th_name from movies
    left join showtimes on showtimes.mov_rootId = movies.mov_rootId
    left join theaters on theaters.th_id = showtimes.th_id
    where movies.mov_rootId = $1
            --and showtimes.shw_time >= datetime('now')
            --and showtimes.shw_time BETWEEN date('2016-08-20') and date('2016-08-21')
    group by theaters.th_id order by th_order asc")
  (sql-fetch sql id))

(define (get-calendar id)
  (define ls (sql-fetch "select distinct date(showtimes.shw_time) as day from showtimes
  where showtimes.th_id = $1
  order by date(showtimes.shw_time) asc" id))
  ls)

(define (get-showtimes id [the-date (today)])
  (define stm "select theaters.th_id, theaters.th_name, showtimes.shw_time from movies
    left join showtimes on showtimes.mov_rootId = movies.mov_rootId
    left join theaters on theaters.th_id = showtimes.th_id
    where movies.mov_rootId = $1
      and date(showtimes.shw_time) = date('now','localtime')
    order by th_order asc")
  (define rr (query db-conn stm id))
  (define groupings 
    (list->vector (remove "shw_time" (columns (rows-result-headers rr)))))
  (rows-result->assoc (group-rows rr 
                                  #:group (list groupings) 
                                  #:group-mode '(list))))

(define (poptrailers)
  (define sql "select count(showtimes.shw_time) as N, trailers.tr_url, movies.mov_title from showtimes 
       join movies on movies.mov_rootId = showtimes.mov_rootId
       join trailers on trailers.mov_rootId = movies.mov_rootId
       where tr_description like '%trailer%' 
      group by showtimes.mov_rootId order by N desc limit 2")
  (sql-fetch sql))

(define (get-other-trailers id)
  (define sql "select tr_eclipid, tr_description, tr_url from trailers 
                 where mov_rootId = $1 order by tr_date asc")
  (sql-fetch sql id))

(define (get-trailer id)
  (define sql "select tr_eclipid, tr_description, tr_url from trailers 
                 where mov_rootId = $1 and tr_description like '%trailer%' 
                 order by date(tr_date) asc limit 1")
  (define ls (sql-fetch sql id))
  (if (not (empty? ls)) (first ls) false))

(define (get-movie-by-id id)
  (define h (first (sql-fetch "select * from movies where mov_rootId = $1" id)))
  (hash-set! h 'theaters (get-theaters-by-movie-id id))
  (hash-set! h 'shows (get-showtimes id))
  (hash-set! h 'trailer (get-trailer id))
  (hash-set! h 'trailers (get-other-trailers id))
  h)

(define (get-movies-all)
  (define movies (sql-fetch "select * from movies order by mov_title asc"))
  (for/list ([h movies])
    (define id (hash-ref h 'mov_rootId))
    ;; (hash-set! h 'theaters (get-theaters-by-movie-id id))
    (hash-set! h 'shows (get-showtimes id))
    h))

(define (get-movies-by-theater id [arg-date (today)])
  (define the-date (~t arg-date "yyyy-MM-dd"))
  (define stm "select movies.*, showtimes.shw_time as shows from movies 
    left join showtimes on showtimes.mov_rootId = movies.mov_rootId
      where showtimes.th_id = $1 
            and date(showtimes.shw_time) = date($2)")
  (define rr (query db-conn stm id the-date))
  (define groupings 
    (list->vector (remove "shows" (remove-duplicates (columns (rows-result-headers rr))))))
  (rows-result->assoc (group-rows rr 
                                  #:group `(,groupings) 
                                  #:group-mode '(list))))

(define (get-theater id)
  (first (sql-fetch "select * from theaters where th_id = $1 order by th_order asc" id)))

(define (get-theaters-all)
  (define sql 
    "select count(distinct showtimes.mov_rootId) as RANK, theaters.* from showtimes 
       join theaters on theaters.th_id = showtimes.th_id
        group by showtimes.th_id order by RANK desc")
  (sql-fetch sql))

(define (get-popular-movies how-many)
  (define sql 
    "select count(showtimes.shw_time) as N, movies.* from showtimes 
       join movies on movies.mov_rootId = showtimes.mov_rootId
      group by showtimes.mov_rootId order by N desc limit $1")
  (sql-fetch sql how-many))

;; rows-result -> assoc-list
;; Makes a list of assoc-lists.  Each element of this list is a movie.
;; In each movie, you'll find its values, which as assoc-lists.
(define (sql-fetch . args)
  (define rr (apply query (cons db-conn args)))
  (rows-result->assoc rr))

(define (get-update-logs)
  (define path "logs")
  (for/list ([f (directory-list path)]
             #:when (and (file-exists? (build-path path f))
                         (regexp-match #rx"\\.log$" f)))
    (path->string f)))

(define (daily-update [n-days *the-number-of-days*] [log? true])
  (tomorrow-at *the-update-hour* *the-update-minute* (daily-update))
  (define old-port (current-output-port))
  (when log?
    (current-output-port 
     (open-output-file 
      #:exists 'truncate 
      (string->path (~t (today) "'logs/update-'yyyy-MM-dd'.log'")))))
  (file-stream-buffer-mode (current-output-port) 'line)
  (printf "Update for ~a.~n" (~t (now) "yyyy-MM-dd HH:mm:ss"))
  (delete-old-showtimes)
  (delete-old-movies)
  (populate-theaters)
  (populate-showtimes n-days)
  (populate-movies)
  #;(populate-trailers)
  (printf "Update completely done.~n")
  (when log?
    (close-output-port (current-output-port)))
  (current-output-port old-port))

(define (schedule-daily-update)
  (printf "Scheduling updates...~n")
  (today-at *the-update-hour* *the-update-minute* (daily-update)))

(define (delete-old-showtimes)
  (define yesterday (~t (-days (today) 1) "yyyy-MM-dd"))
  (define sql "delete from showtimes where date(shw_time) <= date(?) ")
  (query-exec db-conn sql yesterday)
  (define rows (query-value db-conn "select changes()"))
  (displayln (format "Deleted ~a showtimes." rows)))

(define (delete-old-movies)
  (define sql "select count(showtimes.shw_time) as N, 
     movies.mov_rootId, movies.mov_title from movies 
     left join showtimes on showtimes.mov_rootId = movies.mov_rootId 
     group by movies.mov_rootId
     having N = 0")
  (for ([(n id title) (in-query db-conn sql)])
    (define mov (format "delete from movies where mov_rootId = ~a -- ~a" id title))
    (define trl (format "delete from trailers where mov_rootId = ~a" id))
    (displayln mov)
    (displayln trl)
    (query-exec db-conn mov)
    (query-exec db-conn trl)))

(define (get-connection)
  (sqlite3-connect #:database (the-database) #:mode 'create))

(define (table-exists? tbl)
  (define sql "select name from sqlite_master where name=$1")
  (query-maybe-value db-conn sql tbl))

;; setting up for the first time

(define (create-table-trailers)
  (define stm
    "create table if not exists trailers
     (mov_rootId integer not null
      ,tr_title text
      ,tr_url text not null unique
      ,tr_type text
      ,tr_description text
      ,tr_date datetime
      ,tr_studio text
      ,tr_year text
      ,tr_eclipid text
      ,tr_lang text
      ,tr_bitrateid integer
      ,tr_formatid )")
  (query-exec db-conn stm))

(define (create-table-theaters)
  (define stm
    "create table if not exists theaters
     ( th_id integer primary key not null
      ,th_name text
      ,th_street text
      ,th_city text
      ,th_state text
      ,th_zip text
      ,th_country text
      ,th_latitude text
      ,th_longitude text
      ,th_phone text
      ,th_order integer)")
  (query-exec db-conn stm))

(define (create-table-movies)
  (define stm
    "create table if not exists movies
    ( mov_rootId integer not null primary key
     ,mov_tmsId text 
     ,mov_title text
     ,mov_releaseYear text 
     ,mov_releaseDate text
     ,mov_genres text 
     ,mov_advisories text 
     ,mov_audience text
     ,mov_ratings text 
     ,mov_shortDescription text 
     ,mov_longDescription text
     ,mov_image_name text
     ,mov_image_caption text
     ,mov_image_category text
     ,mov_image_primary text
     ,mov_directors text 
     ,mov_topcast text
     ,mov_runtime text)")
  (query-exec db-conn stm))

(define (create-table-showtimes)
  (define stm
    "create table if not exists showtimes
    ( mov_rootId integer not null
     ,mov_tmsId text not null
     ,th_id integer not null
     ,shw_time datetime not null
     ,shw_ticket_uri text
     ,UNIQUE(mov_rootId, th_id, shw_time) )")
  (query-exec db-conn stm))

(define (set-up)
  (printf "Preparing database...~n")
  (create-table-theaters) 
  (create-table-movies)
  (create-table-trailers)
  (create-table-showtimes)
  'done)

(define (populate)
  (populate-theaters)
  (populate-showtimes *the-number-of-days*)
  (populate-trailers)
  'done)

(define (movie-insert alist-movie)
  (define stm "INSERT INTO movies
    ( mov_rootId
     ,mov_tmsId
     ,mov_title
     ,mov_releaseYear
     ,mov_releaseDate
     ,mov_genres
     ,mov_advisories
     ,mov_audience
     ,mov_ratings
     ,mov_shortDescription
     ,mov_longDescription
     ,mov_image_name
     ,mov_image_caption
     ,mov_image_category
     ,mov_image_primary
     ,mov_directors
     ,mov_topcast
     ,mov_runtime)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, 
            $11, $12, $13, $14, $15, $16, $17, $18)")
  (define ls-of-desired-syms
    (filter (lambda (x) (not (symbol=? x 'showtimes))) 
            (for/list ([f alist-movie]) (car f))))
  (define ls-of-desired-vals
    (for/list ([k ls-of-desired-syms])
      (last (assoc k alist-movie))))
  (define args (append (list db-conn stm) ls-of-desired-vals))
  (when (not (movie-exists? (assoc-val 'rootId alist-movie)))
    (printf "New movie ~a ~a.~n" (assoc-val 'rootId alist-movie) (assoc-val 'tmsId alist-movie))
    (apply query-exec args)))

(define (trailer-exists? rootId url)
  (define stm "SELECT tr_url from trailers where tr_url = $1 and mov_rootId = $2")
  (query-maybe-value db-conn stm url rootId))

(define (movie-exists? root-id)
  (define stm "SELECT mov_rootId from movies where mov_rootId = $1")
  (query-maybe-value db-conn stm root-id))

(define (trailer-insert h)
  (define stm "INSERT INTO trailers
    ( mov_rootId
     ,tr_title
     ,tr_url
     ,tr_type
     ,tr_description 
     ,tr_date 
     ,tr_studio 
     ,tr_year 
     ,tr_eclipid 
     ,tr_lang 
     ,tr_bitrateid 
     ,tr_formatid) 
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)")
  (query-exec db-conn stm 
              (hash-ref h 'RootId)
              (hash-ref h 'Title)
              (hash-ref h 'Url)
              (hash-ref h 'TrailerTypeId)
              (hash-ref h 'Description)
              (hash-ref h 'DateAdded)
              (hash-ref h 'Studio)
              (hash-ref h 'Year)
              (hash-ref h 'EClipId)
              (hash-ref h 'LanguageName)
              (hash-ref h 'BitrateId)
              (hash-ref h 'FormatId)))

(define (populate-trailers)
  (displayln "Populating trailers...")
  (for ([h (tms:fetch-trailers (movies-for-trailers))]
        #:unless (trailer-exists? (hash-ref h 'RootId) (hash-ref h 'Url)))
    (printf "New trailer [~a] ~a.~n" 
            (hash-ref h 'Title) (hash-ref h 'Description))
    (trailer-insert h))
  'done)

(define (populate-movies [the-date (today)])
  (displayln "Populating movies...")
  (for ([t (in-list (tms:fetch-theaters))])
    (for ([m (tms:by-theater (hash-ref t 'theatreId) the-date)]
          #:unless (movie-exists? (assoc-val 'rootId m)))
      (movie-insert m))))

(define (datetime->sql-datetime-string gregor-dt)
  (~t gregor-dt "yyyy-MM-dd HH:mm:SS"))

(define (showtime-exists? shw mov)
  (define stm "SELECT mov_rootId, th_id, shw_time from showtimes 
     where mov_rootId = $1 and th_id = $2 and shw_time = $3 ")
  (define dt (datetime->sql-datetime-string (assoc-val 'datetime shw)))
  (query-maybe-row db-conn stm (assoc-val 'rootId mov) (assoc-val 'tid shw) dt))

(define (showtime-insert m)
  (define stm "INSERT INTO showtimes
    (mov_rootId, mov_tmsId, th_id, shw_time, shw_ticket_uri) VALUES ($1, $2, $3, $4, $5)")
  (for ([s (in-list (assoc-val 'showtimes m))]
        #:unless (showtime-exists? s m))
    (printf "New showtime ~a ~a ~a.~n" 
            (assoc-val 'tmsId m) (assoc-val 'tid s) (assoc-val 'datetime s))
    (query-exec db-conn stm 
                (assoc-val 'rootId m)
                (assoc-val 'tmsId m)
                (assoc-val 'tid s)
                (datetime->sql-datetime-string (assoc-val 'datetime s))
                (assoc-val 'ticketURI s))))

(define (populate-showtimes n-days)
  (for ([t (in-list (tms:fetch-theaters))])
    (for ([n (in-range n-days)])
      (displayln (format "Populating showtimes for ~a." (+days (today) n)))
      (for ([m (tms:by-theater (hash-ref t 'theatreId) (+days (today) n))])
        (movie-insert m)
        (showtime-insert m))))
   'done)

(define (theater-exists? tid)
  (define stm "SELECT th_id from theaters where th_id = ?")
  (query-maybe-row db-conn stm tid))

(define (populate-theaters)
  (displayln "Populating theaters...")
  (define stm "INSERT INTO theaters
    (th_id ,th_name ,th_street ,th_city ,th_state, th_zip, th_country
    ,th_latitude, th_longitude, th_phone) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)")
  (for ([t (in-list (tms:fetch-theaters))]
        #:unless (theater-exists? (hash-ref t 'theatreId)))
    (displayln (format "New theather: ~a." (hash-ref t 'name)))
    (query-exec db-conn stm 
                (hash-ref t 'theatreId)
                (hash-ref t 'name) 
                (hash-ref (hash-ref (hash-ref t 'location) 'address) 'street)
                (hash-ref (hash-ref (hash-ref t 'location) 'address) 'city) 
                (hash-ref (hash-ref (hash-ref t 'location) 'address) 'state) 
                (hash-ref (hash-ref (hash-ref t 'location) 'address) 'postalCode) 
                (hash-ref (hash-ref (hash-ref t 'location) 'address) 'country)
                (hash-ref (hash-ref (hash-ref t 'location) 'geoCode) 'latitude)
                (hash-ref (hash-ref (hash-ref t 'location) 'geoCode) 'longitude)
                (hash-ref (hash-ref t 'location) 'telephone "")))
  'done)

(define (movies-for-trailers)
  (define sql "select mov_rootId from movies")
  (query-list db-conn sql))

(define (drop-table t)
  (query-exec db-conn (string-append "drop table if exists " t)))

(define (drop-all-tables)
  (for ([t '("theaters" "movies" "showtimes" "trailers")])
    (drop-table t)))

;; rows-result -> list-of column
(define (columns rr)
  (for/list ([alist rr])
    (cdr (first alist))))

;; rows-result -> assoc
(define (rows-result->assoc rr)
  (define (grouped->header blob)
    (cdr (first (last blob))))
  (define names-and-types
    (for/list ([header (rows-result-headers rr)]) 
      (cons (cdar header) (cdadr header))))
   (for/list ([vec (rows-result-rows rr)])
     (begin 
       (define h (make-hash))
       (for ([header names-and-types]
             [val vec])
         (if (string=? (car header) "grouped")
             (hash-set! h (string->symbol (grouped->header header)) (sql-null->false val))
          (hash-set! h (string->symbol (car header)) (sql-null->false val))))
       h)))

;; key assoc -> value
(define (assoc-val key alist)
  (last (assoc key alist)))

(provide (all-defined-out))
