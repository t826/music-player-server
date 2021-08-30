#lang racket/base

(require net/http-easy
         racket/string
         net/uri-codec
         json
         db
         "./meifannao-db.rkt"
         racket/file)

(provide (all-defined-out))
               



;访问网站主页数
(define  body (car (regexp-match #rx"<body.*</body>" (bytes->string/utf-8 (response-body (get "http://www.bumimi88.com/"))))))
;(define  body (file->string "test.txt"))

;查找一个模块
(define (search-modul search-key key not-key  body index)
  ;模块定位开头位置
  (define start-index (regexp-match-positions (pregexp search-key ) body index))
  
  ;定义文本结尾位置                   
  (define (end-index start-index  [n 1])
    (cond
      [(equal? n 0)(cdar start-index )]
      [(equal? (car(regexp-match (regexp (string-append key "|" not-key)) body (cdar start-index ))) key)
       (define now-key-index (regexp-match-positions (regexp (string-append key "|" not-key))  body  (cdar start-index ))) 
       (end-index now-key-index (+ n 1)) ]
      [(equal? (car(regexp-match (regexp (string-append key "|" not-key)) body (cdar start-index ) )) not-key)
       (define now-key-index (regexp-match-positions (regexp (string-append key "|" not-key))  body  (cdar start-index ))) 
       (end-index now-key-index (- n 1)) ]
      [else #f]))
  (substring body (caar start-index ) (end-index start-index)))
    





;搜索节目数据（基本）
(define (search-movies movie-name)
  (car (regexp-match #px"\\[.*\\]" (bytes->string/utf-8 (response-body (get (string-append "http://159.75.7.49:7211/sssv.php?top=10&q=" (uri-encode movie-name))
                                                                            #:headers (hasheq 'Origin "ttp://www.bumimi99.com" )))))))
;获取整个首页（基本）
(define (get-home-data)
  (define body-page (regexp-match-positions* #rx"<div class=\"content rmcontent\"" body #:match-select caar))
  ;获取首页版块1
  (define home-page1
    (let* ([ data (search-modul "<div class=\"mod mod-new\"" "<div" "</div>"  body 0)]
           [home-page1-data (regexp-match* #px"(?<=href=)\".*?(\\w+/\\d+/)\" title=\"(.*?)\".*?src=\"(.*?)\"" data  #:match-select cdr )])
      (map (λ (one)
             (hasheq  'vid (car one) 'name (list-ref one 1) 'img (list-ref one 2)))
           home-page1-data)))
  (define reg #px"(?<=href=)\".*?(\\w+/\\d+/)\".*?src=\"(.*?)\".*?class=\"s1\">(.*?)</span>.*?class=\"y-newfigure-desc\">(.*?)</p>")
  ;获取首页版块2
  (define home-page-acg
    (let* ([ data (search-modul "<div class=\"content rmcontent\"" "<div" "</div>"  body (list-ref body-page 0))]
           [home-page2-data (regexp-match* reg data  #:match-select cdr )])
      (map (λ (one)
             (hasheq  'vid (car one) 'name (list-ref one 2) 'img (list-ref one 1) 'new (list-ref one 3)))
           home-page2-data)))
  ;获取首页版块3
  (define home-page-tv
    (let* ([ data (search-modul "<div class=\"content rmcontent\"" "<div" "</div>"  body (list-ref body-page 1))]
           [home-page2-data (regexp-match* reg data  #:match-select cdr )])
      (map (λ (one)
             (hasheq  'vid (car one) 'name (list-ref one 2) 'img (list-ref one 1) 'new (list-ref one 3)))
           home-page2-data)))
  ;获取首页版块4
  (define home-page-mov
    (let* ([ data (search-modul "<div class=\"content rmcontent\"" "<div" "</div>"  body (list-ref body-page 2))]
           [home-page2-data (regexp-match* reg data  #:match-select cdr )])
      (map (λ (one)
             (hasheq  'vid (car one) 'name (list-ref one 2) 'img (list-ref
                                                                  one 1) 'new (list-ref one 3) ))
           home-page2-data)))
  ;获取首页版块5
  (define home-page-zongyi
    (let* ([ data (search-modul "<div class=\"content rmcontent\"" "<div" "</div>"  body (list-ref body-page 3))]
           [home-page2-data (regexp-match* reg data  #:match-select cdr )])
      (map (λ (one)
             (hasheq  'vid (car one) 'name (list-ref one 2) 'img (list-ref one 1) 'new (list-ref one 3)))
           home-page2-data)))
(jsexpr->string
 (hasheq 'homeData (list (hasheq 'home-page1 home-page1 'home-page-acg home-page-acg 'home-page-tv  home-page-tv 'home-page-mov home-page-mov 'home-page-zongyi home-page-zongyi)))))

;访问网站分类数据（基本）
(define (get-class-data class)
  (sleep 3)
  (let* ([data (car(regexp-match #rx"<body.*</body>" (bytes->string/utf-8 (response-body (get (string-append "http://www.bumimi88.com/" class "/0/0/all/1.html"))))))]
         [data-page (regexp-match-positions* #rx"<dl class=\"b-listfilter-item js-listfilter\"" data #:match-select caar)]
         [class-time (hasheq 'time (apply hasheq 
          (apply append (map  (λ(one) (list (string->symbol (car one)) (list-ref one 1)))
          (regexp-match* #px"href=\".*?/([^0]\\d+)/\\d+/\\w+/.*?>(.*?)<"
                                     (search-modul "<dl class=\"b-listfilter-item js-listfilter\"" "<dl" "</dl>"  data (list-ref data-page 1)) #:match-select cdr)))))]
         [class-type (hasheq 'type (apply hasheq 
          (apply append (map  (λ(one) (list (string->symbol (car one)) (list-ref one 1)))
                             (regexp-match* #px"href=\".*?/\\d+/([^0]\\d+)/\\w+/.*?>(\\W+)<"
                                     (search-modul "<dl class=\"b-listfilter-item js-listfilter\"" "<dl" "</dl>"  data (list-ref data-page 2)) #:match-select cdr)))))]
         
         [class-palce (hasheq 'place (apply hasheq 
          (apply append (map  (λ(one) (list (string->symbol (car one)) (list-ref one 1)))
                             (regexp-match* #px"href=\".*?/\\d+/\\d+/(\\w+)/.*?>(\\W+)<"
                                     (search-modul "<dl class=\"b-listfilter-item js-listfilter\"" "<dl" "</dl>"  data (list-ref data-page 3)) #:match-select cdr)))))]
         [listData (map (λ (one) (hasheq  'vid (list-ref one 0)  'name (list-ref one 1)  'img (list-ref one 2) 'new (list-ref one 3)))
                         (regexp-match* #px"<a class=\"js-tongjic\" href=\"([^$].*?)\" title=\"(.*?)\".*?data-img=\"(.*?)\".*?class=\"hint\">(.*?)<"
                                   (search-modul "<div class=\"b-listtab\"" "<div" "</div>"  data (list-ref data-page 3)) #:match-select cdr))])
   ; (values class-time)))

    (jsexpr->string	 (hasheq (string->symbol class) (list(hasheq 'class (list class-time  class-type  class-palce)  'listData listData ))))))


;返回视频播放地址(基本）
(define (get-vid-address vid)
  (let* ([data (bytes->string/utf-8 (response-body (get (string-append "http://d.gqyy8.com:8077/ne2/s" vid ".js?"(number->string(current-seconds))))))]
         [par (regexp-match* #px"(playarr[_\\d]*\\[\\d+\\])=\"(.*?)," data #:match-select cdr)])
    (if (null? par)  '()
    (map (λ(one) (hasheq (string->symbol (car one)) (list-ref one 1))) par ))))

;基本数据定时更新到数据库
(define (set-data)
  (thread
    (λ()
      (let loop ()
          (query-exec meifannao  "update  movies set data=? where class=? "(get-home-data) "homeData") ;更新首页信息
          (map (λ(one) ;更新分类信息
                 (query-exec meifannao  "update  movies set data=? where class= ? "(get-class-data one) one))
               (list "acg" "tv" "mov" "zongyi"))
   (display "更新了基本数据")
          (sleep 600) (loop)))))
;(set-data)
;即时获取最新分类
(define (now-get-data class time type place page) 
  (let* ([data (car(regexp-match #rx"<body.*</body>" (bytes->string/utf-8 (response-body (get (string-append "http://www.bumimi88.com/"class"/"time"/"type"/"place"/"page".html"))))))]
         [data-page (regexp-match-positions* #rx"<dl class=\"b-listfilter-item js-listfilter\"" data #:match-select caar)]
          [class-time  (hasheq 'time
                       (map (λ(one) (hasheq (string->symbol (car one)) (list-ref one 1)))
                             (regexp-match* #px"href=\".*?/(\\d+)/\\d+/\\w+/.*?>(.*?)<"
                                     (search-modul "<dl class=\"b-listfilter-item js-listfilter\"" "<dl" "</dl>"  data (list-ref data-page 1)) #:match-select cdr)))]
         [class-type (hasheq 'type
                       (map (λ(one) (hasheq (string->symbol (car one)) (list-ref one 1)))
                             (regexp-match* #px"href=\".*?/\\d+/(\\d+)/\\w+/.*?>(\\W+)<"
                                     (search-modul "<dl class=\"b-listfilter-item js-listfilter\"" "<dl" "</dl>"  data (list-ref data-page 2)) #:match-select cdr)))]
         
         [class-palce (hasheq 'place
                       (map (λ(one) (hasheq (string->symbol (car one)) (list-ref one 1)))
                             (regexp-match* #px"href=\".*?/\\d+/\\d+/(\\w+)/.*?>(\\W+)<"
                                     (search-modul "<dl class=\"b-listfilter-item js-listfilter\"" "<dl" "</dl>"  data (list-ref data-page 3)) #:match-select cdr)))]
         [item (regexp-match* #px"<li class=\"item\".*?href=\"([^$].*?)\" title=\"(.*?)\".*?data-img=\"(.*?)\".*?class=\"hint\">(.*?)<" data #:match-select cdr)])
    (hasheq 'now-data (list (hasheq 'class (list (hasheq 'class-time class-time 'class-type class-type 'class-palce class-palce))) (hasheq 'item 
    (if (null? item)  '()
    (map (λ (one) (hasheq  'vid (list-ref one 0)  'name (list-ref one 1)  'img (list-ref one 2) 'new (list-ref one 3))) item)))))))
 
  

