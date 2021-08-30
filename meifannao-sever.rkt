#lang racket

(require web-server/servlet
         web-server/servlet-env
         json
         db
         "meifannao-db.rkt"
          web-server/http
          "response-cors.rkt"
          "movie.rkt"
         "get-movies-data.rkt")
;定时更新数据库基本数据
;(set-data)

;路由
(define-values (dispatcher url)
  (dispatch-rules
   
   [("get" (string-arg)) ;获取首页及分类基本数据
    #:method (or "get" "options")
    (λ (req arg)
      (display "s")
      (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (cond [(index-of  '("acg" "homeData" "tv" "mov" "zongyi") arg)
             (response/cors/jsexpr (string->jsexpr (vector-ref (get-data arg) 1)))]
            [else (response/cors/jsexpr (hasheq 'error 404 ))])))]
   
   [("search") ; 获取搜索数据
    #:method (or "post" "options")
    (λ (req)
      (display "搜索了")
      (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([js (bytes->jsexpr (request-post-data/raw req))]
             [name  (hash-ref js 'name  #f)])
        (response/cors/jsexpr (search-movies name)))))]
   
   [("player-address" (string-arg)) ;获取播放地址
    #:method (or "get" "options")
    (λ(req arg)
      (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (response/cors/jsexpr
      (if (string->number arg) (hasheq 'data (get-vid-address arg)) (hasheq 'error 404)))))]
   
   [((string-arg) (integer-arg) (integer-arg) (string-arg) (string-arg) ) ;即时获取分类信息
    #:method (or "get" "options")
    (λ (req  class time type place page)
      (if (index-of  '("acg" "tv" "mov" "zongyi") class)
          (response/cors/jsexpr (now-get-data class (number->string time) (number->string type) place page))
          (response/cors/jsexpr (hasheq 'data '()))))]))
                                                                   
(serve/servlet dispatcher
               #:command-line? #t
               #:listen-ip #f
               #:port 5000
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "htdocs"))
               #:ssl? #f
               #:stateless? #t
               #:log-file "jzkd-web.log")
