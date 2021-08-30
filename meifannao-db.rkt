#lang racket/base

(require db)
(provide (all-defined-out))

;连接数据库
(define meifannao
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:server "127.0.0.1"
                     #:port 3306
                     #:database "meifannao"
                     #:user "root"
                     #:password "1234Abc")))))
