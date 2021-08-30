#lang racket

(require db "meifannao-db.rkt")
(provide (all-defined-out))

;读取电影数据库基本信息返回
(define (get-data class)
  (query-row meifannao "select * from movies where class =?" class))
