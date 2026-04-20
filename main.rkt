#lang racket
(module+ test (require rackunit))

;; An Expr represents an expression in umlang
;; It is one of:
;; - a (num Number) , representing a number
;; - a (plus Expr Expr) , denotes the addition of two expressions
(struct num [n] #:transparent)
(struct plus [left right] #:transparent)
(struct sub  [left right] #:transparent)

;; Examples:
(define two (num 2))
(define ten (num 10))
(define two-plus-ten (plus two ten))

;; Template for Expr
;; F : Expr -> X
;; <purpose statement goes here>
#;(define (F expr)
  (match expr
    [(num n) ... n ...]
    [(plus left right) ... (F left) ... (F right) ...]))

;; expr->python : Expr -> String
;; Compiles umlang expressions to String representing python script
(define (expr->python expr)
  (match expr
    [(num n) (number->string n)]
    [(plus left right)
     (string-append
       "(" (expr->python left) " + " (expr->python right) ")" )]
    [(sub left right)
     (string-append
       "(" (expr->python left) " - " (expr->python right) ")" )]))

(module+ test
  (check-equal? (expr->python (num 42)) "42")
  (check-equal? (expr->python (num 3.14)) "3.14")
  (check-equal? (expr->python (num -2)) "-2")
  (check-equal? (expr->python (sub two ten)) "(2 - 10)")
  (check-equal? (expr->python two-plus-ten) "(2 + 10)"))
