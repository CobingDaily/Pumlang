#lang racket
(module+ test (require rackunit))

;; An Expr represents an expression in umlang
;; It is one of:
;; - a (num Number) , representing a number
;; - a (bool Boolean) , representing a boolean
;; - a (plus Expr Expr) , denotes the addition of two expressions
;; - a (sub Expr Expr) , denotes the subtraction of right from left expression
;; - a (conditional Expr Expr Expr) , denotes if `c` then `t` else `e`
;; ;; - a (id String) , denotes an identifier name, represented by String
(struct num [n] #:transparent)
(struct bool [b] #:transparent)
(struct plus [left right] #:transparent)
(struct sub  [left right] #:transparent)
(struct conditional [c t e] #:transparent)
;; (struct id  [name] #:transparent)

;; Examples:
(define two (num 2))
(define ten (num 10))
(define two-plus-ten (plus two ten))
(define two-minus-ten (sub two ten))
(define if-t-one-else-minusone 
  (conditional (bool #t) (num 1) (num -1)))

;; Template for Expr
;; F : Expr -> X
;; <purpose statement goes here>
#;(define (F expr)
  (match expr
    [(num n) ... n ...]
    [(plus left right) ... (F left) ... (F right) ...]
    [(sub  left right) ... (F left) ... (F right) ...]))

;; expr->python : Expr -> String
;; Compiles umlang expressions to String representing python script
(define (expr->python expr)
  (match expr
    [(num n)  (number->string n)]
    [(bool b) (if b "True" "False")]
    [(plus left right)
     (string-append
       "(" (expr->python left) " + " (expr->python right) ")" )]
    [(sub left right)
     (string-append
       "(" (expr->python left) " - " (expr->python right) ")" )]
    [(conditional c t e)
     (create-python-conditional
       (expr->python c) (expr->python t) (expr->python e))]))

;; Expr Expr Expr -> String
;; Helper function to compile umlang conditional into Python.
;; Only called from expr->python
(define (create-python-conditional c t e)
  (string-append t " if " c " else " e))


(module+ test
  (check-equal? (expr->python (num 42)) "42")
  (check-equal? (expr->python (num 3.14)) "3.14")
  (check-equal? (expr->python (num -2)) "-2")
  (check-equal? (expr->python (bool #t)) "True")
  (check-equal? (expr->python (bool #f)) "False")
  (check-equal? (expr->python two-minus-ten) "(2 - 10)")
  (check-equal? (expr->python two-plus-ten) "(2 + 10)")
  (check-equal? (expr->python if-t-one-else-minusone) "1 if True else -1"))
