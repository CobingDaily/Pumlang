#lang racket
(module+ test (require rackunit))

;; An Expr represents an expression in umlang
;; It is one of:
;; - a (num Number) , representing a number
;; - a (bool Boolean) , representing a boolean
;; - a (plus Expr Expr) , denotes the addition of two expressions
;; - a (sub Expr Expr) , denotes the subtraction of right from left expression
;; - a (conditional Expr Expr Expr) , denotes if `c` then `t` else `e`
(struct num  [n] #:transparent)
(struct bool [b] #:transparent)
(struct plus [left right] #:transparent)
(struct sub  [left right] #:transparent)
(struct mul  [left right] #:transparent)
(struct div  [left right] #:transparent)
(struct conditional [c t e] #:transparent)

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
    [(bool b) ... b ...]
    [(plus left right) ... (F left) ... (F right) ...]
    [(sub  left right) ... (F left) ... (F right) ...]
    [(mul  left right) ... (F left) ... (F right) ...]
    [(div  left right) ... (F left) ... (F right) ...]
    [(conditional c t e) ... (F c) ... (F t) ... (F e) ...]))

;; expr->python : Expr -> String
;; Compiles umlang expressions to String representing python script
(define (expr->python expr)
  (match expr
    [(num n)  (number->string n)]
    [(bool b) (if b "True" "False")]
    [(plus left right)
     (plus->python (expr->python left) (expr->python right))]
    [(sub left right)
     (sub->python (expr->python left) (expr->python right))]
    [(mul left right)
     (mul->python (expr->python left) (expr->python right))]
    [(div left right)
     (div->python (expr->python left) (expr->python right))]
    [(conditional c t e)
     (conditional->python
       (expr->python c) (expr->python t) (expr->python e))]))

;; Expr Expr -> String
;; Helper function to compile umlang plus into Python.
;; Only called from expr->python.
(define (plus->python left-expr right-expr)
  (string-append "(" left-expr " + " right-expr ")"))

;; Expr Expr -> String
;; Compile umlang sub into Python.
;; Only called from expr->python.
(define (sub->python left-expr right-expr)
  (string-append "(" left-expr " - " right-expr ")"))

;; Expr Expr -> String
;; Compile umlang mul into Python.
;; Only called from expr->python.
(define (mul->python left-expr right-expr)
  (string-append "(" left-expr " * " right-expr ")"))

;; Expr Expr -> String
;; Compile umlang div into Python.
;; Only called from expr->python.
(define (div->python left-expr right-expr)
  (string-append "(" left-expr " / " right-expr ")"))

;; Expr Expr Expr -> String
;; Compile umlang conditional into Python.
;; Only called from expr->python.
(define (conditional->python c t e)
  (string-append "(" t " if " c " else " e ")"))


(module+ test
  (check-equal? (expr->python (num 42)) "42")
  (check-equal? (expr->python (num 3.14)) "3.14")
  (check-equal? (expr->python (num -2)) "-2")
  (check-equal? (expr->python (bool #t)) "True")
  (check-equal? (expr->python (bool #f)) "False")
  (check-equal? (expr->python two-minus-ten) "(2 - 10)")
  (check-equal? (expr->python two-plus-ten) "(2 + 10)")
  (check-equal? (expr->python (mul (num 2) (num 2))) "(2 * 2)")
  (check-equal? (expr->python (div (num 1) (num 2))) "(1 / 2)")
  (check-equal? (expr->python if-t-one-else-minusone) "(1 if True else -1)")
  (check-equal? (expr->python
                  (conditional (bool #f) (num 1) (num -1))) "(1 if False else -1)"))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser
;;
;; parse : S-Exp -> Expr
;; Parses Umlang program to produce an AST
;; Partial
(define (parse s)
  (match s
    [(? number? n)  (num n)]
    [(? boolean? b) (bool b)]
    [(list '+ L R) (plus (parse L) (parse R))]
    [(list '- L R) (sub  (parse L) (parse R))]
    [(list '* L R) (mul  (parse L) (parse R))]
    [(list '/ L R) (div  (parse L) (parse R))]
    [(list 'if c t e) (conditional (parse c) (parse t) (parse e))]
    [_ (error 'parse "Parse error ~v" s)]))

(module+ test
  (check-equal? (parse 0) (num 0))
  (check-equal? (parse -10) (num -10))
  (check-equal? (parse #t) (bool #t))
  (check-equal? (parse #f) (bool #f))
  (check-equal? (parse `(+ 1 2)) (plus (num 1) (num 2)))
  (check-equal? (parse `(- 2 3)) (sub  (num 2) (num 3)))
  (check-equal? (parse `(* 3 4)) (mul  (num 3) (num 4)))
  (check-equal? (parse `(/ 4 5)) (div  (num 4) (num 5)))
  (check-equal? (parse `(if #t 1 -1))
                       (conditional (bool #t) (num 1) (num -1))))

;; compile : S-Exp -> String
;; Transforms the `umlang` s-expression into String denoting python code.
;; Partial, because `parse` is partial
(define (compile umlang)
  (expr->python (parse umlang)))

(module+ test
  (check-equal? (compile `(+ 1 (* 2 3)))
                "(1 + (2 * 3))")
  (check-equal? (compile `(if #t (/ 45 5) (* 123 234)))
                "((45 / 5) if True else (123 * 234))")
  ;; Check for exceptions
  (check-exn #px"Parse error" (lambda () (compile `(2 * 3))))
  (check-exn #px"Parse error"
             (lambda () (compile `(if #t 1)))))

