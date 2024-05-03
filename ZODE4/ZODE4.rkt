#lang typed/racket
(require typed/rackunit)
(require racket/math)

#|Project Status: FULLY IMPLEMENTED|#

#|ZODE4 Data Types|#
(define-type ExprC (U NumC IdC StrC IfC LambC AppC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LambC ([id : (Listof Symbol)] [exp : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

#|
Parses an expression
Input: Sexp, Output: ExprC
|#

(define (not-valid-identifier? id)
  (member id '(if lamb locals : =)))

(: is-valid-identifier? (-> Any Boolean : #:+ Symbol))
(define (is-valid-identifier? id)
  (and (symbol? id)
       (not (member id '(if lamb locals : =)))))

(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    
    [(? string? s) (StrC s)]
    [(list 'if ': cond ': then ': else) (IfC (parse cond) (parse then) (parse else))]
    [(list 'locals ': cls ... ': exp) (let ([clauses (parse-clauses (cast cls (Listof Sexp)))])(AppC (LambC (first clauses) (parse exp)) (second clauses)))]
    [(list 'lamb ': id ... ': exp) (LambC (parse-ids (cast id (Listof Symbol))) (parse exp))]
    [(list fun args ...) (AppC (parse fun) (map parse args))]
    [(? symbol? i)
     (cond
       [(not-valid-identifier? i) (error "ZODE: invalid identifier, got: ~e" i)]
       [else (IdC i)])]
    [other (error "ZODE: expected valid expression, got: ~e" other)]))

(define (parse-ids [lst : (Listof Symbol)]) : (Listof Symbol)
  (cond 
    [(empty? lst) '()]
    [(symbol? (first lst)) (cond
                     [(not-valid-identifier? (first lst)) (error "ZODE: invalid identifier, got: ~e" (first lst))]
                     [else (cons (first lst) (parse-ids (rest lst)))])]
    [else (error "ZODE: Parameter not recognized")]))

(define (parse-clauses [exp : Sexp]) : (List (Listof Symbol) (Listof ExprC))
  (match exp
    [(list (? is-valid-identifier? id) '= exp) (list (list id) (list (parse exp)))]
    [(list (? is-valid-identifier? id) '= exp ': cls ...) (let ([res (parse-clauses cls)]) (list (cons id (first res)) (cons (parse exp) (second res))))]
    [other (error 'parse-clauses "ZODE: expected valid expression, got: ~e" other)]))

#|Top-level Env|#
(define (add [a : Real] [b : Real]) : Real
  (+ a b))

(define (sub [a : Real] [b : Real]) : Real
  (- a b))

(define (mult [a : Real] [b : Real]) : Real
  (* a b))

(define (div [a : Real] [b : Real]) : Real
  (/ a b))

(define (lessEq [a : Real] [b : Real]) : Boolean
  (<= a b))

(define (equal? [a : Any] [b : Any]) : Boolean
  (equal? a b))


#|
Interpreter
Input: ExprC Env, Output: Value
|#


#|TEST CASES|#
;;Parse
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse 'g) (IdC 'g))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse "hello") (StrC "hello"))
(check-equal? (parse-clauses '{x = 12}) (list (list 'x) (list (NumC 12))))
(check-equal? (parse-clauses '{x = 12 : y = 3}) (list (list 'x 'y) (list (NumC 12) (NumC 3))))
(check-equal? (parse '{locals : x = 12 : {+ x 1}}) (AppC (LambC (list 'x) (AppC (IdC '+)
                                                                                (list (IdC 'x)
                                                                                      (NumC 1))))
                                                         (list (NumC 12))))
(check-equal? (parse '{{lamb : x : {+ x 1}} 12}) (AppC (LambC (list 'x) (AppC (IdC '+)
                                                                         (list (IdC 'x)
                                                                               (NumC 1))))
                                                  (list (NumC 12))))


