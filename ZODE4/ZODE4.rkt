#lang typed/racket
(require typed/rackunit)
(require racket/math)

#|Project Status: FULLY IMPLEMENTED|#

#|ZODE4 Data Types|#
(define-type ExprC (U NumC IdC StrC IfC LambC AppC IfLeqZeroC))
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
    [other (error 'parse "ZODE: expected valid expression, got: ~e" other)]))

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

#|Values|#
(define-type Value (U BoolV NumV StrV PrimV CloV))
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([s : Symbol]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([p : Symbol]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)

(struct Binding ([name : Symbol] [val : Value]) #:transparent)


(define-type Environment (Listof Binding))

#|Top-level Env Functions|#
(define (apply-func [op : Symbol] [args : (Listof Value)]) : Value
  (cond
    [(equal? op '+) (apply-op '+ args)]
    [(equal? op '-) (apply-op '- args)]
    [(equal? op '*) (apply-op '* args)]
    [(equal? op '/) (apply-op '/ args)]
    [(equal? op '<=) (apply-op '<= args)]
    [(equal? op 'equals?) (BoolV (equals? (first args) (rest args)))]
    [else (error "ZODE: Unknown operator, got: ~e" op)]))

(define (apply-op [op : Symbol] [args : (Listof Value)]) : Value
  (match args
    [(list (NumV a) (NumV b))
     (cond
       [(equal? op '+) (NumV (+ a b))]
       [(equal? op '-) (NumV (- a b))]
       [(equal? op '*) (NumV (* a b))]
       [(equal? op '/) (if (not (zero? b))
                       (NumV (/ a b))
                       (error "ZODE: Division by zero"))]
       [(equal? op '<=) (BoolV (<= a b))]
       [else (error "ZODE: Unknown operator, got: ~e" op)])]
    [else (error "ZODE: Invalid arguments for operation")]))

(define (equals? [a : Any] [b : Any]) : Boolean
  (and (not (or (PrimV? a) (CloV? a)))
       (not (or (PrimV? b) (CloV? b)))
       (equal? a b)))

(define (user-error [v : Value]) : Nothing
  (error 'user-error (serialize v)))



;;temp
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct BinOpC ( [operator : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
(struct IfLeqZeroC ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
;(struct AppC ([fun : Symbol] [args : (Listof ExprC)]))


#|
Serialize
Input: ZODE4 Value, Output: String
|#

(define (serialize (v : Value)) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV b) (if b "true" "false")]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV op) (format "#<primop>")]))

#|
Interpreter
Input: ExprC Env, Output: Value
|#

(define top-env : Environment (list
                 (Binding 'true (BoolV #t))
                 (Binding 'false (BoolV #f))
                 (Binding '+ (PrimV '+))
                 (Binding '- (PrimV '-))
                 (Binding '* (PrimV '*))
                 (Binding '/ (PrimV '/))))


;;add-env
(define (add-env [env : Environment] [args : (Listof ExprC)] [params : (Listof Symbol)]) : Environment
  (cond
    [(empty? args) env]
    [else (cons (Binding (first params) (interp (first args) env)) (add-env env (rest args) (rest params)))]))

;;interp-IdC
(define (interp-id [s : Symbol] [env : Environment]) : Value
  (cond
    [(empty? env) (error 'interp-id "ZODE: No parameter matching id: ~e" s)]
    [(equal? s (Binding-name (first env))) (Binding-val (first env))]
    [else (interp-id s (rest env))]))

;;interp-args
(define (interp-args [args : (Listof ExprC)] [env : Environment]) : (Listof Value)
  (cond
    [(empty? args) '()]
    [else (cons (interp (first args) env)(interp-args (rest args) env))]))

;interp
;;accepts an ExprC and a list of function definitions and returns a Real number (the value)
(define (interp [exp : ExprC] [env : Environment]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(IfLeqZeroC c i e) (cond
                          [(<= (NumV-n (let ([num (interp c env)])
                                         (cond
                                           [(NumV? num) (cast num NumV)]
                                           [else (error 'interp "ZODE: Type Mismatch, Expected Real, got ~e" num)]))) 0) (interp i env)]
                          [else (interp e env)])]
    [(AppC expr args) (let ([clo (let ([temp-clo (interp expr env)])
                                    (cond
                                      [(CloV? temp-clo) (cast temp-clo CloV)]
                                      [(PrimV? temp-clo) (cast temp-clo PrimV)]
                                      [else (error 'interp "ZODE: Expected CloV or PrimV, got ~e" temp-clo)]))])
                     (match clo
                       [(? CloV?)(cond
                         [(= (length args) (length (CloV-args clo))) (interp (CloV-body clo) (add-env (CloV-env clo) args (CloV-args clo)))]
                         [else (error 'interp "ZODE: Number of Argument Mismatch, expected
                               ~e, got ~e" (length (CloV-args clo)) (length args))])]
                       [(? PrimV?) (apply-func (PrimV-p clo) (interp-args args env))]))]
    
    [(LambC params expr) (CloV params expr env)]
    [(IdC s) (interp-id s env)]))


;interp test cases
(check-equal? (interp (AppC (IdC '+) (list (AppC (LambC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 3) (NumC 5))) (NumC 2))) top-env) (NumV 10))

(check-exn #rx"ZODE: Expected CloV" (lambda () (interp (AppC (IdC '+) (list (AppC (NumC 4) (list (NumC 3) (NumC 5))) (NumC 2))) top-env)))
(check-exn #rx"ZODE: Number of Argument" (lambda () (interp (AppC (IdC '+) (list (AppC (LambC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 3) (NumC 5) (NumC 5))) (NumC 2))) top-env)))


(check-equal? (interp (AppC (IdC '-) (list (NumC 1) (NumC 2))) top-env) (NumV -1))
(check-equal? (interp (AppC (IdC '*) (list (NumC 1) (NumC 2))) top-env) (NumV 2))
(check-equal? (interp (AppC (IdC '/) (list (NumC 1) (NumC 2))) top-env) (NumV 1/2))
(check-equal? (interp (IfLeqZeroC (NumC -1) (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env) (NumV 3))
(check-equal? (interp (IfLeqZeroC (NumC 2) (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env) (NumV 1))






#|TEST CASES|#
;;Parse
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse 'g) (IdC 'g))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse "hello") (StrC "hello"))
(check-equal? (parse '{if : 3 : 4 : 5}) (IfC (NumC 3) (NumC 4) (NumC 5)))
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

(check-exn #rx"ZODE: invalid identifier, got: " (lambda () (parse '{if : 3 : 4 : 'locals})))
(check-exn #rx"ZODE: invalid identifier, got: " (lambda () (parse '{{lamb : x locals : {+ x 1}} 12})))
(check-exn #rx"ZODE: expected valid expression, got: " (lambda () (parse '{})))
(check-exn #rx"ZODE: expected valid expression, got: " (lambda () (parse-clauses '{})))

;;Top-level Env Functions
(check-equal? (apply-func '+ (list (NumV 5) (NumV 3))) (NumV 8))
(check-equal? (apply-func '- (list (NumV 10) (NumV 4))) (NumV 6))
(check-equal? (apply-func '* (list (NumV 7) (NumV 2))) (NumV 14))
(check-equal? (apply-func '/ (list (NumV 12) (NumV 4))) (NumV 3))
(check-exn #rx"ZODE: Division by zero" (lambda () (apply-func '/ (list (NumV 3) (NumV 0)))))
(check-equal? (apply-func '<= (list (NumV 12) (NumV 4))) (BoolV #f))
(check-equal? (apply-func '<= (list (NumV 4) (NumV 4))) (BoolV #t))
(check-equal? (apply-func '<= (list (NumV 3) (NumV 4))) (BoolV #t))
(check-exn #rx"ZODE: Unknown operator, got: " (lambda () (apply-func 'j (list (NumV 3) (NumV 0)))))
(check-exn #rx"ZODE: Unknown operator, got: " (lambda () (apply-op 'j (list (NumV 3) (NumV 0)))))
(check-exn #rx"ZODE: Invalid arguments for operation" (lambda () (apply-op 'j (list (BoolV #t) (NumV 0)))))
(check-equal? (apply-func 'equals? (list (NumV 5) (NumV 6))) (BoolV #f))

;interp test cases
(check-equal? (interp (BinOpC '+ (AppC (LambC (list 'x 'y) (BinOpC '+ (IdC 'x) (IdC 'y))) (list (NumC 3) (NumC 5))) (NumC 2)) top-env) (NumV 10))

(check-exn #rx"ZODE: Expected LambC" (lambda () (interp (BinOpC '+ (AppC (NumC 4) (list (NumC 3) (NumC 5))) (NumC 2)) top-env)))
(check-exn #rx"ZODE: Number of Argument" (lambda () (interp (BinOpC '+ (AppC (LambC (list 'x 'y) (BinOpC '+ (IdC 'x) (IdC 'y))) (list (NumC 3) (NumC 5) (NumC 5))) (NumC 2)) top-env)))


(check-equal? (interp (BinOpC '- (NumC 1) (NumC 2)) top-env) (NumV -1))
(check-equal? (interp (BinOpC '* (NumC 1) (NumC 2)) top-env) (NumV 2))
(check-equal? (interp (BinOpC '/ (NumC 1) (NumC 2)) top-env) (NumV 1/2))
(check-equal? (interp (IfLeqZeroC (NumC -1) (BinOpC '+ (NumC 1) (NumC 2)) (NumC 1)) top-env) (NumV 3))
(check-equal? (interp (IfLeqZeroC (NumC 2) (BinOpC '+ (NumC 1) (NumC 2)) (NumC 1)) top-env) (NumV 1))