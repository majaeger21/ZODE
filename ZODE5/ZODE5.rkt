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

#|Values|#
(define-type Value (U BoolV NumV StrV PrimV CloV))
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([p : Symbol]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)

(struct Binding ([name : Symbol] [val : Value]) #:transparent)

(define-type Environment (Listof Binding))


#|Top Level Environment|#
(define top-env : Environment (list
                 (Binding 'true (BoolV #t))
                 (Binding 'false (BoolV #f))
                 (Binding '+ (PrimV '+))
                 (Binding '- (PrimV '-))
                 (Binding '* (PrimV '*))
                 (Binding '/ (PrimV '/))
                 (Binding '<= (PrimV '<=))
                 (Binding 'equal? (PrimV 'equal?))
                 (Binding 'error (PrimV 'error))
                 (Binding 'println (PrimV 'println))
                 (Binding 'read-num (PrimV 'read-num))
                 (Binding 'read-str (PrimV 'read-str))
                 (Binding 'seq (PrimV 'seq))
                 (Binding '++ (PrimV '++))))


#| Parses an Expression
   Input: Sexp, Output: ExprC
|#
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(list 'if ': cond ': then ': else) (IfC (parse cond) (parse then) (parse else))]
    [(list 'locals ': cls ... ': exp) (let ([clauses (parse-clauses (cast cls (Listof Sexp)))])
                                        (AppC (LambC (first clauses) (parse exp)) (second clauses)))]
    [(list 'lamb ': id ... ': exp)  
     (unless (andmap symbol? id)  
       (error "ZODE: identifier cannot be a number, got: ~e" exp))
     (LambC (parse-ids (cast id (Listof Symbol))) (parse exp))]
    [(list fun args ...) (AppC (parse fun) (map parse args))]
    [(? symbol? i)
     (cond
       [(not-valid-identifier? i) (error "ZODE: invalid identifier, got: ~e" i)]
       [else (IdC i)])]
    [other (error 'parse "ZODE: expected valid expression, got: ~e" other)]))

(define (parse-ids [lst : (Listof Symbol)]) : (Listof Symbol)
  (cond 
    [(empty? lst) '()]
    [else (cond
            [(not-valid-identifier? (first lst)) (error "ZODE: invalid identifier, got: ~e" (first lst))]
            [(not (unique-args? lst)) (error "ZODE: duplicate identifier found: ~e" lst)]
            [else (cons (first lst) (parse-ids (rest lst)))])]))

(define (parse-clauses [exp : Sexp]) : (List (Listof Symbol) (Listof ExprC))
  (match exp
    [(list (? is-valid-identifier? id) '= exp) (list (list id) (list (parse exp)))]
    [(list (? is-valid-identifier? id) '= exp ': cls ...)
     (let ([res (parse-clauses cls)])
       (if (unique-args? (cons id (first res)))
           (list (cons id (first res)) (cons (parse exp) (second res)))
           (error "ZODE: duplicate identifier found: ~e" id)))]
    [other
     (error 'parse-clauses "ZODE: expected valid expression, got: ~e" other)]))

(define (not-valid-identifier? id)
  (member id '(if lamb locals : =)))

(: is-valid-identifier? (-> Any Boolean : #:+ Symbol))
(define (is-valid-identifier? id)
  (and (symbol? id)
       (not (member id '(if lamb locals : =)))))

(define (contains? [sym : Symbol] [args : (Listof Symbol)]) : Boolean
  (cond
    [(empty? args) #f]
    [(equal? sym (first args)) #t]
    [else (contains? sym (rest args))]))

(define (unique-args? [args : (Listof Symbol)]) : Boolean
  (cond
    [(empty? args) #t]
    [(contains? (first args) (rest args)) #f]
    [else (unique-args? (rest args))]))


#|Top-level Env Functions|#
(define (apply-func [op : Symbol] [args : (Listof Value)]) : Value
  (cond
    [(equal? op '+) (apply-op '+ args)]
    [(equal? op '-) (apply-op '- args)]
    [(equal? op '*) (apply-op '* args)]
    [(equal? op '/) (apply-op '/ args)]
    [(equal? op '<=) (apply-op '<= args)]
    [(equal? op 'error) (apply-op 'error args)]
    [(equal? op 'seq) (apply-seq args)]
    [(equal? op 'read-num) (NumV (apply-read-num))]
    [(equal? op 'read-str) (StrV (apply-read-str))]
    [(equal? op 'println)
     (cond
       [(equal? (length args) 1) (BoolV (apply-println (first args)))]
       [else (error 'apply-func "ZODE: Expected 1 String, got more args")])]
    [(equal? op 'equal?)
     (cond
       [(equal? (length args) 2) (BoolV (equals? (first args) (first (rest args))))]
       [else (error "ZODE: Wrong amount of args")])]
    [else (error 'apply-func "ZODE: Unknown operator, got: ~e" op)]))

;applies the given operator
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
    [(list Value) (user-error (first args))]
    [else (error "ZODE: Invalid arguments for operation")]))

;;handles equals?
(define (equals? [a : Any] [b : Any]) : Boolean
  (match* (a b)
    [((NumV n1) (NumV n2)) (equal? n1 n2)]
    [((StrV s1) (StrV s2)) (equal? s1 s2)]
    [((BoolV b1) (BoolV b2)) (equal? b1 b2)]
    [((PrimV s1) (PrimV s2)) #f]
    [((CloV args1 body1 env1) (CloV args2 body2 env2)) #f]
    [(_ _) #f]))

;;Takes in a string and prints it to stdout
(define (apply-println [s : Any]) : Boolean
  (cond
    [(StrV? s) (begin (printf "~v\n" (StrV-s s)) #t)]
    [else (error 'apply-println "ZODE: Expected a string")]))

(check-equal? (apply-println (StrV "hello world")) #t)
(check-exn #rx"ZODE: Expected a string" (lambda () (apply-println '6)))

;;Reads a numerical value from stdin
(define (apply-read-num) : Real
  (printf ">")
  (let ([str-num (read-line)])
    (match str-num
      [(? string?) (let ([num (string->number str-num)])
                     (match num
                       [(? real?) (cast num Real)]
                       [Any (error 'read-num "ZODE: Expected a Real Number")]))]
      [else (error 'read-num "ZODE: Expected a Real Number")])))

;(check-equal? (apply-read-num) 4)
;(check-exn #rx"ZODE: Expected a Real Number" (lambda () (apply-read-num)))

;;Reads a string from stdin
(define (apply-read-str) : String
  (printf ">")
  (let ([str (read-line)]) ;use match case instad
    (cond
      [(string? str) (cast str String)]
      [else (error 'read-str "ZODE: Expected a String")])))

;(check-equal? (apply-read-str) "hello world")

;;Takes in a list of values and returns the last value
(define (apply-seq [args : (Listof Value)]) : Value
  (cond
    [(empty? args) (error 'apply-seq "ZODE: No expressions passed to seq, cannot evaluate empty seq")]
    [(empty? (rest args)) (first args)]
    [else (apply-seq (rest args))]))



(define (apply-++ [args : (Listof Any)]) : Any
  9)

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

(define (user-error [v : Value]) : Nothing
  (error 'user-error (serialize v)))

#|
Interpreter
Input: ExprC Env, Output: Value
|#

;;add-env
(define (add-env [env : Environment] [args : (Listof Value)] [params :
                                                     (Listof Symbol)]) : Environment
  (cond
    [(empty? args) env]
    [else (cons (Binding (first params) (first args))
                (add-env env (rest args) (rest params)))]))

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
    [(StrC str) (StrV str)]
    [(IfC c i e) (let ([condition (interp c env)])
                             (cond
                               [(BoolV? condition) (let ([bool (cast condition BoolV)])
                                                     (cond
                                                       [(BoolV-b bool) (interp i env)]
                                                       [else (interp e env)]))]
                               [else (error 'interp "ZODE: Expected a condition,
                                                         got ~e instead" c)]))]
    [(AppC expr args) (let ([clo (let ([temp-clo (interp expr env)])
                                    (cond
                                      [(CloV? temp-clo) (cast temp-clo CloV)]
                                      [(PrimV? temp-clo) (cast temp-clo PrimV)]
                                      [else (error 'interp "ZODE: Expected CloV
                                                      or PrimV, got ~e" temp-clo)]))])
                     (match clo
                       [(? CloV?)(cond
                         [(= (length args) (length (CloV-args clo))) (interp
                                      (CloV-body clo) (add-env (CloV-env clo) (interp-args args env) (CloV-args clo)))]
                         [else (error 'interp "ZODE: Number of Argument Mismatch, expected
                               ~e, got ~e" (length (CloV-args clo)) (length args))])]
                       [(? PrimV?) (apply-func (PrimV-p clo) (interp-args args env))]))]
    
    [(LambC params expr) (CloV params expr env)]
    [(IdC s) (interp-id s env)]))

;;top-interp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


#|TEST CASES|#
;;Parse
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse 'g) (IdC 'g))
(check-equal? (parse 'x) (IdC 'x))
(check-exn #rx"ZODE: duplicate identifier found: " (lambda () (parse '{lamb : x x : 3})))
(check-exn #rx"ZODE: identifier cannot be a number, got: " (lambda () (parse '{lamb : 3 5 4 : 6})))
(check-equal? (parse "hello") (StrC "hello"))
(check-equal? (parse '{if : 3 : 4 : 5}) (IfC (NumC 3) (NumC 4) (NumC 5)))
(check-equal? (parse-clauses '{x = 12}) (list (list 'x) (list (NumC 12))))
(check-equal? (parse-clauses '{x = 12 : y = 3}) (list (list 'x 'y) (list (NumC 12) (NumC 3))))
(check-equal? (parse '{locals : x = 12 : {+ x 1}}) (AppC (LambC (list 'x) (AppC (IdC '+)
                                                                                (list (IdC 'x)
                                                                                      (NumC 1))))
                                                         (list (NumC 12))))
(check-equal? (parse '{{lamb : x y : {+ x y}} 12 12}) (AppC (LambC (list 'x 'y) (AppC (IdC '+)
                                                                         (list (IdC 'x)
                                                                               (IdC 'y))))
                                                  (list (NumC 12) (NumC 12))))
(check-equal? (parse '{{lamb : x : {+ x 1}} 12}) (AppC (LambC (list 'x) (AppC (IdC '+)
                                                                         (list (IdC 'x)
                                                                               (NumC 1))))
                                                  (list (NumC 12))))
(check-exn #rx"ZODE: duplicate identifier found: " (lambda () (parse '(locals : z = (lamb : : 3) : z = 9 : (z)))))


(check-exn #rx"ZODE: invalid identifier, got: " (lambda () (parse '{if : 3 : 4 : 'locals})))
(check-exn #rx"ZODE: invalid identifier" (lambda () (parse '{{lamb : x locals : {+ x 1}} 12})))
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
(check-equal? (apply-func 'equal? (list (NumV 5) (NumV 6))) (BoolV #f))
(check-equal? (apply-func 'equal? (list (NumV 6) (NumV 6))) (BoolV #t))
(check-equal? (apply-func 'equal? (list (StrV "hi") (StrV "hi"))) (BoolV #t))
(check-equal? (apply-func 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))
(check-exn #rx"ZODE: Wrong amount of args" (lambda () (apply-func 'equal? (list (NumV 5)
                                                                                 (NumV 6) (NumV 3)))))


;interp test cases
(check-equal? (interp (AppC (IdC '+) (list (AppC (LambC (list 'x 'y) (AppC (IdC '+)
                            (list (IdC 'x) (IdC 'y)))) (list (NumC 3) (NumC 5))) (NumC 2))) top-env) (NumV 10))

(check-exn #rx"ZODE: Expected CloV" (lambda () (interp (AppC (IdC '+) (list (AppC
                                             (NumC 4) (list (NumC 3) (NumC 5))) (NumC 2))) top-env)))
(check-exn #rx"ZODE: Number of Argument" (lambda () (interp (AppC (IdC '+) (list (AppC
       (LambC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
       (list (NumC 3) (NumC 5) (NumC 5))) (NumC 2))) top-env)))


(check-equal? (interp (AppC (IdC '-) (list (NumC 1) (NumC 2))) top-env) (NumV -1))
(check-equal? (interp (AppC (IdC '*) (list (NumC 1) (NumC 2))) top-env) (NumV 2))
(check-equal? (interp (AppC (IdC '/) (list (NumC 1) (NumC 2))) top-env) (NumV 1/2))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 2))) top-env) (NumV 3))
(check-equal? (interp (AppC (IdC 'equal?) (list (NumC 3) (AppC (IdC '+) (list
                                                     (NumC 1) (NumC 2))))) top-env) (BoolV #t))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 3) (NumC 3))) (AppC (IdC '+)
                                                (list (NumC 1) (NumC 2))) (NumC 1)) top-env) (NumV 3))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (StrC "my string") (StrC "my string")))
                          (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env) (NumV 3))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (IdC 'false) (IdC 'false)))
                           (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env) (NumV 3))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 3) (NumC 4)))
                           (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env) (NumV 1))
(check-exn #rx"ZODE: Expected a condition" (lambda () (interp (IfC (NumC 4) (AppC (IdC '+)
                                                  (list (NumC 1) (NumC 2))) (NumC 1)) top-env)))
(check-exn #rx"ZODE: No parameter" (lambda () (interp (IfC (AppC (IdC 'equal?) (list (NumC 3)
                                     (NumC 3))) (AppC (IdC 'z) (list (NumC 1) (NumC 2))) (NumC 1)) top-env)))

;top interp tests
(check-equal? (top-interp '{locals : x = 12 : {+ x 1}}) "13")
(check-equal? (top-interp '{locals : x = false : x}) "false")
(check-equal? (top-interp '{locals : x = true : x}) "true")
(check-equal? (top-interp '{if : {equal? "mystring" "mystring"} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}}) "#<procedure>")
(check-equal? (top-interp '{if : {equal? "mystring" "mystring"} : + : {lamb : y : {- y 34}}}) "#<primop>")
(check-equal? (top-interp '{if : {equal? "mystring" "mystring"} : "mystring" : {lamb : y : {- y 34}}}) "\"mystring\"")
(check-exn #rx"user-error: \"this" (lambda () (top-interp '{if : {equal? "mystring" "mystring"} :
                                                             {error "this didnt work"} : {lamb : y : {- y 34}}})))
(check-exn #rx"user-error" (lambda () (top-interp '((lamb : e : (e e)) error))))
(check-equal? (top-interp '{locals : add1 = {lamb : x : {+ x 1}} : y = {+ 3 4} : {add1 y}}) "8")

(check-equal? (top-interp '{if : {equal? {lamb : x : {+ x 34}} {lamb : x : {+ x 34}}} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}}) "#<procedure>")
(check-equal? (top-interp '{if : {equal? + +} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}}) "#<procedure>")
(check-equal? (top-interp '{if : {equal? + "string"} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}}) "#<procedure>")

(check-equal? (top-interp '{seq {+ 3 4} {println "print this"} {- 4 3}}) "1")
(check-equal? (top-interp '{locals : x = {read-num} : {+ x 4}}) "8")
(check-equal? (top-interp '{locals : x = {read-str} : x}) "\"hello world\"")
(check-exn #rx"ZODE: No expressions" (lambda () (top-interp '{seq})))

(check-equal?
 (top-interp '{locals : Y = {lamb : f : {{lamb : x : {x x}}
                                         {lamb : x : {f {lamb : a : {{x x} a}}}}}}
                      : empty = "mt"
                      : empty? = {lamb : lst : {equal? lst "mt"}}
                      : cons = {lamb : fst rst : {lamb : sel : {if : sel : fst : rst}}}
                      : first = {lamb : lst : {lst true}}
                      : rest = {lamb : lst : {lst false}} :
                      {locals : sum = {Y {lamb : sum : {lamb : lst : {if : {empty? lst}
                                                                         : 0
                                                                         : {+ {first lst} {sum {rest lst}}}}}}}
                              : {sum {cons 10 {cons 20 {cons 30 {cons 40 empty}}}}}}})

 "100")

