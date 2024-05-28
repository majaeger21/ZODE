#lang typed/racket
(require typed/rackunit)
(require racket/math)

#|Project Status: FULLY IMPLEMENTED|#

#|ZODE4 Data Types|#
(define-type ExprC (U NumC IdC StrC IfC LambC AppC MutC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LambC ([id : (Listof Symbol)] [exp : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct MutC ([orig : IdC] [new : ExprC]) #:transparent)

#|Values|#
(define-type Value (U BoolV NumV StrV PrimV CloV NullV ArrayV))
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([p : Symbol]) #:transparent)
(struct NullV ([nu : Symbol]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)
(struct ArrayV ([loc : Integer] [len : Integer]) #:transparent)

(struct Binding ([name : Symbol] [loc : Integer]) #:transparent)

(define-type Environment (Listof Binding))
(define-type Store (Mutable-Vectorof Value))


#|Top Level Environment|#
(define top-env : Environment (list
                 (Binding 'true 1)
                 (Binding 'false 2)
                 (Binding 'null 3)
                 (Binding '+ 4)
                 (Binding '- 5)
                 (Binding '* 6)
                 (Binding '/ 7)
                 (Binding '<= 8)
                 (Binding 'equal? 9)
                 (Binding 'error 10)
                 (Binding 'println 11)
                 (Binding 'read-num 12)
                 (Binding 'read-str 13)
                 (Binding 'seq 14)
                 (Binding '++ 15)))

(define top-store : (Mutable-Vectorof Value)(vector
                     (NumV 16)
                     (BoolV #t)
                     (BoolV #f)
                     (NullV 'null)
                     (PrimV '+)
                     (PrimV '-)
                     (PrimV '*)
                     (PrimV '/)
                     (PrimV '<=)
                     (PrimV 'equal?)
                     (PrimV 'error)
                     (PrimV 'println)
                     (PrimV 'read-num)
                     (PrimV 'read-str)
                     (PrimV 'seq)
                     (PrimV '++)))

#|
   Parses an Expression
   Input: Sexp, Output: ExprC
|#
(define (parse [exp : Sexp]) : ExprC
  (match exp
    ;<num>
    [(? real? n) (NumC n)]
    ;<string>
    [(? string? s) (StrC s)] 
    ; { if : ‹expr› : ‹expr› : ‹expr› }
    [(list 'if ': cond ': then ': else) (IfC (parse cond) (parse then) (parse else))] 
    ; { locals : ‹clauses› : ‹expr› }
    [(list 'locals ': cls ... ': exp) (let ([clauses (parse-clauses (cast cls (Listof Sexp)))])
                                        (AppC (LambC (first clauses) (parse exp)) (second clauses)))]
    ; { lamb : ‹id›* : ‹expr› }
    [(list 'lamb ': id ... ': exp)
     (unless (andmap symbol? id)  
       (error "ZODE: identifier cannot be a number, got: ~e" exp))
     (LambC (parse-ids (cast id (Listof Symbol))) (parse exp))]
    ; { mut : <id> : <expr>}
    [(list id ':= new-exp) (MutC (IdC (cast id Symbol)) (parse new-exp))]
    ; { ‹expr› ‹expr›* }
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
    ; ‹id› = ‹expr›
    [(list (? is-valid-identifier? id) '= exp) (list (list id) (list (parse exp)))]
    ; ‹id› = ‹expr› : ‹clauses›
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

;create the store
(define (create-store [size : Integer])
  (cond
    [(< size 16) (error 'create-store "ZODE: Allocated Memory Insufficient")]
    [else (vector-append top-store (cast (make-vector (- size 16) (BoolV #f)) Store))]))

(check-exn #rx"ZODE: Allocated" (lambda () (create-store 10)))

;;add-env
(define (add-env [env : Environment] [args : (Listof Value)] [params :
                                                     (Listof Symbol)] [store : Store]) : Environment
  (cond
    [(empty? args) env]
    [else (cons (let ([index (add-store (first args) store)]) (Binding (first params) index))
                (add-env env (rest args) (rest params) store))]))

;;mutate-store
(define (mutate-store [env : Environment] [store : Store] [orig : IdC] [new : ExprC]) : NullV
  (cond
    [(empty? env) (error 'mutate-store "ZODE: unbound identifier: `e")]
    [(equal? (Binding-name (first env)) (IdC-s orig)) (begin
                                         (set-store store (interp new env store) (Binding-loc (first env))))]
    [else (mutate-store (rest env) store orig new)]))

(check-exn #rx"ZODE: unbound" (lambda () (mutate-store top-env (create-store 35) (IdC 'x) (NumC 34))))

;;set-store
(define (set-store [store : Store] [value : Value] [index : Integer]) : NullV
  (vector-set! store index value)
  (NullV 'null))

;;add-store
(define (add-store [v : Value] [store : Store]) : Integer
  
  (let ([index (cast (NumV-n (cast (vector-ref store 0) NumV)) Integer)])
    (println (~v index))
    (cond
      [(<= (vector-length store) index) (error 'add-store
                "ZODE: Index out of Bounds Error, attempting to add ~e out of bounds" v)]
      [else (begin (vector-set! store index v)
           (println "This ran")
           
           (vector-set! store 0 (NumV (+ index 1)))
           (println (~v (cast (NumV-n (cast (vector-ref store 0) NumV)) Integer)))
           index)])))

(check-exn #rx"ZODE: Index" (lambda () (add-store (NumV 0) top-store)))


;;allocate
#;(define (allocate [store : Store] [values : (Listof Value)]) : Integer
  (let ([base (add-store (first values) store)])
    (cond
      [(empty? (rest values)) base]
      [else (allocate store (rest values))])))


;;while
'{locals : while = {lamb
                   : self guard body
                   : {if
                      : {guard}
                      : {seq {body}
                             {self self guard body}}
                      : null}}
         : {locals : in-order = {lamb
                                 : arr len
                                 : {locals : i = 0 : condition = false
                                           : {seq {while while {lamb : : {<= i len}}
                                                         {lamb : : {if : {<= i {- len 1}}
                                                                        : {i := {+ i 1}}
                                                                        : {if : {<= {aref arr i} {aref arr i + 1}}
                                                                              : {i := {+ i 1}}
                                                                              : {condition := false}}}}}
                                                  condition}}}
                   : {in-order {array 3 5 6 17 18 90 104} 7}}}



#| Top-level Env Functions  |#
(define (apply-func [op : Symbol] [args : (Listof Value)] [store : Store]) : Value
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
    [(equal? op '++) (apply-++ args)]
    [(equal? op 'println)
     (cond
       [(equal? (length args) 1) (BoolV (apply-println (first args)))]
       [else (error 'apply-func "ZODE: Expected 1 String, got more args")])]
    [(equal? op 'equal?)
     (cond
       [(equal? (length args) 2) (BoolV (equals? (first args) (first (rest args))))]
       [else (error "ZODE: Wrong amount of args")])]
    [(equal? op 'make-array)
     (cond 
       [(equal? (length args) 2)
        (let ([size (cast (NumV-n (cast (first args) NumV)) Integer)]
              [value (second args)])
          (make-array size value store))]
       [else (error 'apply-func "ZODE: make-array expects 2 arguments")])]
    [(equal? op 'array) (array args store)]
    [(equal? op 'aref)
     (cond
       [(equal? (length args) 2)
        (let ([arr (cast (first args) ArrayV)]
              [index (cast (NumV-n (cast (second args) NumV)) Integer)])
          (aref arr index store))]
       [else (error 'apply-func "ZODE: aref expects 2 arguments")])]
    [(equal? op 'aset!)
     (cond
       [(equal? (length args) 3)
        (let ([arr (cast (first args) ArrayV)]
              [index (cast (NumV-n (cast (second args) NumV)) Integer)]
              [new (third args)])
          (aset! store arr index new))]
       [else (error 'apply-func "ZODE: aset! expects 3 arguments")])]
    [(equal? op 'sub-string)
     (cond
       [(equal? (length args) 3)
        (let ([str-index (cast (NumV-n (cast (first args) NumV)) Integer)]
              [start (cast (NumV-n (cast (second args) NumV)) Integer)]
              [end (cast (NumV-n (cast (third args) NumV)) Integer)])
          (sub-string str-index start end store))]
       [else (error 'apply-func "ZODE: sub-string expects 3 arguments")])]
    [else (error 'apply-func "ZODE: Unknown operator, got: ~e" op)]))

;; applies the given operator
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

;; creates a fresh array of the given size, with all cells filled with the given value
(define (make-array [size : Integer] [value : Value] [store : Store]) : ArrayV
  (if (<= size 0)
      (error 'make-array "ZODE: Size must be greater than 0")
      (let ([start-index (cast (NumV-n (cast (vector-ref store 0) NumV)) Integer)])
        (for ([i (in-range size)])
          (vector-set! store (+ start-index i) value))
        (vector-set! store 0 (NumV (+ start-index size)))
        (ArrayV start-index size))))

              
;; creates a fresh array containing the given values
(define (array [values : (Listof Value)] [store : Store]) : ArrayV
  (if (empty? values)
      (error 'array "ZODE: Array must contain at least one element")
      (let ([start-index (cast (NumV-n (cast (vector-ref store 0) NumV)) Integer)])
        (for ([i (in-range (length values))])
          (vector-set! store (+ start-index i) (list-ref values i)))
        (vector-set! store 0 (NumV (+ start-index (length values))))
        (ArrayV start-index (length values)))))

;; returns the contents of given element of the array 
(define (aref [array : ArrayV] [index : Integer] [store : Store]) : Value
  (let ([start-index (ArrayV-loc array)]
        [length (ArrayV-len array)])
    (if (or (< index 0) (>= index length))
        (error 'aref "ZODE: Index out of range")
        (vector-ref store (+ start-index index)))))

;; sets the given element to be the result of calling function
(define (aset! [store : Store] [arr : ArrayV] [index : Integer] [new : Value]) : NullV
  (cond
    [(or (< index 0) (>= index (ArrayV-len arr))) (error 'aset!
                "ZODE: Index out of Bounds Error, attempting to add ~e out of bounds" new)]
    [else (set-store store new (+ (ArrayV-loc arr) index))]))


;; accepts a string and a start and end position and returns the corresponding substring
(define (sub-string [str-index : Integer] [start : Integer] [end : Integer] [store : Store]) : StrV
  (let* ([str-val (vector-ref store str-index)]
         [s (StrV-s (cast str-val StrV))])
    (cond
      [(< start 0) (error 'sub-string "ZODE: Start needs to be 0 or greater")]
      [(> end (string-length s)) (error 'sub-string "ZODE: End needs to be less than/equal to string length")]
      [(>= start end) (error 'sub-string "ZODE: Start must be less than end")]
      [else (StrV (substring s start end))])))

;; handles equals?
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

(check-exn #rx"ZODE: Ex" (lambda () (apply-println 34)))

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

;;Reads a string from stdin
(define (apply-read-str) : String
  (printf ">")
  (let ([str (read-line)]) ;use match case instad
    (cond
      [(string? str) (cast str String)]
      [else (error 'read-str "ZODE: Expected a String")])))

;;Takes in a list of values and returns the last value
(define (apply-seq [args : (Listof Value)]) : Value
  (cond
    [(empty? args) (error 'apply-seq "ZODE: No expressions passed to seq, cannot evaluate empty seq")]
    [(empty? (rest args)) (first args)]
    [else (apply-seq (rest args))]))


;; Takes a list of Values and returns the arguments into a single string
;; Helper function to extract the string from Value
(define (value->string [v : Value]) : String
  (match v
    [(NumV n) (number->string n)]
    [(StrV s) s]
    [(BoolV b) (if b "true" "false")]
    [(PrimV p) (symbol->string p)]
    [(CloV args body env) "#<procedure>"]
    [else (error 'value->string "Unexpected Value type")]))

;; Main function to concatenate values into a single string
(define (apply-++ [args : (Listof Value)]) : Value
  (cond
    [(empty? args) (error 'apply-++ "ZODE: No expressions passed to ++, cannot evaluate empty args")]
    [(empty? (rest args)) (first args)]
    [else (StrV (string-append (value->string (first args))
                               (value->string (apply-++ (rest args)))))]))

#|
Serialize
Input: ZODE4 Value, Output: String
|#
(define (serialize (v : Value)) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV b) (if b "true" "false")]
    [(NullV nu) "null"]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV op) (format "#<primop>")]))

(define (user-error [v : Value]) : Nothing
  (error 'user-error (serialize v)))

#|
Interpreter
Input: ExprC Env, Output: Value
|#

;;interp-IdC
(define (interp-id [s : Symbol] [env : Environment] [store : Store]) : Value
  (cond
    [(empty? env) (error 'interp-id "ZODE: No parameter matching id: ~e" s)]
    [(equal? s (Binding-name (first env))) (vector-ref store (Binding-loc (first env)))]
    [else (interp-id s (rest env) store)]))

;;interp-args
(define (interp-args [args : (Listof ExprC)] [env : Environment] [store : Store]) : (Listof Value)
  (cond
    [(empty? args) '()]
    [else (cons (interp (first args) env store)(interp-args (rest args) env store))]))

;interp
;;accepts an ExprC and a list of function definitions and returns a Real number (the value)
(define (interp [exp : ExprC] [env : Environment] [store : Store]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(StrC str) (StrV str)]
    [(MutC orig new) (mutate-store env store orig new)]
    [(IfC c i e) (let ([condition (interp c env store)])
                             (cond
                               [(BoolV? condition) (let ([bool (cast condition BoolV)])
                                                     (cond
                                                       [(BoolV-b bool) (interp i env store)]
                                                       [else (interp e env store)]))]
                               [else (error 'interp "ZODE: Expected a condition,
                                                         got ~e instead" c)]))]
    [(AppC expr args) (let ([clo (let ([temp-clo (interp expr env store)])
                                    (cond
                                      [(CloV? temp-clo) (cast temp-clo CloV)]
                                      [(PrimV? temp-clo) (cast temp-clo PrimV)]
                                      [else (begin (printf "~v" expr)(error 'interp "ZODE: Expected CloV
                                                      or PrimV, got ~e in ~e" temp-clo expr))]))])
                     (match clo
                       [(? CloV?)(cond
                         [(= (length args) (length (CloV-args clo))) (interp
                                      (CloV-body clo) (add-env (CloV-env clo)
                                                (interp-args args env store) (CloV-args clo) store) store)]
                         [else (error 'interp "ZODE: Number of Argument Mismatch, expected
                               ~e - ~e, got ~e - ~e" (length (CloV-args clo)) (CloV-args clo) (length args) args)])]
                       [(? PrimV?) (apply-func (PrimV-p clo) (interp-args args env store) store)]))]
    
    [(LambC params expr) (CloV params expr env)]
    [(IdC s) (interp-id s env store)]
    ))



;;top-interp
(define (top-interp [s : Sexp] [memsize : Integer]) : String
  (serialize (interp (parse s) top-env (create-store memsize))))


(check-equal? (top-interp '{locals : i = 0 : {i := 2}} 35) "null")


;; PROGRAM: Euclidean Algorithm for GCD
'{locals
                            : getA = {lamb : : {read-num}}
                            : getB = {lamb : : {read-num}}
                            : gcd = {lamb : self a b :
                                          {locals : getQ = {lamb : self a b q
                                                                 : {if : {<= {* b q} a}
                                                                       : {if : {equal? {* q b} a}
                                                                             : q
                                                                             : {self self a b {+ q 1}}}
                                                                       : {- q 1}}}
                                                  : getRemainder = {lamb : a b q : {- a {* b q}}}
                                                  : {locals
                                                     : q = {getQ getQ a b 1}
                                                     : {if
                                                        : {equal? {getRemainder a b q} 0}
                                                        : b
                                                        : {self self b {getRemainder a b q}}}}}}
          : {seq {println "Please enter two numbers: \n"} {locals : myGCD = {gcd gcd {getA} {getB}}
                                                                  : {seq {println {++ "GCD : " myGCD}} myGCD}}}}

#|
Results:
"Please enter two numbers to calculate their GCD:"
>3422
>4582
"GCD : 58"
|#

; Uncomment program below 
#;(top-interp '{locals
                            : getA = {lamb : : {read-num}}
                            : getB = {lamb : : {read-num}}
                            : gcd = {lamb : self a b :
                                          {locals : getQ = {lamb : self a b q
                                                                 : {if : {<= {* b q} a}
                                                                       : {if : {equal? {* q b} a}
                                                                             : q
                                                                             : {self self a b {+ q 1}}}
                                                                       : {- q 1}}}
                                                  : getRemainder = {lamb : a b q : {- a {* b q}}}
                                                  : {locals
                                                     : q = {getQ getQ a b 1}
                                                     : {if
                                                        : {equal? {getRemainder a b q} 0}
                                                        : b
                                                        : {self self b {getRemainder a b q}}}}}}
          : {seq {println "Please enter two numbers to calculate their GCD:"} {locals : myGCD = {gcd gcd {getA} {getB}}
                                                                  : {seq {println {++ "GCD : " myGCD}} myGCD}}}})



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
(define store (create-store 50))
(check-equal? (apply-func '+ (list (NumV 5) (NumV 3)) store) (NumV 8))
(check-equal? (apply-func '- (list (NumV 10) (NumV 4)) store) (NumV 6))
(check-equal? (apply-func '* (list (NumV 7) (NumV 2)) store) (NumV 14))
(check-equal? (apply-func '/ (list (NumV 12) (NumV 4)) store) (NumV 3))
(check-exn #rx"ZODE: Division by zero" (lambda () (apply-func '/ (list (NumV 3) (NumV 0)) store)))
(check-equal? (apply-func '<= (list (NumV 12) (NumV 4)) store) (BoolV #f))
(check-equal? (apply-func '<= (list (NumV 4) (NumV 4)) store) (BoolV #t))
(check-equal? (apply-func '<= (list (NumV 3) (NumV 4)) store) (BoolV #t))
(check-exn #rx"ZODE: Unknown operator, got: " (lambda () (apply-func 'j (list (NumV 3) (NumV 0)) store)))
(check-exn #rx"ZODE: Unknown operator, got: " (lambda () (apply-op 'j (list (NumV 3) (NumV 0)))))
(check-exn #rx"ZODE: Invalid arguments for operation" (lambda () (apply-op 'j (list (BoolV #t) (NumV 0)))))
(check-equal? (apply-func 'equal? (list (NumV 5) (NumV 6)) store) (BoolV #f))
(check-equal? (apply-func 'equal? (list (NumV 6) (NumV 6)) store) (BoolV #t))
(check-equal? (apply-func 'equal? (list (StrV "hi") (StrV "hi")) store) (BoolV #t))
(check-equal? (apply-func 'equal? (list (BoolV #t) (BoolV #f)) store) (BoolV #f))
(check-exn #rx"ZODE: Wrong amount of args" (lambda () (apply-func 'equal? (list (NumV 5)
                                                                                (NumV 6) (NumV 3)) store)))
;(check-equal? (apply-read-num) 4)
;(check-exn #rx"ZODE: Expected a Real Number" (lambda () (apply-read-num)))
;(check-equal? (apply-read-str) "hello world")
(check-equal? (apply-++ (list (StrV "Hello"))) 
              (StrV "Hello"))
(check-equal? (apply-++ (list (StrV "123") (StrV "456") (StrV "789"))) 
              (StrV "123456789"))
(check-equal? (apply-++ (list (NumV 9) (NumV 10)))
              (StrV "910"))
(check-equal? (apply-++ (list (NumV 8) (BoolV #t))) (StrV "8true"))
(check-equal? (apply-++ (list (NumV 8) (BoolV #f))) (StrV "8false"))
(check-equal? (apply-++ (list (PrimV '+) (BoolV #f))) (StrV "+false"))

;; apply-func test cases for 'make-array, 'array, 'aref, 'aset!, 'sub-string 
(let ([store (create-store 25)])
  ;; Test make-array with size 3 and value 0.0
  (check-equal? (apply-func 'make-array (list (NumV 3) (NumV 0.0)) store) (ArrayV 16 3))
  (check-equal? (vector-ref store 16) (NumV 0.0))
  (check-equal? (vector-ref store 17) (NumV 0.0))
  (check-equal? (vector-ref store 18) (NumV 0.0))
  (check-equal? (vector-ref store 0) (NumV 19)))


(let ([store (create-store 25)])
  ;; Test make-array with invalid size 0
  (check-exn #rx"ZODE: Size must be greater than 0" (lambda ()
                                 (apply-func 'make-array (list (NumV 0) (NumV 0.0)) store))))

(let ([store (create-store 25)])
  ;; Test array with two string values
  (check-equal? (apply-func 'array (list (StrV "hello") (StrV "world")) store) (ArrayV 16 2))
  (check-equal? (vector-ref store 16) (StrV "hello"))
  (check-equal? (vector-ref store 17) (StrV "world"))
  (check-equal? (vector-ref store 0) (NumV 18)))

(let ([store (create-store 25)])
  ;; Test array with invalid empty list
  (check-exn #rx"ZODE: Array must contain at least one element" (lambda () (apply-func 'array '() store))))

(let ([store (create-store 25)])
  ;; Create an array using array with multiple values
  (define arr (apply-func 'array (list (NumV 1) (NumV 2) (NumV 3)) store))
  ;; Test aref to access elements within the array
  (check-equal? (apply-func 'aref (list arr (NumV 0)) store) (NumV 1))
  (check-equal? (apply-func 'aref (list arr (NumV 1)) store) (NumV 2))
  (check-equal? (apply-func 'aref (list arr (NumV 2)) store) (NumV 3))
  ;; Test aref to access an out-of-bounds index
  (check-exn #rx"ZODE: Index out of range" (lambda () (apply-func 'aref (list arr (NumV 3)) store)))
  (check-exn #rx"ZODE: Index out of range" (lambda () (apply-func 'aref (list arr (NumV -1)) store))))

(let ([store (create-store 25)])
  ;; Create an array using array with multiple values
  (define arr (apply-func 'array (list (NumV 1) (NumV 2) (NumV 3)) store))
  ;; Test aset! to set elements within the array
  (apply-func 'aset! (list arr (NumV 0) (NumV 4)) store)
  (apply-func 'aset! (list arr (NumV 1) (NumV 5)) store)
  (apply-func 'aset! (list arr (NumV 2) (NumV 6)) store)
  ;; Verify the changes in the store
  (check-equal? (vector-ref store 16) (NumV 4))
  (check-equal? (vector-ref store 17) (NumV 5))
  (check-equal? (vector-ref store 18) (NumV 6))
  ;; Test aset! to set an out-of-bounds index
  (check-exn #rx"ZODE: Index out of Bounds Error" (lambda () (apply-func 'aset! (list arr (NumV 3) (NumV 7)) store)))
  (check-exn #rx"ZODE: Index out of Bounds Error" (lambda () (apply-func 'aset! (list arr (NumV -1) (NumV 7)) store))))

(let ([store (create-store 25)])
  ;; Store the string "hello world" at index 16
  (vector-set! store 16 (StrV "hello world"))

  ;; Test sub-string with valid range
  (check-equal? (sub-string 16 0 5 store) (StrV "hello"))
  (check-equal? (sub-string 16 6 11 store) (StrV "world"))
  (check-equal? (sub-string 16 3 8 store) (StrV "lo wo"))

  ;; Test sub-string with start index out of range
  (check-exn #rx"ZODE: Start needs to be 0 or greater" (lambda () (sub-string 16 -1 5 store)))

  ;; Test sub-string with end index out of range
  (check-exn #rx"ZODE: End needs to be less than/equal to string length" (lambda () (sub-string 16 0 12 store)))

  ;; Test sub-string with start index greater than end index
  (check-exn #rx"ZODE: Start must be less than end" (lambda () (sub-string 16 5 3 store)))
)


(let ([store (create-store 25)])
  ;; Test error for println with incorrect number of arguments
  (check-exn #rx"ZODE: Expected 1 String, got more args" 
             (lambda () (apply-func 'println (list (StrV "Hello") (StrV "World")) store)))

  ;; Test error for equal? with incorrect number of arguments
  (check-exn #rx"ZODE: Wrong amount of args" 
             (lambda () (apply-func 'equal? (list (NumV 1) (NumV 2) (NumV 3)) store)))

  ;; Test error for make-array with incorrect number of arguments
  (check-exn #rx"ZODE: make-array expects 2 arguments" 
             (lambda () (apply-func 'make-array (list (NumV 10)) store)))

  ;; Test error for array with no arguments
  (check-exn #rx"ZODE: array expects at least 1 argument" 
             (lambda () (apply-func 'array '() store)))

  ;; Test error for aref with incorrect number of arguments
  (check-exn #rx"ZODE: aref expects 2 arguments" 
             (lambda () (apply-func 'aref (list (ArrayV 0 10)) store)))

  ;; Test error for aset! with incorrect number of arguments
  (check-exn #rx"ZODE: aset! expects 3 arguments" 
             (lambda () (apply-func 'aset! (list (ArrayV 0 10) (NumV 0)) store)))

  ;; Test error for sub-string with incorrect number of arguments
  (check-exn #rx"ZODE: sub-string expects 3 arguments" 
             (lambda () (apply-func 'sub-string (list (NumV 0) (NumV 0)) store))))


;; Test cases for make-array
(let ([store (create-store 25)]) ;; Ensure sufficient size for operations
  (check-equal? (make-array 3 (NumV 0.0) store) (ArrayV 16 3))
  (check-equal? (vector-ref store 16) (NumV 0.0))
  (check-equal? (vector-ref store 17) (NumV 0.0))
  (check-equal? (vector-ref store 18) (NumV 0.0))
  (check-equal? (vector-ref store 0) (NumV 19))
  ;; New check to retrieve an element from the store and verify it matches the initialization value
  (let ([array (make-array 3 (NumV 0.0) store)])
    (check-equal? (vector-ref store (ArrayV-loc array)) (NumV 0.0))))

(let ([store (create-store 25)]) ;; Ensure sufficient size for operations
  (check-equal? (make-array 5 (BoolV #t) store) (ArrayV 16 5))
  (check-equal? (vector-ref store 16) (BoolV #t))
  (check-equal? (vector-ref store 17) (BoolV #t))
  (check-equal? (vector-ref store 18) (BoolV #t))
  (check-equal? (vector-ref store 19) (BoolV #t))
  (check-equal? (vector-ref store 20) (BoolV #t))
  (check-equal? (vector-ref store 0) (NumV 21)))

(check-exn #rx"ZODE: Size must be greater than 0" (lambda () (make-array 0 (NumV 0.0) (create-store 25))))

;; Test cases for array
(let ([store (create-store 25)]) 
  (check-equal? (array (list (NumV 3) (NumV 14) (BoolV #f) (NumV 5)) store) (ArrayV 16 4))
  (check-equal? (vector-ref store 16) (NumV 3))
  (check-equal? (vector-ref store 17) (NumV 14))
  (check-equal? (vector-ref store 18) (BoolV #f))
  (check-equal? (vector-ref store 19) (NumV 5))
  (check-equal? (vector-ref store 0) (NumV 20)))

(let ([store (create-store 25)]) 
  (check-equal? (array (list (BoolV #t)) store) (ArrayV 16 1))
  (check-equal? (vector-ref store 16) (BoolV #t))
  (check-equal? (vector-ref store 0) (NumV 17)))

(let ([store (create-store 25)]) 
  (check-equal? (array (list (StrV "hello") (StrV "world")) store) (ArrayV 16 2))
  (check-equal? (vector-ref store 16) (StrV "hello"))
  (check-equal? (vector-ref store 17) (StrV "world"))
  (check-equal? (vector-ref store 0) (NumV 18)))

(check-exn #rx"ZODE: Array must contain at least one element" (lambda () (array '() (create-store 25))))

;; Test cases for aref
(let ([store (create-store 25)])
  (let ([array (make-array 4 (NumV 42.0) store)])
    (check-equal? (aref array 0 store) (NumV 42.0))
    (check-equal? (aref array 1 store) (NumV 42.0))
    (check-equal? (aref array 2 store) (NumV 42.0))
    (check-equal? (aref array 3 store) (NumV 42.0))
    (check-exn #rx"ZODE: Index out of range" (lambda () (aref array 4 store)))
    (check-exn #rx"ZODE: Index out of range" (lambda () (aref array -1 store)))))

(let ([store (create-store 25)])
  (let ([array (make-array 3 (BoolV #t) store)])
    (check-equal? (aref array 0 store) (BoolV #t))
    (check-equal? (aref array 1 store) (BoolV #t))
    (check-equal? (aref array 2 store) (BoolV #t))
    (check-exn #rx"ZODE: Index out of range" (lambda () (aref array 3 store)))
    (check-exn #rx"ZODE: Index out of range" (lambda () (aref array -1 store)))))

(let ([store (create-store 25)]) 
  (let ([array (make-array 2 (StrV "hello") store)])
    (check-equal? (aref array 0 store) (StrV "hello"))
    (check-equal? (aref array 1 store) (StrV "hello"))
    (check-exn #rx"ZODE: Index out of range" (lambda () (aref array 2 store)))
    (check-exn #rx"ZODE: Index out of range" (lambda () (aref array -1 store)))))


;interp test cases
(check-equal? (interp (AppC (IdC '+) (list (AppC (LambC (list 'x 'y) (AppC (IdC '+)
                            (list (IdC 'x) (IdC 'y)))) (list (NumC 3) (NumC 5)))
                                           (NumC 2))) top-env (create-store 35)) (NumV 10) )

#;(check-exn #rx"ZODE: Expected CloV" (lambda () (interp (AppC (IdC '+) (list (AppC
                                             (NumC 4) (list (NumC 3) (NumC 5))) (NumC 2))) top-env)))
#;(check-exn #rx"ZODE: Number of Argument" (lambda () (interp (AppC (IdC '+) (list (AppC
       (LambC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
       (list (NumC 3) (NumC 5) (NumC 5))) (NumC 2))) top-env)))


(check-equal? (interp (AppC (IdC '-) (list (NumC 1) (NumC 2))) top-env (create-store 35)) (NumV -1))
(check-equal? (interp (AppC (IdC '*) (list (NumC 1) (NumC 2))) top-env (create-store 35)) (NumV 2))
(check-equal? (interp (AppC (IdC '/) (list (NumC 1) (NumC 2))) top-env (create-store 35)) (NumV 1/2))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 2))) top-env (create-store 35)) (NumV 3))

(check-equal? (interp (AppC (IdC 'equal?) (list (NumC 3) (AppC (IdC '+) (list
                                                     (NumC 1) (NumC 2))))) top-env (create-store 35)) (BoolV #t))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 3) (NumC 3))) (AppC (IdC '+)
                                                (list (NumC 1) (NumC 2))) (NumC 1)) top-env (create-store 35)) (NumV 3))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (StrC "my string") (StrC "my string")))
                          (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env (create-store 35)) (NumV 3))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (IdC 'false) (IdC 'false)))
                           (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env (create-store 35)) (NumV 3))
(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 3) (NumC 4)))
                           (AppC (IdC '+) (list (NumC 1) (NumC 2))) (NumC 1)) top-env (create-store 35)) (NumV 1))
(check-exn #rx"ZODE: Expected a condition" (lambda () (interp (IfC (NumC 4) (AppC (IdC '+)
                                                  (list (NumC 1) (NumC 2))) (NumC 1)) top-env (create-store 35))))
(check-exn #rx"ZODE: No parameter" (lambda () (interp (IfC (AppC (IdC 'equal?) (list (NumC 3)
                                     (NumC 3))) (AppC (IdC 'z) (list (NumC 1) (NumC 2)))
                                                (NumC 1)) top-env (create-store 35))))

;top interp tests
(check-equal? (top-interp '{locals : x = 12 : {+ x 1}} 35) "13")
(check-equal? (top-interp '{locals : x = false : x} 35) "false")
(check-equal? (top-interp '{locals : x = true : x} 35) "true")
(check-equal? (top-interp '{if : {equal? "mystring" "mystring"} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}} 35) "#<procedure>")
(check-equal? (top-interp '{if : {equal? "mystring" "mystring"} : + : {lamb : y : {- y 34}}} 35) "#<primop>")
(check-equal? (top-interp '{if : {equal? "mystring" "mystring"}
                               : "mystring" : {lamb : y : {- y 34}}} 35) "\"mystring\"")
(check-exn #rx"user-error: \"this" (lambda () (top-interp '{if : {equal? "mystring" "mystring"} :
                                                             {error "this didnt work"} : {lamb : y : {- y 34}}} 35)))
(check-exn #rx"user-error" (lambda () (top-interp '((lamb : e : (e e)) error) 35)))
(check-equal? (top-interp '{locals : add1 = {lamb : x : {+ x 1}} : y = {+ 3 4} : {add1 y}} 35) "8")

(check-equal? (top-interp '{if : {equal? {lamb : x : {+ x 34}} {lamb : x : {+ x 34}}} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}} 35) "#<procedure>")
(check-equal? (top-interp '{if : {equal? + +} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}} 35) "#<procedure>")
(check-equal? (top-interp '{if : {equal? + "string"} :
                               {lamb : x : {+ x 34}} : {lamb : y : {- y 34}}} 35) "#<procedure>")

(check-equal? (top-interp '{seq {+ 3 4} {println "print this"} {- 4 3}} 35) "1")
(check-equal? (top-interp '{seq {+ 3 4} {println {++ "print this" " and then this"}} {- 4 3}} 35) "1")

;(check-equal? (top-interp '{locals : x = {read-num} : {+ x 4}} 35) "8")
;(check-equal? (top-interp '{locals : x = {read-str} : x} 35) "\"hello world\"")
(check-exn #rx"ZODE: No expressions" (lambda () (top-interp '{seq} 35)))

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
                              : {sum {cons 10 {cons 20 {cons 30 {cons 40 empty}}}}}}} 500)

 "100")