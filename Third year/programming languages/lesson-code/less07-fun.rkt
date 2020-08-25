#lang pl

(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [Id Symbol]
  [With Symbol FLANG FLANG]
  [Fun Symbol FLANG]
  [Call FLANG FLANG]
)
 
(: parse : String -> FLANG)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(: parse-sexpr : Sexpr -> FLANG)   
(define (parse-sexpr sexpr)     
  (match sexpr
    [(number: n ) (Num n)] 
    [(symbol: name) (Id name)]    
    [(cons 'with _ )
      (match sexpr
        [(list 'with (list (symbol: name) named-expr) body) (With name (parse-sexpr named-expr) (parse-sexpr body))]
        [else (error 'parse-sexpr "bad 'with' syntax in ~s" sexpr)])]

    [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]

    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
    [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
    [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]
    ))


#|
Formal specification for subst
  N[v/x]                = N
    {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
    {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
    {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
    {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
    y[v/x]                = y
    x[v/x]                = v
    {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
    {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
    {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
    {fun {y} E}[v/x]      = {fun {y} E[v/x]}
    {fun {x} E}[v/x]      = {fun {x} E
|#

(: subst : FLANG Symbol FLANG -> FLANG )
;; takes a FLANG tree expr and a name and a value
;; and return a new treee with the same  structure
;; but without aby free instances of name (these are replaced by value
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With name named-expr body)
     (With name ; name
           (subst named-expr from to) ; substituted name-expr
           (if (eq? name from)
               body ; non-substituted body
               (subst body from to)))] ;; substituted
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
    )) 
 



(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper (note H.O type)
(define (arith-op op expr1 expr2)
  (: Num->number : FLANG -> Number)
  (define (Num->number e)
    (cases e
      [(Num n) n]
      [else (error 'arith-op "expects a number, got: ~s" e)]))
  (Num (op (Num->number expr1) (Num->number expr2))))

 
 

(: eval : FLANG -> FLANG)                      ; <- note return type
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]                             ; <- change here
    [(Add l r) (arith-op + (eval l) (eval r))] ; <- change here
    [(Sub l r) (arith-op - (eval l) (eval r))] ; <- change here
    [(Mul l r) (arith-op * (eval l) (eval r))] ; <- change here
    [(Div l r) (arith-op / (eval l) (eval r))] ; <- change here
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]         ; <- no `(Num ...)'
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]           ; <- similar to `Num'

    [(Call fun-e arg-e) ; <- nested pattern
     (let ([funv (eval fun-e)])
       (cases funv
         [(Fun name body)
          (eval (subst  body name (eval arg-e)))]
         [ else (error 'eval "expected a function ,got ~s" funv)]))]))
      


(: run : String -> Number)
  ;; evaluate a FLANG program contained in a string
  (define (run str)
    (let ([result (eval (parse str))])
      (cases result
        [(Num n) n]
        [else (error 'run
                     "evaluation returned a non-number: ~s" result)])))


;;--------tests-----------

(test (parse "{call sqr 5}") =>
             (Call (Id 'sqr) (Num 5)))


(test (eval (Num 1 )) => (Num 1))
(test (eval (Mul (Num 1) (Num 6))) => (Num 6))
(test (eval (Fun 'x (Add (Id 'x) (Num 1)))) =>
            (Fun 'x (Add (Id 'x) (Num 1))))
 

(test (run "{with {sqr {fun {x} {* x x}}} {+ {call sqr 5 } {call sqr 6 }}}") => 
                61)

(test (run "{call {with {y 2} 
              {fun {x} {+ x y}}}
           3}") => 5)
(test (run "{call f 5 }") =error> "free identifier")

(test (run "{call {fun {x} 
                   {* x x}}
                    5}") => 25)


           


#|
(test (eval (With 'y
                  (Num 5)
                  (Fun 'x (Add (Id 'x) (Num 'y))))) =>
                  (Fun 'x (Add (Id 'x) (Num 5))))

|#

