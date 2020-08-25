;; The Flang interpreter, using environments

#lang pl

#|
  The grammar:
    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }
              | { cons <FLANG> <FLANG> }
              | { first <FLANG> }
              | { second <FLANG> }


  Evaluation rules:
    eval(N,env)                = N
    eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
    eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
    eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
    eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
    eval(x,env)                = lookup(x,env)
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x} E},env)      = <{fun {x} E}, env>
    eval({call E1 E2},env1)
             = eval(Ef,extend(x,eval(E2,env1),env2))
                               if eval(E1,env1) = <{fun {x} Ef}, env2>
             = error!          otherwise
   eval({cons E1 , E2 } , env } = <eval (E1, env) ,eval (E2, env) >
   eval({first E), env )      = if <eval(E,env) = < a,b>
                                 then a
                                 else Error
   eval({second E), env )      = if <eval(E,env) = < a,b>
                                 then b
                                 else Error
  |#

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG]
  [Cons FLANG FLANG]
  [First FLANG]
  [Second FLANG]
  )

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list 'cons f s) (Cons (parse-sexpr f) (parse-sexpr s))]
    [(list 'first p) (First (parse-sexpr p))]
    [(list 'second p) (First (parse-sexpr p))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV Symbol FLANG ENV]
  [PairV ((VAL VAL -> VAL) -> VAL)])



(: mycons : VAL VAL -> ((VAL VAL -> VAL) -> VAL))
(define (mycons f s)
  (: mypair : (VAL VAL -> VAL) -> VAL)
  (define (mypair loc-sel)
    (loc-sel f s))
  mypair)
;;

(: myfirst : ((VAL VAL -> VAL) -> VAL) -> VAL)
(define (myfirst p)
  (: f-sel : VAL VAL -> VAL)
  (define (f-sel a b) a)
  (p f-sel))

    ;;

(: mysecond  : ((VAL VAL -> VAL) -> VAL) -> VAL)
(define (mysecond  p)
  (: s-sel : VAL VAL -> VAL)
  (define (s-sel a b) b)
  (p s-sel))

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (: NumV->number : VAL -> Number)
  (define (NumV->number v)
    (cases v
      [(NumV n) n]
      [else (error 'arith-op "expects a number, got: ~s" v)]))
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: PairV->pair : VAL -> ((VAL VAL -> VAL) -> VAL))
(define (PairV->pair v)
  (cases v
    [(PairV p) p]
    [else (error 'PairV->pair "expects a pair, got: ~s" v)]))


(: eval : FLANG ENV -> VAL)
;; evaluates FLANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(Num n) (NumV n)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id bound-body)
     (FunV bound-id bound-body env)]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]
    [(Cons a b) (PairV (mycons (eval a env) (eval b env)))]
    [(First p) (myfirst (PairV->pair (eval p env)))]
    [(Second p) (mysecond (PairV->pair (eval p env)))]
    ))

(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) (EmptyEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
                {with {foo {fun {x} {+ x 1}}}
                  {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
                {with {f {fun {y} {+ x y}}}
                  {with {x 5}
                    {call f 4}}}}")
      => 7) ;; the example we considered for subst-caches
(test (run "{call {call {fun {x} {call x 1}}
                          {fun {x} {fun {y} {+ x y}}}}
                    123}")
      => 124)

;;  -- tests --

(test (run "{first {cons {call {with {x 3}
                      {fun {y} {+ x y}}}
                      4}
                     {fun {e} e}}}")       => 7)

(test (run "{call {second {cons {call {with {x 3}
                      {fun {y} {+ x y}}}
                      4}
                     {fun {e} e}}}
                    {first {cons {+ 5 2} 9}}}")       => 7)



(define p1 (mycons (NumV 1) (NumV 2)))
(define p2 (mycons (FunV 'x (Id 'x) (EmptyEnv)) (NumV 2)))
(test (myfirst p1) => (NumV 1))
(test (mysecond p1) => (NumV 2))


(test (myfirst p2) => (FunV 'x (Id 'x) (EmptyEnv)))
(test (mysecond p2) => (NumV 2))


