
#lang pl

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG])

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
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; a type for substitution caches:
(define-type SubstCache = (Listof (List Symbol FLANG)))

(: empty-subst : SubstCache)
(define empty-subst null)

(: extend : Symbol FLANG SubstCache -> SubstCache)
(define (extend name val sc)
  (cons (list name val) sc)) 

(: lookup : Symbol SubstCache -> FLANG)
(define (lookup name sc)
  (let ([cell (assq name sc)])
    (if cell
        (second cell)
        (error 'lookup "no binding for ~s" name))))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (: Num->number : FLANG -> Number)
  (define (Num->number e)
    (cases e
      [(Num n) n]
      [else (error 'arith-op "expects a number, got: ~s" e)]))
  (Num (op (Num->number expr1) (Num->number expr2))))




(: counterx : Natural)
(define counterx 0)
;;;above eval 

(: eval : FLANG SubstCache -> FLANG)
;; evaluates FLANG expressions by reducing them to expressions
(define (eval expr sc)
  (set! counterx (add1 counterx))
  (printf "ActParmam(~a) = ~a~nSubstCache(~a) = ~a~n~n"  counterx expr counterx sc)
  
  (let ([cnt counterx]
        [res
         (cases expr
           [(Num n) expr]
           [(Add l r) (arith-op + (eval l sc) (eval r sc))]
           [(Sub l r) (arith-op - (eval l sc) (eval r sc))]
           [(Mul l r) (arith-op * (eval l sc) (eval r sc))]
           [(Div l r) (arith-op / (eval l sc) (eval r sc))]
           [(With bound-id named-expr bound-body)
            (eval bound-body
                  (extend bound-id (eval named-expr sc) sc))]
           [(Id name) (lookup name sc)]
           [(Fun bound-id bound-body) expr]
           [(Call fun-expr arg-expr)
            (let ([fval (eval fun-expr sc)])
              (cases fval
                [(Fun bound-id bound-body)
                 (eval bound-body
                       (extend bound-id (eval arg-expr sc) sc))]
                [else (error 'eval "`call' expects a function, got: ~s"
                             fval)]))])])
    (printf "Res(~a) = ~a~n~n" cnt res)
    res))


(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) empty-subst)])
    (cases result
      [(Num n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))


(run "{with {+ {fun {x} {fun {y} {* x y}}}} {with {x 4} {call {call + {+ 1 2}} 6}}}")