#lang pl

(define-type WAE
  [Num Number]
  [Add WAE WAE]
  [Sub WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  [Id Symbol]
  [With Symbol WAE WAE])

(: parse : String -> WAE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(: parse-sexpr : Sexpr -> WAE)   
(define (parse-sexpr sexpr)     
  (match sexpr
    [(number: n ) (Num n)] 
    [(symbol: name) (Id name)]
    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
    [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
    [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
    [(cons 'with _ )
      (match sexpr
        [(list 'with (list (symbol: name) named-expr) body) (With name (parse-sexpr named-expr) (parse-sexpr body))]
        [else (error 'parse-sexpr "bad 'with' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]
    ))

(: subst : WAE Symbol WAE -> WAE )
;; takes a WAE trre expr and a name and a value
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
               (subst body from to)))])) ;; substituted

 
 #|
How to evaluate (With name named-expr body)
1. v <- (eval named-expr)
2. body <- (subst body name v)
3. (eval body)
|#

(: eval : WAE -> Number)
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Id name) (error 'eval "free identifier ~s" name)]
    [(With name named-expr body)
     (eval (subst body name (Num (eval named-expr))))]))






(: run : String -> Number)
(define (run code)
  (eval (parse code)))

#|

(test (run "3") => 3)
(test (run "{ + 3 4 }") => 7)
(test (run "{ - { + 3 4 } 6 }") => 1) 




(test (parse "5") => (Num 5))
(test (parse "{ + 3 4 }") => (Add (Num 3) (Num 4)))
(test (parse "x") => (Id 'x))
(test (parse "{ + 4 2 }") => (Add (Num 4) (Num 2)))
(test (parse "{ * x x }") => (Mul (Id 'x) (Id 'x)))

(test (parse " { with {x { + 4 2 }} {* x x }}") => (With 'x
                                                         (Add (Num 4) (Num 2))
                                                         (Mul (Id 'x) (Id 'x))
                                                             ))
(test (parse "{ with x {+ 4 2 } {* x x }}") =error> "bad 'with'")
|#
 
(test (run "5") => 5)
(test (run "z") =error> "free ")
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)

(test (run "{with {x {+ 5 5}}
              {with {y {- x 3}}
                {+ x y}}}") => 17)

(test (run "{with {x 5}
              {+ x {with {x 3} 10}}}") => 15)

(test (run "{with {x 5}
             {+ x {with {x 3} {+ x x}}}}") => 11)

(test (run "{with {x {+ 3 4}} {+ x {with {y 3} x}}}") => 14)

(test (run "{with {x 5}
               {with {y {* x 3}}
               {- y x}}}") => 10)

(test (run "{with {x 8}
              {with {x {* x x}}
                 {/ x 4}}}") => 16)

(test (run "{with {x 1} y}") =error> "free identifier")





;;-------------------------------------------------------------------
#|

(test (subst
       (Mul (Id 'x) (Id 'y))  ;;expr
       'x ;; from
       (Num 5)) => ;; to
                (Mul (Num 5) (Id 'y)))


(test (subst (With 'x ;; name 
                   (Num 3) ;; name-expr
                   (Add (Id 'x) (Num 5 ))) ;;body
             'x ;; from
             (Num 8)) ;;to
      => (With 'x
               (Num 3)
               (Add (Id 'x) (Num 5))))


(test (subst (With 'x ;; name 
                   (Add (Id 'x) (Id 'x)) ;; name-expr
                   (Add (Id 'x) (Num 5 ))) ;;body
             'x ;; from
             (Num 8)) ;;to
      => (With 'x
               (Add (Num 8) (Num 8))
               (Add (Id 'x) (Num 5))))

(test (subst (With 'y ;; name 
                   (Num 3) ;; name-expr
                   (Add (Id 'x) (Num 5 ))) ;;body
             'x ;; from
             (Num 8)) ;;to
      => (With 'y
               (Num 3)
               (Add (Num 8) (Num 5))))



(test (subst (With 'y ;; name 
                   (Add (Id 'x) (Id 'x)) ;; name-expr
                   (Add (Id 'x) (Num 5 ))) ;;body
             'x ;; from
             (Num 8)) ;;to
      => (With 'y
               (Add (Num 8) (Num 8))
               (Add (Num 8) (Num 5))))

|#