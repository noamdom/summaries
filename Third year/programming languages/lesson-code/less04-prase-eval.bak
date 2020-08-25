#lang pl

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE])

(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(: parse-sexpr : Sexpr -> AE)   
(define (parse-sexpr sexpr)     
  (match sexpr
    [(number: n ) (Num n)]
    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
    ))

(: eval : AE -> Number)
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]))

(: run : String -> Number)
(define (run code)
  (eval (parse code)))



(test (run "3") => 3)
(test (run "{ + 3 4 }") => 7)
(test (run "{ - { + 3 4 } 6 }") => 1)


