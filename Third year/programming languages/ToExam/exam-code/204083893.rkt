#lang pl

#|##########################################
                 Section 1

############################################
|#


#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#

 
;; -----------------------------------------------------


;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [Id    Symbol]
  ;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL])


#|##########################################
                 Section 2

############################################
|#


;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
#|
This function is checking if number is member in list of number
opreate in a recursive way return true if it is, otherwise false
|#
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))

(: remove-duplicates : SET  -> SET)
#|
This function is checking remove duplicates numbers in the list
I filled the case of checking if first number is memeber in ther rest of the list and if it is, we skip over it,
and in the and the list we will stay with will be without it.
|#
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l)) ] ;; #t <=> first is dup' --> skip over first
        [else (cons (first l) (remove-duplicates (rest l)))]))
 
(test (remove-duplicates '(3 4 5)) => '(3 4 5))
(test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))

 
(: create-sorted-set : SET -> SET)
#|
This function get list of numbers  sort the by size (<) and  than removed the duplicate numbers and
 return the list
you lead as to implelmnt that in this oreder, and I think it's better to first to remove
the duplicates and the sort
|#
(define (create-sorted-set l)
  (remove-duplicates (sort l <) ))
  ;;(sort (remove-duplicates l ) <)) ;; more efficient way

(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '(3 4 5 1 3 4)) => '(1 3 4 5))
(test (create-sorted-set '(1)) => '(1))



   
(: set-union : SET SET -> SET)
#|
 This function get a  2 sets =  2 list of numbers and return one sorted wothout duplicate number list
The function used the append to create one list and the create-sorted-set to sort and remove elements
|#

(define (set-union A B)
  (create-sorted-set (append A B)))

(test (set-union  '(1) '(3 4 5)) => '(1 3 4 5))
(test (set-union  '(3 4 3 7 1) '(2 6 3 2 4 5)) => '(1 2 3 4 5 6 7))
(test (set-union  '(5) '(5)) => '(5))



(: set-intersection : SET SET -> SET)
#|
 This function get a  2 sets =  2 list of numbers and only the numbers the exists in the two lists
It's used the ismember? to check if number own to 2 list, and filter the numbers has not
|#

(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

(test (set-intersection  '(1) '(3 4 5)) => '())
(test (set-intersection  '(4 3 7 1) '(4 6 3 2 4 5)) => '(3 4 ))
(test (set-intersection  '(4 3 4 1) '(4 10 1 3 2 4 5)) => '(1 3 4 ))
(test (set-intersection  '(5) '(5)) => '(5))


#|##########################################
                 Section 3

############################################
|#

;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        #|
         there is no With constructor replace with existing constructors
         so I did as we saw in th TA  to create 'with with call and fun
         the idea is:
             with -> id
                  -> named
                  -> body
              (=)
              call -> fun -> id
                          -> body
                   -> named

         and I used call-static it will be explained in Q3
|#
        (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] 
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name1) (symbol: name2)) body)
        (if (eq? name1 name2)
            (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr ) ;; cannot use the same param name twice
            (Fun name1 name2 (parse-sexpr body)))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr )])]
    
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    
    ;; note the real diffrence will be in the eval
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]


    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 
     
 

(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters 


#|##########################################
                 Section 4-5

############################################
|#

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env1)
             = eval(Ef,extend(x2,eval(E2,env) extend(x2,eval(E2,env) envf   )
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env1)
             = eval(Ef,extend(x2,eval(E2,env) extend(x2,eval(E2,env) env   )
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise


|#
;; Types for environments, values, and a lookup function

(define-type ENV
  ;; This is the definetion environment as a stack
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  ;; This  the Val type we had
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  )

(: lookup : Symbol ENV -> VAL)
; This function look for Id in the env
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env) 
     (if (eq? id name) val (lookup name rest-env))]))



;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: SetV->set : VAL -> SET)
;; function that unwarppered the SetV in return the Set itself
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else (error 'SetV->set "expects a set, got: ~s" v)]))
  
(: smult-set : Number VAL -> VAL)
;; this function map each element to his multipcation  with the scalat and create new SetV obj'
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
  (SetV (map mult-op (SetV->set s))))

(: set-op : (SET SET -> SET) VAL VAL -> VAL )
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2))))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV S)]

    #|
    I filled all the set opration with their match helper function we implemented above
   |# 
    [(Smult n set) (smult-set n  (eval set env))]
    [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
    [(Union l r) (set-op set-union (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)]
    [(CallS fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                #|
                The env is f-env - and this that what make it a static scope
                |# 
                (Extend bound-id2 (eval arg-expr2 env)(Extend bound-id1 (eval arg-expr1 env) f-env)))] ;; f-env
         [else (error 'eval "`call-static' expects a function, got: ~s"
                      fval)]))]
    [(CallD fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                #|
                The env is env - and this that what make it a dynamic scope
                |# 
                (Extend bound-id2 (eval arg-expr2 env)(Extend bound-id1 (eval arg-expr1 env) env)))] ;; env
         [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                      fval)]))]

      ))

 
  
;; simple operations tests
(test (eval (Smult 3 (Set '(1 2 3))) (EmptyEnv)) => (SetV '(3 6 9)))  
(test (eval (Union (Set '(1 2 3)) (Set '(2 3 4))) (EmptyEnv)) =>  (SetV '(1 2 3 4)))
(test (eval (Inter (Set '(1 2 3)) (Set '(2 3 4))) (EmptyEnv)) =>  (SetV '(2 3)))

;; with tests
(test (eval ( parse "{with { x {1 2 3} }  x }") (EmptyEnv)) => (SetV '(1 2 3)))
(test (eval ( parse "{with { x {1 2 3} }  {scalar-mult -3 x}}") (EmptyEnv)) => (SetV '(-3 -6 -9)))


  
(: createGlobalEnv : -> ENV)
#|
In the GlobalEnv we insert the function of cons first and second , and allow the user to use this function
My way to solce this is to create the match string then parse it and the ojbect i get back if what you can see here:
|#
(define (createGlobalEnv)
 (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
         (Extend 'first
                 (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))  
                 (Extend 'cons (FunV
                                'f
                                's
                                (CallS
                                 (Fun 'p 'p (Id 'p))
                                 (Fun 'sel '_ (CallD (Id 'sel) (Id 'f) (Id 's)))
                                 (Fun 'sel '_ (CallD (Id 'sel) (Id 'f) (Id 's))))
                                (EmptyEnv)) (EmptyEnv)))))


(: run : String -> (U SET VAL))
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))]) 
    (cases result
      [(SetV S) S]
      [else (error 'run "evaluation returned a non-set: ~s" result)])))    
 

(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
 

(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
 {with {foo {fun {x y} {intersect x y}}}
 {call-static p foo {}}}}")
 => '(2 3))


(test (run "{union {10 2 3} {4 3 2}}") => '(2 3 4 10 ))
(test (run "{intersect {10 2 3} {4 3 2}}") => '(2 3 ))


(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}") 
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")

#|

#|##########################################
                 Section 6

############################################
|#

note: I'm really sorry about the messy and the typos , I didn't have all the time, bacause I had to go to wedding

Q1:
--------------------

I used:
 a) EvalENVCode from the model
 b) lesson 11 - to get all the details about the pairs issue


Q2:
----------------------------------
I hope I understand your Intention

SOL contain:
1) the option to hold a list of numbers - sorted and with no duplicates
2) make some set operation as: union, Scalar multiplication, intersection
3) With - bind  the num list to exresion
4) Function : send/get back the numers list to functions in FIrst-class mode
    note: the option allow as to implement the cons/first/secone - because this is a function (and not types)


Q3:
------------------------------------------
This is implementation of With:

[(list 'with (list (symbol: name) named) body)
  (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] 

a) as we can se I decided to call the constructor of Fun with the same name and same named (value)
   to fit the 'with statment to Fun
b) as we can se I decided to use the CallS =  call-static because by defination from the lesson:
   Static Scope– In a language with static scope, each identifier gets its value from the scope in which it was defined

   and while we want to connect a name to experation , we want the connection will be to the value
   (=named) in that moment = defiention moment

Q4:
----------------------------------------------------
ismember? - A tail-recursive , each round it's check if the number is the rest is yes it's immediately return #t,
           and the caller no need to "know" the anser
remove-duplicates = A "regulaer" recirsive
      that line:
        [else (cons (first l) (remove-duplicates (rest l)))]))
      couse to tree-call and each round wait for the answer of previous call
  
Q5:
------------------------------------------------------
In the 'first and 'second I used the call-static because i want that every time it will oprate in the same way and alway retun
the fisrst/second element as I did in the fucntion defination , and most important prenvent the user from change this behavior

In ' cons I decided to use the call-dynamic for to let the user the option to change his pair through the program

Q6:
---------------------------------------------------

If we change the 'foo to dynmic we from on side we can crate another 'foo function which oprate in diffrent way and oferride the
behavior but also can create an error of missing binding for element so it's should be in carfull way

If we change the cons to dynamic it's lets as the option to work with another cons later.

|#






