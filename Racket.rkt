;; CMPE-314 Project Group 
;; Racket.rkt file

;; Mehmetcan Güleşçi 112200032
;; Muhammet Yusuf Ceylan 111200030
;; Ali Cennet 110200005

#lang racket
(require plai-typed)

;; Function definition
 ; F is a function
 ; Ls is a list of parameters
 ; B is body
 ; F -> (Name)Ls{B}
 ; Name -> symbol
 ; B-> exp
 ; Ls-> listOfSymbols

;; Function Application
 ;Fa is function application
 ;Fs is a function symbol
 ;La is a list of arguments
 ;Fa -> FsLa
 ;La  -> listOfSymbols
 ;Fs -> symbol

;; Type defined for ExprC
;; Defining arithmetic expressions
 ; exp -> number
 ; exp -> symbol
 ; exp -> + exp exp
 ; exp -> - exp exp
 ; exp -> * exp exp
 ; exp -> (exp)


(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  [factC (n : number)]
  [idC (s : symbol)] 
  [factaccC (n : number) (acc : number)]
  [ifgz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [appC (fun : symbol) (arg : (listof ExprC))]
 )

;; Function def with multiple parameters
;; Function definitions have a one name, one argument, one body
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol)) (body : ExprC)])

;; parse s-expression -> ExprC
;; convert a quoted s expression into the equivalent ExprC form
;; examples
;;  '(+ 12 (+ 12 6)))-> (plusC (numC 12)(plusC (numC 12) (numC 6))))
;; (symbol->s-exp 'x))->  (idC 'x))
;; '(if 1 2 3)->(ifC (numC 1) (numC 2) (numC 3)))

(define (parse [s :  (listof s-expression)]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'ifgz (s-exp->symbol (first sl)))
              (ifgz (parse (second sl))
                       (parse (third sl))
                       (parse (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parse (second sl)) (parse (third sl)))]
            [(*) (multC (parse (second sl)) (parse (third sl)))]
            [(**) (expC (parse (second sl)) (parse (third sl)))]
            [(-) (subC (parse (second sl)) (parse (third sl)))]
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))

;; Tests;
(test (parse (number->s-exp 2)) (numC 2))
(test (parse (symbol->s-exp 'a)) (idC 'a))
(test (parse '(+ 5 2)) (plusC (numC 5) (numC 2)))
(test (parse '(+ 3 4)) (plusC (numC 3) (numC 4)))
(test (parse '(+ 2 7)) (plusC (numC 2) (numC 7)))
(test (parse '(+ 1 8)) (plusC (numC 1) (numC 8)))
(test (parse '(+ 3 6)) (plusC (numC 3) (numC 6)))
(test (parse '(+ a b)) (plusC (idC 'a) (idC 'b)))
(test (parse '(+ (- 1 2) (* 3 4))) (plusC (subC (numC 1) (numC 2)) (multC (numC 3) (numC 4))))
(test (parse '(+ a b)) (plusC (idC 'a) (idC 'b)))
(test (parse '(- 1 3)) (subC (numC 1) (numC 3)))
(test (parse '(- 3 4)) (subC (numC 3) (numC 4)))
(test (parse '(- 1 4)) (subC (numC 1) (numC 4)))
(test (parse '(- 5 1)) (subC (numC 5) (numC 1)))
(test (parse '(- 8 4)) (subC (numC 8) (numC 4)))
(test (parse '(* 7 8)) (multC (numC 7) (numC 8)))
(test (parse '(* 2 4)) (multC (numC 2) (numC 4)))
(test (parse '(* 1 4)) (multC (numC 1) (numC 4)))
(test (parse '(* 3 4))(multC (numC 3)(numC 4)))
(test (parse '(* 3 2)) (multC (numC 3) (numC 2)))
(test (parse '(* 3 7)) (multC (numC 3) (numC 7)))
(test (parse '(** 7 3)) (expC (numC 7) (numC 3)))
(test (parse '(+ 8 (- 8 5))) (plusC (numC 8) (subC (numC 8) (numC 5))))
(test (parse '(* 15 (+ 7 5))) (multC (numC 15) (plusC (numC 7) (numC 5))))
(test (parse '(+ 23 (* 3 5))) (plusC (numC 23) (multC (numC 3) (numC 5))))
(test (parse (symbol->s-exp 'x)) (idC 'x))
(test (parse '(double 13))(appC 'double (numC 13)))
(test (parse (number->s-exp 5))(numC 5))
(test (parse (number->s-exp 1))(numC 1))
(test (parse (number->s-exp 8))(numC 8))
(test (parse (symbol->s-exp 5))(numC 5))
(test (parse (symbol->s-exp 'x))(idC 'x))
(test (parse '(+ x x))(plusC (idC 'x)(idC 'x)))
(test (parse '(- x x))(subC (idC 'x)(idC 'x)))
(test (parse '(- 8 4))(subC (numC 8)(numC 4)))
(test (parse '(* x x))(multC (idC 'x)(idC 'x)))
(test (parse '(f (* x x)))(appC 'f (multC (idC 'x)(idC 'x))))
(test (parse '(f (+ x x)))(appC 'f (plusC (idC 'x)(idC 'x))))
(test (parse '(f (- x x)))(appC 'f (subC (idC 'x)(idC 'x))))
(test (parse '(ifgz 4 5 6))(ifgz (numC 4)(numC 5)(numC 6)))
(test (parse '(ifgz 7 2 3))(ifgz (numC 7)(numC 2)(numC 3)))
(test (parse '(ifgz 2 1 3))(ifgz (numC 2)(numC 1)(numC 3)))
(test (parse '(ifgz 8 2 4))(ifgz (numC 8)(numC 2)(numC 4)))
(test (parse '(f (** x y))) (appC 'f (expC (idC 'x) (idC 'y))))
(test (parse '(ifgz 1 2 3)) (ifgz (numC 1) (numC 2) (numC 3)))

;;get-fundef
;;symbol (list of func definitions)-> : FunDefC
;;Purpose
;; it takes a symobol and generate a function definition.
;; a recursive helper function to find the representation of a function 
;; definition from the list, given its name
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))

;; example list of function definitions
(define myFun(list  
  (fdC 'double 'x (plusC (idC 'x) (idC 'x))) 
  (fdC 'inc5 'x (multC (idC 'x) (idC 'x)))) )
; (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))) ; (define (quadruple x) (double (double x)))
; (fdC 'const5 '_ (numC 5))  ; (define (const5 _) 5)
;; Short Tests for fundef exampleList; 
  (test (get-fundef 'double myFun) (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
  (test (get-fundef 'inc5 myFun) (fdC 'inc5 'x (multC (idC 'x) (idC 'x)))) 
  (fdC 'double1  '(x , y) (plusC (idC  'x) (idC  'y)))
  
;; Subst
;; ExprC symbol ExprC -> ExprC
;; Purpose
;; it takes a expression ( numC 7) , argument ('x) and the function it self. It produces the function with changes(numC 7) placed for every 'x in function
;; Examples
;; (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) -> (plusC (plusC (numC 7) (numC 7)) (numC 7))
(define (subst [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
     (type-case ExprC in
     [numC (n) in]
     [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
     [appC (f a) (appC f (subst what for a))]
     [plusC (l r) (plusC (subst what for l)
                         (subst what for r))]
     [subC (l r) (plusC (subst what for l)
                         (subst what for r))]
     [multC (l r) (multC (subst what for l)
                         (subst what for r))]
     [expC (l r) (multC (subst what for l)
                          (subst what for r))]
     [factC (x) (factC (subst what for x))]
     [ifgz (exp1 exp2 exp3) (ifgz (subst what for exp1) (subst what for exp2) (subst what for exp3))]
     [factaccC (x fact) (factaccC (subst what for x) (subst what for fact))]))

;; Interp
;; ExprC -> fds -> number
;; it takes an expression and list of function definitions and output 
;; a number
;; Example:
;; (interp (numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x))))  -->  2
;; Function Application
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
               (interp (subst a
                              (fdC-arg fd)
                              (fdC-body fd))
                       fds))]
   [ifgz (exp1 exp2 exp3) (cond
                           [(> (interp exp1 fds) 0) (interp exp2 fds)]
                        [else (interp exp3 fds)])]
   [plusC (l r) (+ (interp l fds) (interp r fds))]
   [subC (l r) (- (interp l fds) (interp r fds))]
   [multC (l r) (* (interp l fds) (interp r fds))]
   [expC (l r) (expt (interp l fds) (interp r fds))]
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]
   [factaccC (x acc) (cond
                       [(= x 1) acc]
                       [else (interp (factaccC (- x 1) (* x acc)) fds)])]))

;; TESTS FOR INTERP(Lazy)
(test (interp (numC 5) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp (numC 15) (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp (numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (numC 7) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp (numC 11) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 11)
(test (interp (numC 6) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp (numC 25) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 25)
(test (interp (numC 33) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 33)

;; Tests for plus operation
(test (interp (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp (plusC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp (plusC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp (plusC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
(test (interp (plusC (numC 444) (numC 59)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 503)
(test (interp (plusC (numC 47) (numC 25)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 72)
(test (interp (plusC (numC 357) (numC 35)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 392)

;; Tests for igz (if greater than zero)
(test (interp (ifgz(numC 5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (ifgz(numC -5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp (ifgz(numC 55) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (ifgz(numC 555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp (ifgz(numC -5555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp (ifgz(numC 10) (numC 5) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 5)
(test (interp (ifgz(numC 7) (numC 5) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 5)
(test (interp (ifgz(numC 60) (numC 2) (numC 9)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 2)
(test (interp (ifgz(numC 444) (numC 3) (numC 1)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 3)

;; Tests for subtraction operation
(test (interp (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (subC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp (subC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp (subC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
(test (interp (subC (numC 27) (numC 115)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -88)
(test (interp (subC (numC 445) (numC 19)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 426)
(test (interp (subC (numC 81) (numC 25)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 56)

;; Tests for multiplication
(test (interp (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp (multC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp (multC (numC 10) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp (multC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
(test (interp (multC (numC 68) (numC 26)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1768)
(test (interp (multC (numC 19) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 95)
(test (interp (multC (numC 27) (numC 115)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3105)

;; Tests for exponention operation
(test (interp (expC (numC 2) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp (expC (numC 11) (numC 2)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp (expC (numC 7) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp (expC (numC 10) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp (expC (numC 4) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)
(test (interp (expC (numC 15) (numC 6)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 11390625)
(test (interp (expC (numC 5) (numC 9)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1953125)
(test (interp (expC (numC 22) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 234256)





