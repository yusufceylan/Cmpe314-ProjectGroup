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

(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([val (s-exp->list s)])
       (case (s-exp->symbol (first val))
         [(+) (msl-add (parse (second val)) (parse (third val)))]
         [(*) (msl-mul (parse (second val)) (parse (third val)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

