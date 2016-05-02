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
(test (parse '(- 1 3)) (subC (numC 1) (numC 3)))
(test (parse '(* 7 8)) (multC (numC 7) (numC 8)))
(test (parse '(** 7 3)) (expC (numC 7) (numC 3)))
(test (parse '(+ (- 1 2) (* 3 4))) (plusC (subC (numC 1) (numC 2)) (multC (numC 3) (numC 4))))
(test (parse '(+ a b)) (plusC (idC 'a) (idC 'b)))
(test (parse '(f (** x y))) (appC 'f (expC (idC 'x) (idC 'y))))
(test (parse '(ifgz 1 2 3)) (ifgz (numC 1) (numC 2) (numC 3)))

