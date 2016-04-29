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
  [idC (s : symbol)] )

;; Function def with multiple parameters
;; Function definitions have a one name, one argument, one body
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol)) (body : ExprC)])

