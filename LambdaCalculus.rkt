;; Mehmetcan Güleşçi 112200032
;; Muhammet Yusuf Ceylan 111200030
;; Ali Cennet 110200005

#lang plai-typed

;Binding
;this function takes symbol as name and value which is number
;to bind any funciton
(define-type Binding
  [bind (name : symbol) (val : number)])

;; An alias to work easily on Environment.
(define-type-alias Env (listof Binding))

;; Empty environment.
(define mt-env empty)

