;; Mehmetcan Güleşçi 112200032
;; Muhammet Yusuf Ceylan 111200030
;; Ali Cennet 110200005

#lang racket
(require plai-typed)

;Binding
;this function takes symbol as name and value which is number
;to bind any funciton
(define-type Binding
  [bind (name : symbol) (val : number)])

;; An alias to work easily on Environment.
(define-type-alias Env (listof Binding))

;; Empty environment.
(define mt-env empty)

;; Extending environment
(define extend-env cons)

(define-type Value
  [numvalue (n : number)]
  [functionvalue (params : (listof symbol)) (body : ExtendedMPF) (env : Env)])




