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

;;lookup function takes n as a symbol and environment which includes binding values,
;; then it checks wheter this funciton in environment or not?
;;if there is,it produces value otherwise it gives error
(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "Symbol not found in env")]
    [(cons? env) (cond
                   [(equal? n (bind-name (first env))) (bind-val (first env))]
                   [else (lookup n (rest env))])]))




