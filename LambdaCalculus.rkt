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


;; λ-expression grammar
;; λ-calc -> v
;; λ-calc -> (λ-calc λ-calc)
;; λ-calc -> (λ v λ-calc)
;; where v is a symbol.


;; λ-exp is an abstract syntax grammar or a parse tree definition for
;; - λ-exp that defined above.
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

;; parse : s-exp -> λ-exp
;; Purpose : To transform given s-expression to corresponding
(define (parsel (sexp : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (cond
         [(= 2 (length sexp-list))
          (λ-app (parsel (first sexp-list))(parsel (second sexp-list)))]
         [(= 3 (length sexp-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
                   (s-exp-symbol? (second sexp-list)))
              (λ-def (s-exp->symbol(second sexp-list))
                     (parsel (third sexp-list)))
              (error parsel "Not valid λ-definition")
              )]
         [else (error parsel "Not valid length λ-exp")]
         ))]
    [else (error parsel "Not valid λ-exp")]
))

;; Tests:
(test (parsel (symbol->s-exp 'y))(λ-sym 'y))
(test (parsel '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parsel '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
(test (parsel '((λ x x)(λ y y)))
      (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
(test (parsel '(λ x (λ y (y x))))
      (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))

;; unparse : λ-exp -> s-exp
;; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparse (le : λ-exp)) : s-expression
  (type-case λ-exp le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparse l)(unparse r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparse p))))
    ))

;; Test:
(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))'((λ x x) y))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))'((λ x x)(λ y y)))    
(test (unparse (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))'(λ x (λ y (y x))))

;; substituter : λ-exp symbol λ-exp -> λ-exp
;; Purpose : we will use this in beta-transformer
(define (substituter [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp 
  (type-case λ-exp in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (substituter what for l)
                        (substituter what for r)))
    (λ-def (v p)(λ-def v (substituter what for p)))))

; beta-transformer ((λ x M) N) --> [M:x=N]
(define (beta-transformer (le : λ-exp)) : λ-exp
  (type-case λ-exp le
    (λ-sym (v) le) 
    (λ-app (l r) (if (λ-def? l)
                     (substituter r (λ-def-v l) (λ-def-p l))
                     (λ-app (beta-transformer l) (beta-transformer r))))
    (λ-def (v p) (λ-def v (beta-transformer p)))))

(define zero '(λ f (λ x x)))
(define one '(λ f (λ x (f x))))
(define two '(λ f (λ x (f (f x)))))
(define succ '(λ n (λ f (λ x (f ((n f) x))))))
(define (church->number n) ((n add1) 0))
(define add '(λ n (λ m (λ f (λ x ((n f) ((m f) x)))))))
(beta-transformer(parsel add))

;; A set represented as a list.
;; union:  (listof symbol) (listof symbol) -> (listof symbol)
;; Purpose: Finding the union of two sets.
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; set-difference: (listof symbol) (listof symbol) -> (listof symbol)
;; To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; free-identifier: λ-exp -> (listof symbol)
;; find free identifiers in given λ expression.
(define (free-identifier (le : λ-exp)) : (listof symbol)
  (type-case λ-exp le
    (λ-sym (v) (list v))
    (λ-app (l r)(union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))

;; New identifier from GitHub
(define (quotient [a : number] [b : number]) : number
  (/ (- a (remainder a b)) b))

(define (number->string [x : number]) : string
                       (if (zero? x) "" (string-append (number->string (quotient x 10))
                                                       (list->string (list (string-ref "0123456789" (modulo x 10)))))))

(define (chop-tail [l : (listof char)]) : (listof char)
  (if (or (empty? l) (char=? (first l) #\- )) empty (cons (first l) (chop-tail (rest l )))))

(define (clear-suffix-number [old : symbol]) : symbol
  (string->symbol (list->string (chop-tail (string->list (symbol->string old))))))

(define (add-new-suffix-number [old-chopped : symbol] [n : number]) : symbol
  (string->symbol (string-append (string-append (symbol->string old-chopped) "-") (number->string n))))

(define (new-ident-fun-maker [start : number]) : [symbol -> symbol]
  (lambda ([old : symbol]) : symbol
    (begin
      (set! start (+ start 1))
      (add-new-suffix-number (clear-suffix-number old) start))))

(define make-new-ident (new-ident-fun-maker 0))


;; use examples (from GitHub)
(make-new-ident 'a)
(make-new-ident 'a-123)
(make-new-ident 'hıdır)
(make-new-ident 'hıdır-7)

;; TESTS:
;; (λ-sym 'x)
;; (λ-app (λ-sym 'x)(λ-sym 'y))
;; (λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

(test (union empty empty) empty)
(test (union empty (list 'x)) (list 'x))
(test (union (list 'x)(list 'x 'y)) (list 'x 'y))

(test (parsel (symbol->s-exp 'y))(λ-sym 'y))
(test (parsel '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parsel '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))

(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y))) '((λ x x) y))

(test (free-identifier (parsel '(λ x x))) empty)
(test (free-identifier (parsel '(λ x y))) (list 'y))
(test (free-identifier (parsel '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parsel '((λ f y)(λ z z)))) (list 'y))

(test (beta-transformer (parsel '((λ x x) a))) (parsel (symbol->s-exp 'a)))
(test (beta-transformer (parsel '((λ x y) a))) (parsel (symbol->s-exp 'y)))
(test (beta-transformer (parsel '((λ x (a b)) k))) (parsel '(a b)))
(test (beta-transformer (parsel '((λ x (λ x y)) k))) (parsel '(λ x y)))
(test (beta-transformer (parsel '((λ x (λ y x)) k))) (parsel '(λ y k)))
(test (beta-transformer (parsel '((λ x (λ y (x y))) k))) (parsel '(λ y (k y))))
