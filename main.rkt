#lang typed/racket
(require typed/rackunit)


(define-type ExprC (U NumC IdC WordC BoolV ErrC CondC AppC AnonC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([id : Symbol]) #:transparent)
(struct WordC ([str : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct ErrC ([v : Any]) #:transparent)
(struct CondC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AppC    ([fun : Symbol] [e1 : ExprC] [e2 : ExprC]) #:transparent)
(struct AnonC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)


;IS-ALLOWED?
;takes in a symbol
;returns false if it is a keyword, otherwise true
(define (is-allowed? [s : Symbol]) : Boolean
  (match s
    ['if (error 'parse "if is a keyword")]
    ['let (error 'parse "let is a keyword")]
    ['anon (error 'parse "anon is a keyword")]
    [': (error 'parse ": is a keyword")]
    ['<- (error 'parse "<- is a keyword")]
    [else #t]))

(define (is-primop? [s : Symbol]) : Boolean
  (match s
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['<= #t]
    ['equal? #t]
    [else #f]))

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? num) (NumC num)] ;number
    ['true (BoolV #t)] ;true value
    ['false (BoolV #f)] ;false value
    [(? symbol? (? is-allowed? sym)) (IdC sym)] ;id
    [(? string? str) (WordC str)] ;string
    [(list 'error v) (ErrC v)] ;error
    [(list 'if i 'then t 'else e) (CondC (parse i) (parse t) (parse e))] ;if statement
    [(list 'anon (list (? symbol? (? is-allowed? args)) ...) ': body) (AnonC (cast args (Listof Symbol)) (parse body))] ;function definition
    #;[(list 'let (list (? symbol? (? is-allowed? var)) '<- (? real? val)) ... ': body) ???] ;let (sugar anon)
    #;[(list exprs +) (for/list ([item (in-list exprs)]) 
                        (parse item))] ;highest level function
    [(list (? symbol? (? is-primop? name)) l r) (AppC name (parse l) (parse r))] ;embedded function
    [other (error 'parse "OAZO5 syntax error in ~e" other)])) ;syntax error

(check-equal? (parse 0) (NumC 0))
(check-equal? (parse 'true) (BoolV #t))
(check-equal? (parse 'false) (BoolV #f))
(check-equal? (parse 'p) (IdC 'p))
(check-equal? (parse "OAZO5") (WordC "OAZO5"))
(check-equal? (parse '{error "invalid"}) (ErrC "invalid"))
(check-equal? (parse '{if {<= x 5} then 5 else 6}) (CondC (AppC '<= (IdC 'x) (NumC 5)) (NumC 5) (NumC 6)))
(check-equal? (parse '{anon {x} : 5}) (AnonC (list 'x) (NumC 5)))
(check-equal? (parse '{/ {* {- {+ x 10} 10} 10} 10})
              (AppC '/ (AppC '* (AppC '- (AppC '+ (IdC 'x) (NumC 10)) (NumC 10)) (NumC 10)) (NumC 10)))
(check-equal? (parse '{equal? 1 2}) (AppC 'equal? (NumC 1) (NumC 2)))
(check-exn #rx"syntax" (lambda () (parse '{not valid})))
(check-exn #rx"keyword" (lambda () (parse '{anon {if} : 5})))
(check-exn #rx"keyword" (lambda () (parse '{anon {let} : 5})))
(check-exn #rx"keyword" (lambda () (parse '{anon {anon} : 5})))
(check-exn #rx"keyword" (lambda () (parse '{anon {:} : 5})))
(check-exn #rx"keyword" (lambda () (parse '{anon {<-} : 5})))



