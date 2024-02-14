#lang typed/racket
(require typed/rackunit)


(define-type ExprC (U NumC IdC WordC CondC AnonC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([id : Symbol]) #:transparent)
(struct WordC ([str : String]) #:transparent)
(struct CondC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
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



(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? num) (NumC num)]
    [(? symbol? (? is-allowed? sym)) (IdC sym)]
    [(? string? str) (WordC str)]
    #;[(list 'if i 'then t 'else e) (CondC (parse i) (parse t) (parse e))]
    [(list 'anon (list (? symbol? (? is-allowed? args)) ...) ': body) (AnonC (cast args (Listof Symbol)) (parse body))]
    #;multiple-exprc
    [other (error 'parse "OAZO5 syntax error in ~e" other)]))

(check-equal? (parse 0) (NumC 0))
(check-equal? (parse 'p) (IdC 'p))
(check-equal? (parse "OAZO5") (WordC "OAZO5"))
(check-equal? (parse '{anon {x} : 5}) (AnonC (list 'x) (NumC 5)))
(check-exn #rx"syntax" (lambda () (parse '{not valid})))
(check-exn #rx"keyword" (lambda () (parse 'if)))
(check-exn #rx"keyword" (lambda () (parse 'let)))
(check-exn #rx"keyword" (lambda () (parse 'anon)))
(check-exn #rx"keyword" (lambda () (parse ':)))
(check-exn #rx"keyword" (lambda () (parse '<-)))





