;#1 union: set1 set2 -> set3
;A function that unions two sets, this set will not have multiple instances of one element
(define (union set1 set2) (unionHelp set1 (append '() set2)))
;A helperfunction for union function to actually compute the union of two sets
(define (unionHelp set1 finalSet) (if(null? set1) finalSet (if(member (car set1) finalSet) (unionHelp (cdr set1) finalSet) (unionHelp (cdr set1) (append (list(car set1)) finalSet)))))
(union '(1 2 3 4) '(2 4 5 7 6 3))
;Given code
; Terminals are quoted.
; A rule A -> X1 ... Xn is written (A (X1 ... Xn))
; A grammar is a list of rules.

(define *grammar*
  '((S (E 'eof))
    (E (T E2))
    (E2 ('+ T E2))
    (E2 ('- T E2))
    (E2 ())
    (T (F T2))
    (T2 ('* F T2))
    (T2 ('/ F T2))
    (T2 ())
    (F ('n))
    (F ('id))
    (F ('- F))
    (F ('OP E 'CP))))

; rule-lhs : Rule -> Variable
(define rule-lhs car)

; (eq? (rule-lhs '(E (T E2))) 'E)

; rule-rhs : Rule -> List(Variables or Terminals)
(define rule-rhs cadr)

; (equal? (rule-rhs '(E (T E2))) '(T E2))

; variable? : Any -> Boolean
(define variable? symbol?)

; (variable? 'E)
; (not (variable? ''+))

; terminal? : Any -> Boolean
(define (terminal? a) (and (pair? a) (eq? (car a) 'quote)))

; (terminal? ''+)
; (not (terminal? 'E))

;filter by method written to get a set of rules for a specific variable. predicate list-> list
(define (filter-by p L) (filtering p L '()))
(define (filtering p L finalList) (if(null? L ) finalList (if(p (car L) ) (filtering p (cdr L) (append finalList (list (car L)))) (filtering p (cdr L) finalList))))
;#2
(define (first3 grammer alpha seen) (first3Helper grammer alpha seen '()))
(define (first3Helper grammer alpha seen finalList) (if(null? alpha) '(()) (if(terminal? alpha) (append (list (car alpha)) finalList)
