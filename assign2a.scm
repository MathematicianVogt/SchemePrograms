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


;#2
;get rules method written to get a set of rules for a specific variable. variable name list-> list
(define (getRules rule L finalRuleList) (if(null? L) finalRuleList (if(equal? (car (car L)) rule) (getRules rule (cdr L) (append (list (car L)) finalRuleList)) (getRules rule (cdr L) finalRuleList))))
(define (first3 grammer alpha seen) (first3Helper grammer alpha seen '()))
(define (first3Helper grammer alpha seen finalList) (if(null? alpha) (union finalList '()) (if(terminal? (car alpha)) (union (list (car alpha)) finalList) (if(containsEpsilon? (getRules (car alpha) grammer '())) (first3Helper grammer (cdr alpha) seen finalList) (firstvar3Helper grammer (getRules (car alpha) grammer '()) seen '())))))
;(define (first-var3 grammer rules seen) (firstvar3Helper grammer rules seen '()))
(define (firstvar3Helper grammer rules seen finalList) (if(null? rules) finalList (if(seen? (car rules) seen) (firstvar3Helper grammer (cdr rules) seen finalList) (firstvar3Helper grammer (cdr rules) (cons (car rules) seen) (append (rule-rhs (car rules)) finalList )))))
(define (seen? specificRule ruleList) (if(null? ruleList) #f (if(equal? specificRule (car ruleList)) #t (seen? specificRule (cdr ruleList)))))
(define (containsEpsilon? L) (if(null? L) #f (if(equal? (car L) '()) #t (containsEpsilon? (cdr L)))))