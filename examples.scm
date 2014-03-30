(define (getEvens L)
  (cond ((null? L) '()) 
        ((even? (car L)) (cons (car L) (getEvens (cdr L)))) 
        ((odd? (car L) ) (getEvens (cdr L))))) 
(define (union set1 set2) 
  (cond 
    ((null? set1) set2)
    ((not (member (car set1) set2)) (cons (car set1) (union (cdr set1) set2)))
    (else (union (cdr set1) set2))))

    