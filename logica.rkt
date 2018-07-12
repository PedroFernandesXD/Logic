#lang racket

(require 2htdp/image)

;(define V (rectangle 10 10 'solid 'black)) 
;(define F (rectangle 10 10 'outline 'black))
(define V 'V)
(define F 'F)



(define pp (lambda (f) (remove-duplicates (regexp-match* #rx"[pqrs][0-9]*" f))))

(define (sp l)
  (let loop((n (length l)) (r '()))
    (if (zero? n)
        r
        (loop (sub1 n) (cons '(V F) r)))))

(define c1 (lambda (l r)
      (if (null? l) 
             l 
             (append (list (list r (car l))) (c1 (cdr l) r)))))

(define c2 (lambda (l1 l2)
                (if(null? l2) 
                   '() 
                   (append (c1 l1 (car l2)) (c2 l1 (cdr l2))))))
(define fls
  (lambda (f)
    (cond [(null? f) f]
          [(list? (car f)) (append (fls (car f)) (fls (cdr f)))]
          [else (cons (car f) (fls (cdr f)))]))) 

(define fl
  (lambda (l)
    (if(null? l)
       l
       (cons (fls (car l)) (fl (cdr l))))))

(define c
  (lambda (lista aux)
    (if (null? lista)
        aux
        (c (cdr lista) (c2 (car lista) aux)))))

(define (cn l) (fl (c (cdr l ) (car l))))

(define (~ f)
  (if (equal? f V)
      F
      V))

(define (v f g)
  (if (and (equal? f F) (equal? g F))
      F
      V))

(define (^ f g)
  (if (and (equal? f V) (equal? g V))
      V
      F))

(define (-> f g)
  (if (and (equal? f V) (equal? g F))
      F
      V))

(define (<-> f g)
  (if (equal? f g)
      V
      F))

(define (@ f g)
  (if (not (equal? f g))
      V
      F))

(define (eval-string string)
  (eval (read (open-input-string string))))

(define (line-xs xs lp li)
  (let loop((s xs) (p lp) (i li))
    (if (and (null? p) (null? i))
             s
      (loop (regexp-replace* (car p) s (format "~a" (car i)))
            (cdr p)
            (cdr i)))))

;; transforma a "p->q" para algo assim '(p . -> . q)


(define (resultado f)
  (let loop((i (cn (sp (pp f)))) (r '()) (p (pp f)))
    (if (null? i)
        r
        (loop (cdr i)
              (cons (eval-string (line-xs f p (car i))) r)
              p))))
(define preposições (lambda (f) (pp f)))
(define interpetações (lambda (f) (cn (sp (preposições f)))))

(define table (lambda (f)
                (let loop((x (interpetações f)) (y (resultado f)) (r '()))
                  (if (and (empty? x) (empty? y))
                       r
                      (loop (cdr x) (cdr y) (cons (cons (car x) (car y)) r)) )) ))

(define (normaliz f)
  (for/list ((i f)) (~a i))) 
  
  ;; Exemplo de USO:
  
 ;; (resultado "(-> p (~ p)")
  
  
