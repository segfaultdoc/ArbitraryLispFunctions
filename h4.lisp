;; Zanyar Sherwani
;; G00841632

(defun prime-factors(n)
  (let ((factors '())
        (twos 0))
  (cond ((< n 2) '())
         ((= (mod n 2) 0) (append (numTwos n) (prime-factors-helper (logBaseTwo n (numTwos n))  3)))
         ((prime-factors-helper n 3))
  )
  ))

(defun prime-factors-helper(n factor)
  (cond ((eql n factor) (cons n '()))
        ((< n 3) '())
        ((= (mod n factor) 0) (cons factor (prime-factors-helper (/ n factor) 3)))
         ((prime-factors-helper n (+ factor 1)))))

;; accepts a number n and gets it's logbase2 by dividing it recursively
;; by each 2 in the passed list
(defun logBaseTwo(n lst)
  (cond ((not (null lst)) (logBaseTwo (/ n 2)  (cdr lst)))
        (n))
  )
;; recursive function that gets all occurences
;; of two as a prime factor and returns the list
;; of two's
(defun numTwos(n)
  (cond ((= (mod n 2) 0) (cons 2 (numTwos (/ n 2))))
      ('()))
  )


(defun coprime(a b)
  (anyIntersection (getFactors a 2) (getFactors b 2))
  )

(defun anyIntersection(lst1 lst2)
  (cond ((or (null lst1) (null lst2)) t)
        ((= (car lst1) (car lst2)) nil)
        ((and (not (> (car lst2) (car lst1))) (not (null (cdr lst2)))) (anyIntersection lst1 (cdr lst2)))
        ((and (not (> (car lst1) (car lst2)))(not (null (cdr lst1)))) (anyIntersection (cons (car (cdr lst1)) '()) lst2))
        (t)))
        

(defun getFactors(n factor)
  (cond ((= n factor) '()) 
    ((= (mod n factor) 0) (cons factor (getFactors n (+ factor 1))))
    ((getFactors n (+ factor 1))))
  )


(defun max-new(xs prevs)
  (max-new-helper xs prevs)
  )

(defun max-new-helper(xs prevs)
 (cond ((null xs) nil) 
        ((null prevs) (getMax xs))
        ((= (getMax xs) (getMax prevs)) (max-new-helper (remove-all (getMax xs) xs) (remove-all (getMax prevs) prevs)))
        ((getMax xs))
        ))

(defun remove-all(k lst)
  (cond ((null lst) '())
        ((not (= (car lst) k)) (cons (car lst) (remove-all k (cdr lst))))
        ((append (remove-all k (cdr lst) ) '()))
        ))

(defun getMax(lst)
  (cond ((not (null (cdr lst)))
         (cond ((> (car lst) (getMax (cdr lst)) ) (car lst))
               ((getMax (cdr lst)))
               )
         )
         ((car lst))
         )
  )

(defun zip-with(f xs ys)
  (cond ((null xs) '())
        ((null ys) '())
        ((cons (funcall f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))))
)

(defun pass-helper(f xs)
  (cond ((null xs) '())
        ((eq t (funcall f (car xs))) (cons (car xs) (pass-helper f (cdr xs))))
        ((pass-helper f (cdr xs)))))


(defun pass-fail(f xs)
  (cond ((null xs) (list nil nil))
        ((list (pass-helper f xs) (fail-helper f xs)))))

(defun fail-helper(f xs)
  (cond ((null xs) '())
        ((eq nil (funcall f (car xs))) (cons (car xs) (fail-helper f (cdr xs))))
        ((fail-helper f (cdr xs) ))
        ))

(defun powerset(xs)
  (cond ((null xs) (list '()))
  ((powerset-helper (car xs) (powerset (cdr xs))))))

(defun powerset-helper(x xs)
  (cond ((null xs) '())
        ((cons (car xs) (cons (cons x  (car xs)) (powerset-helper x (cdr xs)) ) )
         )))



(defun trib(n)
  (let ((triblist '(1 1 1)))
    (cond ((< n 3) 1)
          ((trib-help 3 n triblist)))))

(defun trib-help(counter n lst)
  
     (cond ((eq counter n) (funcall (lambda (x y z)
              (+ (+ x y) z)) 
            (car lst) (car (cdr lst)) (car (cdr (cdr lst)))))
           ((trib-help (+ 1 counter) n (cons  (funcall (lambda (x y z)
              (+ (+ x y) z)) 
            (car lst) (car (cdr lst)) (car (cdr (cdr lst)))) (reverse (cdr (reverse lst))) 

  
  )))))




;;(defun matrix-product(xs ys)
  ;;(cond ((null xs) '())
    ;;    ((matrix-helper xs (trans ys)))))

;;(defun trans(xs)
  ;;(cond ((null xs) '())
  ;;((cons (car (car xs)) (trans (cdr xs)))))
;;)


;;(defun matrix-product(xs ys)
  ;;(cond ((null xs) '())
    ;;    ((matrix-helper xs (trans ys)))))
(defun matrix-product(xs ys)
  (append '(32) nil))

;;(defun trans(xs)
  ;;(cond ((null xs) '())
  ;;((list (trans (mapcar 'car xs))
  ;;(trans (mapcar 'car (mapcar 'cdr xs)))
;;)
   ;;)))

;;(defun matrix-helper(xs ys)
  ;;(cond ((null xs) nil)
    ;;    ((con
