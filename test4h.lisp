;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STUDENTS: use in the following way:
;;
;; (1) via manual commands:
;;
;;    os-prompt$ clisp -q
;;    [1]> (load "yourcode.lisp")
;;    T
;;    [2]> (load "tester4h.lisp")
;;    T
;;    [3]> (run-tests)



;;    [1]> (load "tester4h.lisp")
;;    T
;;    [2]> (main '("YOURFILE.lisp"))
;;    ....
;;
;; (2) via command line options (sbcl provides these, clisp does not...)
;;
;;    os-prompt$ sbcl --load "tester4h.lisp" --eval '(progn (main (list "YOURFILE.lisp")) (sb-ext:quit))'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *numwrong* 0)
(defvar *haserrored* nil)

;;(declaim (optimize (SPACE 3) (DEBUG 2))) ; to turn on tco


;(load "h4.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *tests* nil)

(defun repeat-character (char len)
  (let ((str (make-array 0 :element-type 'character :adjustable t
			 :fill-pointer t)))
    (loop for i from 0 below len do
	 (vector-push-extend char str))
    str))

(defun run-tests ()
  (setf *numwrong* 0)
  (mapcar
   (lambda (test-pair)
     (destructuring-bind (name fn) test-pair
       (let* ((top-str (format nil "====Running ~a====~%" name))
	      (bot-str (repeat-character #\= (length top-str))))
	 (format t "~a" top-str)
	 (funcall fn)
	 
	 (format t "~a~%" bot-str))))
   *tests*)
  (if (equalp *numwrong* 0) (format t "OK~%")
      (format t "FAILED (failures=~D)" *numwrong*)))

(defun main (args)
  (load (first args))
  (run-tests))

(defmacro with-gensyms-v2 ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; Macro used to create test functions
(defmacro define-test (function &body in-outs)
  ;; Gensym for the result from the function
  (let ((name (intern
	       (string-upcase
		(format nil "test-~a"
			(string function))))))
    (with-gensyms-v2 (res)
      ;; Define the function with the name test-<FUNCTION>
      `(progn (defun ,name
		  ;; Test function takes no arguments
		  ()
		;; Set up all the function calls and test harnesses
		,@(mapcar
		   (lambda (pair)
		     `(progn
			;; Call the function, store the result
			(let ((,res
			       (handler-case 
				   (apply #',function (list ,@(first pair)))
				 (condition (caught-err)
				   (progn
				     (incf *numwrong*)
				     (setf *haserrored* t)
				     (print caught-err))))))
			  ;; If it is the expected value...
			  (if (equal ,res ,(second pair))
			      ;; Print a PASSED message
			      (setf *haserrored* nil) ; don't print a passed message
			      ;; Otherwise, Show what failed
			      (if *haserrored*
				  (progn
				    (setf *haserrored* nil)
				    (format t "ERROR: Given \"~{~a~^ ~}\" called function coughed and died."
					    (list ,@(first pair)))
				    ())
				  (progn
				    (format t "FAIL: Expected \"~a\" got \"~a\"~%"
					    ,(second pair)
					    ,res)
				    ;; and reduce grade.
				    (incf *numwrong*)))))))
		   ;; in-outs is the list of list pairs that define the test
		   in-outs))
	      (push (list ',name #',name) *tests*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-or (xs)
  (if (null xs) NIL
      (if (first xs) T (my-or (rest xs)))))

(defun my-and (xs)
  (if (null xs) T
      (if (not (first xs)) NIL (my-and (rest xs)))))

(defun foldr (f z xs)
  (if (null xs) z
      (foldr f (funcall f z (first xs)) (rest xs))))
  
(defun elem (v xs)
  (my-or (mapcar (lambda (x) (equalp x v)) xs)))

(defun same-nums (xs ys)
  (and
   (equalp (length xs) (length ys))
   (my-and (mapcar (lambda (x) (elem x ys)) xs))))

(defun sub-set (xss yss)
  (my-and
   (mapcar (lambda (xs) (my-or (mapcar (lambda (ys) (same-nums xs ys)) yss))) xss)))

(defun same-items (xss yss)
  (and
   (sub-set xss yss)
   (sub-set yss xss)
   ))

;; for consistency's sake, the tester actually "tests" this function,
;; which calls the student's implementation of powerset.
(defun compare-powerset (input expected)
  (let ((got (powerset input)))
    (same-items got expected)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 13 tests
(define-test prime-factors
  ((1)    NIL)
  ((2)    '(2))
  ((3)    '(3))
  ((4)    '(2 2))
  ((5)    '(5))
  ((50)   '(2 5 5))
  ((66)   '(2 3 11))
  ((100)  '(2 2 5 5))
  ((463)  '(463))
  ((512)  '(2 2 2 2 2 2 2 2 2))
  ((117)  '(3 3 13))
  ((1117) '(1117))
  ((123456789) '(3 3 3607 3803))
)

;; 13 tests
(define-test coprime
  ((  2   3) T)
  ((  3   7) T)
  ((  3  10) T)
  (( 10  15) NIL)
  (( 15  10) NIL)
  (( 50  61) T)
  ((100 200) NIL)
  (( 97  98) T)
  (( 66 201) NIL)
  (( 49  28) NIL)
  ((367 463) T)
  ((330 463) T)
  ((441 1000) T)
)
  

;; 13 tests
(define-test  trib
  ((0) 1)
  ((2) 1)
  ((3) 3)
  ((4) 5)
  ((5) 9)
  ((6) 17)
  ((7) 31)
  ((10) 193)
  ((12) 653)
  ((13) 1201)
  ((14) 2209)
  ((20) 85525)
  ((30)37895489)
)

;; 12 tests
(define-test max-new
  ((NIL NIL) NIL)
  (('(5) NIL) 5)
  ((NIL '(5)) NIL)
  (('(1 2 3 4 5 6) '(5 6)) 4)
  (('(1 5 3 4 6 2 4 4) '(5 6)) 4)
  (('(-5 -4 -3) '(-4)) -3)
  (('(-3 3) '(3)) -3)
  (('(1 1 1 1 1) '(1)) NIL)
  (('(20 10) '(200)) 20)
  (('(1 2 3) '(4 5 6)) 3)
  (('(112 211 262 330 367 463) '(330 367)) 463)
  (('(112 211 262 330 367 463) '(330 367 463)) 262)
)

;; 13 tests. They are really seven tests that each count double
;; (except the last).
(let
    ((add (lambda (x y) (+ x y)))
     (mul (lambda (x y) (* x y))))
  ;; Usually you have to use #' (pronounced "sharp quote") to pass a
  ;; function as a value because it is in the function column of the
  ;; symbol table.  Today I didn't wanna bother running one more
  ;; regex, and put it in the value column instead.
  (define-test zip-with
    ((add  '(1 2 3 4) '(10 10 10 10)) '(11 12 13 14))
    ((add  '(1 2 3 4) '(10 10 10 10)) '(11 12 13 14))
    ((add  '(1 2 3 4) '(5 6 7 8)) '(6 8 10 12))
    ((add  '(1 2 3 4) '(5 6 7 8)) '(6 8 10 12))
    ((mul  '(2 3 4)  '(5 5 5 5 5))  '(10 15 20))
    ((mul  '(2 3 4)  '(5 5 5 5 5))  '(10 15 20))
    ((mul  '(2 3 4 5 6 7 8)  '(5 5 5))  '(10 15 20))
    ((mul  '(2 3 4 5 6 7 8)  '(5 5 5))  '(10 15 20))
    ((mul  '()  '(5 5 5 5 5))  '())
    ((mul  '()  '(5 5 5 5 5))  '())
    ((mul  '(1 2 3 4 5)  '())  '())
    ((mul  '(1 2 3 4 5)  '())  '())
    ((mul  '() '())  '())
  )
)


;; 12 tests.
(let
    (
     (even   (lambda (x) (= 0 (mod x 2))))
     (pos    (lambda (x) (> x 0)))
     (big    (lambda (x) (> x 10)))
     (lengthy(lambda(xs) (> (length xs) 3)))
     (diiiv  (lambda (x) (= 0 (mod x 3))))
     )
  (define-test pass-fail
    ((even '(1 2 3 4 5)) '((2 4) (1 3 5)))
    ((even '(9 9 9)) '(() (9 9 9)))
    ((even '(6 8 10)) '((6 8 10) ()))
    ((pos  '(1 -1 3 -3 2 -2)) '((1 3 2) (-1 -3 -2)))
    ((pos  '(-3 -4 -5)) '(() (-3 -4 -5)))
    ((big  '(8 9 10 11 12 13 1)) '((11 12 13) (8 9 10 1)))
    ((big  '(10 10 10)) '(() (10 10 10)))
    ((diiiv '()) '(()()))
    ((diiiv '(0 1 2 3 4 5 6 7 8)) '((0 3 6) (1 2 4 5 7 8)))
    ((diiiv '(-3 0 3000 300000000)) '((-3 0 3000 300000000) ()))
    ((lengthy '((1 2) (1 2 3) (1 2 3 4 5) (1 2 3 4 5 6 7 8 9 10))) '(((1 2 3 4 5) (1 2 3 4 5 6 7 8 9 10)) ((1 2) (1 2 3))))
    ((lengthy '("hello" "hi")) '(("hello") ("hi")))
))




;; 12 tests. The compare-powerset function, defined above, both calls
;; student's version of powerset and also accounts for different
;; orderings.
(define-test compare-powerset
  ((NIL '(())) T)
  (('(1) '(() (1))) T)
  (('(5) '(() (5))) T)
  
  (('(1 2) '(() (1) (2) (1 2))) T)
  (('(2 1) '(() (1) (2) (1 2))) T)
  
  (('(1 2 3) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))) T)
  (('(3 2 1) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))) T)

  (('(-4 -5) '(() (-4) (-5) (-4 -5))) T)
  (('(1 2 3 4) '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 3 4))) T)
  
  (('(0 2 4 6 8) '(() (0) (2) (4) (6) (8)
		    (0 2) (0 4) (0 6) (0 8) (2 4) (2 6) (2 8) (4 6) (4 8) (6 8)
		    (0 2 4) (0 2 6) (0 2 8) (0 4 6) (0 4 8) (0 6 8) (2 4 6) (2 4 8) (2 6 8) (4 6 8) (0 2 4 6)
		    (0 2 4 8) (0 2 6 8) (0 4 6 8) (2 4 6 8)
		    (0 2 4 6 8))) T)
   
  (('(4 3 2 1) '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 3 4))) T)
   
  (('(9 8 7 6 5) '(()
                   (5) (6) (7) (8) (9)
		   (5 6) (5 7) (5 8) (5 9) (6 7) (6 8) (6 9) (7 8) (7 9) (8 9)
		   (5 6 7) (5 6 8) (5 6 9) (5 7 8) (5 7 9) (5 8 9) (6 7 8) (6 7 9) (6 8 9) (7 8 9)
		   (5 6 7 8) (5 6 7 9) (5 6 8 9) (5 7 8 9) (6 7 8 9)
		   (5 6 7 8 9))) T)  
  )

;; 12 tests
(define-test matrix-product
  (( '((1 2 3))     '((4)(5)(6)))   '((32)))
  (( '((4)(5)(6))   '((1 2 3)))     '((4 8 12)(5 10 15)(6 12 18)))
  (( '((1 2)(3 4))  '((5 6)(7 8)))  '((19 22)(43 50)))
  (( '((1 0)(0 1))  '((5 6)(7 8)))  '((5 6)(7 8)))
  (( '((5 6)(7 8))  '((1 0)(0 1)))  '((5 6)(7 8)))
  (( '((1 2 3))     '((1)(1)(1)))   '((6)))
  (( '((1 2 3))     '((2)(2)(2)))   '((12)))
  (( '((1)(2)(3))   '((1 1 1)))     '((1 1 1)(2 2 2)(3 3 3)))
  (( '((1)(2)(3))   '((0 0 0)))     '((0 0 0)(0 0 0)(0 0 0)))
  (( '((1 2)(3 4))  '((1 0 0)(0 1 0)))  '((1 2 0)(3 4 0)))
  (( '((1 2)(3 4)(5 6)) '((1 0 0)(0 1 0)))   '((1 2 0)(3 4 0)(5 6 0)))
  (( '((11)) '((13)))   '((143)))
  )


;; (load "yourfile.lisp")
;; (run-tests)


