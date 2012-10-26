;
;interprete the CJK unicode integer to stroke orders
;
[module stroke racket/base
	(provide codesk updatestroke savemap)
(define *CJK_Unified_Ideographs* (call-with-input-file "./CJK_Unified_Ideographs.lisp" (lambda (in) (read in))))

(define  getstroke
	(lambda (l)
	(car (cddr l))))

(define newstrokerecord
	(lambda (sk record)
	(list (car record) (cadr record) sk)))
	

(define hasnumber? (lambda (x) 
	(cond 
		[(null? x) #f]
		[(number? (car x)) #t] 
		[else (hasnumber? (cdr x))])))

(define codesk 
	(lambda (code [db *CJK_Unified_Ideographs*])
	(cond
		[(null? db)
		(error "cannot find")]

		[(number? code) 
		(let ([lcode (car db)])
			(cond 
				([= (car lcode) code] [codesk (getstroke lcode)])
				(else (codesk code (cdr db)))
			))]
		
		[(list? code)
		(cond 
			[(not (hasnumber? code)) code]
			[(number? (car code))
			(append (codesk (car code) db) (cdr code))]
			[else  (list* (car code) (codesk (cdr code) db))]
		)]
		[else (error "illegal argument")])))

(define updatestroke
	(lambda (code sk [db *CJK_Unified_Ideographs*])
	(cond 
		[(null? db)
		(error "cannot find")]

		[(number? code) 
		(let ([lcode (car db)])
			(cond 
				([= (car lcode) code] [append [list (newstrokerecord sk lcode)]   (cdr db)])
				(else (append [list lcode] [updatestroke code sk  (cdr db)]))
			))]

		[else (error "illegal auguments")])))

(define savemap
	(lambda (db) (call-with-output-file "./CJK_Unified_Ideographs.lisp" #:exists 'replace
		(lambda (out) 
		(write db out)))))
]
