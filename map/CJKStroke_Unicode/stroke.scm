;
;interprete the CJK unicode integer to stroke orders
;
[module stroke racket/base
	(provide unisk updatestroke savemap printloop)
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

(define unisk 
	(lambda (uni . db )
	(cond
		[(null? db)
		(unisk uni *CJK_Unified_Ideographs*)]
		
		[else 
		(let ([db (car db)])
		(cond
			[(null? db)
			(error "Cannot find record in the specified list")]
	
			[(number? uni) 
			(let ([luni (car db)])
				(cond 
					([= (car luni) uni] [unisk (getstroke luni)])
					(else (unisk uni (cdr db)))
				))]

			[(char? uni)
			(unisk (char->integer uni) db)]
			
			[(list? uni)
			(cond 
				[(not (hasnumber? uni)) uni]
				[(number? (car uni))
				(append (unisk (car uni) db) (cdr uni))]
				[else  (list* (car uni) (unisk (cdr uni) db))]
			)]
			[else (error "illegal argument")]))])))


(define updatestroke
	(lambda (code sk . db )
	(cond 
		[(null? db)
		(updatestroke code sk *CJK_Unified_Ideographs*)]

		[else
		(let ([db (car db)])
		(cond 
			[(null? db)
			(error "cannot find")]
        
			[(char? code)
			(updatestroke (char->integer code) sk db)]

			[(number? code)
			(let ([lcode (car db)])
				(cond 
					([= (car lcode) code] [append [list (newstrokerecord sk lcode)]   (cdr db)])
					(else (append [list lcode] [updatestroke code sk  (cdr db)]))
				))]

			[else (error "illegal auguments")]))])))

(define (printloop out db)
	(cond 
		[(null? (cdr db)) (fprintf out "\t~a\n" (car db))]
		[else 
		[begin ;seems file IO still needs to be implemented in an imperative way, though I tried functional way.
			(fprintf out "\t~a\n" (car db)) 
			(printloop out (cdr db))]]))

(define savemap
	(lambda (db) (call-with-output-file "./CJK_Unified_Ideographs.lisp" #:exists 'replace
		(lambda (out) 
		(begin
			[fprintf out "(\n" ]
			[printloop out db]
			[fprintf out "i)" ]
		)))))

]
