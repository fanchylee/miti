;
;interprete the CJK unicode integer to stroke orders
;
(module evlsk racket
	(provide hasnumber? evlsk)
(define *CJK_Unified_Ideographs* (read (open-input-file "./CJK_Unified_Ideographs.lisp")))

(define  getstroke
	(lambda (l)
	(car (cddr l))))
	

(define hasnumber? (lambda (x) 
	(cond 
		[(null? x) #f]
		[(number? (car x)) #t] 
		[else (hasnumber? (cdr x))])))

(define evlsk 
	(lambda (evl [db *CJK_Unified_Ideographs*])
	(cond
		[(null? db)
		(error "cannot find")]

		[(number? evl) 
		(let ([levl (car db)])
			(cond 
				([= (car levl) evl] [evlsk (getstroke levl)])
				(else (evlsk evl (cdr db)))
			))]
		
		[(list? evl)
		(cond 
			[(not (hasnumber? evl)) evl]
			[(number? (car evl))
			(append (evlsk (car evl) db) (cdr evl))]
			[else  (list* (car evl) (evlsk (cdr evl) db))]
		)]
		[else (error "illegal argument")]))))
