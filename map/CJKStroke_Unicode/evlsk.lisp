
(load "./CJK_Unified_Ideographs.lisp")
(define hasnumber? (lambda (x) 
	(cond 
		[(null? x) #f]
		[(number? (car x)) #t] 
		[else (hasnumber? (cdr x))])))

(define evl->sk 
	(lambda (evl [db *CJK_Unified_Ideographs*])
	(cond
		[(null? db)
		(error "cannot find")]

		[(number? evl) 
		(let ([levl (car db)])
		(cond 
			([= (car levl) evl] [evl->sk (last levl)])
			(else (evl->sk evl (cdr db)))
		))]
		
		[(list? evl)
		(cond 
			[(not (hasnumber? evl)) evl]
			[(number? (car evl))
			(append (evl->sk (car evl) db) (cdr evl))]
			
			[else  (list* (car evl) (evl->sk (cdr evl) db))]
		)]
		[else (error "illegal argument")])))
(define evl->test
	(lambda (evl [db *CJK_Unified_Ideographs*])
	(cond
		[(null? db)
		(error "cannot find")]

		[(number? evl) 
		evl]
	)))
