;
;interprete the CJK unicode integer to stroke orders
;
;
;;deleted this part to run in other interpreters
;
[module stroke racket/base
	(provide selectsk updatesk savemap printloop ussk intsk initmap)
;;

;
;;Errors 
;
(define *ERR_NO_RECORD* "Cannot find the character in the specified list")
;;
;
;;local viariables

(define *CJK_Unified_Ideographs* (call-with-input-file "./CJK_Unified_Ideographs.lisp" (lambda (in) (read in))))
;(define *CJK_Unified_Ideographs* (loadmap "./CJK_Unified_Ideographs.lisp"))
;;
;
;;Functions
;
(define loadmap (lambda (dbpath)
	(call-with-input-file dbpath (lambda (in) (read in)))))
(define getstroke
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

(define selectsk 
	(lambda (uni . db )
	(cond
		[(null? db)
		(selectsk uni *CJK_Unified_Ideographs*)]
		
		[else 
		(let ([db (car db)])
		(cond
			[(null? db)
			(error *ERR_NO_RECORD*)]
	
			[(number? uni) 
			(let ([luni (car db)])
				(cond 
					([= (car luni) uni] [selectsk (getstroke luni)])
					(else (selectsk uni (cdr db)))
				))]

			[(char? uni)
			(selectsk (char->integer uni) db)]
			
			[(list? uni)
			(cond 
				[(not (hasnumber? uni)) uni]
				[(number? (car uni))
				(append (selectsk (car uni) db) (selectsk (cdr uni) db))]
				[else  (append (list (car uni)) (selectsk (cdr uni) db))]
			)]
			[else (error "illegal argument")]))])))

(define (intsk lt) (cond
	[(null? lt)
	lt]

	[(list? lt)
	(let ([sk (car lt)]) (cond
		([char? sk] 
		[append (list (char->integer sk)) (intsk (cdr lt))])

		(else [append (list sk) (intsk (cdr lt))])))]))
		
(define updatesk
	(lambda (code sk . db )
	(cond 
		[(null? db)
		(updatesk code sk *CJK_Unified_Ideographs*)]

		[else
		(let ([db (car db)] [sk (intsk sk)])
		(cond 
			[(null? db)
			(error *ERR_NO_RECORD*)]
			
			[(null? code)
			db]
        
			[(char? code)
			(updatesk (char->integer code) sk db)]

			[(number? code)
			(let ([lcode (car db)])
				(cond 
					([= (car lcode) code] [append [list (newstrokerecord sk lcode)]   (cdr db)])
					(else (append [list lcode] [updatesk code sk  (cdr db)]))
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
	(lambda (db . dbpath) 
	(cond
		[(null? dbpath) (savemap db "./CJK_Unified_Ideographs.lisp")]

		[else
		(let ([dbpath (car dbpath)])
			(if
			(string? dbpath)
			(call-with-output-file dbpath #:exists 'replace (lambda (out) (begin
				[fprintf out "(\n" ]
				[printloop out db]
				[fprintf out ")" ])))
			(error "not a string, specify a string for the path")))])))

(define ussk (lambda (uni sk . dbspec)
	(cond
		[(null? dbspec)
		(ussk uni sk "./CJK_Unified_Ideographs.lisp" *CJK_Unified_Ideographs*)]

		[else
		(let 
		([dbspec1 (car dbspec)] 
		[dbspec2 (cond ([null? (cdr dbspec)] #f) (else [cadr dbspec])) ])
			(let 
			([dbpath (cond ([string? dbspec1] dbspec1) ([string? dbspec2] dbspec2) (else "./CJK_Unified_Ideographs.lisp"))]
			[db (cond ([list? dbspec1] dbspec1) ([list? dbspec2] dbspec2) (else *CJK_Unified_Ideographs*))])
			(savemap (updatesk uni sk db) dbpath)))])))

(define initmap (lambda () (updatesk '() '())))
;;

]
;
