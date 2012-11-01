;
;(description "use ikarus as the interpreter (in fact, it's a compiler)")
;

;
;;(description "libraries")
;
(import (rnrs))
;;

;
;;(description "transcoders (r6rs)")
;
(define transcoder (make-transcoder (utf-8-codec)))
;;

;
;;(description "errors")
;
(define *ERR_NO_RECORD* "Cannot find the character in the specified list")
;;

;
;;(description "local viariables")
;
(define *Default_Map_Path* "./CJK_Unified_Ideographs.lisp")
(define *CJK_Unified_Ideographs* (call-with-input-file *Default_Map_Path* (lambda (in) (read in))))
;;

;
;;(description "Functions")
;
;
;;;(description "load a CJK-character-to-stroke map ")
;;;(argument "the path of the map")
;
(define loadmap (lambda (dbpath)
	(call-with-input-file dbpath (lambda (in) (read in)))))
;;;

;
;;;(description "internal function, get  stroke in a map")
;;;(argument "l" "a record in a map")
;
(define getstroke
	(lambda (l)
	(car (cddr l))))
;;;
(define newstrokerecord
	(lambda (sk record)
	(list (car record) (cadr record) sk)))
	

(define hasnumber? (lambda (x) 
	(cond 
		[(null? x) #f]
		[(number? (car x)) #t] 
		[else (hasnumber? (cdr x))])))

(define haschar? (lambda (x)
	(cond
		[(null? x) #f]

		[(char? (car x)) #t]

		[else hasnumber? (cdr x)])))
;
;;;(description "find the stroke of a certain character") 
;;;(argument "uni" "a character or the unicode integer of the character")
;;;(optional (argument "db" "the map to be searched"))
;
(define selectsk 
	(lambda (uni . db )
	(cond
		[(null? db)
		(selectsk uni *CJK_Unified_Ideographs*)]
		
		[else 
		(let ([db (car db)])
		(cond
			[(null? db)
			(display *ERR_NO_RECORD*)]
	
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
				[(not (or (hasnumber? uni) (haschar? uni))) uni]
				
				[(char? (car uni))
				(append (selectsk (char->integer (car uni)) db) (selectsk (cdr uni) db))]

				[(number? (car uni))
				(append (selectsk (car uni) db) (selectsk (cdr uni) db))]

				[else  (append (list (car uni)) (selectsk (cdr uni) db))]
			)]
			[else (display "illegal argument")]))])))
;;;

(define (select_stroke_in_default_map uni) (selectsk uni))


(define (integer_unicode_to_stroke lt) (cond
	[(null? lt)
	lt]

	[(list? lt)
	(let ([sk (car lt)]) (cond
		([char? sk] 
		[append (list (char->integer sk)) (integer_unicode_to_stroke (cdr lt))])

		(else [append (list sk) (integer_unicode_to_stroke (cdr lt))])))]))
		
(define get_map_with_updated_stroke
	(lambda (code sk . db )
	(cond 
		[(null? db)
		(get_map_with_updated_stroke code sk *CJK_Unified_Ideographs*)]

		[else
		(let ([db (car db)] )
		(cond 
			[(null? db)
			(display *ERR_NO_RECORD*)]
			
			[(null? code)
			db]
        
			[(char? code)
			(get_map_with_updated_stroke (char->integer code) sk db)]

			[(number? code)
			(let ([lcode (car db)])
				(cond 
					([= (car lcode) code] [append [list (newstrokerecord sk lcode)]   (cdr db)])
					(else (append [list lcode] [get_map_with_updated_stroke code sk  (cdr db)]))
				))]

			[else (display "illegal auguments")]))])))

(define (printloop outport db)
	(cond 
		[(null? (cdr db)) (begin 
			[put-string outport "\t"]
			[write (car db) outport]
			[put-string outport "\n"])]
		[else (begin ;seems file IO still needs to be implemented in an imperative way, though I tried functional way.
			[put-string outport "\t"]
			[write (car db) outport]
			[put-string outport "\n"]
			[printloop outport (cdr db)])]))

(define save_map
	(lambda (db . dbpath) 
	(cond
		[(null? dbpath) (save_map db *Default_Map_Path*)]

		[else
		(let ([dbpath (car dbpath)])
			(if
			[string? dbpath]
			[let ([outport (open-file-output-port dbpath (file-options no-fail) 'block transcoder)])
				(begin
				[put-string  outport "(\n"]
				[printloop outport db]
				[put-char outport #\)]
				[close-output-port outport])]
			[display "not a string, specify a string for the path"]))])))

(define initmap (lambda () (get_map_with_updated_stroke '() '())))

(define update_default_map (lambda (db) (set! *CJK_Unified_Ideographs* db)))

(define update_default_map_with_stroke (lambda (code sk) (update_default_map (get_map_with_updated_stroke code sk))))

(define (save_default_map_to_default_path) (save_map *CJK_Unified_Ideographs*))

(define save save_default_map_to_default_path)

(define-syntax s (syntax-rules () ([_ CJKch] [selectsk (string-ref (symbol->string (quote CJKch)) 0)])))

(define-syntax u (syntax-rules () ([_ CJKch sk] [update_default_map_with_stroke (string-ref (symbol->string (quote CJKch)) 0) sk])))
;;
