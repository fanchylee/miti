#! /usr/bin/ikarus --script

(import (rnrs))
(load "./stroke.ss")

(define printstrokes (lambda (outport strIn)
	[write (map selectsk (string->list strIn)) outport]
	[put-char outport #\newline]
	[flush-output-port outport]))

(define rpl (lambda () 
	(let ([sym (read)])
	(cond
	([eof-object?  sym] #f);#f means exit in a script
	(else (let ([strIn (symbol->string sym)]) 
		[printstrokes (current-output-port) strIn]
		[rpl]))))))

(let ([command-line-arg (cdr (command-line))])
	(cond 
	[(null? command-line-arg) (rpl)]
	[else (printstrokes (current-output-port) (car command-line-arg))]))
