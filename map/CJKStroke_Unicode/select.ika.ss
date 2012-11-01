#! /usr/bin/ikarus --script

(import (rnrs))
(load "./stroke.ss")

(define rpl (lambda () 
	(let ([sym (read)])
	(cond
	([eof-object?  sym] #f);#f means exit in a script
	(else (let ([strIn (symbol->string sym)]) 
		[write (map selectsk (string->list strIn))]
		[put-char (current-output-port) #\newline]
		[flush-output-port (current-output-port)]
		[rpl]))))))
(rpl)
