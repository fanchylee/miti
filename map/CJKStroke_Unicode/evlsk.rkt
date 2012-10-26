#! /usr/bin/env racket 
#lang racket

(require "./evlsk.scm")
(define rpl (lambda () 
	(let ([str (format "~a" (evlsk (read)))])
		(printf "~a\n" (substring str 1 (- (string-length str) 1)))
		(rpl))))
(rpl)
