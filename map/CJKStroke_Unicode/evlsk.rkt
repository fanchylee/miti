#! /usr/bin/env racket 
#lang racket

(require "./stroke.scm")
(define rpl (lambda () 
	(let ([str (format "~a" (unisk (read)))])
		(printf "~a\n" (substring str 1 (- (string-length str) 1)))
		(rpl))))
(rpl)
