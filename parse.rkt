#lang racket
(require "scan.rkt")
(provide parse)

(define (parse)
  (define (parse-error t) (error 'parse "Unexpected token ~a" t))
  (define (maybe-datum name) 
    (define d (parse-datum (scan)))
    (if (eof-object? d) (error 'parse "expected an element for ~a (found end-of-file)" name) d))
  (define (parse-datum t)
    (match t
      [(boolean-token v) v]
      [(number-token v) v]
      [(character-token v) v]
      [(string-token v) v]
      [(identifier-token v) v]
      [(lparen-token) (parse-list)]
      [(vector-token) (parse-vector empty)]
      [(quote-token) (list 'quote (maybe-datum "quoting"))]
      [(quasiquote-token) (list 'quasiquote (maybe-datum "quasiquoting"))]
      [(unquote-token) (list 'unquote (maybe-datum "unquoting"))]
      [(splice-token) (list 'unquote-splicing (maybe-datum "unquoting"))]
      [(eof-token) eof]
      [_ (parse-error t)]))
  (define (parse-list)
    (match (scan)
      [(rparen-token) empty]
      [(dot-token) (parse-datum (scan))]
      [t (let ([d (parse-datum t)]) (cons d (parse-list)))]))
  (define (parse-vector l)
    (match (scan)
      [(rparen-token) (list->vector (reverse l))]
      [t (let ([d (parse-datum t)]) (parse-vector (cons d l)))]))
  (parse-datum (scan)))
      