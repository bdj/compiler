#lang racket
(require "scan.rkt")
(require "parse.rkt")
(require "core-convert.rkt")

(define pass-count 0)
(define fail-count 0)
(define (passed!)
  (set! pass-count (add1 pass-count)))
(define (failed! s ts)
  (printf "expected: ~s, actual: ~s~n" s ts)
  (set! fail-count (add1 fail-count)))

(define (test s expected f)
  (define actual (with-handlers ([exn:fail? (λ (e) (exn-message e))])
                  (with-input-from-string s f)))
  (if (equal? expected actual)
      (passed!)
      (failed! expected actual)))

(define (until p? f)
  (define v (f))
  (if (p? v)
      empty
      (cons v (until p? f))))

(define (test-scan s expected)
  (test s expected (λ () (until eof-token? scan))))

(define (test-parse s)
  (test s (with-input-from-string s read) parse))

(test-scan 
 "(\"hello\" \"world\") ; here is a comment
(#f #t) ; more stuff" (list (lparen-token) (string-token "hello") (string-token "world") (rparen-token)
                            (lparen-token) (boolean-token #f) (boolean-token #t) (rparen-token)))

(test-scan "1234" (list (number-token 1234)))
(test-scan "-1234" (list (number-token -1234)))
(test-scan "h" (list (identifier-token 'h)))
(test-scan "hoohaw" (list (identifier-token 'hoohaw)))
(test-scan "#\\h" (list (character-token #\h))) 
(test-scan "#\\space" (list (character-token #\space)))
(test-scan "1234a" "scan: Unexpected char #\\a")

(test-parse "#f")
(test-parse "#t")
(test-parse "123")
(test-parse "#\\h")
(test-parse "\"hello\"")
(test-parse "x")
(test-parse "(x k c d)")
(test-parse "(x k c . d)")
(test-parse "#(x k c d)")
(test-parse "((1 2) (3 (4)) (5))")
(test-parse "'(a b c)")
(test-parse "'a")
(test-parse "`(foo ,(bar baz) x y z)")
(test-parse "`(foo ,@(bar baz))")

(printf "~a tests passed~n~a tests failed~n" pass-count fail-count)
