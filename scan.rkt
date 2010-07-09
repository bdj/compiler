#lang racket
(require (for-syntax unstable/syntax))

(provide scan)

(struct token () #:transparent)
(provide (struct-out token))

(define-for-syntax (define-tokens stx fields)
  (define (define-token t)
    (define id (format-id t "~a-token" t))
    #`(begin
        (struct #,id token #,fields #:transparent)
        (provide (struct-out #,id))))
  (syntax-case stx ()
    [(_ t)
     (define-token #'t)]
    [(define-tokens t0 t1 ...)
     #`(begin
         #,(define-token #'t0)
         (define-tokens t1 ...))]))

(define-syntax (define-empty-tokens stx)
  (define-tokens stx #'()))

(define-syntax (define-value-tokens stx)
  (define-tokens stx #'(v)))

(define-empty-tokens lparen rparen quote quasiquote dot vector eof unquote splice)

(define-value-tokens boolean string number identifier character)

(define (scan)
  (define (whitespace-char? c) (memv c '(#\space #\newline)))
  (define (sign-char? c) (memv c '(#\+ #\-)))
  (define (digit-char? c) (char<=? #\0 c #\9))
  (define (initial-char? c) (or (letter-char? c) (special-initial-char? c)))
  (define (letter-char? c) (char<=? #\a c #\z))
  (define (special-initial-char? c) (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\? #\~ #\_ #\^)))
  (define (delimiter-char? c) (or (eof-object? c) (whitespace-char? c) (memv c '(#\( #\) #\" #\;))))
  (define (subsequent-char? c) (or (initial-char? c) (digit-char? c) (memv c '(#\. #\+ #\-))))
  (define (char->sign c) (if (eqv? #\+ c) + -))
  (define (char->digit c) (- (char->integer c) (char->integer #\0)))
  (define (scan-error c) (error 'scan "Unexpected char ~s" c))
  (define (start)
    (define c (read-char))
    (cond
      [(eof-object? c) (eof-token)]
      [(eqv? #\( c) (lparen-token)]
      [(eqv? #\) c) (rparen-token)]
      [(eqv? #\# c) (scan-hash)]
      [(eqv? #\' c) (quote-token)]
      [(eqv? #\` c) (quasiquote-token)]
      [(eqv? #\, c) (scan-unquote)]
      [(eqv? #\" c) (scan-string)]
      [(eqv? #\; c) (scan-comment)]
      [(eqv? #\. c) (scan-dot)]
      [(whitespace-char? c) (start)]
      [(sign-char? c) (scan-sign c)]
      [(digit-char? c) (scan-number + (char->digit c))]
      [(initial-char? c) (scan-identifier c)]
      [else (scan-error c)]))
  (define (scan-hash)
    (define c (read-char))
    (cond
      [(eqv? #\t c) (boolean-token #t)]
      [(eqv? #\f c) (boolean-token #f)]
      [(eqv? #\( c) (vector-token)]
      [(eqv? #\\ c) (scan-character)]
      [else (scan-error c)]))
  (define (scan-unquote)
    (define c (peek-char))
    (cond
      [(eqv? #\@ c) (read-char) (splice-token)]
      [else (unquote-token)]))
  (define (scan-string)
    (string-token (list->string (scan-string-elements))))
  (define (scan-string-elements)
    (define c (read-char))
    (cond 
      [(eqv? c #\\) (scan-escape)]
      [(eqv? c #\") empty]
      [else (cons c (scan-string-elements))]))
  (define (scan-escape)
    (define c (read-char))
    (cond
      [(or (eqv? c #\\) (eqv? c #\")) c]
      [else (scan-error c)]))
  (define (scan-comment)
    (define c (read-char))
    (cond
      [(eof-object? c) (eof-token)]
      [(eqv? #\newline c) (start)]
      [else (scan-comment)]))
  (define (scan-dot)
    (define c (peek-char))
    (cond
      [(delimiter-char? c) (dot-token)]
      [(eqv? #\. c) (read-char) (scan-ellipsis)]
      [else (scan-error c)]))
  (define (scan-ellipsis)
    (define c (read-char))
    (cond
      [(eqv? #\. c) (identifier-token '...)]
      [else (scan-error c)]))
  (define (scan-sign c)
    (define cp (peek-char))
    (cond
      [(delimiter-char? cp) (identifier-token (string->symbol (string c)))]
      [else (scan-number (char->sign c) 0)]))
  (define (scan-number sign acc)
    (define c (peek-char))
    (cond
      [(delimiter-char? c) (number-token (sign acc))]
      [(digit-char? c) (read-char) (scan-number sign (+ (* 10 acc) (char->digit c)))]
      [else (scan-error c)]))
  (define (scan-identifier c)
    (define cp (peek-char))
    (cond
      [(delimiter-char? cp) (identifier-token (string->symbol (string c)))]
      [(subsequent-char? cp) (scan-subsequent (list (read-char) c))]
      [else (scan-error cp)]))
  (define (scan-subsequent acc)
    (define cp (peek-char))
    (cond
      [(delimiter-char? cp) (identifier-token (string->symbol (list->string (reverse acc))))]
      [(subsequent-char? cp) (scan-subsequent (cons (read-char) acc))]
      [else (scan-error cp)]))
  (define (scan-character)
    (define c (read-char))
    (define cp (peek-char))
    (cond
      [(delimiter-char? cp) (character-token c)]
      [(eqv? #\n c) (scan-newline)]
      [(eqv? #\s c) (scan-space)]
      [else (scan-error c)]))
  (define (scan-newline)
    (define s (read-string 6))
    (if (equal? "ewline" s)
        (character-token #\newline)
        (scan-error s)))
  (define (scan-space)
    (define s (read-string 4))
    (if (equal? "pace" s)
        (character-token #\space)
        (scan-error s)))
  (start))

