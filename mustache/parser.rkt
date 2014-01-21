#lang racket

;; Copyright (c) 2014 Adolfo Perez Alvarez <adolfo.pa@gmail.com>
;;
;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 2.1 of the License, or (at your option)
;; any later version.
;;
;; This library is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
;; details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 

(provide mustache-read
         mustache-read-syntax)

(struct tag (name pos) #:transparent)

(struct ref tag () #:transparent)
(struct seq tag (body) #:transparent)
(struct end tag () #:transparent)
(struct esc tag () #:transparent)
(struct neg tag (body) #:transparent)

(struct del (start end end-pos) #:transparent)
(struct txt (content end-pos) #:transparent)

(define regexp-append (compose regexp string-append))

(define default-tag-open "{{")
(define default-tag-close "}}")

(define (make-tag-regexp s e)
  (regexp-append "^" (regexp-quote s) " *([#/&^]?) *([a-zA-Z0-9?]+) *" (regexp-quote e) "\n?"))

(define current-tag-regexp
  (make-parameter (make-tag-regexp default-tag-open default-tag-close)))

(define (make-comment-regexp s e)
  (regexp-append "^" (regexp-quote s) " *!.*?" (regexp-quote e)))

(define current-comment-regexp
  (make-parameter (make-comment-regexp default-tag-open default-tag-close)))

(define (make-delims-regexp s e)
  (regexp-append "^" (regexp-quote s) " *= *([^ ]+) *([^ ]+?) *" (regexp-quote e)))

(define current-delims-regexp
  (make-parameter (make-delims-regexp default-tag-open default-tag-close)))

(define (make-text-regexp s e)
  (regexp-append "^[^" (string (string-ref s 0)) "]+"))

(define current-text-regexp
  (make-parameter (make-text-regexp default-tag-open default-tag-close)))

(define ((make-lexer regexp) in)
  (regexp-try-match (regexp) in))

(define read-simple-tag (make-lexer current-tag-regexp))
(define read-comment-tag (make-lexer current-comment-regexp))
(define read-delims-tag (make-lexer current-delims-regexp))
(define read-escape-tag (make-lexer (thunk #rx"^{{{ *([a-zA-Z0-9?]+) *}}}")))
(define read-raw-text (make-lexer current-text-regexp))

(define (make-pos src in)
  (define-values (line col pos) (port-next-location in))
  (list src line col pos))

(define (extend-pos pos span)
  (append pos (list span)))

(define (merge-pos a b)
  (match-define (list src line col pos a-span) a)
  (match-define (list _   _    _   _   b-span) b)
  (list src line col pos (+ a-span b-span)))

(define (read-tag in)
  (define pos (make-pos #f in))
  (define tag
    (match (read-simple-tag in)
      [(list s c id)
       ((case c
          [(#"" ) ref]
          [(#"#") (λ (name pos) (seq name pos #f))]
          [(#"/") end]
          [(#"&") esc]
          [(#"^") (λ (name pos) (neg name pos #f))])
        (bytes->string/utf-8 id) (extend-pos pos (bytes-length s)))]
      [#f
       (cond [(read-comment-tag in)
              #f]
             [(read-delims-tag in)
              => (match-lambda [(list s start end)
                                (del start end (extend-pos pos (bytes-length s)))])]
             [(read-escape-tag in)
              => (match-lambda [(list s id)
                                (esc (bytes->string/utf-8 id) (extend-pos pos (bytes-length s)))])]
             [else
              (define contents (bytes (char->integer (read-char in))))
              (txt contents
                   (extend-pos pos (bytes-length contents)))])]))
  (match tag
    [(del start end end-pos)
     (define s (bytes->string/utf-8 start))
     (define e (bytes->string/utf-8 end))
     (parameterize ([current-tag-regexp (make-tag-regexp s e)]
                    [current-delims-regexp (make-delims-regexp s e)]
                    [current-comment-regexp (make-comment-regexp s e)]
                    [current-text-regexp (make-text-regexp s e)])
       (read-text in))]
    [#f
     (read-text in)]
    [other
     (cons other (read-text in))]))

(define (read-text in)
  (cond [(read-raw-text in)
         => (λ (s)
              (define contents (first s))
              (cons (txt contents (extend-pos (make-pos #f in) (bytes-length contents)))
                    (read-text in)))]
        [(eof-object? (peek-char in))
         '()]
        [else
         (read-tag in)]))

(define (mustache-parse [in #f])
  (define-values (tags pending)
    (normalize (read-text (or in (current-input-port))) '()))
  tags)

(struct exn:fail:mustache:close-tag exn:fail (tag-name pos))

(define (normalize lst acc [name #f])
  (match lst
    [(list) 
     (if name
         (raise (exn:fail:mustache:close-tag
                 (~a "Expected close tag" tag-name "; reached EOF")
                 (current-continuation-marks)
                 name 'eof))
         (values (reverse acc) #f))]
    [(list (or (seq name pos _) (neg name pos _)) more ...)
     (define-values (body succ)
       (normalize more '() name))
     (define kons (if (seq? (first lst)) seq neg))
     (normalize (or succ '()) (cons (kons name pos body) acc) #f)]
    [(list (end n pos) more ...)
     (if (equal? n name) 
         (values (reverse acc) more)
         (raise (exn:fail:mustache:close-tag
                 (~a "Unexpected end tag" tag-name "at position" pos)
                 (current-continuation-marks)
                 name pos)))]
    [(list (txt a-content a-pos) (txt b-content b-pos) more ...)
     (normalize (cons (txt (bytes-append a-content b-content)
                           (merge-pos a-pos b-pos))
                      more)
                acc
                name)]
    [(cons tag more)
     (normalize more (cons tag acc) name)]))

(define (decorate expr pos)
  (datum->syntax #f expr pos))

(define (mustache-read src in)
  (syntax->datum (mustache-read-syntax src in)))

;; TODO: This needs a serious cleanup.
(define (mustache-read-syntax src in)
  (if (eof-object? (peek-char in))
      eof
      (with-syntax ([body 
                     (let loop ([tokens (mustache-parse in)])
                       (cond [(null? tokens)
                              #'()]
                             [else
                              (with-syntax ([more (loop (rest tokens))]
                                            [expr
                                             (match (first tokens)
                                               [(ref name pos)
                                                (decorate `(display-escaped ,name) pos)]
                                               [(seq name pos body)
                                                ;; TODO: Fix this. There must be a better way
                                                ;; of attaching source information to the syntax
                                                ;; object.
                                                (with-syntax ([body (loop body)]
                                                              [(op x) (decorate `(sequence ,name) pos)])
                                                  #'(op x . body))]
                                               [(esc name pos)
                                                (decorate `(display-raw ,name) pos)]
                                               [(neg name pos body)
                                                (with-syntax ([body (loop body)]
                                                              [(op x) (decorate `(inversion ,name) pos)])
                                                  #'(op x . body))] ; decorate this
                                               [(txt content pos)
                                                (decorate `(display-txt ,content) pos)])])
                                #'(expr . more))]))])
        #'(begin . body))))

(module+ test
  ;; TODO: Add tests for `mustache-read-syntax'
  
  (require rackunit)
  
  (define/match (tag-contents x)
    [((txt content _)) content]
    [((tag name _)) name])
  
  (define ^^ (curry map tag-contents))
  
  ;; An empty template must return en empty set of items.
  (check-equal? (^^ (mustache-parse (open-input-string ""))) '())
  ;; A template with no expressions must return an unique text item.
  (check-equal? (^^ (mustache-parse (open-input-string " abc ")))
                (list #" abc "))
  ;; Expression delimiters must match exactly.
  (check-equal? (^^ (mustache-parse (open-input-string "{ {foo}}")))
                (list #"{ {foo}}"))
  ;; An invalid expression should be interpreted as a text item.
  (check-equal? (^^ (mustache-parse (open-input-string "{{ not an expression }}")))
                (list #"{{ not an expression }}"))
  
  ;; Recognizes simple references.
  (check-equal? (^^ (mustache-parse (open-input-string "{{foo}}")))
                (list "foo"))
  ;; Blanks inside an expression will be ignored.
  (check-equal? (^^ (mustache-parse (open-input-string "{{ foo }}")))
                (list "foo"))
  ;; Blanks around expressions are not needed.
  (check-equal? (^^ (mustache-parse (open-input-string "123{{foo}}456")))
                (list #"123" "foo" #"456"))
  ;; Blanks between expressions are not needed.
  (check-equal? (^^ (mustache-parse (open-input-string "{{foo}}{{bar}}")))
                (list "foo" "bar"))
  ;; Recognizes sequence closing expressions.
  (check-equal? (^^ (mustache-parse (open-input-string "{{#foo}}{{/foo}}")))
                (list "foo"))
  ;; Recognizes escaping expressions.
  (check-equal? (^^ (mustache-parse (open-input-string "{{&foo}}")))
                (list "foo"))
  ;; Recognizes inversion expressions.
  (check-equal? (^^ (mustache-parse (open-input-string "{{^foo}}{{/foo}}")))
                (list "foo"))
  
  ;; Comments are ignored, and no blanks are inserted.
  (check-equal? (^^ (mustache-parse (open-input-string "123{{ ! ignore this ... }}456")))
                (list #"123456"))
  ;; Blanks are not needed to separate comments from other expressions.
  (check-equal? (^^ (mustache-parse (open-input-string "{{!ignore}}{{foo}}")))
                (list "foo"))
  
  ;; User defined delimiters are supported.
  (check-equal? (^^ (mustache-parse (open-input-string "{{=$$ $$}}$$foo$$")))
                (list "foo"))
  ;; Any character is allowed as a delimiter.
  (check-equal? (^^ (mustache-parse (open-input-string "{{=\\ \\}}\\foo\\abc\\=1 2\\1a2")))
                (list "foo" #"abc" "a"))
  ;; Basic uses work as expected.
  (check-equal? (^^ (mustache-parse (open-input-string "123{{ =  $$  $$ }}456$$foo$$")))
                (list #"123456" "foo"))
  ;; Even when start and end delimiters are equal, no separation between expressions needed.
  (check-equal? (^^ (mustache-parse (open-input-string "{{=$$ $$}}$$a$$$$={{ }}$${{b}}")))
                (list "a" "b"))
  
  ;; An unbalanced close expression fails
  (check-exn exn:fail:mustache:close-tag? (thunk (mustache-parse (open-input-string "{{/foo}}")))))
