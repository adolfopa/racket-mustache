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

(provide display-escaped
         display-raw
         display-txt
         sequence
         inversion
         #%datum
         #%app
         (rename-out [module-begin #%module-begin]))

(module+ test
  (require rackunit))

(define-syntax-rule (module-begin stmt ...)
  (#%plain-module-begin
   (provide render)
   (define (render env out)
     (parameterize ([current-output-port out])
       (with-env env stmt ...)))))

;; escape: (U String Bytes Any) = T-> T
;; If the input value is a string or byte string, escape its contents
;; so that it is safe to include the content as part of an XML document.
;; If the input value is not a string or byte string, return it unchanged.
(define escape
  (let ([amp (reverse '(#\& #\a #\m #\p #\;))]
        [lt (reverse '(#\& #\l #\t #\;))]
        [gt (reverse '(#\& #\g #\t #\;))])
    (λ (bs)
      (cond [(or (string? bs) (bytes? bs))
             (define-values  (->list ->string)
               (if (string? bs)
                   (values string->list list->string)
                   (values (compose (curry map integer->char) bytes->list)
                           (compose list->bytes (curry map char->integer)))))
             (let loop ([xs (->list bs)] [acc '()])
               (if (null? xs)
                   (->string (reverse acc))
                   (loop (rest xs)
                         (let ([c (first xs)])
                           (append (case c
                                     [(#\&) amp]
                                     [(#\<) lt]
                                     [(#\>) gt]
                                     [else
                                      (list c)])
                                   acc)))))]
            [else
             bs]))))

(module+ test
  (check-equal? (escape "") "")
  (check-equal? (escape "abc") "abc")
  (check-equal? (escape "<>&") "&lt;&gt;&amp;")
  
  (check-equal? (escape #"") #"")
  (check-equal? (escape #"abc") #"abc")
  (check-equal? (escape #"<>&") #"&lt;&gt;&amp;")
  
  (check-equal? (escape 'foo) 'foo))

;; display-escaped: Any -> Void
;; Display the value bound to `name', escaping any XML entity.
(define (display-escaped name)
  (display (escape (env-ref name))))

;; display-raw: Any -> Void
;; Display the value bound to `name' in the current environment.
(define (display-raw name)
  (display (env-ref name)))

;; display-txt: Bytes -> Void
;; Display a block of text.
(define (display-txt text)
  (display text))

;; current-env: (ParameterOf (ListOf (HashTable Any Any)))
;; The current environment.
(define current-env (make-parameter '()))

;; env-ref: Any -> Any
;; Return the value bound to `name' in the current environment. If no value
;; is bound, return the empty string.
(define (env-ref name)
  (define value
    (for/first ([x (current-env)]
                #:when (hash-has-key? x name))
      (hash-ref x name)))
  (or value ""))

(module+ test
  (check-equal? (env-ref 'x) "")
  (check-equal? (with-env (hash 'x 'y) (env-ref 'x)) 'y))

;; Extend the current environment with the given object.
;; If the object is not a hash, no environment extension will be done.
(define-syntax-rule (with-env obj stmt ...)
  (if (hash? obj)
      (parameterize ([current-env (cons obj (current-env))])
        stmt ...)
      (begin stmt ...)))

(module+ test
  (check-equal? (with-env 'a (env-ref 'a)) "")
  (check-equal? (with-env (hash 'a 'b) (env-ref 'a)) 'b))

;; Evaluate the body if `name' is bound in the current environment.
;; If the value bound to `name' is a list, the body will be evaluated
;; once per element, using the element to extend the environment; if
;; the bound value is a hash, the body will be evaluated once with
;; the environment extended by the hash; if it is any other non-false
;; value, the body will be evaluated once with no environment extension.
;;
;; Mustache stx: {{#foo}}...{{/foo}}
(define-syntax-rule (sequence name stmt ...)
  (let ([val (env-ref name)])
    (when val
     (cond [(list? val)
            (for ([elt val])
              (with-env elt stmt ...))]
           [(mustache-false? val)
            (void)]
           [else
            (with-env val stmt ...)]))))

(module+ test
  (check-equal? (with-env (hash) (sequence 'a 'b)) (void))
  (check-equal? (with-env (hash 'a '()) (sequence 'a 'b)) (void))
  (check-equal? (with-env (hash 'a "") (sequence 'a 'b)) (void))
  (check-equal? (with-env (hash 'a #"") (sequence 'a 'b)) (void))
  
  (check-equal? (let ([out (open-output-string)])
                  (parameterize ([current-output-port out])
                    (with-env (hash 'a '(1 2 3))
                      (sequence 'a (display "x"))))
                  (get-output-string out))
                "xxx")
  (check-equal? (with-env (hash 'a 'b) (sequence 'a 'b)) 'b))

;; mustache-false?: Any -> Boolean
;; Test if `datum' is a falsy value.  A value is considered falsy
;; if it is #f, "", #"" or '().
(define (mustache-false? datum)
  (or (not datum)
      (null? datum)
      (and (string? datum) (zero? (string-length datum)))
      (and (bytes? datum) (zero? (bytes-length datum)))))

(module+ test
  (check-true (mustache-false? #f))
  (check-true (mustache-false? '()))
  (check-true (mustache-false? ""))
  (check-true (mustache-false? #"")))

;; Evaluate the given statements when `name' is not bound or is a "falsy" value.
;; Mustache stx: {{^foo}}...{{/foo}}
(define-syntax-rule (inversion name stmt ...)
  (let ([val (env-ref name)])
    (when (mustache-false? val)
      stmt ...)))

(module+ test
  (check-equal? (with-env (hash) (inversion 'a 'b)) 'b)
  (check-equal? (with-env (hash 'a "") (inversion 'a 'b)) 'b)
  (check-equal? (with-env (hash 'a #"") (inversion 'a 'b)) 'b)
  (check-equal? (with-env (hash 'a '()) (inversion 'a 'b)) 'b))