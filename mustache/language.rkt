#lang racket/base

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

(require racket/dict
         racket/function
         racket/list
         racket/string
         (only-in srfi/13 string-contains))

(provide display-escaped
         display-raw
         display-txt
         sequence
         inversion
         partial
         #%datum
         #%app
         (rename-out [module-begin #%module-begin]))

(module+ test
  (require racket/runtime-path)
  (require racket/stream)
  (require rackunit))

(define-syntax-rule (module-begin stmt ...)
  (#%plain-module-begin
   (require racket/runtime-path)
   
   (provide partial-load-path
            render)
   
   (define-runtime-module-path-index template-module-path ".")

   (define partial-load-path
     (resolved-module-path-name
      (module-path-index-resolve template-module-path)))
   
   (define (render env out)
     (parameterize ([current-output-port out]
                    [current-load-relative-directory partial-load-path])
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

(define-values (dict:has-key? dict:ref)
  (values dict-has-key? dict-ref))

(struct environment (frames) #:transparent
  #:methods gen:dict
  [(define (dict-ref dict key [failure
                               (λ () (error "no value found for key" key))])
     (define value
       (for/first ([frame (environment-frames dict)]
                   #:when (and (dict? frame) (dict:has-key? frame key)))
         (box (dict:ref frame key))))
     (if (box? value)
         (unbox value)
         (if (procedure? failure) (failure) failure)))
   (define (dict-has-key? dict key)
     (for/or ([frame (environment-frames dict)])
       (and (dict? frame) (dict:has-key? frame key))))])

(module+ test
  (check-equal? (dict-ref empty-environment 'a 'b) 'b)
  (check-equal? (dict-ref empty-environment 'a (thunk 'b)) 'b)
  (check-exn exn:fail:contract:arity?
             (thunk (dict-ref empty-environment 'a (λ (x y z) 'b)))))

(define empty-environment (environment '()))

(define (environment-extend env frame)
  (environment (cons frame (environment-frames env))))

;; current-env: (ParameterOf (Dict Any Any))
;; The current environment.
(define current-env (make-parameter empty-environment))

;; env-ref: Any -> Any
;; Return the value bound to `name' in the current environment. If no value
;; is bound, return the value of `default' (by default, the empty string).
(define (env-ref name [default ""])
  (if (and (string? name) (string-contains name "."))
      (let ([trimmed (string-trim name)])
        (if (eq? "." trimmed)
          (car (environment-frames (current-env)))
          (for/fold ([env (current-env)])
                    ([n (string-split trimmed #px"\\s*\\.\\s*")])
            (if (and env (dict? env) (dict:has-key? env n))
                (dict:ref env n)
                default))))
      (dict-ref (current-env) name (λ () default))))

(module+ test
  (check-equal? (env-ref 'x) "")
  (check-equal? (with-env (hash 'x 'y) (env-ref 'x)) 'y)
  (check-equal? (with-env (hash "x" (hash "y" (hash "z" "!"))) (env-ref "x.y.z")) "!")
  (check-equal? (with-env (hash "x" (hash "y" (hash "z" "!"))) (env-ref " x . y . z ")) "!")
  (check-equal? (with-env "!" (env-ref ".")) "!")
  (check-equal? (with-env (hash "x" "_" "y" "_") (env-ref "x.y")) "")
  (check-equal? (with-env (hash "x" "_" "y" "_") (env-ref "x.y")) ""))

;; Extend the current environment with the given object.
;; If the object is not a dict, no environment extension will be done.
(define-syntax-rule (with-env obj stmt ...)
  (parameterize ([current-env (environment-extend (current-env) obj)])
    stmt ...))

(module+ test
  (check-equal? (with-env 'a (env-ref 'a)) "")
  (check-equal? (with-env (hash 'a 'b) (env-ref 'a)) 'b)
  (check-equal? (with-env (hash) (with-env (current-env) (env-ref 'a))) ""))

;; Evaluate the body if `name' is bound in the current environment.
;; If the value bound to `name' is a list, the body will be evaluated
;; once per element, using the element to extend the environment; if
;; the bound value is a dict, the body will be evaluated once with
;; the environment extended by the dict; if it is any other non-false
;; value, the body will be evaluated once with no environment extension.
;;
;; Mustache stx: {{#foo}}...{{/foo}}
(define-syntax-rule (sequence name stmt ...)
  (let ([val (env-ref name #f)])
    (when val
     (cond [(mustache-seq? val)
            (for ([elt val])
              (with-env elt stmt ...))]
           [(mustache-false? val)
            (void)]
           [else
            (with-env val stmt ...)]))))

(module+ test
  (check-equal? (with-env (hash) (sequence 'a 'b)) (void))
  (check-equal? (with-env (hash 'a '()) (sequence 'a 'b)) (void))
  (check-equal? (with-env (hash 'a "") (sequence 'a 'b)) 'b)
  (check-equal? (with-env (hash 'a #"") (sequence 'a 'b)) 'b)

  (check-equal? (let ([out (open-output-string)])
                  (parameterize ([current-output-port out])
                    (with-env (hash 'a '(1 2 3))
                      (sequence 'a (display "x"))))
                  (get-output-string out))
                "xxx")
  (check-equal? (with-env (hash 'a 'b) (sequence 'a 'b)) 'b))

;; sequence-empty?: (All (A) ((Sequence A) -> Boolean)
(define (sequence-empty? seq)
  (define-values (has-values? _) (sequence-generate seq))
  (not (has-values?)))

(module+ test
  (check-true (sequence-empty? '()))
  (check-true (sequence-empty? #()))
  (check-true (sequence-empty? empty-stream)))

;; mustache-false?: Any -> Boolean
;; Test if `datum' is a falsy value.  A value is considered falsy
;; if it is #f or '().
(define (mustache-false? datum)
  (or (not datum)
      (and (mustache-seq? datum)
           (sequence-empty? datum))))

(module+ test
  (check-true (mustache-false? #f))
  (check-true (mustache-false? '()))
  (check-true (mustache-false? #()))
  (check-true (mustache-false? empty-stream))
  (check-false (mustache-false? ""))
  (check-false (mustache-false? #""))
  (check-false (mustache-false? (hash)))
  (check-false (mustache-false? '((a . 1)))))

;; mustache-seq?: Any -> Boolean
;; Test if `datum' is a mustache sequence (any Racket sequence except dicts,
;; strings and byte strings).
(define (mustache-seq? datum)
  ;; Both '() and vectors satisfy dict?, so we must rule them out before
  ;; checking the general case.
  (or (null? datum)
      (vector? datum)
      (and (sequence? datum)
           (not (dict? datum))
           (not (string? datum))
           (not (bytes? datum)))))

(module+ test
  (check-true (mustache-seq? '()))
  (check-true (mustache-seq? '(1 2 3)))
  (check-true (mustache-seq? #()))
  (check-false (mustache-seq? ""))
  (check-false (mustache-seq? #""))
  (check-false (mustache-seq? (hash 'foo (hash 'bar "abc")))))

;; Evaluate the given statements when `name' is not bound or is a "falsy" value.
;; Mustache stx: {{^foo}}...{{/foo}}
(define-syntax-rule (inversion name stmt ...)
  (let ([val (env-ref name #f)])
    (when (mustache-false? val)
      stmt ...)))

(module+ test
  (check-equal? (with-env (hash) (inversion 'a 'b)) 'b)
  (check-equal? (with-env (hash 'a "") (inversion 'a 'b)) (void))
  (check-equal? (with-env (hash 'a #"") (inversion 'a 'b)) (void))
  (check-equal? (with-env (hash 'a '()) (inversion 'a 'b)) 'b))

(define partial
  (let ([partials (make-hash)])
    (lambda (name)
      (unless (hash-has-key? partials name)
              (hash-set! partials name
                (let ([ns (make-base-empty-namespace)])
                  (namespace-attach-module (current-namespace) 'racket/dict ns) ; gen:dict
                  (parameterize ([current-namespace ns])
                    (dynamic-require name 'render (thunk (error "couldn't load partial" name)))))))
      ((hash-ref partials name) (current-env) (current-output-port)))))

(module+ test
  (define-runtime-path module-path ".")
  
  (define (check-partial module-name expected [initial-dict (hash)])
    (parameterize ([current-output-port (open-output-string)]
                   [current-load-relative-directory module-path])
      (with-env initial-dict
        (partial module-name)
        (check-equal? (get-output-string (current-output-port))
                      expected))))
  
  (check-partial "tests/only-text.ms" "I contain only text.\n")
  (check-partial "tests/simple-ref.ms" "The variable value is bar.\n" (hash "foo" "bar"))
  (check-partial "tests/simple-ref.ms" "The variable value is .\n" (hash)))
