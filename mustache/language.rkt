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

(define-syntax-rule (module-begin stmt ...)
  (#%plain-module-begin
   (provide render)
   (define (render env out)
     (parameterize ([current-output-port out])
       (with-env env stmt ...)))))

;; TODO: implement a better HTML escaping procedure.
(define escape
  (let ([amp (reverse '(#\& #\a #\m #\p #\;))]
        [lt (reverse '(#\& #\l #\t #\;))]
        [gt (reverse '(#\& #\g #\t #\;))])
    (λ (bs)
      (cond [(or (string? bs) (bytes? bs))
             (define ->list
               (if (string? bs)
                   string->list
                   (compose (curry map integer->char) bytes->list)))                    
             (let loop ([xs (->list bs)] [acc '()])
               (if (null? xs)
                   (list->string (reverse acc))
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

(define (display-escaped name)
  (display (escape (env-ref name))))

(define (display-raw name)
  (display (env-ref name)))

(define (display-txt text)
  (display text))

(define current-env (make-parameter '()))

(define (env-ref name)
  (define value
    (for/first ([x (current-env)]
                #:when (hash-has-key? x name))
      (hash-ref x name)))
  (or value ""))

(define-syntax-rule (with-env obj stmt ...)
  (parameterize ([current-env (cons obj (current-env))])
    stmt ...))

(define-syntax-rule (sequence name stmt ...)
  (let ([val (env-ref name)])
    (when val
     (cond [(list? val)
            (for ([elt val])
              (with-env elt stmt ...))]
           [(hash? val)
            (with-env val stmt ...)]
           [else
            (error "not a hash or list!" val)])))) ; TODO: fix error message and exn

;; TODO: This is wrong. The empty list, string,
;; and so on, should be considered false values.
(define-syntax-rule (inversion name stmt ...)
  (let ([val (env-ref name)])
    (unless val stmt ...)))