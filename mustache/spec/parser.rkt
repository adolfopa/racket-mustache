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

(require "../parser.rkt"
         racket/syntax
         yaml)

(provide spec-read
         spec-read-syntax)

(define (spec-read in)
  (syntax->datum (spec-read-syntax #f in)))

(define (spec-read-syntax src in)
  (define yml (read-yaml in))
  (if yml (emit-module-body yml) eof))

(define (emit-module-body yml)
  (for/fold ([body #'()]) ([test (dict-ref yml "tests")])
    (define test-name (dict-ref test "name"))
    (define module-name (template-name test-name))
    (with-syntax ([template-submodule
                   (emit-template-submodule module-name
                                            (dict-ref test "template"))]
                  [test-submodule
                   (emit-test-submodule module-name
                                        (dict-ref test "expected")
                                        (dict-ref test "data")
                                        test-name
                                        (dict-ref test "desc"))]
                  [(forms ...) body])
      #'(forms ...
         (module+ test
           (require rackunit))
           template-submodule
           test-submodule))))


(module compound-lang racket
  (require mustache/language)
    
  (provide (all-from-out mustache/language)
           begin))

(define (emit-template-submodule name template)
  (with-syntax ([name (datum->syntax #f name)]
                [module-body
                 (call-with-input-string template
                   (λ (in) (mustache-read-syntax #'here in #f)))])
    #'(module name (submod mustache/spec/parser compound-lang)
        module-body)))

(define (emit-test-submodule template-name expected data test-name desc)
  (with-syntax* ([template-name template-name]
                 [expected (datum->syntax #f expected)]
                 [data (datum->syntax #f data)]
                 [test-name (datum->syntax #f test-name)]
                 [desc (datum->syntax #f desc)]
                 [ns: (generate-temporary)]
                 [ns:render (format-id #'ns: "~a~a" #'ns: 'render)]
                 [ns:partial-load-path (format-id #'ns: "~a~a" #'ns: 'partial-load-path)])
    #'(module+ test
        (require (rename-in (submod ".." template-name)
                            [render ns:render]
                            [partial-load-path ns:partial-load-path]))
        (check-equal? (call-with-output-string
                       (λ (out) (ns:render data out)))
                      expected
                      (~a test-name ": " desc)))))

(define (template-name test-name)
  (string->symbol (string-downcase (string-replace test-name #px"\\s+" "-"))))