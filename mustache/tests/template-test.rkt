#lang racket/base

(require (prefix-in only-text: "only-text.ms")
         (prefix-in simple-ref: "simple-ref.ms"))

(module+ test
  (require rackunit))

(module+ test
  (define (check-tpl f env expected)
    (let ([out (open-output-string)])
      (f env out)
      (check-equal? (get-output-string out) expected)))

  (check-tpl only-text:render (hash) "I contain only text.\n")  
  (check-tpl simple-ref:render (hash "foo" "bar") "The variable value is bar.\n"))
