#lang racket/base

(require (prefix-in only-text: "only-text.ms")
         (prefix-in simple-ref: "simple-ref.ms")
         (prefix-in sequence: "sequence.ms")
         (prefix-in sequence-ref: "sequence-ref.ms"))

(module+ test
  (require rackunit))

(module+ test
  (define (check-tpl f env expected)
    (let ([out (open-output-string)])
      (f env out)
      (check-equal? (get-output-string out) expected)))

  (check-tpl only-text:render (hash) "I contain only text.\n")
  
  (check-tpl simple-ref:render (hash "foo" "bar") "The variable value is bar.\n")
  (check-tpl simple-ref:render (hash) "The variable value is .\n")
  
  (check-tpl sequence:render (hash "foo" #t) "Here are some X.\n")
  (check-tpl sequence:render (hash "foo" (list 1 2 3)) "Here are some XXX.\n")
  (check-tpl sequence:render (hash "foo" #f) "Here are some .\n")
  (check-tpl sequence:render (hash) "Here are some .\n")
  
  (check-tpl sequence-ref:render (hash "foo" (list (hash "bar" 42))) "The answer is 42.\n"))
