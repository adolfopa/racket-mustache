#lang racket/base

(require (prefix-in only-text: "only-text.ms")
         (prefix-in simple-ref: "simple-ref.ms")
         (prefix-in escaping: "escaping.ms")
         (prefix-in section: "section.ms")
         (prefix-in section-ref: "section-ref.ms")
         (prefix-in inversion: "inversion.ms")
         (prefix-in delimiter: "delimiter.ms"))

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
  (check-tpl simple-ref:render (hash "foo" "<&bar>") "The variable value is &lt;&amp;bar&gt;.\n")
  
  (check-tpl escaping:render (hash) "The values are  and .\n")
  (check-tpl escaping:render (hash "foo" "<foo>" "bar" "&bar")
             "The values are <foo> and &bar.\n")
  
  (check-tpl section:render (hash "foo" #t) "Here are some X.\n")
  (check-tpl section:render (hash "foo" (list 1 2 3)) "Here are some XXX.\n")
  (check-tpl section:render (hash "foo" #f) "Here are some .\n")
  (check-tpl section:render (hash) "Here are some .\n")
  
  (check-tpl section-ref:render (hash "foo" (list (hash "bar" 42))) "The answer is 42.\n")
  
  (check-tpl inversion:render (hash "foo" "xyz") "\n")
  (check-tpl inversion:render (hash "foo" #f) "Maybe ...\n")
  (check-tpl inversion:render (hash) "Maybe ...\n")
  
  (check-tpl delimiter:render (hash "foo" "bar") "The value is bar.\n"))
