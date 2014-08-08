#lang racket/base

(provide (rename-out [module-begin #%module-begin])
         module
         module+
         #%datum)

(define-syntax-rule (module-begin (stmt ...))
  (#%plain-module-begin stmt ...))