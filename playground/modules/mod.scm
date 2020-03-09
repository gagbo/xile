;; coding: utf-8

(define-module (mod)
  #:export (test-func))

(define test-func (lambda* (arg #:optional optarg) (error "test-func is defined but not properly set")))

(let
    ((closure-specific-var "yay"))
  (set! test-func
    (lambda* (arg #:optional optarg)
      (format #t "Got ~a, ~a ; ~a !~%" arg optarg closure-specific-var))))
