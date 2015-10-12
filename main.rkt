#! /usr/bin/env racket
#lang racket/base

(require "primitives.rkt"
         "schema.rkt"
         "codegen.rkt")

(module+ test
  (require rackunit))

(module+ test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  (let* ((msg (read-stream-message))
         (cgr (code-generator-request msg (root-struct-location msg))))
    (generate-code (cgr-nodes cgr) (current-output-port))))
