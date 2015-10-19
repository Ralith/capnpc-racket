#! /usr/bin/env racket
#lang racket/base

(require "primitives.rkt"
         "schema.rkt"
         "codegen.rkt"
         "accessors.rkt")

(module+ test
  (require rackunit))

(module+ test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  (let* ((msg (read-stream-message))
         (cgr (let ((ref (root-struct msg)))
                (code-generator-request msg (reference-location ref) (struct-reference-data-size ref)))))
    (generate-code (code-generator-request-nodes cgr) (current-output-port))))
