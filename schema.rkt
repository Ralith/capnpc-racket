#lang typed/racket

(require "accessors.rkt")

(provide (all-defined-out))

(define-ref code-generator-request)
(define-list code-generator-request-list)
(define-ref node)
(define-list node-list)
(define-ref node-nested-node)
(define-list node-nested-node-list)
(define-ref annotation)
(define-list annotation-list)
(define-ref enumerant)
(define-list enumerant-list)
(define-ref field)
(define-list field-list)
(define-struct-list-ref field-list-ref field-list field 56)
(define-ref type)

(define-ref requested-file)
(define-list requested-file-list)
(define-ref import)
(define-list import-list)

(define-list-accessor cgr-nodes code-generator-request node-list 0)

(define-struct-list-ref nodes-ref node-list node 80)

(define-int-accessor node-id node 3 #f 0)
(define-text-accessor node-name node 5)
(define-int-accessor node-name-prefix-length node 2 #f 2)
(define-int-accessor node-scope-id node 3 #f 2)
(define-list-accessor node-nested-nodes node node-nested-node-list 6)
(define-list-accessor node-annotations node annotation-list 7)

(define-union-which node-which node 6
  file struct enum interface const annotation)

(define-int-accessor node-struct-data-word-count node 1 #f 7)
(define-int-accessor node-struct-pointer-count node 1 #f 12)
(define-bool-accessor node-struct-is-group node 224)
(define-list-accessor node-struct-fields node field-list 8)

(define-list-accessor node-enum-enumerants node enumerant-list 8)
(define-struct-list-ref enumerant-list-ref enumerant-list enumerant 24)

(define-text-accessor field-name field 3)
(define-union-which field-which field 4
  slot group)
(define-int-accessor field-slot-offset field 2 #f 1)
(define-struct-accessor field-slot-type field type 5)

(define-union-which type-which type 0
  void bool int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64
  text data list enum struct interface any-pointer)
(define-struct-accessor type-list-element-type type type 2)
(define-int-accessor type-enum-type-id type 3 #f 1)
(define-int-accessor type-struct-type-id type 3 #f 1)
(define-int-accessor type-interface-type-id type 3 #f 1)

(define-text-accessor enumerant-name enumerant 1)
(define-int-accessor enumerant-code-order enumerant 1 #f 0)
(define-list-accessor enumerant-annotations enumerant annotation-list 2)

(define-list-accessor cgr-requested-files code-generator-request requested-file-list 1)

(define-struct-list-ref rfl-ref requested-file-list requested-file 24)

(define-int-accessor requested-file-id requested-file 3 #f 0)
(define-text-accessor requested-file-name requested-file 1)
(define-list-accessor requested-file-imports requested-file import-list 2)

(define-struct-list-ref imports-ref import-list import 16)

(define-int-accessor import-id import 3 #f 0)
(define-text-accessor import-name import 1)
