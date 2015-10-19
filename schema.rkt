#lang typed/racket

(require "accessors.rkt")

(provide (all-defined-out))

(define-struct code-generator-request)
(define-struct node)
(define-struct node-nested-node)
(define-struct annotation)
(define-struct enumerant)
(define-struct field)
(define-struct-list-ref field-list-ref field)
(define-struct type)

(define-struct requested-file)
(define-struct import)

(define-list-accessor code-generator-request-nodes code-generator-request node 0)

(define-struct-list-ref node-list-ref node)

(define-int-accessor node-id node 3 #f 0)
(define-text-accessor node-display-name node 0)
(define-int-accessor node-display-name-prefix-length node 2 #f 2)
(define-int-accessor node-scope-id node 3 #f 2)
(define-list-accessor node-nested-nodes node node-nested-node 1)
(define-list-accessor node-annotations node annotation 2)

(define-tag Node-Case node-case
  file struct enum interface const annotation)
(define node-case (compose tag->node-case (cast (make-int-accessor 1 #f 6) (-> node Nonnegative-Fixnum))))

(define-int-accessor node-struct-data-word-count node 1 #f 7)
(define-int-accessor node-struct-pointer-count node 1 #f 12)
(define-bool-accessor node-struct-is-group node 224)
(define-int-accessor node-struct-discriminant-offset node 2 #f 8)
(define-list-accessor node-struct-fields node field 3)

(define-tag ElementSize element-size
  empty bit byte two-bytes four-bytes eight-bytes pointer inline-composite)

(define-list-accessor node-enum-enumerants node enumerant 3)
(define-struct-list-ref enumerant-list-ref enumerant)

(define-text-accessor field-name field 0)
(define-tag Field-Case field-case
  slot group)
(define field-case (compose tag->field-case (cast (make-int-accessor 1 #f 4) (-> field Nonnegative-Fixnum))))
(define-int-accessor field-slot-offset field 2 #f 1)
(define-struct-accessor field-slot-type field type 2)

(define-int-accessor field-discriminant-value field 1 #f 1 65535)

(: field-no-discriminant Natural)
(define field-no-discriminant 65535)

(define-tag Type-Case type-case
  void bool int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64 text data list enum struct interface any-pointer)
(define type-case (compose tag->type-case (cast (make-int-accessor 1 #f 0) (-> type Nonnegative-Fixnum))))

(define-struct-accessor type-list-element-type type type 0)
(define-int-accessor type-enum-type-id type 3 #f 1)
(define-int-accessor type-struct-type-id type 3 #f 1)
(define-int-accessor type-interface-type-id type 3 #f 1)

(define-text-accessor enumerant-name enumerant 0)
(define-int-accessor enumerant-code-order enumerant 1 #f 0)
(define-list-accessor enumerant-annotations enumerant annotation 1)

(define-list-accessor code-generator-request-requested-files code-generator-request requested-file 1)

(define-struct-list-ref rfl-ref requested-file)

(define-int-accessor requested-file-id requested-file 3 #f 0)
(define-text-accessor requested-file-name requested-file 0)
(define-list-accessor requested-file-imports requested-file import 1)

(define-struct-list-ref imports-ref import)

(define-int-accessor import-id import 3 #f 0)
(define-text-accessor import-name import 0)
