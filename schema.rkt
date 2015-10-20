#lang typed/racket/base
(require "accessors.rkt")
(provide (all-defined-out))

(define-struct CodeGeneratorRequest-RequestedFile-Import)
(define-struct-list-ref code-generator-request-requested-file-import-list-ref CodeGeneratorRequest-RequestedFile-Import)

(define-struct CodeGeneratorRequest)
(define-struct-list-ref code-generator-request-list-ref CodeGeneratorRequest)

(define-struct CodeGeneratorRequest-RequestedFile)
(define-struct-list-ref code-generator-request-requested-file-list-ref CodeGeneratorRequest-RequestedFile)

(define-struct Method)
(define-struct-list-ref method-list-ref Method)

(define-struct Enumerant)
(define-struct-list-ref enumerant-list-ref Enumerant)

(define-struct Node)
(define-struct-list-ref node-list-ref Node)

(define-struct Value)
(define-struct-list-ref value-list-ref Value)
(define-tag ElementSize element-size
  empty bit byte two-bytes four-bytes eight-bytes pointer inline-composite)

(define-struct Type)
(define-struct-list-ref type-list-ref Type)

(define-struct Field)
(define-struct-list-ref field-list-ref Field)

(define-struct Node-NestedNode)
(define-struct-list-ref node-nested-node-list-ref Node-NestedNode)

(define-struct Annotation)
(define-struct-list-ref annotation-list-ref Annotation)


(define-int-accessor code-generator-request-requested-file-import-id CodeGeneratorRequest-RequestedFile-Import 3 #f 0)
(define-text-accessor code-generator-request-requested-file-import-name CodeGeneratorRequest-RequestedFile-Import 0)


(define-list-accessor code-generator-request-nodes CodeGeneratorRequest Node 0)
(define-list-accessor code-generator-request-requested-files CodeGeneratorRequest CodeGeneratorRequest-RequestedFile 1)


(define-int-accessor code-generator-request-requested-file-id CodeGeneratorRequest-RequestedFile 3 #f 0)
(define-text-accessor code-generator-request-requested-file-filename CodeGeneratorRequest-RequestedFile 0)
(define-list-accessor code-generator-request-requested-file-imports CodeGeneratorRequest-RequestedFile CodeGeneratorRequest-RequestedFile-Import 1)

(define-int-accessor type-interface-type-id Type 3 #f 1)

(define-int-accessor type-struct-type-id Type 3 #f 1)


(define-text-accessor method-name Method 0)
(define-int-accessor method-code-order Method 1 #f 0)
(define-int-accessor method-param-struct-type Method 3 #f 1)
(define-int-accessor method-result-struct-type Method 3 #f 2)
(define-list-accessor method-annotations Method Annotation 1)


(define-text-accessor enumerant-name Enumerant 0)
(define-int-accessor enumerant-code-order Enumerant 1 #f 0)
(define-list-accessor enumerant-annotations Enumerant Annotation 1)

(define-struct-accessor node-const-type Node Type 3)
(define-struct-accessor node-const-value Node Value 4)

(define-int-accessor node-struct-data-word-count Node 1 #f 7)
(define-int-accessor node-struct-pointer-count Node 1 #f 12)
(define-enum-accessor node-struct-preferred-list-encoding Node 13 tag->element-size)
(define-bool-accessor node-struct-is-group Node 224)
(define-int-accessor node-struct-discriminant-count Node 1 #f 15)
(define-int-accessor node-struct-discriminant-offset Node 2 #f 8)
(define-list-accessor node-struct-fields Node Field 3)

; const Field-NoDiscriminant


(define-int-accessor node-id Node 3 #f 0)
(define-text-accessor node-display-name Node 0)
(define-int-accessor node-display-name-prefix-length Node 2 #f 2)
(define-int-accessor node-scope-id Node 3 #f 2)
(define-list-accessor node-nested-nodes Node Node-NestedNode 1)
(define-list-accessor node-annotations Node Annotation 2)
(define-tag Node-Case node-case
  file struct enum interface const annotation)
(define node-case (compose tag->node-case (cast (make-int-accessor 1 #f 6) (-> Node Nonnegative-Fixnum))))

(define-struct-accessor node-annotation-type Node Type 3)
(define-bool-accessor node-annotation-targets-file Node 112)
(define-bool-accessor node-annotation-targets-const Node 113)
(define-bool-accessor node-annotation-targets-enum Node 114)
(define-bool-accessor node-annotation-targets-enumerant Node 115)
(define-bool-accessor node-annotation-targets-struct Node 116)
(define-bool-accessor node-annotation-targets-field Node 117)
(define-bool-accessor node-annotation-targets-union Node 118)
(define-bool-accessor node-annotation-targets-group Node 119)
(define-bool-accessor node-annotation-targets-interface Node 120)
(define-bool-accessor node-annotation-targets-method Node 121)
(define-bool-accessor node-annotation-targets-param Node 122)
(define-bool-accessor node-annotation-targets-annotation Node 123)


(define-bool-accessor value-bool Value 16)
(define-int-accessor value-int8 Value 0 #t 2)
(define-int-accessor value-int16 Value 1 #t 1)
(define-int-accessor value-int32 Value 2 #t 1)
(define-int-accessor value-int64 Value 3 #t 1)
(define-int-accessor value-uint8 Value 0 #f 2)
(define-int-accessor value-uint16 Value 1 #f 1)
(define-int-accessor value-uint32 Value 2 #f 1)
(define-int-accessor value-uint64 Value 3 #f 1)
(define-float32-accessor value-float32 Value 1)
(define-float64-accessor value-float64 Value 1)
(define-text-accessor value-text Value 0)
(define-blob-accessor value-data Value 0)
; any-pointer value-list
(define-int-accessor value-enum Value 1 #f 1)
; any-pointer value-struct
; any-pointer value-any-pointer
(define-tag Value-Case value-case
  void bool int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64 text data list enum struct interface any-pointer)
(define value-case (compose tag->value-case (cast (make-int-accessor 1 #f 0) (-> Value Nonnegative-Fixnum))))


(define-tag Type-Case type-case
  void bool int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64 text data list enum struct interface any-pointer)
(define type-case (compose tag->type-case (cast (make-int-accessor 1 #f 0) (-> Type Nonnegative-Fixnum))))


(define-text-accessor field-name Field 0)
(define-int-accessor field-code-order Field 1 #f 0)
(define-list-accessor field-annotations Field Annotation 1)
(define-int-accessor field-discriminant-value Field 1 #f 1 65535)
(define-tag Field-Case field-case
  slot group)
(define field-case (compose tag->field-case (cast (make-int-accessor 1 #f 4) (-> Field Nonnegative-Fixnum))))

; annotation Namespace


(define-text-accessor node-nested-node-name Node-NestedNode 0)
(define-int-accessor node-nested-node-id Node-NestedNode 3 #f 0)

(define-list-accessor node-interface-methods Node Method 3)
(define-list-accessor node-interface-extends Node Natural 4)

(define-int-accessor field-group-type-id Field 3 #f 2)

(define-struct-accessor type-list-element-type Type Type 0)


(define-int-accessor annotation-id Annotation 3 #f 0)
(define-struct-accessor annotation-value Annotation Value 0)

(define-int-accessor type-enum-type-id Type 3 #f 1)

(define-list-accessor node-enum-enumerants Node Enumerant 3)

(define-int-accessor field-slot-offset Field 2 #f 1)
(define-struct-accessor field-slot-type Field Type 2)
(define-struct-accessor field-slot-default-value Field Value 3)
(define-bool-accessor field-slot-had-explicit-default Field 128)

(define-int-accessor field-ordinal-explicit Field 1 #f 6)
(define-tag Field-Ordinal-Case field-ordinal-case
  implicit explicit)
(define field-ordinal-case (compose tag->field-ordinal-case (cast (make-int-accessor 1 #f 5) (-> Field Nonnegative-Fixnum))))
