#lang typed/racket/base

(require "schema.rkt"
         "primitives.rkt"
         "accessors.rkt"
         racket/format)

(provide generate-code)

(: header String)
(define header
  "#lang typed/racket/base\n(require \"accessors.rkt\")\n(provide (all-defined-out))\n")

(: concat-map (All (a b) (-> (-> a (Listof b)) (Listof a) (Listof b))))
(define (concat-map proc xs)
  (if (null? xs) null
      (append (proc (car xs)) (concat-map proc (cdr xs)))))

(: camel->hyphen (-> String String))
(define (camel->hyphen str)
  (if (= 0 (string-length str))
      str
      (list->string
       (cons (char-downcase (string-ref str 0))
             (concat-map (lambda ((c : Char))
                           (if (char-upper-case? c)
                               (list #\- (char-downcase c))
                               (list c)))
                         (cdr (string->list str)))))))

(: node-type-fragment (-> Node String))
(define (node-type-fragment node)
  (let ((str (substring (node-display-name node) (node-display-name-prefix-length node))))
    (string-set! str 0 (char-upcase (string-ref str 0)))
    str))

(: for-nodes (-> (ListReference Node) (-> Node Void) Void))
(define (for-nodes nodes proc)
  (for ((i (list-length nodes)))
    (proc (node-list-ref nodes i))))

(struct node-info ((type-fragment : String) (scope : Word) (cases : (Option (Listof String))) (group? : Boolean))
        #:transparent)

(define-type NodeTable (HashTable Word node-info))

(: node-type-name (->* (NodeTable node-info) (Boolean) String))
(define (node-type-name table info (skip-groups #t))
  (let ((fragment (node-info-type-fragment info))
        (parent (hash-ref table (node-info-scope info))))
    (cond
     ((= 0 (node-info-scope parent)) fragment)
     ((and skip-groups (node-info-group? info)) (node-type-name table parent))
     (else (string-append (node-type-name table parent #t) "-" fragment)))))

(: node-field-base-name (-> NodeTable node-info String))
(define (node-field-base-name table info)
  (let ((fragment (string-downcase (camel->hyphen (node-info-type-fragment info))))
        (parent (hash-ref table (node-info-scope info))))
    (cond
     ((= 0 (node-info-scope parent)) fragment)
     (else (string-append (node-field-base-name table parent) "-" fragment)))))

;;; exponent, sign
(: int-types (HashTable Symbol (Pairof Byte Boolean)))
(define int-types
  #hasheq((int8   . (0 . #t))
          (int16  . (1 . #t))
          (int32  . (2 . #t))
          (int64  . (3 . #t))
          (uint8  . (0 . #f))
          (uint16 . (1 . #f))
          (uint32 . (2 . #f))
          (uint64 . (3 . #f))))

(: type-name (-> (HashTable Word node-info) Type String))
(define (type-name table ty)
  (case (type-case ty)
    ((enum) (node-type-name table (hash-ref table (type-enum-type-id ty))))
    ((struct) (node-type-name table (hash-ref table (type-struct-type-id ty))))
    ((interface) (node-type-name table (hash-ref table (type-interface-type-id ty))))
    ((int8 int16 int32 int64) "Integer")
    ((uint8) "Byte")
    ((uint16 uint32 uint64) "Natural")
    ((float32) "Single-Flonum")
    ((float64) "Flonum")
    (else (symbol->string (type-case ty)))))

;;; Enums and unions
(: generate-tag-support (-> Output-Port String String (Listof String) Void))
(define (generate-tag-support out name small-name tags)
  (fprintf out "(define-tag ~a ~a\n " name small-name)
  (for ((tag tags))
    (write-string " " out)
    (write-string tag out))
  (write-string ")\n" out)
  (void))

(: value-int (-> Value Integer))
(define (value-int value)
  (case (value-case value)
    ((int8) (value-int8 value))
    ((int16) (value-int16 value))
    ((int32) (value-int32 value))
    ((int64) (value-int64 value))
    ((uint8) (value-uint8 value))
    ((uint16) (value-uint16 value))
    ((uint32) (value-uint32 value))
    ((uint64) (value-uint64 value))
    (else (error "not an integer value"))))

(: empty-value? (-> Value Boolean))
(define (empty-value? value)
  (case (value-case value)
    ((int8 int16 int32 int64 uint8 uint16 uint32 uint64)
     (= 0 (value-int value)))
    ;; ((float32) (= 0 (value-float32 value)))
    ;; ((float64) (= 0 (value-float64 value)))
    ((bool) (eq? #f (value-bool value)))
    ((void) #t)
    ((enum) (= 0 (value-enum value)))
    ((text data list struct) (null-pointer? (msg-ref (reference-message value) (reference-location value))))
    ((interface any-pointer) #t)
    (else (error "impossible"))))

(: generate-code (-> (ListReference Node) Output-Port Void))
(define (generate-code nodes out)
  (write-string header out)
  (let ((table : (HashTable Word node-info) (make-hasheqv)))
    (for-nodes
     nodes
     (lambda (node)
       (let ((cases
              (case (node-case node)
                ((enum)
                 (let ((enumerants (node-enum-enumerants node)))
                   (for/list : (Listof String) ((i (list-length enumerants)))
                     (camel->hyphen (enumerant-name (enumerant-list-ref enumerants i))))))
                (else #f))))
         (hash-set! table (node-id node) (node-info (node-type-fragment node) (node-scope-id node) cases
                                                    (and (eq? 'struct (node-case node))
                                                         (node-struct-is-group node)))))))
    (for-nodes
     nodes
     (lambda (node)
       (unless (eq? 'file (node-case node))
        (let* ((name (node-type-name table (hash-ref table (node-id node))))
               (field-base-name (node-field-base-name table (hash-ref table (node-id node)))))
          (case (node-case node)
            ((struct)
                (unless (node-struct-is-group node)
                  (fprintf out "\n(define-struct ~a)\n(define-struct-list-ref ~a-list-ref ~a)\n"
                           name field-base-name name))
              (let ((fields (node-struct-fields node))
                    (anon-union-cases : (Listof Field) null))
                (for ((i (list-length fields)))
                  (let ((field (field-list-ref fields i)))
                    (unless (= 65535 (field-discriminant-value field))
                      (set! anon-union-cases (cons field anon-union-cases)))
                    (when (eq? 'slot (field-case field))
                      (let* ((fname (string-append field-base-name "-" (camel->hyphen (field-name field))))
                             (type (field-slot-type field))
                             (class (type-case type))
                             (offset (field-slot-offset field))
                             (default (field-slot-default-value field)))
                        (case class
                          ((list) (fprintf out "(define-list-accessor ~a ~a ~a ~a)\n"
                                           fname name (type-name table (type-list-element-type type)) offset))
                          ((struct) (fprintf out "(define-struct-accessor ~a ~a ~a ~a)\n"
                                             fname name (node-type-name table (hash-ref table (type-struct-type-id type)))
                                             offset))
                          ((text) (fprintf out "(define-text-accessor ~a ~a ~a)\n" fname name offset))
                          ((data) (fprintf out "(define-blob-accessor ~a ~a ~a)\n" fname name offset))
                          ((enum) (fprintf out "(define-enum-accessor ~a ~a ~a\n " fname name offset)
                                 (for ((n (assert (node-info-cases (hash-ref table (type-enum-type-id type))) list?)))
                                   (write-string " " out)
                                   (write-string n out))
                                 (write-string ")\n" out))
                          ((bool) (fprintf out "(define-bool-accessor ~a ~a ~a)\n"
                                           fname name offset))
                          ((void) (void))
                          (else (if (hash-has-key? int-types class)
                                    (let ((int-type (hash-ref int-types class)))
                                      (fprintf out "(define-int-accessor ~a ~a ~a ~a ~a"
                                               fname name (car int-type) (cdr int-type) offset)
                                      (unless (empty-value? default)
                                        (fprintf out " ~v" (value-int default)))
                                      (write-string ")\n" out))
                                    (fprintf out "; ~a ~a\n" class fname))))))))
                (unless (null? anon-union-cases)
                  (let ((tag-type-name (string-append (node-type-name table (hash-ref table (node-id node)) #f) "-Case"))
                        (accessor-name (string-append field-base-name "-case")))
                    (generate-tag-support
                     out tag-type-name accessor-name
                     (map (compose camel->hyphen field-name)
                          (sort anon-union-cases
                                (lambda ((x : Field) (y : Field))
                                  (< (field-discriminant-value x)
                                     (field-discriminant-value y))))))
                    (fprintf out "(define ~a (compose tag->~a (cast (make-int-accessor 1 #f ~a) (-> ~a Nonnegative-Fixnum))))\n"
                             accessor-name accessor-name (node-struct-discriminant-offset node) name)))))
            ((enum)
             (let ((enumerants (node-enum-enumerants node)))
               (generate-tag-support
                out name field-base-name
                (for/list : (Listof String) ((i (list-length enumerants)))
                          (camel->hyphen (enumerant-name (enumerant-list-ref enumerants i)))))))
            (else (fprintf out "\n; ~a ~a\n" (node-case node) name)))))
       (void)))))
