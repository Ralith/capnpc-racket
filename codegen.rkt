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

(: node-type-fragment (-> node String))
(define (node-type-fragment node)
  (camel->hyphen (substring (node-name node) (node-name-prefix-length node))))

(: for-nodes (-> node-list (-> node Void) Void))
(define (for-nodes nodes proc)
  (for ((i (list-reference-length nodes)))
    (proc (nodes-ref nodes i))))

(struct node-info ((type-fragment : String) (scope : Word) (cases : (Option (Vectorof String))))
        #:transparent)

(: node-type-name (-> (HashTable Word node-info) node-info String))
(define (node-type-name table info)
  (let ((fragment (node-info-type-fragment info))
        (parent (hash-ref table (node-info-scope info))))
    (if (= 0 (node-info-scope parent))
        fragment
        (string-append (node-type-name table parent) "-" fragment))))

;;; exponent, sign
(: int-types (HashTable Symbol (Pairof Byte Boolean)))
(define int-types
  #hash((int8   . (0 . #t))
        (int16  . (1 . #t))
        (int32  . (2 . #t))
        (int64  . (3 . #t))
        (uint8  . (0 . #f))
        (uint16 . (1 . #f))
        (uint32 . (2 . #f))
        (uint64 . (3 . #f))))

(: type-name (-> (HashTable Word node-info) type String))
(define (type-name table ty)
  (case (type-which ty)
    ('enum (node-type-name table (hash-ref table (type-enum-type-id ty))))
    ('struct (node-type-name table (hash-ref table (type-struct-type-id ty))))
    ('interface (node-type-name table (hash-ref table (type-interface-type-id ty))))
    (else (symbol->string (type-which ty)))))

(: generate-code (-> node-list Output-Port Void))
(define (generate-code nodes out)
  (write-string header out)
  (let ((table : (HashTable Word node-info) (make-hash)))
    (for-nodes
     nodes
     (lambda (node)
       (let ((cases
              (case (node-which node)
                ('enum
                 (let ((enumerants (node-enum-enumerants node)))
                   (cast
                    (for/vector ((i (list-reference-length enumerants)))
                      (camel->hyphen (enumerant-name (enumerant-list-ref enumerants i))))
                    (Vectorof String))))
                (else #f))))
        (hash-set! table (node-id node) (node-info (node-type-fragment node) (node-scope-id node) cases)))))
    (for-nodes
     nodes
     (lambda (node)
       (unless (eq? 'file (node-which node))
        (let* ((name (node-type-name table (hash-ref table (node-id node))))
               (small-name (string-downcase name)))
          (case (node-which node)
            ('struct
                (unless (node-struct-is-group node)
                  (fprintf out "\n(define-ref ~a)\n(define-list ~a-list)\n(define-struct-list-ref ~a-list-ref ~a-list ~a ~a)\n"
                           name name small-name name name
                           (* 8 (+ (node-struct-data-word-count node)
                                   (node-struct-pointer-count node)))))
              (let ((fields (node-struct-fields node))
                    (arg-type (if (node-struct-is-group node)
                                  (node-type-name table (hash-ref table (node-info-scope (hash-ref table (node-id node)))))
                                  small-name)))
                ;; TODO: union which
                (for ((i (list-reference-length fields)))
                  (let ((field (field-list-ref fields i)))
                    (when (eq? 'slot (field-which field))
                      (let* ((fname (string-append small-name "-" (camel->hyphen (field-name field))))
                             (type (field-slot-type field))
                             (class (type-which type))
                             (offset (field-slot-offset field)))
                        (case class
                          ('list (fprintf out "(define-list-accessor ~a ~a ~a-list ~a)\n"
                                          fname arg-type (type-name table (type-list-element-type type)) offset))
                          ('struct (fprintf out "(define-struct-accessor ~a ~a ~a ~a)\n"
                                            fname arg-type (node-type-name table (hash-ref table (type-struct-type-id type)))
                                            offset))
                          ('text (fprintf out "(define-text-accessor ~a ~a ~a)\n" fname arg-type offset))
                          ('data (fprintf out "(define-blob-accessor ~a ~a ~a)\n" fname arg-type offset))
                          ('enum (fprintf out "(define-enum-accessor ~a ~a ~a" fname arg-type offset)
                                 (for ((n (assert (node-info-cases (hash-ref table (type-enum-type-id type))) vector?)))
                                   (write-string " " out)
                                   (write-string n out))
                                 (write-string ")\n" out))
                          ('bool (fprintf out "(define-bool-accessor ~a ~a ~a)\n"
                                          fname arg-type offset))
                          (else (if (hash-has-key? int-types class)
                                    (let ((int-type (hash-ref int-types class)))
                                      (fprintf out "(define-int-accessor ~a ~a ~a ~a ~a)\n"
                                               fname arg-type (car int-type) (cdr int-type) offset))
                                    (fprintf out "; ~a ~a\n" class fname))))))))))
            ('enum
             (let ((enumerants (node-enum-enumerants node)))
              (fprintf out "\n(define-type ~a (U" name)
              (for ((i (list-reference-length enumerants)))
                (write-string " '" out)
                (write-string (camel->hyphen (enumerant-name (enumerant-list-ref enumerants i))) out))
              (write-string ")\n" out))))))
       (void)))))
