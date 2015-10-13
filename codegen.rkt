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
  (let ((str (substring (node-name node) (node-name-prefix-length node))))
    (string-set! str 0 (char-upcase (string-ref str 0)))
    str))

(: for-nodes (-> node-list (-> node Void) Void))
(define (for-nodes nodes proc)
  (for ((i (list-reference-length nodes)))
    (proc (nodes-ref nodes i))))

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
  (case (type-case ty)
    ('enum (node-type-name table (hash-ref table (type-enum-type-id ty))))
    ('struct (node-type-name table (hash-ref table (type-struct-type-id ty))))
    ('interface (node-type-name table (hash-ref table (type-interface-type-id ty))))
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

(: generate-code (-> node-list Output-Port Void))
(define (generate-code nodes out)
  (write-string header out)
  (let ((table : (HashTable Word node-info) (make-hash)))
    (for-nodes
     nodes
     (lambda (node)
       (let ((cases
              (case (node-case node)
                ('enum
                 (let ((enumerants (node-enum-enumerants node)))
                   (for/list : (Listof String) ((i (list-reference-length enumerants)))
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
            ('struct
                (unless (node-struct-is-group node)
                  (fprintf out "\n(define-ref ~a)\n(define-list ~a-List)\n(define-struct-list-ref ~a-list-ref ~a-List ~a ~a)\n"
                           name name field-base-name name name
                           (* 8 (+ (node-struct-data-word-count node)
                                   (node-struct-pointer-count node)))))
              (let ((fields (node-struct-fields node))
                    (anon-union-cases : (Listof field) null))
                (for ((i (list-reference-length fields)))
                  (let ((field (field-list-ref fields i)))
                    (unless (= field-no-discriminant (field-discriminant-value field))
                      (set! anon-union-cases (cons field anon-union-cases)))
                    (when (eq? 'slot (field-case field))
                      (let* ((fname (string-append field-base-name "-" (camel->hyphen (field-name field))))
                             (type (field-slot-type field))
                             (class (type-case type))
                             (offset (field-slot-offset field)))
                        (case class
                          ('list (fprintf out "(define-list-accessor ~a ~a ~a-List ~a)\n"
                                          fname name (type-name table (type-list-element-type type)) offset))
                          ('struct (fprintf out "(define-struct-accessor ~a ~a ~a ~a)\n"
                                            fname name (node-type-name table (hash-ref table (type-struct-type-id type)))
                                            offset))
                          ('text (fprintf out "(define-text-accessor ~a ~a ~a)\n" fname name offset))
                          ('data (fprintf out "(define-blob-accessor ~a ~a ~a)\n" fname name offset))
                          ('enum (fprintf out "(define-enum-accessor ~a ~a ~a\n " fname name offset)
                                 (for ((n (assert (node-info-cases (hash-ref table (type-enum-type-id type))) list?)))
                                   (write-string " " out)
                                   (write-string n out))
                                 (write-string ")\n" out))
                          ('bool (fprintf out "(define-bool-accessor ~a ~a ~a)\n"
                                          fname name offset))
                          (else (if (hash-has-key? int-types class)
                                    (let ((int-type (hash-ref int-types class)))
                                      (fprintf out "(define-int-accessor ~a ~a ~a ~a ~a)\n"
                                               fname name (car int-type) (cdr int-type) offset))
                                    (fprintf out "; ~a ~a\n" class fname))))))))
                (unless (null? anon-union-cases)
                  (let ((tag-type-name (string-append (node-type-name table (hash-ref table (node-id node)) #f) "-Case"))
                        (accessor-name (string-append field-base-name "-case")))
                    (generate-tag-support
                     out tag-type-name accessor-name
                     (map (compose camel->hyphen field-name)
                          (sort anon-union-cases
                                (lambda ((x : field) (y : field))
                                  (< (field-discriminant-value x)
                                     (field-discriminant-value y))))))
                    (fprintf out "(define ~a (compose tag->~a (cast (make-int-accessor 1 #f ~a) (-> ~a Nonnegative-Fixnum))))\n"
                             accessor-name accessor-name (node-struct-discriminant-offset node) name)))))
            ('enum
             (let ((enumerants (node-enum-enumerants node)))
               (generate-tag-support
                out name field-base-name
                (for/list : (Listof String) ((i (list-reference-length enumerants)))
                          (camel->hyphen (enumerant-name (enumerant-list-ref enumerants i))))))))))
       (void)))))
