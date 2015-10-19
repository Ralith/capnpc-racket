#lang typed/racket

(require "primitives.rkt"
         (for-syntax racket/syntax))

(provide (except-out (all-defined-out)
                     ListReference-length
                     ListReference-element-type
                     ListReference-element-data-size
                     ListReference-element-pointers))

;;; A reference always addresses the data of an object, and carries necessary information from the object's pointer.
(struct reference ((message : Message) (location : location)))

(struct struct-reference reference ((data-size : Index)))

(define-type ElementType (U 'data 'bit 'pointer 'composite))

(struct (a) ListReference reference ((length : Index) (element-type : ElementType) (element-data-size : Index) (element-pointers : Index)))

(define-syntax-rule (struct-cast x to)
  (let ((x* x))
   (to (reference-message x*) (reference-location x*) (struct-reference-data-size x*))))

(define list-length ListReference-length)

(: struct-list-ref (-> (ListReference struct-reference) Index struct-reference))
(define (struct-list-ref xs i)
  (let ((msg (reference-message xs))
        (loc (reference-location xs))
        (data-size (ListReference-element-data-size xs)))
    (case (ListReference-element-type xs)
      ((composite)
       (struct-reference msg (location+ loc (* i (* 8 (+ data-size (ListReference-element-pointers xs)))))
                         data-size))
      ((data)
       (struct-reference msg (location+ loc (* i data-size)) 1))
      ((pointer)
       (assert data-size zero?)
       (struct-reference msg (location+ loc (* i 8)) 0))
      (else (raise-user-error 'struct-list-ref "not a reference to a struct list")))))

(: list-list-ref (-> (ListReference (ListReference reference)) Index (ListReference reference)))
(define (list-list-ref xs i)
  (let ((msg (reference-message xs))
        (loc (reference-location xs)))
    (assert (eq? 'pointer (ListReference-element-type xs)))
    (list-deref msg (location+ loc (* 8 i)))))

(: int-list-ref (-> Natural Boolean (ListReference Integer) Index Integer))
(define (int-list-ref size signed xs i)
  (let* ((msg (reference-message xs))
         (loc (reference-location xs))
         (start (+ (location-byte loc) (* i size)))
         (end (+ start size)))
    (assert (eq? 'data (ListReference-element-type xs)))
    (integer-bytes->integer (vector-ref msg (location-segment loc)) signed #f start end)))

(: bool-list-ref (-> (ListReference Boolean) Index Boolean))
(define (bool-list-ref xs i)
  ((make-bool-accessor i) xs))

;;; Takes (possibly far) pointer location. Returns a fully initialized ListReference
(: list-deref (-> Message location (ListReference reference)))
(define (list-deref msg loc)
  (let-values (((target-loc tag) (traverse-far-pointer msg loc)))
    (assert tag raw-list-pointer?)
    (let ((outer-len (raw-list-pointer-length tag))
          (type (raw-list-pointer-element-type tag)))
      (case type
        ((0) (ListReference msg target-loc outer-len 'data 0 0))
        ((1) (ListReference msg target-loc outer-len 'bit 0 0))
        ((2 3 4 5)
         (ListReference msg target-loc outer-len 'data 1 0))
        ((6) (ListReference msg target-loc outer-len 'pointer 0 1))
        ((7) (let* ((tag (msg-ref msg target-loc))
                    (inner-len (near-pointer-offset tag)))
               (assert (raw-struct-pointer? tag))
               (ListReference msg (location+ target-loc 8) inner-len 'composite
                              (raw-struct-pointer-size-data tag)
                              (raw-struct-pointer-size-pointer tag))))
        (else (raise-user-error 'list-deref "illegal list pointer element type tag ~a" type))))))

(: print-struct-ref (-> struct-reference Void))
(define (print-struct-ref x)
  (printf "loc: ~a data-size: ~a\n" (reference-location x) (struct-reference-data-size x)))

(: print-list-ref (All (a) (-> (ListReference a) Void)))
(define (print-list-ref x)
  (printf "loc: ~a length: ~a\n" (reference-location x) (list-length x)))

(: make-list-accessor (-> Index (-> struct-reference (ListReference reference))))
(define (make-list-accessor offset)
  (lambda ((ref : struct-reference))
    (list-deref (reference-message ref)
                (location+ (reference-location ref)
                           (* 8 (+ (struct-reference-data-size ref) offset))))))

(: make-blob-accessor (->* (Index) (Boolean) (-> struct-reference Bytes)))
(define (make-blob-accessor offset (drop-last #f))
  (let ((access (make-list-accessor offset)))
    (lambda ((ref : struct-reference))
      (let ((blob-ref (access ref)))
        (decode-blob (reference-message ref)
                     (reference-location blob-ref)
                     (ListReference-length blob-ref)
                     drop-last)))))

(: make-text-accessor (-> Index (-> struct-reference String)))
(define (make-text-accessor offset)
  (compose bytes->string/utf-8 (make-blob-accessor offset #t)))

(: make-struct-accessor (-> Index (-> struct-reference struct-reference)))
(define (make-struct-accessor offset)
  (lambda ((ref : struct-reference))
    (let ((msg (reference-message ref)))
     (let-values (((loc tag)
                   (traverse-far-pointer msg
                                         (location+ (reference-location ref)
                                                    (* 8 (+ (struct-reference-data-size ref)
                                                            offset))))))
       (struct-reference msg loc (raw-struct-pointer-size-data tag))))))

(: make-int-accessor (-> Byte Boolean Index (-> reference Integer)))
(define (make-int-accessor exponent signed offset)
  (let ((size (expt 2 exponent)))
    (lambda ((ref : reference))
      (let* ((loc (reference-location ref))
             (start (+ (* size offset) (location-byte loc))))
        (integer-bytes->integer
         (vector-ref (reference-message ref)
                     (location-segment loc))
         signed #f start (+ start size))))))

(: make-bool-accessor (-> Natural (-> reference Boolean)))
(define (make-bool-accessor bit)
  (let* ((byte (floor (/ bit 8)))
         (byte-bit (modulo bit 8)))
    (lambda ((ref : reference))
      (let ((loc (reference-location ref)))
       (bitwise-bit-set? (bytes-ref (vector-ref (reference-message ref)
                                                (location-segment loc))
                                    (+ byte (location-byte loc)))
                         byte-bit)))))

(: make-enum-accessor (-> Index (Vectorof Symbol) (-> reference Symbol)))
(define (make-enum-accessor offset cases)
  (let ((accessor (make-int-accessor 1 #f offset)))
   (lambda ((ref : reference))
     (vector-ref cases (accessor ref)))))

(define-syntax (define-int-accessor stx)
  (syntax-case stx ()
    ((_ name type exponent sign offset)
     #'(define-int-accessor name type exponent sign offset 0))
    ((_ name type exponent sign offset default)
     (with-syntax ((ret (if (syntax->datum #'sign) #'Integer #'Natural)))
       #'(define name (compose (lambda ((x : ret)) (bitwise-xor default x))
                               (cast (make-int-accessor exponent sign offset) (-> type ret))))))))

(define-syntax-rule (define-bool-accessor name type bit)
  (define name (cast (make-bool-accessor bit) (-> type Boolean))))

(define-syntax-rule (define-struct-accessor name arg ret offset)
  (define name
    (let ((access (make-struct-accessor offset)))
      (lambda ((ref : arg))
        (struct-cast (access ref) ret)))))

(define-syntax-rule (define-list-accessor name arg ret offset)
  (define name
    (let ((access (make-list-accessor offset)))
     (lambda ((ref : arg))
       (cast (access ref) (ListReference ret))))))

(define-syntax-rule (define-blob-accessor name arg offset)
  (define name (let ((access (make-blob-accessor offset)))
                 (lambda ((x : arg)) (access x)))))

(define-syntax-rule (define-text-accessor name arg offset)
  (define name (let ((access (make-text-accessor offset)))
                 (lambda ((x : arg)) (access x)))))

(define-syntax-rule (define-struct-list-ref name elt)
  (define (name (xs : (ListReference elt)) (i : Index))
    (struct-cast (struct-list-ref xs i) elt)))

(define-syntax (define-enum-accessor stx)
  (syntax-case stx ()
    ((_ name arg offset case ...)
     (with-syntax ((retty (datum->syntax stx (list* 'U (map (lambda (x) (list 'quote x))
                                                            (syntax->list #'(case ...))))
                                         #'(case ...))))
       #'(define name (cast (make-enum-accessor offset #(case ...)) (-> arg retty)))))))

(define-syntax-rule (define-union-which name arg offset case ...)
  (define-enum-accessor name arg offset case ...))

(define-syntax-rule (define-struct name)
  (struct name struct-reference ()))

(define-syntax (define-tag stx)
  (syntax-case stx ()
    ((_ type-name small-name symbols ...)
     (with-syntax ((marshal (format-id stx "~a->tag" #'small-name #:source #'small-name))
                   (unmarshal (format-id stx "tag->~a" #'small-name #:source #'small-name))
                   (type-def
                    (datum->syntax stx (cons 'U (map (lambda (x) (list 'quote x))
                                                     (syntax->list #'(symbols ...))))
                                   #'(symbols ...)))
                   (mappings
                    (datum->syntax stx (let ((syms (syntax->list #'(symbols ...))))
                                         (for/hasheq ((sym syms)
                                                      (i (length syms)))
                                           (values (syntax->datum sym) i)))
                                   #'(symbols ...))))
      #'(begin
          (define-type type-name type-def)
          (: marshal (-> type-name Nonnegative-Fixnum))
          (define (marshal x)
            (hash-ref (cast mappings (HashTable type-name Nonnegative-Fixnum)) x))
          (: unmarshal (-> Nonnegative-Fixnum type-name))
          (define (unmarshal x)
            (vector-ref (cast #(symbols ...) (Vectorof type-name)) x)))))))

(: root-struct (-> Message struct-reference))
(define (root-struct msg)
  (let-values (((loc tag) (traverse-far-pointer msg (location 0 0))))
    (struct-reference msg loc (raw-struct-pointer-size-data tag))))
