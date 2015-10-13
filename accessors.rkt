#lang typed/racket

(require "primitives.rkt"
         (for-syntax racket/syntax))

(provide (except-out (all-defined-out) List-length))

(struct reference ((message : Message) (location : location)))

(struct struct-reference reference ())

(define-type element-type (U Nonnegative-Fixnum 'bit 'pointer))

(struct (a) List reference ((length : Natural)))

(define list-length List-length)

(: make-list-accessor (-> Natural (-> reference (Values location Natural))))
(define (make-list-accessor offset)
  (lambda ((ref : reference))
    (let ((ptr (decode-list-pointer (reference-message ref) (location+ (reference-location ref) offset))))
      (values (list-pointer-location ptr) (list-pointer-length ptr)))))

(: make-blob-accessor (->* (Natural) (Boolean) (-> reference Bytes)))
(define (make-blob-accessor offset (drop-last #f))
  (lambda ((ref : reference))
    (let-values (((loc length) ((make-list-accessor offset) ref)))
      (decode-blob (reference-message ref) loc length drop-last))))

(: make-text-accessor (-> Natural (-> reference String)))
(define (make-text-accessor offset)
  (compose bytes->string/utf-8 (make-blob-accessor offset #t)))

(: make-struct-accessor (-> Natural (-> reference location)))
(define (make-struct-accessor offset)
  (lambda ((ref : reference))
    (struct-pointer-location (decode-struct-pointer (reference-message ref)
                                                    (location+ (reference-location ref) offset)))))

(: make-int-accessor (-> Byte Boolean Natural (-> reference Integer)))
(define (make-int-accessor exponent signed offset)
  (let ((size (expt 2 exponent)))
    (lambda ((ref : reference))
      (let* ((loc (reference-location ref))
             (start (+ (* size offset) (* 8 (location-word loc)))))
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
                                    (+ byte (* 8 (location-word loc))))
                         byte-bit)))))

(: make-list-ref (All (a) (-> (-> reference a) Natural (-> reference Natural a))))
(define (make-list-ref transformer size)
  (lambda ((ref : reference) (i : Natural))
    (transformer (reference (reference-message ref) (location+ (reference-location ref) (* size i))))))

(: make-enum-accessor (-> Natural (Vectorof Symbol) (-> reference Symbol)))
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
       (ret (reference-message ref) (access ref))))))

(define-syntax-rule (define-list-accessor name arg ret offset)
  (define name
    (let ((access (make-list-accessor offset)))
     (lambda ((ref : arg))
       (let-values (((loc length) (access ref)))
         (cast (List (reference-message ref) loc length) (List ret)))))))

(define-syntax-rule (define-blob-accessor name arg offset)
  (define name (lambda ((x : arg)) (make-blob-accessor offset) x)))

(define-syntax-rule (define-text-accessor name arg offset)
  (define name (lambda ((x : arg)) ((make-text-accessor offset) x))))

;;; FIXME: Read size from pointer so reading of extended messages works
(define-syntax-rule (define-list-ref name arg transformer size)
  (define name
    (let ((access (make-list-ref transformer (cast (/ size 8) Nonnegative-Fixnum))))
     (lambda ((ref : arg) (index : Natural))
       (access ref index)))))

(define-syntax-rule (define-struct-list-ref name ret size)
  (define-list-ref name (List ret) (lambda ((r : reference)) (ret (reference-message r) (reference-location r))) size))

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
