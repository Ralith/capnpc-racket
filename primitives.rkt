#lang typed/racket/base

(require racket/match)

(provide (all-defined-out))

(define-type Message (Vectorof Bytes))

(define-type Word Natural)
(define word? exact-nonnegative-integer?)

(struct location ((segment : Natural) (byte : Natural))
        #:transparent)

(: msg-ref (-> Message location Word))
(define (msg-ref msg loc)
  (let ((addr (location-byte loc)))
    (assert (/ addr 8) integer?)
    (cast (integer-bytes->integer (vector-ref msg (location-segment loc)) #f #f addr (+ 8 addr))
          Natural)))

(: location+ (-> location Natural * location))
(define (location+ l . bytes)
  (location (location-segment l)
            (cast (apply + (location-byte l) bytes) Natural)))

(: pointer-tag (-> Word Byte))
(define (pointer-tag n)
  (cast (bitwise-bit-field n 0 2) Byte))

(: near-pointer-offset (-> Word Index))
(define (near-pointer-offset n)
  (cast (bitwise-bit-field n 2 32) Index))

(define struct-pointer-tag 0)
(define list-pointer-tag 1)
(define far-pointer-tag 2)

(: null-pointer? (-> Word Boolean))
(define (null-pointer? x)
  (= 0 x))

(: raw-struct-pointer? (-> Word Boolean))
(define (raw-struct-pointer? x)
  (= struct-pointer-tag (pointer-tag x)))

(: raw-struct-pointer-size-data (-> Word Index))
(define (raw-struct-pointer-size-data n)
  (cast (bitwise-bit-field n 32 48) Index))

(: raw-struct-pointer-size-pointer (-> Word Index))
(define (raw-struct-pointer-size-pointer n)
  (cast (bitwise-bit-field n 48 64) Index))

(: raw-list-pointer? (-> Word Boolean))
(define (raw-list-pointer? x)
  (= list-pointer-tag (pointer-tag x)))

(: raw-list-pointer-element-type (-> Word Byte))
(define (raw-list-pointer-element-type n)
  (cast (bitwise-bit-field n 32 35) Byte))

(: raw-list-pointer-length (-> Word Index))
(define (raw-list-pointer-length n)
  (cast (bitwise-bit-field n 35 64) Index))

(: raw-far-pointer? (-> Word Boolean))
(define (raw-far-pointer? x)
  (= far-pointer-tag (pointer-tag x)))

(struct far-pointer ((indirect : Boolean) (location : location))
        #:transparent)

(: decode-far-pointer (-> Word far-pointer))
(define (decode-far-pointer n)
  (assert n raw-far-pointer?)
  (far-pointer (bitwise-bit-set? n 2) (location (bitwise-bit-field n 32 64) (* 8 (bitwise-bit-field n 3 32)))))

(: traverse-far-pointer (-> Message location (Values location Word)))
(define (traverse-far-pointer msg loc)
  (unless (integer? (/ (location-byte loc) 8))
    (error "wat0"))
  (let ((n (msg-ref msg loc)))
    (if (raw-far-pointer? n)
        (match-let (((far-pointer indirect landing) (decode-far-pointer n)))
          (if indirect
              (match-let (((far-pointer #f data-loc) (decode-far-pointer (msg-ref msg landing))))
                (unless (integer? (/ (location-byte data-loc) 8))
                  (error "wat"))
                (values data-loc (msg-ref msg (location+ landing 8))))
              (let ((near-ptr (msg-ref msg landing)))
                (unless (integer? (/ (location-byte landing) 8))
                  (error "wat2"))
                (values (location+ landing 8 (* 8 (near-pointer-offset near-ptr))) near-ptr))))
        (begin
          (unless (integer? (/ (location-byte loc) 8))
            (error "wat3"))
          (values (location+ loc 8 (* 8 (near-pointer-offset n))) n)))))

(struct list-pointer ((location : location) (element-type : Byte) (length : Natural))
        #:transparent)

;; (: decode-list-pointer (-> Message location list-pointer))
;; (define (decode-list-pointer msg start)
;;   (traverse-far-pointer
;;    msg start
;;    (lambda ((loc : location) (n : Word))
;;      (assert n raw-list-pointer?)
;;      (list-pointer loc (raw-list-pointer-element-type n) (raw-list-pointer-length n)))))

(: blob-ptr? (-> list-pointer Boolean))
(define (blob-ptr? ptr)
  (= 2 (list-pointer-element-type ptr)))

(: decode-blob (->* (Message location Natural) (Boolean) Bytes))
(define (decode-blob msg loc len (null-terminated #f))
  (let ((off (location-byte loc)))
    (subbytes (vector-ref msg (location-segment loc)) off (+ off len
                                                             (if null-terminated -1 0)))))

(: read-bytes-exact (-> Natural Input-Port Bytes))
(define (read-bytes-exact n port)
  (let ((bytes (read-bytes n port)))
    (if (eof-object? bytes)
        (error 'read-bytes-exact "unexpected EOF while reading ~a bytes" n)
        bytes)))

(: read-stream-message (->* () (Input-Port) Message))
(define (read-stream-message (port (current-input-port)))
  (let* ((n (cast (+ 1 (integer-bytes->integer (read-bytes-exact 4 port) #f #f 0 4)) Nonnegative-Fixnum))
         (sizes : (Vectorof Nonnegative-Fixnum)
                (let ((size-bytes (read-bytes-exact (* 4 n) port)))
                  (build-vector n (lambda ((i : Index))
                                    (let ((off (* 4 i)))
                                      (cast (integer-bytes->integer size-bytes #f #f off (+ 4 off))
                                            Nonnegative-Fixnum)))))))
    (unless (odd? n)
      ;; Discard padding
      (read-bytes-exact 4 port))
    (build-vector n (lambda ((i : Index))
                      (read-bytes-exact (* 8 (vector-ref sizes i)) port)))))
