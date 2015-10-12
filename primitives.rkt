#lang typed/racket/base

(require racket/match)

(provide (all-defined-out))

(define-type Message (Vectorof Bytes))

(define-type Word Natural)

(struct location ((segment : Natural) (word : Natural))
        #:transparent)

(: msg-ref (-> Message location Word))
(define (msg-ref msg loc)
  (let ((word (location-word loc)))
    (cast (integer-bytes->integer (vector-ref msg (location-segment loc)) #f #f (* 8 word) (* 8 (+ 1 word)))
          Natural)))

(: location+ (-> location Natural * location))
(define (location+ l . words)
  (location (location-segment l)
            (cast (apply + (location-word l) words) Natural)))

(: raw-struct-pointer? (-> Word Boolean))
(define (raw-struct-pointer? x)
  (= 0 (bitwise-bit-field x 0 2)))

(: raw-list-pointer? (-> Word Boolean))
(define (raw-list-pointer? x)
  (= 1 (bitwise-bit-field x 0 2)))

(: raw-far-pointer? (-> Word Boolean))
(define (raw-far-pointer? x)
  (= 2 (bitwise-bit-field x 0 2)))

(struct far-pointer ((indirect : Boolean) (location : location))
        #:transparent)

(: decode-far-pointer (-> Word far-pointer))
(define (decode-far-pointer n)
  (assert n raw-far-pointer?)
  (far-pointer (bitwise-bit-set? n 2) (location (bitwise-bit-field n 32 64) (bitwise-bit-field n 3 32))))

(: near-pointer-offset (-> Word Word))
(define (near-pointer-offset n)
  (bitwise-bit-field n 2 32))

(: traverse-far-pointer (All (a) (-> Message location (-> location Word a) a)))
(define (traverse-far-pointer msg loc proc)
  (let ((n (msg-ref msg loc)))
    (if (raw-far-pointer? n)
        (match-let (((far-pointer indirect landing) (decode-far-pointer n)))
          (if indirect
              (match-let (((far-pointer #f data-loc) (decode-far-pointer (msg-ref msg landing))))
                (proc data-loc (msg-ref msg (location+ landing 1))))
              (let ((near-ptr (msg-ref msg landing)))
                (proc (location+ landing 1 (near-pointer-offset near-ptr)) near-ptr))))
        (proc (location+ loc 1 (near-pointer-offset n)) n))))

(struct struct-pointer ((location : location) (size-data : Natural) (size-pointers : Natural))
        #:transparent)

(: decode-struct-pointer (-> Message location struct-pointer))
(define (decode-struct-pointer msg start)
  (traverse-far-pointer msg start
                        (lambda ((loc : location) (n : Word))
                          (assert n raw-struct-pointer?)
                          (struct-pointer loc (bitwise-bit-field n 32 48) (bitwise-bit-field n 48 64)))))

(struct list-pointer ((location : location) (element-type : Byte) (length : Natural))
        #:transparent)

(: decode-list-pointer (-> Message location list-pointer))
(define (decode-list-pointer msg start)
  (traverse-far-pointer
   msg start
   (lambda ((loc : location) (n : Word))
     (assert n raw-list-pointer?)
     (let ((elt-ty (cast (bitwise-bit-field n 32 35) Byte)))
       (if (= 7 elt-ty)
           (let ((tag (msg-ref msg loc)))
             (list-pointer (location+ loc 1) elt-ty (bitwise-bit-field tag 2 32)))
           (list-pointer loc elt-ty (bitwise-bit-field n 35 64)))))))

(: blob-ptr? (-> list-pointer Boolean))
(define (blob-ptr? ptr)
  (= 2 (list-pointer-element-type ptr)))

(: decode-blob (->* (Message location Natural) (Boolean) Bytes))
(define (decode-blob msg loc len (drop-last #f))
  (let ((off (* 8 (location-word loc))))
    (subbytes (vector-ref msg (location-segment loc)) off (+ off len
                                                             (if drop-last -1 0)))))

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

(: root-struct-location (-> Message location))
(define (root-struct-location msg)
  (struct-pointer-location (decode-struct-pointer msg (location 0 0))))
