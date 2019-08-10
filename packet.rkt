#lang typed/racket/base

(require racket/list
         racket/match

         bitsyntax

         "types.rkt"
         "varint.rkt")

(provide (all-defined-out))

(struct untyped-packet ([id : Integer]
                        [payload : Bytes])
  #:transparent)


(define-syntax packet
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       ([(len :: (var-int))
         (actual-payload :: binary bytes len)
         (rest :: binary)]
        (bit-string-case actual-payload
          #:on-short kf
          ([(packet-id :: (var-int))
            (payload :: binary)]
           (ks (untyped-packet packet-id (bit-string->bytes payload))
               rest))
          (else (kf))))
       (else (kf #t)))]
    [(_ #f pkt)
     (match-let ([(struct untyped-packet (id payload)) pkt])
       (define actual-payload
         (bit-string->bytes
          (bit-string [id :: (var-int)]
                      [payload :: binary])))
       (bit-string
        [(bytes-length actual-payload) :: (var-int)]
        [actual-payload :: binary]))]))

(module+ test
  (require typed/rackunit)
  (define-syntax-rule (check-packet-encode input expected)
    (check-equal? (bit-string->bytes (bit-string (input :: (packet))))
                  expected
                  (format "in: ~v out: ~v" input expected)))
  (define-syntax-rule (check-packet-decode input expected)
    (check-equal? (bit-string-case (cast input BitString)
                    ([(res :: (packet))] res)) expected
                  (format "in: ~v out: ~v" input expected)))

  (define testcases
    `((,(untyped-packet 0 #"") #"\x0\x0")
      (,(untyped-packet 1 #"") #"\x0\x1")))
  (test-case "packet-encode"
    (for ([t testcases])
      (check-packet-encode (first t) (second t))))
  (test-case "packet-decode"
    (for ([t testcases])
      (check-packet-decode (second t) (first t)))))


(: write-packet (untyped-packet Output-Port -> Index))
(define (write-packet pkt op)
  (write-bytes (bit-string->bytes
                (bit-string [pkt :: (packet)]))
               op))

(: read-packet (Input-Port -> (U EOF untyped-packet)))
(define (read-packet ip)
  (let loop ([buf (read-bytes 1 ip)])
    (if (eof-object? buf)
        eof
        (bit-string-case (cast buf BitString)
          ([(pkt :: (packet))]
           pkt)
          (else
           (define e (read-bytes 1 ip))
           (loop (if (eof-object? e)
                     e
                     (bytes-append buf e))))))))

(struct handshake-packet ([protocol-version : Integer]
                          [server-address : String]
                          [server-port : Exact-Nonnegative-Integer]
                          [next-state : Integer]))

#;
(define-syntax packet/handshake
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       ([pkt :: (packet)]
        (match-define ([(struct untyped-packet (id payload)) pkt]))
        (unless (= id 0)
          (kf))
        (bit-string-case payload
          ([pv :: (var-int)]
           )))
       (else (kf)))]
    [(_ #f pkt)
     (match-let ([(struct untyped-packet (id payload)) pkt])
       (bit-string
        [(bytes-length payload)  :: (var-int)]
        [id :: (var-int)]
        [payload :: binary]))]))
