#lang typed/racket/base

(provide (all-defined-out))

(require typed/json
         racket/function
         racket/match
         racket/math
         racket/tcp)

(require bitsyntax)

(require "varint.rkt" "types.rkt" "packet.rkt" "json.rkt")

(struct pong ([latency : Exact-Nonnegative-Integer]
              [json : Response])
  #:transparent)

(: server-list-ping ((String) (Integer) . ->* . (U pong EOF #f)))
(define (server-list-ping host [port 25565])
  (log-debug "Connecting to ~a:~a" host port)
  (define-values (ip op) (tcp-connect host port))
  (log-debug "Writing packet ...")
  (write-packet (untyped-packet 0
                                (bit-string->bytes
                                 (bit-string [-1 :: (var-int)]
                                             [host :: (mc-string)]
                                             [port :: big-endian integer bytes 2]
                                             [1 :: (var-int)]))) op)
  (log-debug "Writing second packet...")
  (write-packet (untyped-packet 0 #"") op)
  (log-debug "Flushing output...")
  (flush-output op)
  (log-debug "Reading response...")
  (define res (read-packet ip))
  (define json
    (if (eof-object? res)
        eof
        (match-let ([(struct untyped-packet (_ payload)) res])
          (define s : String (bit-string-case (cast payload BitString) ([(str :: (mc-string))] str)))
          (with-handlers ([exn:fail? (const #f)])
            (read-Response (open-input-string s))))))
  (log-debug "Sending ping...")
  (define ts (exact-round (current-milliseconds)))
  (write-packet (untyped-packet 1 (bit-string->bytes
                                   (bit-string [ts :: big-endian integer bytes 8])))
                op)
  (flush-output op)
  (read-packet ip)
  (define latency (- (exact-round (current-milliseconds)) ts))
  (close-input-port ip)
  (close-output-port op)
  (if (or (not json) (eof-object? json))
      json
      (pong (cast latency Exact-Nonnegative-Integer)  json)))
