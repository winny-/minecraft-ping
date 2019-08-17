#lang typed/racket/base


(require racket/cmdline
         racket/function
         racket/list
         racket/match
         racket/string
         "ping.rkt"
         "json.rkt")

(provide server-list-ping
         (prefix-out minecraft- [combine-out server-list-ping pong Response Version Players Sample]))

(module+ main
  (command-line
   #:args (host port)
   (define pp (string->number (cast port String)))
   (unless (exact-nonnegative-integer? pp)
     (raise-user-error 'main "Invalid port ~a" port))
   (printf "Pinging ~a on ~a\n" host port)
   (define res (server-list-ping (cast host String) (cast pp Integer)))
   (when (eof-object? res)
     (displayln "Host is offline." (current-error-port))
     (exit))
   (unless res
     (displayln "Bad response from host." (current-error-port))
     (exit))
   (match-define (pong latency (Response (Version ver _) (Players pmax pcur samp) desc _)) res)
   (printf "MotD:       ~a\n" desc)
   (printf "Players:    ~a/~a~a\n" pcur pmax (if (empty? samp)
                                                 ""
                                                 (string-append " " (string-join (map Sample-name samp)))))
   (printf "Latency:    ~ams\n" latency)
   (printf "Version:    ~a\n" ver)
   ))
