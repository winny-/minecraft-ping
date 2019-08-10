#lang racket/base


(require racket/cmdline
         racket/function
         racket/match
         racket/string
         "ping.rkt")

(define (get-motd json)
  (define desc (hash-ref json 'description))
  (define motd (if (hash? desc)
                   (hash-ref desc 'text)
                   desc))
  (regexp-replace* #rx"§." motd ""))

(define (get-players json)
  (define p (hash-ref json 'players))
  (format "~a/~a~a"
          (hash-ref p 'online)
          (hash-ref p 'max)
          (let ([sam (hash-ref p 'sample #f)])
            (if (list? sam)
                (format " (~a)"
                        (string-join (map (curryr hash-ref 'name) sam)))
                ""))))


(module+ main
  (command-line
   #:args (host port)
   (define pp (string->number port))
   (unless pp
     (raise-user-error 'main "Invalid port ~a" port))
   (printf "Pinging ~a on ~a\n" host port)
   (define res (server-list-ping host pp))
   (when (eof-object? res)
     (displayln "Host is offline." (current-error-port))
     (exit))
   (match-define (pong latency json) res)
   (printf "MotD:       ~a\n" (get-motd json))
   (printf "Players:    ~a\n" (get-players json))
   (printf "Latency:    ~ams\n" latency)
   (printf "Version:    ~a\n" (hash-ref (hash-ref json 'version) 'name))
   ))
