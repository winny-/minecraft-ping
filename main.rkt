#lang typed/racket/base


(require racket/cmdline
         racket/function
         racket/list
         racket/match
         racket/string
         "ping.rkt"
         "json.rkt"
         "util.rkt")

(provide server-list-ping
         (prefix-out minecraft- [combine-out server-list-ping pong Response Version Players Sample]))



(module+ main
  (define use-short : (Parameter Boolean) (make-parameter #f))
  (: do-ping (String -> (Channelof String)))
  (define (do-ping h)
    (define chan : (Channelof String) (make-channel))
    (thread
     (thunk
      (channel-put chan
                   (match (string->host+port (cast h String) 25565)
                     [(and address (list host (? exact-nonnegative-integer? port)))
                      (format "~a:~a~a~a" host port (if (use-short) "... " "\n")
                              (match (with-handlers ([exn:fail? identity]) (server-list-ping (cast host String) (cast port Integer)))
                                [(? eof-object?) "Host is offline.\n"]
                                [#f "Bad response from host.\n"]
                                [(? exn:fail? e) (format "Error while pinging: ~a\n" (exn-message e))]
                                [(pong latency (Response (Version ver _) (Players pmax pcur samp) desc _))
                                 (if (use-short)
                                     (begin
                                       (format "~a | ~a/~a~a | ~a | ~ams\n"
                                               (string-join (map string-trim (string-split desc)))
                                               pcur pmax (if (empty? samp) "" (format " (~a)" (string-join (map Sample-name samp))))
                                               ver
                                               latency))
                                     (begin
                                       (string-append
                                        (format "MotD:       ~a\n" (string-join (map string-trim (string-split desc))))
                                        (format "Players:    ~a/~a~a\n" pcur pmax (if (empty? samp)
                                                                                      ""
                                                                                      (string-append " " (string-join (map Sample-name samp)))))
                                        (format "Version:    ~a\n" ver)
                                        (format "Latency:    ~ams\n" latency))))]))]))))
    chan)
  (command-line
   #:once-any (["-s" "--short"] "Use short output" (use-short #t))
   #:args (host . hosts)
   (define chans : (Listof (Channelof String)) (for/list ([h (cons host hosts)]) (do-ping (cast h String))))
   (for ([c chans])
     (display (channel-get c)))))
