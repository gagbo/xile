;; coding: utf-8
(define-module (xile application)
  #:declarative? #f
  #:use-module (xile application curses)
  #:use-module (xile application sdl)
  #:use-module (xile xi-protocol json-rpc) ; xile-setup
  #:use-module (xile variables) ; Variables
  #:export (run-xile-demo-sdl)
  #:export (run-xile-demo-curses))

(define (run-xile-demo-sdl)
  ;; Read user configuration
  (load (string-append (getcwd) "/config/xile.scm"))

  ;; HACK Logging init code
  (set-current-error-port (open (string-append (getcwd) "/logs/xile-err.log") (logior O_APPEND O_CREAT O_WRONLY)))
  (set-current-output-port (open (string-append (getcwd) "/logs/xile-out.log") (logior O_APPEND O_CREAT O_WRONLY)))

  ;; (start-xile-in-curses (xile-setup))
  (start-xile-in-sdl (xile-setup "~/.local/share/cargo/target/release/xi-core")))

(define (run-xile-demo-curses)
  ;; Read user configuration
  (load (string-append (getcwd) "/config/xile.scm"))

  ;; HACK Logging init code
  (set-current-error-port (open (string-append (getcwd) "/logs/xile-err.log") (logior O_APPEND O_CREAT O_WRONLY)))
  (set-current-output-port (open (string-append (getcwd) "/logs/xile-out.log") (logior O_APPEND O_CREAT O_WRONLY)))

  (start-xile-in-curses (xile-setup "~/.local/share/cargo/target/release/xi-core")))
