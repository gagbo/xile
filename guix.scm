(use-modules (guix git)
             (guix git-download)
             (guix gexp)
             (guix packages)
             (guix licenses)
             (guix build utils)
             (guix build-system guile)
             (gnu packages)
             (gnu packages sdl)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define %source-dir
  (dirname (current-filename)))

(define %git-commit
  (with-directory-excursion %source-dir
    (get-line (open-input-pipe "git rev-parse HEAD"))))

(package
  (name "xile")
  (version (git-version "0.0.1" "HEAD" %git-commit))
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system guile-build-system)
  (arguments '(#:source-directory "src"))
  (native-inputs (list guile-3.0))
  (inputs (list guile-sdl2
                guile-ncurses guile-ncurses/gpm
                guile-readline
                guile-json-3))
  (synopsis "Guile front-end to xi-editor")
  (description "xile is a toy front-end for now-abandoned xi-editor written
               in Guile Scheme.")
  (home-page "https://github.com/gagbo/xile")
  (license expat))
