(use-modules (guix packages)
             (guix utils)
             (gnu packages guile)
             (gnu packages guile-xyz))

(package
 (inherit guile-ncurses)
 (name "guile3.0-ncurses")
 (arguments
     '(#:configure-flags (list "--with-ncursesw"  ; Unicode support
                               "--with-gnu-filesystem-hierarchy")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-libguile-ncurses-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install"
                     "-C" "src/ncurses"
                     "-j" (number->string
                           (parallel-job-count)))
             (let* ((out   (assoc-ref outputs "out"))
                    (dir   "src/ncurses")
                    (files (find-files dir ".scm")))
               (substitute* files
                 (("\"libguile-ncurses\"")
                  (format #f "\"~a/lib/guile/3.0/libguile-ncurses\""
                          out)))
               #t))))))
 (inputs
  (assoc-set!
   (package-inputs guile-ncurses)
   "guile" `(,guile-next))))
