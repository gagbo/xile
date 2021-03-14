(use-modules (guix packages)
             (guix git-download)
             (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages ncurses)
             (gnu packages pkg-config)
             (gnu packages sdl)
             (gnu packages texinfo))

(define-syntax-rule (search-our-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in ./patches relative to the current file."
  (parameterize
      ((%patch-path (list (string-append (dirname (current-filename)) "/packages/patches"))))
    (list (search-patch file-name) ...)))


(define guile-3.0-sdl2
(package
 (name "guile-3.0-sdl2")
 (version "20210115.dbc5984")
 (source
  (origin
   (method git-fetch)
   (uri
    (git-reference
     (url "https://git.dthompson.us/guile-sdl2.git")
     (commit "dbc5984dc1360944f4dd15f9d846829fab99f04d")))
   (file-name (git-file-name name version))
   (sha256
    (base32
      "1p1lhjgmis23i9wbg0z2rg1ar38rqbg2icz4nrnwg4j4r784c9ib"))))
 (build-system gnu-build-system)
 (arguments
  '(#:make-flags '("GUILE_AUTO_COMPILE=0")
    #:phases
    (modify-phases %standard-phases
                   (add-after 'unpack 'bootstrap
                              (lambda _ (zero? (system* "sh" "bootstrap")))))))
 (native-inputs
  `(("autoconf" ,autoconf)
    ("automake" ,automake)
    ("pkg-config" ,pkg-config)
    ("texinfo" ,texinfo)))
 (inputs
  `(("guile" ,guile-3.0)
    ("sdl2" ,sdl2)
    ("sdl2-image" ,sdl2-image)
    ("sdl2-mixer" ,sdl2-mixer)
    ("sdl2-ttf" ,sdl2-ttf)))
 (synopsis "Guile bindings for SDL2")
 (description "Guile-sdl2 provides pure Guile Scheme bindings to the
  SDL2 C shared library via the foreign function interface.")
 (home-page "https://git.dthompson.us/guile-sdl2.git")
 (license lgpl3+)))

(define guile-3.0-ncurses-with-gpm
(package
 (inherit guile-ncurses/gpm)
 (name "guile-3.0-ncurses-with-gpm")
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
  `(("ncurses" ,ncurses/gpm)
    ("guile" ,guile-3.0)))))

(packages->manifest
(list ;; The Basics
  guile-3.0
  guile-3.0-sdl2
  guile-3.0-ncurses-with-gpm
  guile-readline
  guile-json-3))

