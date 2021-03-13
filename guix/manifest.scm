(use-modules (guix packages)
             (guix git-download)
             (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages texinfo)
             (gnu packages ncurses)
             (gnu packages guile-xyz)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages pkg-config)
             (gnu packages guile)
             (gnu packages sdl))

(define-syntax-rule (search-our-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in ./patches relative to the current file."
  (parameterize
      ((%patch-path (list (string-append (dirname (current-filename)) "/packages/patches"))))
    (list (search-patch file-name) ...)))


(define guile-3.0-sdl2
(package
 (name "guile-3.0-sdl2")
 (version "0.5.1")
 (source
  (origin
   (method git-fetch)
   (uri
    (git-reference
     (url "https://git.dthompson.us/guile-sdl2.git")
     (commit "1b7cdecf240859baa497f10215f3ebf72ec46963")))
   (file-name (git-file-name name version))
   (sha256
    (base32
      "1rpxbpfxz3lan70lcfmi1kbapsjnj52s6vy6p0bqj7way6535sq6"))
   (patches (search-our-patches "0001-ttf-add-more-bindings.patch"))))
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
  guile-3.0-ncurses-with-gpm))

