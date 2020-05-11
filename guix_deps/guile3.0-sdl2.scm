(use-modules (guix packages)
             (guix git-download)
             (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages texinfo)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages pkg-config)
             (gnu packages guile)
             (gnu packages sdl))
(package
 (name "guile-sdl2")
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
   (patches (search-patches "0001-ttf-add-more-bindings.patch"))))
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
 (license lgpl3+))
