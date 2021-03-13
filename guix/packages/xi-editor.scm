(use-modules (guix packages)
             (guix build-system cargo)
             (guix git-download)
             (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages))
(package
 (name "xi-editor")
 (version "20210111.52bf343")
 (source
  (origin
   (method git-fetch)
   (uri
    (git-reference
     (url "https://github.com/xi-editor/xi-editor")
     (commit "52bf34368e52369106d8eca6096a4839f7629e1e")))
   (file-name (git-file-name name version))
   (sha256
    (base32
      "1rpxbpfxz3lan70lcfmi1kbapsjnj52s6vy6p0bqj7way6535sq6"))))
 (build-system cargo-build-system)
 (arguments
  '(#:cargo-inputs `(())
    #:cargo-development-inputs `(())
    #:phases
    (modify-phases %standard-phases
                   (add-before 'build 'chdir
                              (lambda _ (zero? (system* "cd" "rust")))))))
 (synopsis "Xi Editor")
 (description "Xi Editor")
 (home-page "https://github.com/xi-editor/xi-editor")
 (license asl2.0))
