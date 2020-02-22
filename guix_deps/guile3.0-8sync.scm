(use-modules (guix packages)
             (gnu packages guile)
             (gnu packages guile-xyz))

(package
  (inherit guile-8sync)
  (name "guile3.0-8sync")
  (inputs
    (assoc-set!
      (package-inputs guile-8sync)
      "guile" `(,guile-next))))
