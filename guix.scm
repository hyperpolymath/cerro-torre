;; Cerro-Torre - Guix Package Definition
;; Run: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages base))

(define-public cerro_torre
  (package
    (name "Cerro-Torre")
    (version "0.1.0")
    (source (local-file "." "Cerro-Torre-checkout"
                        #:recursive? #t
                        #:select? (git-predicate ".")))
    (build-system gnu-build-system)
    (synopsis "Guix channel/infrastructure")
    (description "Guix channel/infrastructure - part of the RSR ecosystem.")
    (home-page "https://github.com/hyperpolymath/Cerro-Torre")
    (license license:agpl3+)))

;; Return package for guix shell
cerro_torre
