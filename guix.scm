;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; Cerro-Torre - Guix Package Definition
;; Run: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages ada)
             (gnu packages compression))

(define-public cerro-torre
  (package
    (name "cerro-torre")
    (version "0.1.0")
    (source (local-file "." "cerro-torre-checkout"
                        #:recursive? #t
                        #:select? (git-predicate ".")))
    (build-system gnu-build-system)
    (native-inputs
     (list gnat gprbuild))
    (synopsis "Supply-chain-verified Linux distribution for containers")
    (description
     "Cerro Torre is a supply-chain-verified Linux distribution for containers
and immutable systems.  It combines format-agnostic package imports (Debian
primary, Fedora/Alpine/Nix secondary), formally verified tooling in Ada/SPARK,
democratic cooperative governance, and complete cryptographic provenance for
all packages.")
    (home-page "https://github.com/hyperpolymath/cerro-torre")
    ;; Dual-licensed: MIT OR AGPL-3.0-or-later (user's choice)
    (license (list license:expat license:agpl3+))))

;; Return package for guix shell
cerro-torre
