;; meta.scm — Cerro Torre Project Metadata
;; Human-readable project information and contributor registry

(define-module (cerro-torre meta)
  #:export (project-meta contributors licenses))

;;;============================================================================
;;; PROJECT METADATA
;;;============================================================================

(define project-meta
  '((name . "Cerro Torre")
    (version . "0.1.0-alpha")
    (tagline . "Supply-chain-verified Linux distribution for containers")
    (description . "Cerro Torre is a formally verified, democratically governed
                    container base distribution. It combines Ada/SPARK verification,
                    cryptographic provenance for all packages, and multi-stakeholder
                    cooperative governance.")

    (repository
     ((primary . "https://gitlab.com/cerro-torre/cerro-torre")
      (mirror . "https://github.com/hyperpolymath/cerro-torre")))

    (homepage . "https://cerrotorre.dev")
    (documentation . "https://docs.cerrotorre.dev")

    (issue-tracker . "https://gitlab.com/cerro-torre/cerro-torre/-/issues")

    (languages
     ((primary . "Ada/SPARK")
      (build . "Guile Scheme")
      (ci . "YAML")))

    (build-system . "Alire")
    (package-manager . "Guix")))

;;;============================================================================
;;; LICENSES
;;;============================================================================

(define licenses
  '((code
     ((spdx-id . "MIT OR AGPL-3.0-or-later")
      (text . "Dual-licensed: choose MIT for permissive use, AGPL-3.0+ for copyleft.")))

    (documentation
     ((spdx-id . "CC-BY-SA-4.0")
      (text . "Creative Commons Attribution-ShareAlike 4.0 International")))

    (governance
     ((spdx-id . "CC0-1.0")
      (text . "Governance documents are public domain (CC0)")))))

;;;============================================================================
;;; CONTRIBUTORS
;;;============================================================================

(define contributors
  '(((name . "Jonathan D. A. Jewell")
     (role . "Founder, Lead Maintainer")
     (email . "jonathan@cerrotorre.dev")
     (gitlab . "hyperpolymath")
     (github . "ahyperpolymath")
     (gpg-fingerprint . #f)  ;; Add when available
     (orcid . #f)            ;; Add when available
     (since . "2023-12-01")
     (areas . ("Architecture"
               "Core Ada/SPARK implementation"
               "Fedora importer"
               "Governance"
               "Documentation"))
     (status . active))))

;;;============================================================================
;;; GOVERNANCE
;;;============================================================================

(define governance
  '((type . "Multi-stakeholder cooperative")
    (formation-status . "Pre-incorporation / Founder stewardship")

    (stakeholder-classes
     ((maintainers
       ((description . "Active code contributors with commit rights")
        (voting-weight . 40)))
      (users
       ((description . "Registered users of Cerro Torre packages")
        (voting-weight . 30)))
      (advisors
       ((description . "Security researchers, legal experts, domain specialists")
        (voting-weight . 20)))
      (sponsors
       ((description . "Financial supporters")
        (voting-weight . 10)))))

    (documents
     ((articles . "governance/articles.md")
      (covenant . "governance/covenant.md")
      (decisions . "governance/decisions/")))))

;;;============================================================================
;;; FUNDING
;;;============================================================================

(define funding
  '((status . "Unfunded / Volunteer")
    (platforms
     ((github-sponsors . "hyperpolymath")
      (open-collective . #f)
      (liberapay . #f)))

    (transparency
     ((finances-public . #t)
      (expense-reports . #f)))))

;;;============================================================================
;;; COMMUNICATION
;;;============================================================================

(define communication
  '((channels
     ((matrix . "#cerro-torre:matrix.org")
      (mailing-list . #f)
      (forum . #f)))

    (code-of-conduct . "CODE_OF_CONDUCT.md")
    (security-policy . "SECURITY.md")))

;;;============================================================================
;;; EXPORT
;;;============================================================================

(define cerro-torre-meta
  `((project . ,project-meta)
    (licenses . ,licenses)
    (contributors . ,contributors)
    (governance . ,governance)
    (funding . ,funding)
    (communication . ,communication)))

cerro-torre-meta

;;; End of meta.scm
