;;; META.scm — Cerro Torre Metadata
;;; Format: Guile Scheme

(define-module (cerro-torre meta)
  #:export (project-meta))

;;;============================================================================
;;; METADATA
;;;============================================================================

(define metadata
  '((format-version . "1.0")
    (created . "2023-11-15")
    (updated . "2025-12-15")
    (project . "cerro-torre")))

;;;============================================================================
;;; PROJECT IDENTITY
;;;============================================================================

(define identity
  '((name . "Cerro Torre")
    (tagline . "The transparent, cooperative container base")
    (description . "Cerro Torre is a supply-chain-verified Linux distribution for containers and immutable systems.
                    It combines formally verified Ada/SPARK tooling, radical transparency, and cooperative governance
                    to provide a secure, auditable, and community-controlled alternative to Alpine and Wolfi.")
    (vision . "A world where container infrastructure is transparent, secure, and governed by its users—not corporations.")
    (values . ("Radical Transparency" "Formal Verification" "Democratic Governance" "Supply Chain Security" "Community Ownership"))
    (logo . "Stylized mountain peak with container icon (TBD)")
    (colors . ((primary . "#1e3a8a")    ; Dark blue (trust)
               (secondary . "#3b82f6")  ; Light blue (open-source)
               (accent . "#10b981")))   ; Green (governance)
    (domain . "cerrotorre.dev")
    (repository . "https://gitlab.com/cerro-torre")))

;;;============================================================================
;;; LICENSING
;;;============================================================================

(define licensing
  '((core-license . "MIT OR AGPL-3.0-or-later")
    (package-license . "Retains upstream license (e.g., GPL for Debian packages)")
    (rationale . "MIT for permissive use, AGPL for copyleft network use. Packages retain their original licenses.")
    (compliance . ("SPDX identifiers in Containerfile"
                   "REUSE compliance for source files"
                   "License headers in all files"))))

;;;============================================================================
;;; CONTRIBUTORS
;;;============================================================================

(define contributors
  '((maintainers
     ((name . "Jonathan D.A. Jewell")
      (role . "Founder, Lead Developer")
      (email . "jonathan@cerrotorre.dev")
      (gitlab . "hyperpolymath")
      (github . "ahyperpolymath")
      (since . "2023-10-01")))

    (contributors
     ((description . "List of community contributors")
      (how-to-add . "Submit a merge request to add yourself here!")))

    (governance
     ((description . "Multi-stakeholder cooperative")
      (membership . "Open to maintainers and user organizations")
      (voting . "One person, one vote for technical decisions; user members vote on strategic direction")
      (assets . "Locked to cooperative; cannot be privatized")))))

;;;============================================================================
;;; GOVERNANCE
;;;============================================================================

(define governance
  '((model . "Multi-stakeholder cooperative")
    (documents
     ((name . "Articles of Association")
      (status . "draft")
      (location . "governance/articles.md"))
     ((name . "Palimpsest Covenant")
      (status . "complete")
      (location . "governance/covenant.md"))
     ((name . "Decision Process")
      (status . "draft")
      (location . "governance/decisions.md")))
    (membership
     ((type . "Maintainer Member")
      (description . "Active code/infrastructure contributors")
      (rights . ("Vote on technical decisions"
                 "Propose new features"
                 "Access to cooperative assets")))
     ((type . "User Member")
      (description . "Organizations/individuals using Cerro Torre in production")
      (rights . ("Vote on strategic direction"
                 "Propose governance changes"
                 "Access to transparency logs")))
     ((type . "General Contributor")
      (description . "Casual contributors (code, docs, outreach)")
      (rights . ("Submit merge requests"
                 "Participate in discussions"
                 "Join cooperative after 3+ contributions"))))
    (asset-lock . "If dissolved, assets transfer to another cooperative or charity")
    (forking . "Explicitly encouraged; cooperative exists to be useful, not to control")))

;;;============================================================================
;;; TECHNICAL GOALS
;;;============================================================================

(define technical-goals
  '((security
     ((description . "Formal verification and supply chain security")
      (metrics . ("100% SPARK verification for core logic"
                  "Cryptographic provenance for all packages"
                  "SELinux policies for all containers"))
      (status . partial)))

    (transparency
     ((description . "Radical transparency for all artifacts")
      (metrics . ("Federated transparency logs"
                  "Public build reproducibility"
                  "SBOMs for all packages"))
      (status . planned)))

    (performance
     ((description . "Acceptable performance for container builds")
      (metrics . ("<10s to import/verify/export GNU Hello"
                  "<100MB memory usage for builds"
                  "Parallel build support"))
      (status . unmeasured)))

    (flexibility
     ((description . "Support for multiple upstream sources")
      (metrics . ("Debian .dsc importer (v0.1)"
                  "Fedora .spec importer (v0.2)"
                  "Alpine APKBUILD importer (v0.3)"))
      (status . partial)))

    (governance
     ((description . "Democratic and inclusive governance")
      (metrics . ("Cooperative bylaws finalized"
                  "First community vote held"
                  "10+ maintainer members"))
      (status . draft)))))

;;;============================================================================
;;; COMMUNITY GOALS
;;;============================================================================

(define community-goals
  '((adoption
     ((description . "Grow the user and contributor community")
      (metrics . ("100 GitLab stars"
                  "10+ active contributors"
                  "Used in 5+ production environments"))
      (status . early)))

    (outreach
     ((description . "Raise awareness of Cerro Torre")
      (metrics . ("Blog post on Hacker News front page"
                  "Talk at 1+ conference (e.g., FOSDEM, SeaGL)"
                  "Mentioned in 3+ tech publications"))
      (status . planned)))

    (education
     ((description . "Educate users on supply chain security")
      (metrics . ("Tutorial for importing/verifying packages"
                  "Guide to formal verification with SPARK"
                  "Webinar on cooperative governance"))
      (status . planned)))))

;;;============================================================================
;;; EXPORT
;;;============================================================================

(define project-meta
  `((metadata . ,metadata)
    (identity . ,identity)
    (licensing . ,licensing)
    (contributors . ,contributors)
    (governance . ,governance)
    (technical-goals . ,technical-goals)
    (community-goals . ,community-goals)))

;;; End of META.scm
