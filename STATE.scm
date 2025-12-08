;;; STATE.scm — Cerro Torre Project State
;;; Checkpoint file for AI-assisted development sessions
;;; Format: Guile Scheme (state.scm specification)

(define-module (cerro-torre state)
  #:export (project-state))

;;;============================================================================
;;; METADATA
;;;============================================================================

(define metadata
  '((format-version . "1.0")
    (created . "2025-12-08")
    (updated . "2025-12-08")
    (project . "cerro-torre")
    (repository . "https://gitlab.com/cerro-torre")))

;;;============================================================================
;;; PROJECT CONTEXT
;;;============================================================================

(define project-context
  '((name . "Cerro Torre")
    (tagline . "Provenance-verified containers from democratically-governed sources")
    (description . "Supply-chain-verified Linux distribution for containers and
                    immutable systems. Combines formally verified Ada/SPARK tooling,
                    radical transparency, and cooperative governance.")
    (license . "MIT OR AGPL-3.0-or-later")
    (governance . "Multi-stakeholder cooperative")
    (primary-language . "Ada/SPARK")
    (build-system . "Alire")))

;;;============================================================================
;;; CURRENT FOCUS
;;;============================================================================

(define current-focus
  '((phase . "Phase 0: Foundations / Pre-Alpha")
    (milestone . "MVP v0.1 — First Ascent")
    (goal . "End-to-end demonstration with GNU Hello package")
    (target-workflow . "cerro import debian:hello -> cerro build -> cerro export --format=oci")))

;;;============================================================================
;;; ROUTE TO MVP v0.1
;;;============================================================================

(define mvp-roadmap
  '((must-have
     ((task . "TOML Parser Integration")
      (status . pending)
      (completion . 0)
      (description . "Integrate toml_slicer library for .ctp manifest parsing")
      (blockers . ("toml_slicer availability in Alire index"))
      (subtasks . ("Add toml_slicer dependency"
                   "Implement Parse_Manifest in cerro_manifest.adb"
                   "Validate against manifest-format.md specification"
                   "Write unit tests with AUnit")))

     ((task . "SHA-256 Implementation")
      (status . pending)
      (completion . 0)
      (description . "Implement cryptographic hashing with SPARK proofs")
      (blockers . ("Decision: Pure Ada vs libsodium bindings"))
      (subtasks . ("Choose implementation approach"
                   "Implement SHA256_Hash function"
                   "Add SPARK contracts and prove absence of runtime errors"
                   "Implement Verify_SHA256 function"
                   "Create libsodium bindings if chosen")))

     ((task . "Debian Importer (Basic)")
      (status . pending)
      (completion . 10)
      (description . "Import Debian .dsc packages and generate .ctp manifests")
      (blockers . ("TOML serialization for output"))
      (subtasks . ("Complete Parse_Dsc implementation"
                   "Implement HTTP client for mirror downloads"
                   "Download .dsc from Debian mirrors"
                   "Extract source tarball metadata"
                   "Generate skeleton .ctp manifest"
                   "Verify upstream hashes")))

     ((task . "Manifest Verification")
      (status . in-progress)
      (completion . 20)
      (description . "Verify manifest integrity and hash correctness")
      (blockers . ("Depends on SHA-256 implementation"))
      (subtasks . ("Complete Is_Valid_Hash implementation" ;; done
                   "Implement Verify_SHA256 data verification"
                   "Validate manifest structure completeness"
                   "Report verification status to CLI")))

     ((task . "CLI Commands")
      (status . pending)
      (completion . 5)
      (description . "Command-line interface for core operations")
      (blockers . ("Depends on all above components"))
      (subtasks . ("cerro import debian:<pkg>/<version>"
                   "cerro verify <manifest.ctp>"
                   "cerro info <manifest.ctp>"
                   "Argument parsing and help text"
                   "Error reporting and exit codes"))))

    (should-have
     ((task . "Ed25519 Signature Verification")
      (status . pending)
      (completion . 0)
      (description . "Verify existing signatures on manifests")
      (blockers . ("Crypto library choice"))
      (subtasks . ("Implement Verify_Ed25519 function"
                   "Parse signature format from manifests"
                   "Key loading from keys/ directory")))

     ((task . "OCI Export (Basic)")
      (status . pending)
      (completion . 5)
      (description . "Create minimal OCI images from built packages")
      (blockers . ("Requires build completion first"))
      (subtasks . ("Create scratch-based rootfs"
                   "Generate OCI config.json"
                   "Create image tarball"
                   "Test with podman load"))))

    (nice-to-have
     ((task . "Build Execution")
      (status . pending)
      (completion . 0)
      (description . "Run autotools configure/make/install")
      (subtasks . ("Execute build commands in isolated directory"
                   "Capture output files"
                   "Hash resulting artifacts"))))))

;;;============================================================================
;;; IMPLEMENTATION STATUS
;;;============================================================================

(define implementation-status
  '((core-modules
     ((module . "cerro_crypto")
      (interface . complete)
      (implementation . stub)
      (spark-verified . no)
      (notes . "All hash functions return zeros. Ed25519 always returns False.
                Types and contracts defined. Hex conversion implemented.
                Constant-time comparison implemented."))

     ((module . "cerro_manifest")
      (interface . complete)
      (implementation . partial)
      (spark-verified . no)
      (notes . "Type definitions complete. Parse_Version implemented.
                Is_Valid_Hash implemented. Parse_Manifest returns error status.
                Manifest_To_String not implemented."))

     ((module . "cerro_provenance")
      (interface . defined)
      (implementation . stub)
      (spark-verified . no)
      (notes . "Interface exists, implementation placeholder."))

     ((module . "cerro_verify")
      (interface . defined)
      (implementation . stub)
      (spark-verified . no)
      (notes . "Interface exists, implementation placeholder.")))

    (importers
     ((module . "cerro_import_debian")
      (interface . complete)
      (implementation . stub)
      (notes . "Types defined. DSC parsing interface ready. Mirror config defined.
                Implementation body needs HTTP client and actual parsing."))

     ((module . "cerro_import_fedora")
      (interface . defined)
      (implementation . stub)
      (notes . "Placeholder for Phase 2."))

     ((module . "cerro_import_alpine")
      (interface . defined)
      (implementation . stub)
      (notes . "Placeholder for Phase 2.")))

    (exporters
     ((module . "cerro_export_oci")
      (interface . defined)
      (implementation . stub)
      (notes . "OCI image export placeholder."))

     ((module . "cerro_export_ostree")
      (interface . defined)
      (implementation . stub)
      (notes . "OSTree export placeholder for Phase 3.")))

    (other
     ((module . "cerro_builder")
      (interface . defined)
      (implementation . stub)
      (notes . "Build orchestration placeholder."))

     ((module . "cerro_selinux")
      (interface . defined)
      (implementation . stub)
      (notes . "SELinux policy generation placeholder."))

     ((module . "cerro_cli")
      (interface . partial)
      (implementation . minimal)
      (notes . "Basic CLI structure exists.")))))

;;;============================================================================
;;; OPEN QUESTIONS / DECISIONS NEEDED
;;;============================================================================

(define open-questions
  '(((id . 1)
     (question . "Crypto library choice: Pure Ada implementation vs. binding to libsodium?")
     (context . "Need SHA-256, SHA-512, Ed25519 verification. Pure Ada gives full
                 SPARK verification but more implementation work. Libsodium is
                 battle-tested but requires FFI bindings.")
     (options . ("Pure Ada with SPARK proofs"
                 "Libsodium bindings (libsodium-ada)"
                 "Hybrid: Pure Ada for hashes, libsodium for Ed25519"))
     (recommendation . "Hybrid approach - pure Ada SHA-256 for SPARK proofs,
                        libsodium bindings for Ed25519")
     (priority . high)
     (blocking . ("SHA-256 Implementation" "Ed25519 Signature Verification")))

    ((id . 2)
     (question . "Manifest parser: Hand-written recursive descent vs. parser generator?")
     (context . "TOML parsing for .ctp manifests. toml_slicer exists in Alire.
                 Question is whether to use it or write custom parser.")
     (options . ("Use toml_slicer library"
                 "Hand-written recursive descent parser"
                 "Parser generator (e.g., ANTLR with Ada target)"))
     (recommendation . "Use toml_slicer - standard, maintained, less code to verify")
     (priority . high)
     (blocking . ("TOML Parser Integration")))

    ((id . 3)
     (question . "Parallelism model: Ada tasks vs. external process orchestration?")
     (context . "Build operations may benefit from parallelism. Ada has native
                 tasking. Alternative is to shell out to parallel processes.")
     (options . ("Ada native tasks"
                 "External process orchestration"
                 "Hybrid based on operation type"))
     (recommendation . "Defer decision - not needed for MVP single-package demo")
     (priority . low)
     (blocking . ()))

    ((id . 4)
     (question . "Transparency log protocol: Define our own vs. adopt Sigstore's format?")
     (context . "Federated transparency log is core to trust model. Sigstore
                 (Rekor) exists but may not fit federated model.")
     (options . ("Custom protocol designed for federation"
                 "Adopt Sigstore/Rekor"
                 "Extend Sigstore with federation layer"))
     (recommendation . "Defer to v0.2 - not needed for MVP")
     (priority . low)
     (blocking . ()))))

;;;============================================================================
;;; BLOCKING ISSUES
;;;============================================================================

(define blocking-issues
  '(((id . "BLOCK-001")
     (summary . "No working cryptographic implementation")
     (severity . critical)
     (description . "All hash functions in cerro_crypto.adb return zeros.
                     Ed25519 verification always returns False. Cannot verify
                     any provenance claims without working crypto.")
     (resolution . "Implement SHA-256 (Question 1 decision needed)")
     (blocks . ("Manifest Verification" "Debian Importer" "All verification")))

    ((id . "BLOCK-002")
     (summary . "No TOML parsing implementation")
     (severity . critical)
     (description . "Parse_Manifest procedure returns Invalid_Format error.
                     Cannot read or write .ctp manifest files.")
     (resolution . "Integrate toml_slicer and implement parsing")
     (blocks . ("Manifest parsing" "Debian import output" "CLI info command")))

    ((id . "BLOCK-003")
     (summary . "Alire dependencies may need verification")
     (severity . medium)
     (description . "alire.toml lists dependencies (gnatcoll, toml_slicer,
                     json_ada) that need to be verified as available and
                     compatible. Build has not been tested.")
     (resolution . "Run 'alr build' and resolve any dependency issues")
     (blocks . ("Project builds successfully")))))

;;;============================================================================
;;; LONG-TERM ROADMAP
;;;============================================================================

(define long-term-roadmap
  '((v0.1-first-ascent
     (goal . "End-to-end demo with GNU Hello")
     (status . current)
     (estimated-completion . 15) ;; percentage
     (key-deliverables . ("Import hello from Debian"
                          "Verify provenance hashes"
                          "Export OCI image"
                          "Run with podman")))

    (v0.2-base-camp
     (goal . "Multiple packages with dependencies")
     (status . future)
     (key-deliverables . ("Dependency resolution"
                          "Package signing with Ed25519"
                          "Fedora importer"
                          "Reproducibility checking"
                          "Local transparency log")))

    (v0.3-the-wall
     (goal . "Production-ready for single operator")
     (status . future)
     (key-deliverables . ("OSTree export"
                          "SELinux policy generation"
                          "Alpine importer"
                          "SBOM generation (SPDX, CycloneDX)"
                          "Read-only web UI")))

    (v0.4-the-summit
     (goal . "Federated operation, multiple operators")
     (status . future)
     (key-deliverables . ("Federated transparency log"
                          "Multi-operator build verification"
                          "Mirror support"
                          "Vulnerability tracking (CVE integration)")))

    (future-wishlist
     (items . ("Nix importer"
               "Arch importer (PKGBUILD)"
               "Bootable system images"
               "Secure Boot integration"
               "Hardware attestation (TPM)"
               "Mobile support (Android/iOS verification)")))))

;;;============================================================================
;;; CRITICAL NEXT ACTIONS
;;;============================================================================

(define next-actions
  '(((priority . 1)
     (action . "Decide crypto library approach")
     (rationale . "Unblocks SHA-256 and Ed25519 implementation")
     (decision-options . ("Pure Ada" "Libsodium bindings" "Hybrid")))

    ((priority . 2)
     (action . "Verify Alire build works")
     (rationale . "Ensure project compiles before adding implementations")
     (command . "alr build"))

    ((priority . 3)
     (action . "Implement SHA-256 hashing")
     (rationale . "Core primitive needed for all verification")
     (location . "src/core/cerro_crypto.adb:SHA256_Hash"))

    ((priority . 4)
     (action . "Integrate toml_slicer for manifest parsing")
     (rationale . "Enables reading/writing .ctp files")
     (location . "src/core/cerro_manifest.adb:Parse_Manifest"))

    ((priority . 5)
     (action . "Implement basic Debian .dsc parser")
     (rationale . "First step toward importing packages")
     (location . "src/importers/debian/cerro_import_debian.adb"))

    ((priority . 6)
     (action . "Create minimal working CLI")
     (rationale . "Need 'cerro verify' and 'cerro info' commands")
     (location . "src/cli/cerro_main.adb"))))

;;;============================================================================
;;; PROJECT DEPENDENCIES
;;;============================================================================

(define dependencies
  '((ada-libraries
     ((name . "gnatcoll")
      (version . ">=24.0.0")
      (purpose . "General utilities"))
     ((name . "toml_slicer")
      (version . "*")
      (purpose . "TOML parsing for manifests"))
     ((name . "json_ada")
      (version . "*")
      (purpose . "JSON handling")))

    (system-dependencies
     ((name . "dpkg-dev")
      (purpose . "Parsing Debian source packages"))
     ((name . "podman")
      (purpose . "Container operations (not Docker!)"))
     ((name . "ostree")
      (purpose . "OSTree operations (v0.3+)")))

    (potential-additions
     ((name . "libsodium-ada")
      (purpose . "Crypto if not using pure Ada")
      (decision-pending . #t))
     ((name . "aws")
      (purpose . "HTTP client for downloads")
      (needed-for . "Debian importer")))))

;;;============================================================================
;;; DOCUMENTATION STATUS
;;;============================================================================

(define documentation-status
  '((complete
     ("README.md" "ARCHITECTURE.md" "ROADMAP.md"
      "manifest-format.md" "governance/articles.md"
      "governance/covenant.md" "CONTRIBUTING.md"
      "ADR-0001: Ada/SPARK adoption"
      "ADR-0002: TOML for manifests"))

    (specifications
     ("spec/manifest-format.md" . complete)
     ("spec/provenance-chain.md" . complete)
     ("spec/transparency-log.md" . complete)
     ("spec/threshold-signing.md" . complete)
     ("spec/selinux-policy.md" . complete))

    (missing
     ("API documentation for Ada packages"
      "User guide / tutorial"
      "Deployment guide"))))

;;;============================================================================
;;; GOVERNANCE STATUS
;;;============================================================================

(define governance-status
  '((phase . "Pre-cooperative / Founder stewardship")
    (documents-complete . ("Articles of association"
                           "Palimpsest Covenant"
                           "Decision record template"))
    (pending . ("Software Freedom Conservancy application"
                "Initial membership recruitment"
                "First community vote"))))

;;;============================================================================
;;; SESSION NOTES
;;;============================================================================

(define session-notes
  '((observations
     ("Project has excellent documentation and specifications"
      "Architecture is well-designed with clear separation of concerns"
      "SPARK mode enabled on core modules but no proofs yet"
      "Implementation is mostly stubs - interface-first approach"
      "Good decision records capture rationale"))

    (concerns
     ("Large gap between documented design and implementation"
      "Crypto implementation is placeholder-only"
      "No tests exist yet"
      "Build has not been verified to work"))

    (strengths
     ("Clear vision and values (Palimpsest Covenant)"
      "Strong governance foundation"
      "Well-specified manifest format"
      "Thoughtful technology choices (Ada/SPARK)"
      "Good separation of trusted/untrusted code"))))

;;;============================================================================
;;; EXPORT
;;;============================================================================

(define project-state
  `((metadata . ,metadata)
    (project-context . ,project-context)
    (current-focus . ,current-focus)
    (mvp-roadmap . ,mvp-roadmap)
    (implementation-status . ,implementation-status)
    (open-questions . ,open-questions)
    (blocking-issues . ,blocking-issues)
    (long-term-roadmap . ,long-term-roadmap)
    (next-actions . ,next-actions)
    (dependencies . ,dependencies)
    (documentation-status . ,documentation-status)
    (governance-status . ,governance-status)
    (session-notes . ,session-notes)))

;;; End of STATE.scm
