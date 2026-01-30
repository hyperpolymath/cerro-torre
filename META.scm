;; SPDX-License-Identifier: MPL-2.0-or-later
;; META.scm - Project metadata and architectural decisions for Cerro Torre

(define project-meta
  `((version . "1.0.0")
    (schema-version . "1.0")

    (architecture-decisions
      ((adr-001
         ((title . "Use Ada/SPARK for Core Build System")
          (status . "accepted")
          (date . "2025-12-29")
          (context . "Cerro Torre requires formally verified tooling for supply chain security")
          (decision . "Use Ada with SPARK annotations for core modules - SPARK proofs guarantee absence of runtime errors")
          (consequences . ((positive . ("Machine-checked proofs" "No buffer overflows" "Strong typing"))
                           (negative . ("Smaller developer community" "SPARK prover licensing"))))))

       (adr-002
         ((title . "Use libsodium for MVP Cryptography")
          (status . "accepted")
          (date . "2025-12-29")
          (context . "Need SHA256 hashing and Ed25519 signatures for MVP")
          (decision . "Use libsodium-ada bindings for MVP, add liboqs for post-quantum in v0.2")
          (consequences . ((positive . ("Battle-tested" "Good performance" "Alire crate available"))
                           (negative . ("External C dependency"))))))

       (adr-003
         ((title . "Algorithm-Agile Signature Format with Suite Commitment")
          (status . "accepted")
          (date . "2025-12-29")
          (context . "Attestations must be verifiable for years - quantum computers could break Ed25519 by 2035")
          (decision . "Every signed object declares suite_id covered by signature - no silent downgrade")
          (consequences . ((positive . ("Future-proof" "Prevents downgrade attacks" "Clear migration path"))
                           (negative . ("More complex verification"))))))

       (adr-004
         ((title . "Use Podman for Build Isolation")
          (status . "accepted")
          (date . "2025-12-29")
          (decision . "Use Podman with pinned Debian base image for build isolation")))

       (adr-005
         ((title . "Multi-Stakeholder Cooperative Governance")
          (status . "accepted")
          (date . "2025-12-29")
          (decision . "Establish cooperative with Maintainer, User, Worker member classes - asset lock prevents sale to for-profit")))

       (adr-006
         ((title . "Turing-Incomplete TOML Manifest Format")
          (status . "accepted")
          (date . "2025-12-29")
          (decision . "Use declarative, Turing-incomplete .ctp manifest format - analyzable and provable")))))

    (development-practices
      ((code-style . "ada-spark")
       (security . "openssf-scorecard")
       (testing . "property-based")
       (versioning . "semver")
       (documentation . "asciidoc")
       (branching . "trunk-based")))

    (design-rationale
      ((why-ada . "Ada provides strong typing, contract-based design, and SPARK formal verification")
       (why-not-rust . "Rust provides memory safety but not formal verification to same degree as SPARK")
       (why-cooperative . "Critical infrastructure should be owned by community, not venture capital")
       (why-debian-first . "Debian is genuinely community-governed with constitutional documents")
       (why-turing-incomplete . "If manifest can execute arbitrary code, cannot verify what it does")
       (why-threshold-signing . "No single keyholder should sign releases - prevents single point of failure")))))
