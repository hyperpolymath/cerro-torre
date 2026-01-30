;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Cerro Torre position in the hyperpolymath ecosystem

(ecosystem
  (version . "1.0.0")
  (schema-version . "1.0")

  (name . "cerro-torre")
  (display-name . "Cerro Torre")
  (pronunciation . "/sero tore/")
  (etymology . "Patagonian peak - Tower Mountain - isolated, extreme, pure")

  (type . "container-distribution")
  (purpose . "Provenance-verified container distribution with democratic governance")
  (tagline . "Ship containers safely")

  (language-identity
    (primary . ((ada . "Core build system, CLI, cryptographic operations")
                (spark . "Formal verification of security-critical code")))
    (secondary . ((nickel . "Build configuration language")
                  (toml . "Manifest format (.ctp files)")
                  (bash . "Build scripts and automation")))
    (paradigms . (contract-based-design formal-verification supply-chain-security
                  declarative-configuration cryptographic-attestation)))

  (position-in-ecosystem
    (role . "attestation-producer")
    (layer . "build-and-distribution")
    (description . "Cerro Torre produces provenance-verified container bundles (.ctp) with cryptographic attestations. Vordr and Svalinn are consumers that verify what Cerro Torre builds."))

  (related-projects
    ((project (name . "svalinn") (relationship . "consumer") (integration-type . "deep")
              (description . "Edge gateway that verifies and gates containers")))
    ((project (name . "vordr") (relationship . "consumer") (integration-type . "attestation")
              (description . "Attestation verifier")))
    ((project (name . "verified-container-spec") (relationship . "protocol-spec")
              (description . "Specification for verified container attestation format")))
    ((project (name . "oblibeny") (relationship . "sibling")
              (description . "Kubernetes-like orchestration")))
    ((project (name . "bunsenite") (relationship . "sibling")
              (description . "Nickel-based configuration management")))
    ((project (name . "sigstore") (relationship . "external-dependency")
              (description . "Attestation signing and transparency log")))
    ((project (name . "in-toto") (relationship . "external-dependency")
              (description . "Supply chain attestation framework")))
    ((project (name . "oci") (relationship . "external-standard")
              (description . "Open Container Initiative image format")))
    ((project (name . "rhodium-standard") (relationship . "sibling-standard")
              (description . "Repository compliance standard"))))

  (what-this-is
    "Cerro Torre is a supply-chain-verified Linux distribution for containers that:"
    (items
      "Wraps OCI images into verifiable bundles (.ctp) with cryptographic attestations"
      "Provides deterministic verification with specific, actionable error messages"
      "Uses Ada/SPARK for formally verified core security operations"
      "Supports algorithm-agile signatures including post-quantum (ML-DSA-65)"
      "Is governed by a multi-stakeholder cooperative with asset lock"
      "Imports from Debian, Fedora, Alpine - not locked to any upstream"
      "Produces SLSA Level 3 attestations and SPDX SBOMs"))

  (what-this-is-not
    "Cerro Torre is not:"
    (items
      "A container runtime (use Svalinn, Podman, or Docker)"
      "An edge gateway or admission controller (that is Svalinn)"
      "A general CI/CD system (focused on verified containers only)"
      "A specification (that is verified-container-spec)"
      "Windows or macOS containers (Linux only)"))

  (standards-compliance
    ((standard . "RSR") (status . "compliant"))
    ((standard . "verified-container-spec") (status . "target-conformant") (role . "producer"))
    ((standard . "SLSA") (status . "level-3-target"))
    ((standard . "Sigstore") (status . "planned"))
    ((standard . "OCI") (status . "compliant"))
    ((standard . "SPDX") (status . "planned")))

  (competitive-landscape
    ((comparison-with . "Alpine")
     (similarities . ("Minimal container base" "Security focus"))
     (differences . ("Alpine lacks supply chain transparency" "Cerro Torre has formal governance")))
    ((comparison-with . "Wolfi")
     (similarities . ("Supply chain security focus" "Attestations"))
     (differences . ("Wolfi is VC-backed" "Cerro Torre is cooperatively governed" "Cerro Torre uses SPARK proofs")))
    ((positioning . "Choose Alpine or Cerro Torre - you do not need Wolfi, and you should demand supply chain transparency."))))
