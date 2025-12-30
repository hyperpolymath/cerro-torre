;; SPDX-License-Identifier: AGPL-3.0-or-later
;; ECOSYSTEM.scm — Cerro Torre's position in the hyperpolymath ecosystem
;; Format: hyperpolymath/ECOSYSTEM.scm specification

(ecosystem
  (version . "1.0.0")
  (schema-version . "1.0")

  (name . "cerro-torre")
  (display-name . "Cerro Torre")
  (pronunciation . "/ˈsero ˈtore/")
  (etymology . "Patagonian peak: 'Tower Mountain' — isolated, extreme, pure")
  (ascii-safe . "cerro-torre")

  (type . "build-system")
  (purpose . "Provenance-verified container builder with democratic governance")

  (language-identity
    (primary . ((ada . "Build system core with SPARK proofs")
                (spark . "Formal verification of build integrity")))
    (secondary . ((nickel . "Build configuration")
                  (bash . "Build scripts")))
    (paradigms . (contract-based-design
                  formal-verification
                  supply-chain-security)))

  (position-in-ecosystem
    (role . "attestation-producer")
    (layer . "build")
    (description . "Cerro Torre produces provenance-verified container images with
                    Sigstore attestations. It is the 'producer' in the verified
                    container ecosystem — Vörðr and Svalinn are 'consumers' that
                    verify what Cerro Torre builds."))

  (related-projects
    ((project (name . "vordr")
              (relationship . "consumer")
              (integration . "Attestation consumer — Vörðr verifies builds from Cerro Torre")
              (url . "https://github.com/hyperpolymath/vordr")))

    ((project (name . "svalinn")
              (relationship . "consumer")
              (integration . "Edge gateway — Svalinn gates images built by Cerro Torre")
              (url . "https://github.com/hyperpolymath/svalinn")))

    ((project (name . "verified-container-spec")
              (relationship . "protocol-spec")
              (integration . "Conformance target — Cerro Torre produces attestations per this spec")
              (url . "https://github.com/hyperpolymath/verified-container-spec")))

    ((project (name . "oblibeny")
              (relationship . "sibling")
              (integration . "Orchestration — coordinates builds across clusters")
              (url . "https://github.com/hyperpolymath/oblibeny")))

    ((project (name . "sigstore")
              (relationship . "external-dependency")
              (integration . "Attestation format — Cerro Torre signs with Sigstore")
              (url . "https://sigstore.dev")))

    ((project (name . "rhodium-standard")
              (relationship . "sibling-standard")
              (integration . "Repository compliance standard")))

    ((project (name . "gitvisor")
              (relationship . "infrastructure")
              (integration . "Repository management tooling"))))

  (what-this-is
    "Cerro Torre is a provenance-verified build system that:"
    (items
      "Builds container images with formal verification of build steps"
      "Produces Sigstore attestations for every artifact"
      "Enforces democratic governance for build policies"
      "Integrates with verified-container-spec protocol"
      "Uses Ada/SPARK for build integrity proofs"))

  (what-this-is-not
    "Cerro Torre is not:"
    (items
      "A container runtime (that's Vörðr)"
      "An edge gateway (that's Svalinn)"
      "A specification (that's verified-container-spec)"
      "A general CI/CD system (focused on verified containers only)"))

  (standards-compliance
    ((standard . "RSR")
     (status . "compliant"))
    ((standard . "verified-container-spec")
     (status . "conformant")
     (role . "producer"))
    ((standard . "SLSA")
     (status . "level-3")
     (notes . "Supply chain Levels for Software Artifacts"))
    ((standard . "Sigstore")
     (status . "compliant")
     (notes . "Keyless signing with transparency log"))))
