;;; ECOSYSTEM.scm — Cerro Torre Ecosystem
;;; Format: Guile Scheme

(define-module (cerro-torre ecosystem)
  #:export (ecosystem))

;;;============================================================================
;;; METADATA
;;;============================================================================

(define metadata
  '((format-version . "1.0")
    (created . "2023-11-15")
    (updated . "2025-12-15")
    (project . "cerro-torre")))

;;;============================================================================
;;; CORE COMPONENTS
;;;============================================================================

(define core-components
  '((cerro-cli
     ((description . "Command-line interface for import/verify/export")
      (language . "Ada")
      (dependencies . ("GNAT.Command_Line"))
      (status . "partial implementation")
      (repository . "src/cli/")))

    (cerro-manifest
     ((description . "Containerfile (TOML) parsing and validation")
      (language . "Ada")
      (dependencies . ("toml_slicer"))
      (status . "stub implementation")
      (repository . "src/core/")))

    (cerro-crypto
     ((description . "SHA-256 and Ed25519 cryptographic operations")
      (language . "Ada/SPARK")
      (dependencies . ("AdaCryptLib or custom"))
      (status . "stub implementation")
      (repository . "src/core/")))

    (cerro-import-debian
     ((description . "Debian .dsc package importer")
      (language . "Ada")
      (dependencies . ("AWS.Client for HTTP"))
      (status . "stub implementation")
      (repository . "src/importers/debian/")))

    (cerro-import-fedora
     ((description . "Fedora .spec package importer")
      (language . "Ada")
      (dependencies . ("AWS.Client for HTTP"))
      (status . "stub implementation")
      (repository . "src/importers/fedora/")))

    (cerro-import-alpine
     ((description . "Alpine APKBUILD package importer")
      (language . "Ada")
      (dependencies . ("AWS.Client for HTTP"))
      (status . "stub implementation")
      (repository . "src/importers/alpine/")))

    (cerro-export-oci
     ((description . "OCI image exporter")
      (language . "Ada")
      (dependencies . ())
      (status . "stub implementation")
      (repository . "src/exporters/oci/")))

    (cerro-export-ostree
     ((description . "OSTree export for immutable systems")
      (language . "Ada")
      (dependencies . ())
      (status . "stub implementation")
      (repository . "src/exporters/rpm-ostree/")))))

;;;============================================================================
;;; TOOLCHAIN
;;;============================================================================

(define toolchain
  '((alire
     ((description . "Ada dependency manager and build tool")
      (purpose . "Manages Ada/SPARK dependencies (e.g., toml_slicer, gnatcoll)")
      (website . "https://alire.ada.dev")
      (configuration . "alire.toml")))

    (gnat
     ((description . "GNU Ada Compiler (GNAT)")
      (purpose . "Compiles Ada/SPARK code")
      (website . "https://gcc.gnu.org/onlinedocs/gnat_ugn_unw.html")
      (configuration . "src/*.gpr")))

    (gnatprove
     ((description . "SPARK formal verification tool")
      (purpose . "Proves absence of runtime errors and functional correctness")
      (website . "https://www.adacore.com/sparkpro")
      (configuration . "gnatprove.gpr")))

    (guile
     ((description . "GNU Guile (Scheme implementation)")
      (purpose . "State files, package management (Guix)")
      (website . "https://www.gnu.org/software/guile")
      (configuration . "guix.scm")))

    (podman
     ((description . "Container runtime (Docker alternative)")
      (purpose . "Tests OCI exports (e.g., podman load < hello.oci)")
      (website . "https://podman.io")
      (configuration . "End-to-end test script")))))

;;;============================================================================
;;; INTEGRATIONS
;;;============================================================================

(define integrations
  '((debian
     ((description . "Import packages from Debian")
      (format . ".dsc files (source packages)")
      (example . "cerro import debian:hello/2.10-3")
      (status . "partial implementation")
      (repository . "src/importers/debian/")
      (mirror . "https://deb.debian.org/debian")))

    (fedora
     ((description . "Import packages from Fedora")
      (format . ".spec files (source RPMs)")
      (example . "cerro import fedora:nginx/1.26.0-1.fc40")
      (status . "stub implementation")
      (repository . "src/importers/fedora/")
      (mirror . "https://koji.fedoraproject.org")))

    (alpine
     ((description . "Import packages from Alpine")
      (format . "APKBUILD files")
      (example . "cerro import alpine:busybox/1.36.1-r0")
      (status . "stub implementation")
      (repository . "src/importers/alpine/")
      (mirror . "https://git.alpinelinux.org/aports")))

    (oci
     ((description . "Export Containerfiles to OCI images")
      (format . "OCI tarballs")
      (example . "cerro export --format=oci hello.ctp")
      (status . "stub implementation")
      (repository . "src/exporters/oci/")))

    (ostree
     ((description . "Export Containerfiles to OSTree")
      (format . "OSTree commits")
      (example . "cerro export --format=ostree hello.ctp")
      (status . "stub implementation")
      (repository . "src/exporters/rpm-ostree/")))))

;;;============================================================================
;;; COMMUNITY RESOURCES
;;;============================================================================

(define community
  '((website
     ((description . "Project website and documentation")
      (url . "https://cerrotorre.dev")
      (status . "planned")
      (repository . "website/")))

    (gitlab
     ((description . "Primary repository and issue tracker")
      (url . "https://gitlab.com/cerro-torre")
      (status . "active")
      (repository . ".")))

    (github
     ((description . "Mirror repository")
      (url . "https://github.com/hyperpolymath/cerro-torre")
      (status . "active")
      (repository . ".")))

    (matrix
     ((description . "Community chat (Matrix/Element)")
      (url . "https://matrix.to/#/#cerro-torre:matrix.org")
      (status . "planned")))))

;;;============================================================================
;;; ECOSYSTEM PARTNERS
;;;============================================================================

(define partners
  '((debian
     ((description . "Upstream package source and community")
      (relationship . "Cerro Torre imports Debian .dsc packages")
      (website . "https://debian.org")))

    (fedora
     ((description . "Upstream package source and community")
      (relationship . "Cerro Torre imports Fedora .spec packages")
      (website . "https://fedoraproject.org")))

    (alpine
     ((description . "Upstream package source and community")
      (relationship . "Cerro Torre imports Alpine APKBUILD packages")
      (website . "https://alpinelinux.org")))

    (adacore
     ((description . "Ada/SPARK language tools and support")
      (relationship . "Cerro Torre uses GNAT and SPARK for core logic")
      (website . "https://www.adacore.com")))

    (software-freedom-conservancy
     ((description . "Nonprofit home for open-source projects")
      (relationship . "Potential fiscal sponsorship")
      (website . "https://sfconservancy.org")))))

;;;============================================================================
;;; COMPETITORS
;;;============================================================================

(define competitors
  '((alpine
     ((description . "Minimalist container base image")
      (strengths . ("Small (~5MB)"
                    "Widely adopted (Docker default)"
                    "Simple (musl libc + BusyBox)"))
      (weaknesses . ("No supply chain verification"
                     "Limited to APK packages"
                     "No formal verification"))
      (comparison . "Cerro Torre offers provenance verification and multi-source imports")))

    (wolfi
     ((description . "Security-focused container base by Chainguard")
      (strengths . ("Rust-based (memory safety)"
                    "Sigstore integration"
                    "SBOM support"))
      (weaknesses . ("VC-backed governance"
                     "Custom package format (not Debian/Fedora compatible)"
                     "No formal verification"))
      (comparison . "Cerro Torre offers formal verification and democratic governance")))

    (nixos
     ((description . "Declarative Linux distribution with containers")
      (strengths . ("Reproducible builds"
                    "Large package ecosystem"
                    "Declarative configuration"))
      (weaknesses . ("Complex learning curve"
                     "Not container-focused"
                     "No formal verification"))
      (comparison . "Cerro Torre is simpler for containers and offers formal verification")))

    (guix
     ((description . "Functional package manager with containers")
      (strengths . ("Reproducible builds"
                    "Functional package management"
                    "No root privileges needed"))
      (weaknesses . ("Complex for container use cases"
                     "Smaller package ecosystem"
                     "No formal verification"))
      (comparison . "Cerro Torre is more container-focused and offers formal verification")))))

;;;============================================================================
;;; EXPORT
;;;============================================================================

(define ecosystem
  `((metadata . ,metadata)
    (core-components . ,core-components)
    (toolchain . ,toolchain)
    (integrations . ,integrations)
    (community . ,community)
    (partners . ,partners)
    (competitors . ,competitors)))

;;; End of ECOSYSTEM.scm
