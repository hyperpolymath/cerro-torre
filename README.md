# Cerro Torre

**Supply-chain-verified Linux distribution for containers and immutable systems**

Cerro Torre provides complete cryptographic provenance for every package, from upstream source through build to deployment. Every binary can be traced back to its origins.

## Status

🚧 **Pre-alpha / Proof of Concept**

This project is in early development. The architecture is defined but most functionality is stubbed. See [ROADMAP.md](docs/ROADMAP.md) for what's planned.

## What Problem Does This Solve?

Modern software supply chains are vulnerable:
- Source packages can be tampered with
- Build processes are often opaque
- Binary distributions require blind trust
- Provenance chains are incomplete or nonexistent

Cerro Torre addresses this by:
- Recording complete provenance for every package
- Using formally verified tooling (Ada/SPARK) for security-critical operations
- Supporting reproducible builds with attestation
- Providing cryptographic signatures at every step

## Quick Start

```bash
# (Once implemented)
# Import a package from Debian
cerro import debian:hello/2.10-3

# Build it with full provenance tracking
cerro build manifests/hello.ctp

# Export as OCI container image
cerro export --format=oci hello:2.10-3

# Run it
podman run cerro-torre/hello:2.10-3
```

## Building from Source

Requires [Alire](https://alire.ada.dev/) (Ada package manager):

```bash
# Install dependencies and build
alr build

# Run tests
alr run cerro -- --help
```

## Project Structure

```
cerro-torre/
├── docs/                    # Documentation
├── governance/              # Cooperative governance documents
│   ├── articles.md          # Articles of association
│   ├── covenant.md          # The Palimpsest Covenant
│   └── decisions/           # Architectural decision records
├── keys/                    # Public keys for verification
├── manifests/               # Package manifest examples
├── spec/                    # Specifications
│   └── manifest-format.md   # CTP manifest format spec
├── src/
│   ├── core/                # SPARK-verified core (crypto, manifest, provenance)
│   ├── cli/                 # Command-line interface
│   ├── build/               # Build orchestration
│   ├── policy/              # SELinux policy generation
│   ├── importers/           # Package source importers
│   │   ├── debian/          # Debian .dsc importer
│   │   ├── fedora/          # Fedora SRPM importer
│   │   └── alpine/          # Alpine APKBUILD importer
│   └── exporters/           # Output format exporters
│       ├── oci/             # OCI container images
│       └── rpm-ostree/      # OSTree/rpm-ostree integration
├── alire.toml               # Alire package configuration
├── cerro_torre.gpr          # GNAT project file
├── CLAUDE.md                # AI development context
└── README.md                # This file
```

## Key Concepts

### Package Manifests (.ctp files)

Every package has a TOML manifest describing:
- Package identity and version
- Complete provenance chain (upstream source → patches → build)
- Cryptographic hashes of all content
- Builder and maintainer signatures

See [spec/manifest-format.md](spec/manifest-format.md) for the full specification.

### Provenance Chain

Every package records:
1. **Upstream source**: URL, hash, and optional signature
2. **Import source**: Which distribution we based our packaging on
3. **Patches applied**: Every modification documented and hashed
4. **Build attestation**: Signed record of the build process
5. **File hashes**: Every file in the package is hashed

### Formal Verification

Core security operations use SPARK, a formally verifiable subset of Ada:
- Cryptographic operations proven free of runtime errors
- No buffer overflows, no integer overflows
- Functional correctness proofs for critical algorithms

## Governance

Cerro Torre is organised as a democratic cooperative. See:
- [governance/articles.md](governance/articles.md) - Cooperative structure
- [governance/covenant.md](governance/covenant.md) - Community principles

## License

Dual-licensed under Apache 2.0 and MIT. Choose whichever suits your needs.

SPDX-License-Identifier: Apache-2.0 OR MIT

## Contributing

Contributions welcome! During the early phase, please open an issue to discuss before submitting large changes.

By contributing, you agree to the [Palimpsest Covenant](governance/covenant.md).

## Related Projects

- [Sigstore](https://sigstore.dev/) - Software signing infrastructure
- [in-toto](https://in-toto.io/) - Supply chain security framework
- [SLSA](https://slsa.dev/) - Supply chain Levels for Software Artifacts
- [Reproducible Builds](https://reproducible-builds.org/) - Bit-for-bit reproducibility

## Contact

This is a solo project in early stages. Issues and discussions welcome on the repository.
