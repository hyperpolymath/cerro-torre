# Cerro Torre Roadmap

## Current Status: Pre-Alpha

The project structure and architecture are defined. Core modules exist as stubs with interfaces specified but implementations incomplete.

---

## MVP v0.1 — "First Ascent"

**Goal:** End-to-end demonstration with a single package (GNU Hello)

### Must Have

- [ ] **TOML Parser Integration**
  - Integrate toml_slicer or equivalent Ada TOML library
  - Parse .ctp manifest files into Manifest records
  - Validate against manifest-format.md specification

- [ ] **SHA-256 Implementation**
  - Either pure Ada implementation or libsodium bindings
  - Hash file contents and compare to manifest
  - Prove absence of runtime errors with SPARK

- [ ] **Debian Importer (Basic)**
  - Download .dsc from Debian mirrors
  - Parse .dsc control file format
  - Extract source tarball and debian/ directory
  - Generate skeleton .ctp manifest

- [ ] **Manifest Verification**
  - Verify upstream hash matches downloaded source
  - Validate manifest structure
  - Report verification status clearly

- [ ] **CLI Commands**
  - `cerro import debian:<pkg>/<version>` - Import from Debian
  - `cerro verify <manifest.ctp>` - Verify a manifest
  - `cerro info <manifest.ctp>` - Display package info

### Should Have

- [ ] **Ed25519 Signature Verification**
  - Verify existing signatures on manifests
  - Algorithm-agile format (future ML-DSA support)
  - Don't need signing yet, just verification

- [ ] **OCI Export (Basic)**
  - Create minimal OCI image from built package
  - Single-layer scratch-based image
  - Works with `podman load`

- [ ] **SBOM Generation (Basic)**
  - SPDX 2.3 JSON format
  - Package name, version, files, license
  - Embedded in build output

- [ ] **Provenance Statement**
  - in-toto/SLSA v1.0 format
  - Records manifest hash, source hashes, builder
  - Signed with build key

### Nice to Have

- [ ] **Build Execution**
  - Run autotools configure/make/install in Podman
  - Capture build in staging directory
  - Hash resulting files

---

## v0.2 — "Base Camp"

**Goal:** Support multiple packages with dependencies, add post-quantum signatures

### Features

- [ ] **ML-DSA-65 (Dilithium) Signatures**
  - Add liboqs Ada bindings
  - Hybrid Ed25519 + ML-DSA signing
  - Post-quantum verification support

- [ ] **Dependency Resolution**
  - Parse dependency specifications
  - Build packages in correct order
  - Handle circular dependency detection

- [ ] **Package Signing**
  - Generate Ed25519 and ML-DSA keypairs
  - Sign manifests as builder/maintainer
  - Key management basics

- [ ] **Fedora Importer**
  - Parse RPM .spec files
  - Download from Koji/mirrors
  - Generate .ctp manifests

- [ ] **Reproducibility Checking**
  - Build same package twice
  - Compare results bit-for-bit
  - Report differences

- [ ] **Transparency Log (Local)**
  - Append-only log of operations
  - Signed entries
  - Foundation for federated version

---

## v0.3 — "The Wall"

**Goal:** Production-ready for single operator

### Features

- [ ] **OSTree Export**
  - Export to OSTree repository
  - Static delta generation
  - Compatible with rpm-ostree

- [ ] **SELinux Policy Generation**
  - Generate confined policies for packages
  - CIL format output
  - Basic policy templates

- [ ] **Alpine Importer**
  - Parse APKBUILD files
  - Support aports repository

- [ ] **Enhanced SBOM**
  - CycloneDX format (in addition to SPDX)
  - Vulnerability correlation
  - Dependency tree visualization

- [ ] **Web UI (Read-Only)**
  - Browse packages
  - View provenance chains
  - Verify signatures online

- [ ] **Ed25519 Deprecation Evaluation**
  - Assess quantum threat timeline
  - Plan transition to ML-DSA-only if appropriate

---

## v0.4 — "The Summit"

**Goal:** Federated operation, multiple operators

### Features

- [ ] **Federated Transparency Log**
  - Multiple witnesses
  - Consistency verification
  - Gossip protocol

- [ ] **Multi-Operator Build Verification**
  - Multiple builders produce same output
  - Consensus on build results
  - Detect compromised builders

- [ ] **Mirror Support**
  - Pull from upstream Cerro Torre
  - Serve local mirror
  - Delta synchronisation

- [ ] **Vulnerability Tracking**
  - CVE database integration
  - Affected package identification
  - Advisory generation

---

## Future / Wishlist

- **Nix Importer** - Import from nixpkgs
- **Arch Importer** - Import from PKGBUILD
- **Bootable Images** - Full bootable system images
- **Secure Boot Integration** - Sign boot components
- **Hardware Attestation** - TPM-based attestation
- **Mobile Support** - Android/iOS package verification

---

## Non-Goals (For Now)

- **GUI Application** - CLI-first, web UI for viewing only
- **Windows Support** - Linux containers and immutable Linux only
- **Binary Package Management** - We verify, distributions distribute
- **Real-Time Updates** - Batch verification, not continuous

---

## Success Metrics

### MVP Success
- [ ] Import GNU Hello from Debian
- [ ] Build with provenance tracking
- [ ] Export working OCI image
- [ ] Verify all hashes match
- [ ] Documentation complete enough for others to try

### v0.2 Success
- [ ] 10+ packages building successfully
- [ ] At least one external contributor
- [ ] Reproducibility rate >80%

### v0.3 Success
- [ ] Used in at least one production deployment
- [ ] Security audit of core crypto code
- [ ] SPARK proofs complete for crypto module

### v0.4 Success
- [ ] Multiple independent operators
- [ ] Transparency log with >2 witnesses
- [ ] Governance transition to full cooperative
