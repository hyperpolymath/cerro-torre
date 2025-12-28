# Cerro Torre MVP Plan

**Version**: 0.1.0-mvp
**Last Updated**: 2025-12-28
**Status**: Draft

## Executive Summary

The MVP demonstrates Cerro Torre's core thesis: **provenance-verified containers from democratically-governed sources**.

MVP ≠ a whole distro. MVP = one repeatable end-to-end pipeline that:
1. Takes a `.ctp` manifest
2. Produces a runnable OCI container image
3. Emits verifiable provenance attestations

### Success Command

```bash
cerro import debian:hello/2.10-3
cerro build manifests/hello.ctp
cerro export --format=oci hello:2.10-3
podman run cerro-torre/hello:2.10-3
# Output: Hello, world!
```

---

## MVP Success Criteria

| Criterion | Description | Verification |
|-----------|-------------|--------------|
| **E2E Build** | Single command builds from manifest to OCI image | `cerro build manifests/hello.ctp` succeeds |
| **Provenance Bundle** | SBOM + in-toto attestation + signed digests produced | Files exist in `dist/<build-id>/` |
| **Signature Verification** | Output attestations are cryptographically signed | `cerro verify dist/<build-id>/` passes |
| **Runnable Container** | Exported OCI image runs in Podman | `podman run` produces expected output |
| **Reproducibility Hook** | Build records all inputs; same inputs → same output hash | Rebuild produces identical digest |

---

## Implementation Stages

### Stage 0: Contract Definition (1 day)

**Goal**: Pin down the minimum .ctp manifest and output format before writing code.

#### Deliverables

1. **Minimal Manifest Schema** (`spec/mvp-manifest.md`)
   ```toml
   [metadata]
   name = "hello"
   version = "2.10-3"

   [provenance]
   upstream = "debian"
   source = "http://deb.debian.org/debian/pool/main/h/hello/hello_2.10.orig.tar.gz"
   source_hash = "sha256:31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b"
   dsc = "http://deb.debian.org/debian/pool/main/h/hello/hello_2.10-3.dsc"
   dsc_hash = "sha256:..."

   [build]
   system = "autoconf"
   configure_flags = ["--prefix=/usr"]

   [outputs]
   binary = ["hello"]
   ```

2. **Output Directory Structure**
   ```
   dist/<build-id>/
   ├── image.tar              # OCI image tarball
   ├── canonical.ctp          # Canonicalized manifest
   ├── summary.json           # Build summary with digests
   ├── sbom.spdx.json         # SPDX SBOM
   ├── provenance.jsonl       # in-toto attestation (DSSE envelope)
   ├── digests.txt            # SHA256 of all outputs
   └── signatures.json        # Algorithm-agile signatures
   ```

   **signatures.json format** (algorithm-agile from day 1):
   ```json
   {
     "payload_type": "application/vnd.cerro-torre.digests+txt",
     "payload_digest": "sha256:...",
     "signatures": [
       {"algorithm": "ed25519", "keyid": "dev-key-2025", "sig": "base64..."}
     ]
   }
   ```

3. **Error Taxonomy**
   - `MANIFEST_INVALID` - Parse or validation failure
   - `FETCH_FAILED` - Network/download error
   - `HASH_MISMATCH` - Integrity verification failed
   - `BUILD_FAILED` - Compilation/packaging error
   - `ATTESTATION_FAILED` - Provenance generation error

#### Seam Invariants (must hold true)

| Seam | Invariant |
|------|-----------|
| Manifest → Plan | Same manifest + same inputs = same build plan |
| Inputs → Attestations | Every influencing byte is hashed and recorded |
| Outputs → OCI | Image digest in provenance = actual produced digest |
| Keys → Trust | Signatures verifiable with documented trust root |

---

### Stage 1: Core Pipeline (5-7 days)

**Goal**: Implement the fetch → verify → build → pack pipeline for one Debian package.

#### 1.1 TOML Parser Integration (Day 1)

**File**: `src/core/cerro_manifest.adb`

- Integrate `toml_slicer` Alire dependency
- Implement `Parse_String` and `Parse_File` functions
- Map TOML sections to existing Ada record types
- Add validation against manifest schema

**Acceptance**: Parse `manifests/hello.ctp` without errors

#### 1.2 Cryptographic Operations (Day 1-2)

**File**: `src/core/cerro_crypto.adb`

Two options (decide now):

| Option | Pros | Cons |
|--------|------|------|
| **libsodium bindings** | Battle-tested, fast | External C dependency |
| **Pure Ada (SPARKNaCl)** | No C, fully provable | Less mature, more work |

**MVP Recommendation**: Use libsodium bindings for SHA256/Ed25519. SPARK proofs can wrap the calls later.

- Implement `SHA256_Hash` using libsodium
- Implement `SHA256_Verify` (compare computed vs expected)
- Implement `Ed25519_Sign` and `Ed25519_Verify`
- Keep existing constant-time comparison for digest comparison

**Acceptance**: Hash a test string, verify signature round-trip

#### 1.3 Source Fetcher (Day 2-3)

**File**: `src/importers/debian/cerro_debian_fetch.ads/adb` (new)

- HTTP client using AWS (Ada Web Server)
- Download `.dsc` file and parse it
- Download `.orig.tar.gz` and `.debian.tar.xz`
- Verify SHA256 checksums match `.dsc` declarations
- Extract to `work/<package>-<version>/`

**Note**: For MVP, skip Debian signature verification (InRelease chain). Record that it was skipped in attestation.

**Acceptance**: `cerro fetch debian:hello/2.10-3` downloads and verifies hello sources

#### 1.4 Build Executor (Day 3-4)

**File**: `src/build/cerro_builder.adb`

MVP approach: **Wrapper around existing tools**, not replacement.

- Create hermetic build environment (Podman container with Debian base)
- Mount source directory into container
- Execute standard autoconf sequence: `./configure && make && make install DESTDIR=...`
- Capture build log for attestation
- Copy installed files to staging directory

**Build Environment Image**:
```dockerfile
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y build-essential autoconf automake
```

**Acceptance**: Build hello from downloaded sources, produce `/usr/bin/hello`

#### 1.5 OCI Packer (Day 4-5)

**File**: `src/exporters/oci/cerro_oci.ads/adb` (new)

- Create minimal OCI image layout:
  ```
  blobs/sha256/<layer-digest>    # Tarball of rootfs
  blobs/sha256/<config-digest>   # Image config JSON
  blobs/sha256/<manifest-digest> # Image manifest JSON
  index.json                     # OCI image index
  oci-layout                     # {"imageLayoutVersion": "1.0.0"}
  ```
- Generate layer tarball from staging directory
- Create config with entrypoint, environment, labels
- Create manifest linking config + layers
- Write index.json pointing to manifest

**Acceptance**: `podman load < image.tar` works, `podman run` executes hello

---

### Stage 2: Attestations (2-3 days)

**Goal**: Emit provenance artifacts that prove "supply-chain verified."

#### 2.1 SBOM Generation (Day 1)

**File**: `src/attestations/cerro_sbom.ads/adb` (new)

- Generate SPDX 2.3 JSON format
- Include:
  - Package name, version, supplier (Cerro Torre)
  - Files with checksums
  - Upstream source URL
  - License (from manifest or detected)
- Use SPDX-License-Identifier: GPL-3.0-or-later for hello

**Acceptance**: `sbom.spdx.json` validates against SPDX schema

#### 2.2 Provenance Statement (Day 1-2)

**File**: `src/attestations/cerro_provenance_emit.ads/adb` (new)

- Emit in-toto/SLSA v1.0 provenance format
- Include:
  ```json
  {
    "_type": "https://in-toto.io/Statement/v1",
    "subject": [{"name": "hello:2.10-3", "digest": {"sha256": "..."}}],
    "predicateType": "https://slsa.dev/provenance/v1",
    "predicate": {
      "buildDefinition": {
        "buildType": "https://cerro-torre.org/build/v1",
        "externalParameters": {"manifest": "sha256:..."},
        "internalParameters": {},
        "resolvedDependencies": [...]
      },
      "runDetails": {
        "builder": {"id": "cerro-torre/builder:0.1.0"},
        "metadata": {"invocationId": "...", "startedOn": "...", "finishedOn": "..."}
      }
    }
  }
  ```

**Acceptance**: Provenance JSON validates against in-toto schema

#### 2.3 Signing (Day 2)

**File**: `src/attestations/cerro_signer.ads/adb` (new)

- Create `digests.txt` with SHA256 of all outputs
- Sign digests using Ed25519 key from `keys/`
- Output DSSE envelope or detached `.sig` file
- For MVP, use development key (document trust root)

**Acceptance**: `cerro verify dist/<build-id>/` passes signature check

---

### Stage 3: Integration & Testing (2-3 days)

**Goal**: Wire everything together, add tests, document.

#### 3.1 CLI Integration (Day 1)

**File**: `src/cli/cerro_cli.adb`

Implement command handlers:

```
cerro import debian:<pkg>/<version>  # Fetch + parse → generate .ctp
cerro build <manifest.ctp>           # Full pipeline → dist/
cerro verify <build-dir>             # Check signatures + hashes
cerro export --format=oci <image>    # (integrated in build for MVP)
cerro inspect <manifest.ctp>         # Show parsed manifest
```

#### 3.2 Test Suite (Day 1-2)

**Directory**: `tests/`

- **Unit tests** (AUnit):
  - Manifest parsing (valid/invalid inputs)
  - Hash computation
  - Version comparison

- **Integration tests**:
  - End-to-end build of hello.ctp
  - Verify provenance chain
  - Run container and check output

- **Shadow Verifier** (non-authoritative, CI-only):
  - ATS2 tool at `tools/ats-shadow/` provides independent verification
  - Checks: LF-only, no TAB, no trailing whitespace, key ordering
  - Optional: digest coupling (`--check-digest`), idempotence (`--check-idempotence`)
  - Different implementation language = different bugs (defense in depth)

- **CI Pipeline** (`.gitlab-ci.yml`):
  ```yaml
  stages:
    - build
    - test
    - shadow
    - integration

  build:
    script:
      - alr build

  test:
    script:
      - alr run cerro_tests

  shadow-verify:
    stage: shadow
    script:
      - patscc -O2 -DATS_MEMALLOC_LIBC -o ct-shadow tools/ats-shadow/main.dats
      - ./ct-shadow --summary dist/summary.json --check-digest dist/canonical.ctp
    allow_failure: true  # Non-authoritative

  integration:
    script:
      - ./scripts/build-demo.sh
      - ./scripts/verify-demo.sh
  ```

#### 3.3 Documentation (Day 2)

- Update `README.adoc` with quick start
- Add `docs/USAGE.md` with command reference
- Document trust model in `docs/TRUST.md`
- Add `CHANGELOG.md` entry for v0.1.0

---

## What to Defer (Post-MVP)

### SHOULD (v0.2)

| Feature | Reason to Defer |
|---------|-----------------|
| ML-DSA-65 (Dilithium) signatures | Needs liboqs Ada bindings |
| Debian signature chain (InRelease) | Complex PKI, not blocking demo |
| Reproducible builds enforcement | Requires tooling integration |
| Transparency log integration | Needs protocol finalization |
| Fedora/Alpine importers | Debian sufficient for demo |
| SELinux policy generation | Advanced feature |

### COULD (v0.3+)

| Feature | Reason to Defer |
|---------|-----------------|
| SPARK proofs for crypto | Correctness first, proofs later |
| Manifest language verification | Research topic |
| Multi-package builds | Complexity explosion |
| Cooperative governance automation | Organizational, not technical |

---

## Technical Decisions Required

### Decision 1: Signature Algorithm (DECIDED)

**Context**: Cerro Torre attestations are meant to be verifiable for years or decades. Quantum computers could break Ed25519 by 2035, making historical attestations forgeable.

| Factor | Ed25519 (libsodium) | Dilithium/ML-DSA-65 |
|--------|--------------------|--------------------|
| **Quantum resistance** | No (Shor's algorithm) | Yes (NIST FIPS 204) |
| **Signature size** | 64 bytes | ~3,293 bytes |
| **Public key size** | 32 bytes | ~1,952 bytes |
| **Performance** | Faster | ~3-5x slower |
| **Ada bindings** | libsodium-ada exists | Needs liboqs bindings |
| **Standardization** | Mature | NIST standardized 2024 |

**Decision**: Algorithm-agile signatures with phased rollout.

**MVP (v0.1)**: Ed25519 via libsodium, but:
- Design signature format to support multiple algorithms
- Add `algorithm` field to all signature records
- Document that Ed25519 is transitional

**v0.2**: Add ML-DSA-65 (Dilithium) as primary, Ed25519 as fallback (hybrid optional)

**v0.3+**: Evaluate Ed25519 deprecation policy

**Signature Format** (algorithm-agile from day 1):
```json
{
  "signatures": [
    {"algorithm": "ed25519", "keyid": "...", "sig": "..."},
    {"algorithm": "ml-dsa-65", "keyid": "...", "sig": "..."}
  ]
}
```

This ensures attestations signed today can be verified even after quantum computers arrive.

### Decision 2: Crypto Library

**Options**:
1. **libsodium-ada bindings** (Recommended for MVP)
   - Proven implementation
   - Simple API
   - Alire crate available

2. **SPARKNaCl (pure Ada)**
   - No C dependency
   - Fully provable
   - More integration work

3. **liboqs bindings** (Required for v0.2)
   - Post-quantum algorithms (ML-DSA, ML-KEM)
   - C library, needs Ada bindings

**Recommendation**: libsodium for MVP (Ed25519, SHA256). Add liboqs bindings in v0.2 for ML-DSA.

### Decision 3: Build Isolation

**Options**:
1. **Podman container** (Recommended)
   - Strong isolation
   - Reproducible environment
   - Already in toolchain

2. **chroot/bubblewrap**
   - Lighter weight
   - Harder to configure

3. **Native build**
   - Simplest
   - Not hermetic

**Recommendation**: Podman with pinned Debian base image.

### Decision 4: HTTP Client

**Options**:
1. **AWS (Ada Web Server)** (Recommended)
   - Full-featured
   - Already in Alire

2. **curl bindings**
   - Simpler
   - External dependency

**Recommendation**: AWS for consistency with Ada ecosystem.

---

## Implementation Order

```
Week 1:
├── Day 1: Stage 0 (Contract) + TOML parser
├── Day 2: Crypto operations
├── Day 3: Source fetcher
├── Day 4: Build executor
└── Day 5: OCI packer

Week 2:
├── Day 1: SBOM generation
├── Day 2: Provenance statement + signing
├── Day 3: CLI integration
├── Day 4: Test suite
└── Day 5: Documentation + release
```

---

## Files to Create/Modify

### New Files

| Path | Purpose |
|------|---------|
| `src/importers/debian/cerro_debian_fetch.ads` | Debian source fetcher spec |
| `src/importers/debian/cerro_debian_fetch.adb` | Debian source fetcher body |
| `src/exporters/oci/cerro_oci.ads` | OCI packer spec |
| `src/exporters/oci/cerro_oci.adb` | OCI packer body |
| `src/attestations/cerro_sbom.ads` | SBOM generator spec |
| `src/attestations/cerro_sbom.adb` | SBOM generator body |
| `src/attestations/cerro_provenance_emit.ads` | Provenance emitter spec |
| `src/attestations/cerro_provenance_emit.adb` | Provenance emitter body |
| `src/attestations/cerro_signer.ads` | Signing operations spec |
| `src/attestations/cerro_signer.adb` | Signing operations body |
| `manifests/hello.ctp` | GNU Hello manifest (from example) |
| `keys/dev-key.pub` | Development signing key |
| `tests/test_manifest.adb` | Manifest parser tests |
| `tests/test_crypto.adb` | Crypto operation tests |
| `scripts/build-demo.sh` | Demo build script |
| `scripts/verify-demo.sh` | Demo verification script |
| `.gitlab-ci.yml` | CI pipeline |
| `docs/USAGE.md` | Command reference |
| `docs/TRUST.md` | Trust model documentation |
| `tools/ats-shadow/README.adoc` | Shadow verifier documentation |
| `tools/ats-shadow/main.dats` | ATS2 shadow verifier (non-authoritative) |

### Modified Files

| Path | Changes |
|------|---------|
| `src/core/cerro_crypto.adb` | Implement SHA256, Ed25519 with libsodium |
| `src/core/cerro_manifest.adb` | Implement TOML parsing |
| `src/build/cerro_builder.adb` | Implement build orchestration |
| `src/cli/cerro_cli.adb` | Implement command handlers |
| `alire.toml` | Add aws, libsodium dependencies |
| `README.adoc` | Add quick start |

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| libsodium binding issues | Fall back to shelling out to `sha256sum`, `signify` |
| Podman unavailable in CI | Use container-in-container or remote builder |
| Debian package complexity | Start with hello, it's intentionally simple |
| TOML parser edge cases | Use minimal subset, validate strictly |
| Time overrun | Cut SBOM/provenance detail before cutting E2E |

---

## Definition of Done

MVP is complete when:

- [ ] `alr build` produces `cerro` binary
- [ ] `cerro build manifests/hello.ctp` produces OCI image + attestations
- [ ] `podman run` on produced image outputs "Hello, world!"
- [ ] `cerro verify dist/<build-id>/` passes
- [ ] CI pipeline passes all tests
- [ ] README documents quick start
- [ ] CHANGELOG has v0.1.0 entry

---

## Appendix: hello.ctp Reference Manifest

```toml
# Cerro Torre Package Manifest
# Format: CTP 0.1.0

[metadata]
name = "hello"
version = "2.10-3"
summary = "GNU Hello - example package"
description = "Classic GNU Hello program, used to demonstrate the Cerro Torre build system."
license = "GPL-3.0-or-later"
homepage = "https://www.gnu.org/software/hello/"
maintainer = "Cerro Torre Collective <maintainers@cerro-torre.org>"

[provenance]
upstream = "debian"
upstream_version = "2.10"
source_url = "http://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz"
source_hash = "sha256:31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b"
import_date = "2025-01-15"

[provenance.debian]
dsc_url = "http://deb.debian.org/debian/pool/main/h/hello/hello_2.10-3.dsc"
dsc_hash = "sha256:TO_BE_FILLED"
orig_url = "http://deb.debian.org/debian/pool/main/h/hello/hello_2.10.orig.tar.gz"
orig_hash = "sha256:31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b"
debian_url = "http://deb.debian.org/debian/pool/main/h/hello/hello_2.10-3.debian.tar.xz"
debian_hash = "sha256:TO_BE_FILLED"

[build]
system = "autoconf"
configure_flags = ["--prefix=/usr"]
build_dependencies = ["build-essential", "autoconf", "automake", "texinfo"]

[build.phases]
configure = { run = "./configure --prefix=/usr" }
build = { run = "make" }
install = { run = "make install DESTDIR=${DESTDIR}" }

[outputs]
binaries = ["/usr/bin/hello"]
documentation = ["/usr/share/doc/hello/", "/usr/share/info/hello.info", "/usr/share/man/man1/hello.1"]

[attestations]
sbom = true
slsa_provenance = true
sign = true
```
