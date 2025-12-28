# Cerro Torre Package Manifest — Canonical Example
# ct-minbase: Minimal base image for supply-chain verified containers
#
# This is the canonical reference .ctp manifest demonstrating:
# - Turing-incomplete declarative format (no expressions/macros)
# - Deterministic structure (stable key ordering for hashing)
# - Complete provenance (explicit hashes for all inputs)
# - Democratic governance alignment (Debian upstream with snapshot pinning)

ctp_version = "1.0"

[metadata]
name = "ct-minbase"
version = "0.1.0"
revision = 1
kind = "container_image"
summary = "Minimal Cerro Torre base image"
description = """
Minimal base image assembled from democratically-governed upstream (Debian)
with pinned sources and verifiable inputs. Includes only bash, coreutils,
and ca-certificates for a functional but minimal container environment.
"""
license = "MIT OR AGPL-3.0-or-later"
homepage = "https://github.com/hyperpolymath/cerro-torre"
maintainer = "cerro-torre:core-team"

[upstream]
family = "debian"
suite = "bookworm"
snapshot_service = "snapshot.debian.org"
snapshot_timestamp = 2025-12-20T00:00:00Z

[provenance]
# No single upstream URL for multi-source images
# Provenance tracked per-source in [[inputs.sources]]
import_date = 2025-12-28T00:00:00Z

# ============================================================================
# SECURITY — Crypto suite commitment (no downgrade)
# ============================================================================

[security]
suite_id = "CT-SIG-01"
payload_binding = "manifest.canonical_bytes_sha256"

# Optional: algorithms block (must match registry if provided)
# If omitted, resolved from spec/crypto-suites.json during summarization
[security.algorithms]
[security.algorithms.hash]
id = "sha256"
output_bits = 256

[[security.algorithms.signatures]]
id = "ed25519"
required = true

# ============================================================================
# INPUTS — All sources with cryptographic hashes
# ============================================================================

[[inputs.sources]]
id = "debian_source_bash"
type = "debian_dsc"
name = "bash"
version = "5.2.15-2+b7"

[[inputs.sources.artifacts]]
filename = "bash_5.2.15-2+b7.dsc"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/b/bash/bash_5.2.15-2+b7.dsc"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources.artifacts]]
filename = "bash_5.2.15.orig.tar.xz"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/b/bash/bash_5.2.15.orig.tar.xz"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources.artifacts]]
filename = "bash_5.2.15-2.debian.tar.xz"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/b/bash/bash_5.2.15-2.debian.tar.xz"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources]]
id = "debian_source_coreutils"
type = "debian_dsc"
name = "coreutils"
version = "9.1-1"

[[inputs.sources.artifacts]]
filename = "coreutils_9.1-1.dsc"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/c/coreutils/coreutils_9.1-1.dsc"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources.artifacts]]
filename = "coreutils_9.1.orig.tar.xz"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/c/coreutils/coreutils_9.1.orig.tar.xz"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources.artifacts]]
filename = "coreutils_9.1-1.debian.tar.xz"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/c/coreutils/coreutils_9.1-1.debian.tar.xz"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources]]
id = "debian_source_ca_certificates"
type = "debian_dsc"
name = "ca-certificates"
version = "20230311"

[[inputs.sources.artifacts]]
filename = "ca-certificates_20230311.dsc"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/c/ca-certificates/ca-certificates_20230311.dsc"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

[[inputs.sources.artifacts]]
filename = "ca-certificates_20230311.tar.xz"
uri = "https://snapshot.debian.org/archive/debian/20251220T000000Z/pool/main/c/ca-certificates/ca-certificates_20230311.tar.xz"
sha256 = "TO_BE_FILLED_WITH_REAL_HASH"

# ============================================================================
# BUILD — Declarative build plan (no shell, fixed vocabulary)
# ============================================================================

[build]
system = "cerro_image"

[build.environment]
arch = "amd64"
os = "linux"
reproducible = true

[[build.plan]]
step = "import"
using = "debian"
sources = ["debian_source_bash", "debian_source_coreutils", "debian_source_ca_certificates"]

[[build.plan]]
step = "build_debian_source"
source = "debian_source_bash"
profile = "debian_rules"

[[build.plan]]
step = "build_debian_source"
source = "debian_source_coreutils"
profile = "debian_rules"

[[build.plan]]
step = "build_debian_source"
source = "debian_source_ca_certificates"
profile = "debian_rules"

[[build.plan]]
step = "assemble_rootfs"
strip_docs = true
strip_locales = true

[[build.plan.include_packages]]
name = "bash"
from_source = "debian_source_bash"

[[build.plan.include_packages]]
name = "coreutils"
from_source = "debian_source_coreutils"

[[build.plan.include_packages]]
name = "ca-certificates"
from_source = "debian_source_ca_certificates"

[[build.plan]]
step = "emit_oci_image"

[build.plan.image]
entrypoint = ["/bin/bash"]
cmd = ["-lc", "echo 'hello from Cerro Torre'"]

[build.plan.image.labels]
"org.opencontainers.image.title" = "ct-minbase"
"org.opencontainers.image.source" = "https://github.com/hyperpolymath/cerro-torre"
"org.opencontainers.image.description" = "Minimal Cerro Torre base image"

# ============================================================================
# OUTPUTS — What the build produces
# ============================================================================

[outputs]
primary = "ct-minbase"

[[outputs.artifacts]]
type = "oci_image"
name = "ct-minbase"
tag = "0.1.0"

[[outputs.artifacts]]
type = "sbom_spdx_json"
name = "ct-minbase.sbom.spdx.json"

[[outputs.artifacts]]
type = "in_toto_provenance"
name = "ct-minbase.provenance.jsonl"

# ============================================================================
# POLICY — Provenance and attestation requirements
# ============================================================================

[policy.provenance]
require_source_hashes = true
require_reproducible_build = true

[policy.attestations]
emit = ["in_toto", "sbom_spdx_json"]

# ============================================================================
# ATTESTATIONS — What we require/recommend for this package
# ============================================================================

[attestations]
require = ["source-signature", "reproducible-build"]
recommend = ["sbom-complete"]
