# Cerro Torre Package Manifest Format Specification

**Version:** 0.1.0 (Draft)

This document specifies the `.ctp` (Cerro Torre Package) manifest format used for describing packages with complete cryptographic provenance.

## Overview

A CTP manifest is a structured document that describes:

1. **Package identity** — name, version, and description
2. **Provenance chain** — complete lineage from upstream source
3. **Build requirements** — dependencies, build inputs, and environment
4. **Content hashes** — cryptographic digests of all package contents
5. **Signatures** — attestations from builders and maintainers

## File Format

CTP manifests use a strict subset of TOML 1.0 for human readability and unambiguous parsing.

### Why TOML?

- Human-readable and writable
- Unambiguous syntax (unlike YAML)
- Strong typing support
- Widespread tooling support
- Easy to parse in Ada

## Manifest Structure

```toml
# Required: Manifest format version
manifest-version = "0.1.0"

[package]
name = "hello"
version = "2.10-3"
epoch = 0
summary = "Famous friendly greeting program"
description = """
The GNU Hello program produces a familiar, friendly greeting.
It allows non-programmers to use a classic computer science tool.
"""

[provenance]
# Original upstream source
upstream-url = "https://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz"
upstream-hash = { algorithm = "sha256", digest = "31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b" }

# Optional: Upstream cryptographic signature
upstream-signature = "https://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz.sig"
upstream-signer = "gnu-keyring:0x7F2D434B9741E8AC"

# Import source (which distro we based our packaging on)
imported-from = "debian:hello/2.10-3"
import-date = 2024-01-15T14:30:00Z

# Patches applied (in order)
[[provenance.patches]]
name = "01-fix-info-dir.patch"
hash = { algorithm = "sha256", digest = "abc123..." }
origin = "debian"
description = "Fix installation of info directory"

[[provenance.patches]]
name = "02-cerro-hardening.patch"
hash = { algorithm = "sha256", digest = "def456..." }
origin = "cerro-torre"
description = "Enable additional hardening flags"

[build]
# Build system used
system = "autotools"

# Build dependencies (Cerro package references)
dependencies = [
    "gcc >= 13.0",
    "make >= 4.0",
    "gettext >= 0.21",
]

# Build environment requirements
[build.environment]
CFLAGS = "-O2 -fstack-protector-strong -D_FORTIFY_SOURCE=2"
LDFLAGS = "-Wl,-z,relro -Wl,-z,now"

# Build commands (recorded, not prescribed)
[build.commands]
configure = "./configure --prefix=/usr"
build = "make -j$(nproc)"
install = "make DESTDIR=$DESTDIR install"

[runtime]
# Runtime dependencies
dependencies = [
    "glibc >= 2.38",
]

# Files installed by this package
[[runtime.files]]
path = "/usr/bin/hello"
hash = { algorithm = "sha256", digest = "..." }
mode = "0755"
type = "executable"

[[runtime.files]]
path = "/usr/share/man/man1/hello.1.gz"
hash = { algorithm = "sha256", digest = "..." }
mode = "0644"
type = "documentation"

[[runtime.files]]
path = "/usr/share/info/hello.info.gz"
hash = { algorithm = "sha256", digest = "..." }
mode = "0644"
type = "documentation"

[policy]
# SELinux policy module (if any)
selinux-module = "hello_cerro.cil"
selinux-hash = { algorithm = "sha256", digest = "..." }

# AppArmor profile (if any)
apparmor-profile = "usr.bin.hello"
apparmor-hash = { algorithm = "sha256", digest = "..." }

[signatures]
# Builder signature (attests to reproducible build)
[[signatures.attestations]]
type = "build"
signer = "cerro-builder-01"
public-key = "ed25519:abc123..."
signature = "base64-encoded-signature..."
timestamp = 2024-01-16T10:00:00Z

# Maintainer signature (attests to packaging correctness)
[[signatures.attestations]]
type = "maintainer"
signer = "jane.doe@cerro-torre.org"
public-key = "ed25519:def456..."
signature = "base64-encoded-signature..."
timestamp = 2024-01-16T11:00:00Z
```

## Field Specifications

### Package Identity

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Package name, lowercase alphanumeric with hyphens |
| `version` | string | yes | Upstream version plus packaging revision |
| `epoch` | integer | no | Version epoch for upgrade ordering (default: 0) |
| `summary` | string | yes | One-line package summary |
| `description` | string | yes | Full package description |

### Package Name Validation

Package names must:
- Start with a lowercase letter
- Contain only: `a-z`, `0-9`, `-`, `+`, `.`
- Be at least 2 characters long
- Not exceed 128 characters
- Not start or end with `-`, `+`, or `.`

Regex: `^[a-z][a-z0-9+.-]{1,127}$` (with additional constraints)

### Version Format

```
[epoch:]upstream_version[-cerro_revision]
```

- **epoch**: Optional integer prefix for version ordering
- **upstream_version**: Version from upstream project
- **cerro_revision**: Cerro-specific packaging revision

Examples:
- `2.10-3` — upstream 2.10, Cerro revision 3
- `1:2.0-1` — epoch 1, upstream 2.0, revision 1

### Hash Values

```toml
hash = { algorithm = "sha256", digest = "hex-encoded-hash" }
```

Supported algorithms:
- `sha256` — SHA-256 (256 bits, 64 hex chars)
- `sha384` — SHA-384 (384 bits, 96 hex chars)
- `sha512` — SHA-512 (512 bits, 128 hex chars)
- `blake3` — BLAKE3 (256 bits, 64 hex chars)

SHA-256 is the default and minimum requirement. BLAKE3 is recommended for performance where supported.

### Provenance Chain

The provenance section establishes complete lineage:

1. **upstream-url**: Where the original source came from
2. **upstream-hash**: Cryptographic hash of the unmodified source
3. **upstream-signature**: Optional PGP/GPG signature URL
4. **imported-from**: Which distribution's packaging we based on
5. **patches**: Ordered list of all modifications

Every modification must be documented and hashed.

### Signatures

All signatures use Ed25519. Each attestation includes:

| Field | Description |
|-------|-------------|
| `type` | Attestation type (build, maintainer, audit, etc.) |
| `signer` | Identifier for the signing entity |
| `public-key` | Ed25519 public key in format `ed25519:base64...` |
| `signature` | Base64-encoded signature of the manifest hash |
| `timestamp` | ISO 8601 timestamp of signing |

The signature is computed over the SHA-256 hash of the manifest with the `[signatures]` section removed.

## File Types

The `type` field in file entries categorizes contents:

| Type | Description |
|------|-------------|
| `executable` | Binary or script with execute permission |
| `library` | Shared library (.so) |
| `header` | C/C++ header file |
| `configuration` | Default configuration file |
| `documentation` | Man pages, info files, docs |
| `data` | Static data files |
| `license` | License text file |

## Validation Rules

A valid manifest MUST:

1. Have `manifest-version` matching a supported version
2. Have all required fields present and non-empty
3. Have valid package name per the naming rules
4. Have at least one hash using SHA-256 or stronger
5. Have `upstream-hash` verified against `upstream-url` content
6. Have all patch hashes verified
7. Have at least one valid signature from a trusted key

## Example: Minimal Manifest

```toml
manifest-version = "0.1.0"

[package]
name = "hello"
version = "2.10-1"
summary = "Famous friendly greeting program"
description = "The GNU Hello program produces a familiar, friendly greeting."

[provenance]
upstream-url = "https://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz"
upstream-hash = { algorithm = "sha256", digest = "31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b" }
imported-from = "debian:hello/2.10-3"
import-date = 2024-01-15T14:30:00Z

[build]
system = "autotools"
dependencies = ["gcc", "make"]

[runtime]
dependencies = ["glibc"]

[[runtime.files]]
path = "/usr/bin/hello"
hash = { algorithm = "sha256", digest = "..." }
mode = "0755"
type = "executable"
```

## Future Extensions

The following features are planned for future manifest versions:

- **Multi-architecture support**: Architecture-specific file sections
- **Reproducibility attestations**: Bit-for-bit reproducible build proofs
- **SBOM integration**: Software Bill of Materials embedding
- **Transparency log references**: Merkle tree inclusion proofs
- **Delegation signatures**: Hierarchical trust chains

## Changelog

### Version 0.1.0 (Draft)
- Initial specification
- Core provenance model
- Basic signature support
