# Security Policy

## Current Security Posture (2026-01-28)

**Svalinn-Project Compliance**: 40%

Cerro Torre is under active security development. This document describes our current security status, what's implemented, what's stubbed, and what's missing.

### Implemented Features ✅

| Component | Status | Standard | Notes |
|-----------|--------|----------|-------|
| **Ed25519 Signatures** | ✅ Production | RFC 8032 | Via OpenSSL 3.x |
| **SHA-256** | ✅ Production | FIPS 180-4 | SPARK verified |
| **SHA-512** | ✅ Production | FIPS 180-4 | SPARK verified |
| **BLAKE3** | ✅ Production | BLAKE3 spec | 256-bit output |
| **SPARK Verification** | ✅ Production | SPARK 2014 | Core crypto proofs |
| **TLS 1.3** | ✅ Production | RFC 8446 | Via libcurl |
| **HTTP/2** | ✅ Production | RFC 9113 | Registry operations |

### Stubbed Features ⚠️

| Component | Status | Timeline | Notes |
|-----------|--------|----------|-------|
| **ML-DSA-87** | ⚠️ API Only | Q3 2026 | liboqs bindings present, not compiled |
| **Merkle Proofs** | ⚠️ Simplified | Q2 2026 | Works but not RFC 6962 compliant |

**IMPORTANT**: Stubbed features have APIs defined but return `NOT_IMPLEMENTED` or use simplified logic. Do NOT rely on these for security-critical operations yet.

### Missing Features ❌

| Component | Priority | Timeline | Impact |
|-----------|----------|----------|--------|
| **Ed448** | MEDIUM | Q3 2026 | Weaker signatures (128-bit vs 224-bit) |
| **SPHINCS+** | MEDIUM | Q3 2026 | No conservative PQ fallback |
| **BLAKE3-512** | MEDIUM | Q4 2026 | Weaker database indexes |
| **SHAKE3-512** | LOW | TBD | Wait for FIPS 202 update |
| **HTTP/3** | HIGH | Q1 2027 | Slower TLS handshakes |
| **PQ-TLS** | CRITICAL | Q1 2027+ | Vulnerable to quantum computers |
| **IPv6-only** | LOW | Q1 2027 | Dual-stack increases attack surface |

**See Also**: `docs/MISSING-SECURITY-COMPONENTS.adoc` for comprehensive security audit

---

## Supported Versions

| Version | Supported | Notes |
|---------|-----------|-------|
| `main` branch | ✅ Yes | Latest development (pre-alpha) |
| v0.1-alpha | ✅ Yes | First release (planned Q2 2026) |
| Earlier versions | ❌ No | No releases yet |

---

## Reporting a Vulnerability

### Preferred Method: GitHub Security Advisories

1. Navigate to [Report a Vulnerability](https://github.com/hyperpolymath/cerro-torre/security/advisories/new)
2. Click **"Report a vulnerability"**
3. Complete the form with as much detail as possible
4. Submit — we'll receive a private notification

### Alternative: Encrypted Email

| | |
|---|---|
| **Email** | jonathan.jewell@open.ac.uk |
| **Subject Line** | `[SECURITY] Cerro Torre: <brief description>` |

> **⚠️ Important:** Do not report security vulnerabilities through public GitHub issues, pull requests, or discussions.

---

## What to Include

A good vulnerability report helps us understand and reproduce the issue quickly.

### Required Information

- **Description**: Clear explanation of the vulnerability
- **Impact**: What an attacker could achieve (confidentiality, integrity, availability)
- **Affected versions**: Which versions/commits are affected (check `main` branch)
- **Reproduction steps**: Detailed steps to reproduce the issue

### Helpful Additional Information

- **Proof of concept**: Code, scripts, or screenshots demonstrating the vulnerability
- **Attack scenario**: Realistic attack scenario showing exploitability
- **CVSS score**: Your assessment of severity (use [CVSS 3.1 Calculator](https://www.first.org/cvss/calculator/3.1))
- **CWE ID**: Common Weakness Enumeration identifier if known
- **Suggested fix**: If you have ideas for remediation

---

## Response Timeline

We commit to the following response times:

| Stage | Timeframe | Description |
|-------|-----------|-------------|
| **Initial Response** | 48 hours | We acknowledge receipt and confirm we're investigating |
| **Triage** | 7 days | We assess severity, confirm the vulnerability, and estimate timeline |
| **Status Update** | Every 7 days | Regular updates on remediation progress |
| **Resolution** | 90 days | Target for fix development and release (complex issues may take longer) |
| **Disclosure** | 90 days | Public disclosure after fix is available (coordinated with you) |

---

## Security Considerations for Cerro Torre

As a cryptographic container packaging tool, Cerro Torre has specific security considerations:

### Cryptographic Operations
- All cryptographic code is in Ada/SPARK with formal proofs (CT_Crypto module)
- Post-quantum signatures (ML-DSA-87) are in development (Phase 1, Q3 2026)
- Signature verification failures cause hard stops (no permissive mode)
- No hardcoded keys or secrets (enforced by Mustfile)

### Formal Verification
- Core crypto operations have SPARK proofs (SHA-256, SHA-512, Ed25519 verification)
- Proofs are machine-checked, not just reviewed
- `cerro_crypto.adb` is formally verified for absence of runtime errors
- Proof level: Flow analysis + absence of runtime errors (AoRTE)

### Container Signing
- Default signature suite: CT-SIG-01 (Ed25519 only)
- Post-quantum suite (Phase 1): CT-SIG-02 (Ed25519 + ML-DSA-87 hybrid)
- Signature format is algorithm-agile (supports future algorithms)
- Trust store prevents unauthorized signer keys

### Supply Chain
- Alire dependency manager with hash-pinned dependencies
- SLSA Level 3 compliance target (when releases begin)
- All releases will have Sigstore attestations
- Reproducible builds from Debian/Fedora sources

### Registry Operations
- TLS 1.3 enforced for registry communication
- DANE/TLSA certificate verification (opportunistic)
- OAuth2 Bearer token authentication
- HTTP/2 currently (HTTP/3 in Phase 3)

### Transparency
- Rekor transparency log integration (API client ready, submission incomplete)
- Merkle inclusion proofs (simplified implementation, RFC 6962 compliance in progress)
- Signed Entry Timestamps (SET) verification planned

---

## Known Security Limitations (Pre-Alpha)

### CRITICAL Limitations

1. **No Post-Quantum Signatures Yet**
   - ML-DSA-87 API defined but not implemented
   - Vulnerable to "harvest now, decrypt later" attacks
   - **Mitigation**: Do not use for long-term archival (20+ years) until Phase 1 complete

2. **No Post-Quantum TLS**
   - Uses classical TLS 1.3 (RSA/ECDHE key exchange)
   - Session keys vulnerable to future quantum computers
   - **Mitigation**: Await curl/OpenSSL PQ-TLS support (Phase 3)

### HIGH Limitations

1. **Simplified Merkle Proofs**
   - Current implementation doesn't fully follow RFC 6962
   - Doesn't distinguish left/right siblings properly
   - **Mitigation**: Verification still catches tampering, but not cryptographically sound
   - **Fix**: Q2 2026 (before v0.1 release)

2. **No Ed448 Support**
   - Only Ed25519 (128-bit security) available
   - Svalinn-project spec requires Ed448 (224-bit security)
   - **Mitigation**: Ed25519 is still quantum-resistant until large-scale quantum computers exist
   - **Fix**: Phase 1 (Q3 2026)

### MEDIUM Limitations

1. **HTTP/2 Only (No HTTP/3)**
   - Slower TLS handshakes (1-RTT vs 0-RTT)
   - Head-of-line blocking on lossy networks
   - **Fix**: Phase 3 (Q1 2027)

2. **IPv4 + IPv6 Dual-Stack**
   - Cannot disable IPv4 (increases attack surface)
   - Svalinn-project requires IPv6-only mode
   - **Fix**: Phase 3 (Q1 2027)

---

## Security Best Practices for Users

When using Cerro Torre (once v0.1 releases), we recommend:

### For Bundle Creation
- Use the strongest available signature suite (CT-SIG-02 when Phase 1 completes)
- Store private keys in hardware security modules (HSM) or encrypted keyrings
- Enable Argon2id key encryption for local keys
- Use threshold signing for production releases (requires k-of-n signers)
- Submit signatures to transparency logs (Rekor integration when complete)

### For Bundle Verification
- Always verify bundles before deployment (`ct verify --policy strict.json`)
- Use a curated trust store (don't trust all keys)
- Enable policy enforcement (required signers, allowed registries)
- Check transparency log inclusion proofs when available
- Monitor for key revocations

### For Production Deployment
- Keep Cerro Torre up to date (security fixes backported)
- Subscribe to security notifications (Watch → Security alerts on GitHub)
- Use Svalinn gateway for additional runtime security
- Enable SELinux or AppArmor confinement
- Log all verification failures to SIEM

---

## Disclosure Policy

We follow **coordinated disclosure** (responsible disclosure):

1. **You report** the vulnerability privately (GitHub Security Advisories or email)
2. **We acknowledge** and begin investigation (48 hours)
3. **We develop** a fix and prepare a release
4. **We coordinate** disclosure timing with you
5. **We publish** security advisory and fix simultaneously
6. **You may publish** your research after disclosure

### Our Commitments

- We will not take legal action against researchers who follow this policy
- We will work with you to understand and resolve the issue
- We will credit you in the security advisory (unless you prefer anonymity)
- We will notify you before public disclosure

### Your Commitments

- Report vulnerabilities promptly after discovery
- Give us reasonable time to address the issue before disclosure (90 days)
- Do not access, modify, or delete data beyond what's necessary to demonstrate the vulnerability
- Do not share vulnerability details with others until coordinated disclosure

---

## Security Contact

| Purpose | Contact |
|---------|---------|
| **Security issues** | [Report via GitHub](https://github.com/hyperpolymath/cerro-torre/security/advisories/new) or jonathan.jewell@open.ac.uk |
| **Security questions** | [GitHub Discussions - Security Category](https://github.com/hyperpolymath/cerro-torre/discussions/categories/security) |
| **General questions** | See [README.adoc](README.adoc) for contact information |

---

## Related Documentation

- [MISSING-SECURITY-COMPONENTS.adoc](docs/MISSING-SECURITY-COMPONENTS.adoc) - Comprehensive security audit
- [SECURITY-STANDARDS-COMPARISON.adoc](docs/SECURITY-STANDARDS-COMPARISON.adoc) - Svalinn-project compliance status
- [SECURITY-STANDARDS-BY-COMPONENT.adoc](docs/SECURITY-STANDARDS-BY-COMPONENT.adoc) - Per-component security details
- [ROADMAP.adoc](docs/ROADMAP.adoc) - Security implementation timeline
- [Palimpsest Covenant](governance/covenant.md) - Values and ethics

---

*Thank you for helping keep Cerro Torre and its users safe.* 🛡️

---

<sub>Last updated: 2026-01-28 · Policy version: 1.0.0 · Pre-Alpha Status</sub>
