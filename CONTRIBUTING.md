# Contributing to Cerro Torre

Thank you for your interest in contributing to Cerro Torre. This document explains how to participate in the project.

## First: Read the Covenant

Before contributing, please read the [Palimpsest Covenant](governance/covenant.md). It articulates the values that define our community. You don't have to formally adopt it to submit a patch, but understanding it helps you understand us.

## Ways to Contribute

### Code

The core tooling is written in Ada/SPARK. If you're not familiar with Ada, that's okay — it's readable, and we're happy to help you learn. The formal verification aspects (SPARK) are more specialised, but plenty of work doesn't require touching SPARK-annotated code.

Areas that need work:
- **Importers**: Converting Debian/Fedora/Alpine packages to CTP manifests
- **Exporters**: Generating OCI images, OSTree commits
- **CLI**: User interface improvements
- **Documentation**: Always needed

### Packaging

Once the tooling is functional, we'll need people to maintain package manifests. If you're experienced with Debian, Fedora, or Alpine packaging, that knowledge transfers directly.

### Governance

The cooperative structure needs people who understand democratic governance, cooperative law, and community management. This is as important as the code.

### Testing

Try things, break things, report what you find. Good bug reports are valuable contributions.

## Development Setup

### Prerequisites

- GNAT compiler (FSF GNAT or GNAT Community)
- Alire package manager (`alr`)
- GNATprove (for SPARK verification)
- Podman (for container operations)

### Building

```bash
git clone https://gitlab.com/cerro-torre/cerro-torre.git
cd cerro-torre
alr build
```

### Running Tests

```bash
alr run -- --help        # Basic smoke test
alr exec -- gnatprove -P cerro_torre.gpr  # SPARK proofs
```

### Code Style

We follow the GNAT coding standard with these specifics:

- 3-space indentation
- 80-character line limit (soft), 100 (hard)
- All public APIs must have doc comments
- SPARK contracts on security-critical code
- No `pragma Suppress` for proof obligations

## Submitting Changes

### Developer Certificate of Origin

We use the DCO instead of a CLA. Every commit must be signed off:

```bash
git commit --signoff -m "Your commit message"
```

This adds a `Signed-off-by` line certifying that you have the right to submit the code and agree to the project's licenses.

### Merge Requests

1. Fork the repository on GitLab
2. Create a feature branch (`git checkout -b feature/my-contribution`)
3. Make your changes with signed-off commits
4. Push to your fork
5. Open a Merge Request against `main`

### Commit Messages

Use conventional commit format:

```
type(scope): brief description

Longer explanation if needed. Wrap at 72 characters.

Signed-off-by: Your Name <your.email@example.com>
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

Example:
```
feat(importer): add Debian .dsc parser

Implements basic parsing of Debian source control files.
Does not yet handle all fields; see TODO comments.

Signed-off-by: Jane Developer <jane@example.com>
```

### Review Process

1. CI must pass (builds, tests, SPARK proofs)
2. At least one maintainer review
3. Discussion resolved
4. Squash or rebase as appropriate
5. Merge

## Communication

- **GitLab Issues**: Bug reports, feature requests, questions
- **Merge Request discussions**: Code review
- **Mailing list**: (TBD) Governance and strategic discussions

## Becoming a Maintainer

After sustained contribution (typically 3+ months), you may be nominated for Maintainer Membership in the cooperative. This grants:

- Commit access to main repository
- Voting rights on technical decisions
- Responsibility for package/area maintenance

See [governance/articles.md](governance/articles.md) for details.

## Code of Conduct

We don't have a separate code of conduct document. The Palimpsest Covenant serves this purpose. In short:

- Treat everyone with respect
- Argue about ideas, not people
- Assume good faith
- Welcome newcomers

Harassment, discrimination, and abuse are not tolerated.

## Questions?

Open an issue with the `question` label, or reach out to maintainers directly.

---

*Thank you for helping build supply-chain-verified infrastructure for everyone.*
