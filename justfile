# Cerro-Torre - Development Tasks
# SPDX-License-Identifier: Apache-2.0 OR MIT
set shell := ["bash", "-uc"]
set dotenv-load := true

project := "Cerro-Torre"
build_mode := env_var_or_default("CERRO_BUILD_MODE", "Development")

# Toolbox prefix - use toolbox on Fedora Kinoite/Silverblue
tb := if `command -v toolbox >/dev/null 2>&1 && echo yes || echo no` == "yes" { "toolbox run" } else { "" }

# Show all recipes
default:
    @just --list --unsorted

# Build the project (Development mode by default)
build:
    {{tb}} alr build

# Build in Release mode
build-release:
    {{tb}} alr build -- -XCERRO_BUILD_MODE=Release

# Build in Proof mode (for SPARK verification)
build-proof:
    {{tb}} alr build -- -XCERRO_BUILD_MODE=Proof

# Run the CLI
run *args:
    ./bin/cerro_main {{args}}

# Run tests (using AUnit when available)
test:
    @echo "Running Ada tests..."
    {{tb}} alr build
    @if [ -f ./bin/cerro_tests ]; then ./bin/cerro_tests; else echo "No test binary found - see docs/MVP-PLAN.md"; fi

# Clean build artifacts
clean:
    {{tb}} alr clean
    rm -rf obj/ bin/ lib/

# Run SPARK proofs on core modules
prove:
    @echo "Running SPARK proofs..."
    @if {{tb}} command -v gnatprove >/dev/null 2>&1; then \
        {{tb}} gnatprove -P cerro_torre.gpr --mode=check; \
        {{tb}} gnatprove -P cerro_torre.gpr --level=2 --prover=cvc5,z3; \
    else \
        echo "gnatprove not found - requires SPARK Pro or Community"; \
    fi

# Run SPARK proofs with detailed output
prove-verbose:
    {{tb}} gnatprove -P cerro_torre.gpr --level=2 --prover=cvc5,z3,altergo --report=all

# Check SPARK mode annotations only (fast)
prove-check:
    {{tb}} gnatprove -P cerro_torre.gpr --mode=check

# Clean proof artifacts
prove-clean:
    {{tb}} gnatprove -P cerro_torre.gpr --clean

# Format Ada code (using gnatpp if available)
fmt:
    @if {{tb}} command -v gnatpp >/dev/null 2>&1; then \
        find src/ -name "*.adb" -o -name "*.ads" | xargs {{tb}} gnatpp -i3 -M100; \
    else \
        echo "gnatpp not found - install GNAT tools for formatting"; \
    fi

# Lint/check Ada code
lint:
    @echo "Checking Ada code style..."
    {{tb}} alr build -- -gnatwa -gnatwe

# ATS2 shadow verification (advisory only - not part of TCB)
ats-shadow:
    @echo "Running ATS2 shadow checks (advisory)..."
    @if {{tb}} command -v patscc >/dev/null 2>&1; then \
        for f in shadow/*.dats tools/ats-shadow/*.dats; do \
            if [ -f "$$f" ]; then \
                echo "Checking $$f..."; \
                {{tb}} patscc -tcats "$$f" || true; \
            fi \
        done; \
        if [ ! -d shadow ] && [ ! -d tools/ats-shadow ]; then \
            echo "No ATS2 shadow files found."; \
        fi \
    else \
        echo "ATS2 (patscc) not found. Shadow checking skipped."; \
    fi

# Build the ATS2 shadow verifier binary
shadow-build:
    @if {{tb}} command -v patscc >/dev/null 2>&1; then \
        {{tb}} patscc -O2 -DATS_MEMALLOC_LIBC -o ct-shadow tools/ats-shadow/main.dats; \
    else \
        echo "patscc not found - install ATS2 for shadow verifier"; \
    fi

# Install development dependencies
deps:
    @echo "Installing Alire dependencies..."
    {{tb}} alr update
    {{tb}} alr build --fetch-only

# Show project info
info:
    @echo "Project: {{project}}"
    @echo "Build mode: {{build_mode}}"
    {{tb}} alr show

# Generate documentation
docs:
    @echo "Generating API documentation..."
    @if {{tb}} command -v gnatdoc >/dev/null 2>&1; then \
        {{tb}} gnatdoc -P cerro_torre.gpr; \
    else \
        echo "gnatdoc not found."; \
    fi

# Full CI check: build, lint, prove
ci: lint build prove test
    @echo "CI checks complete."

# Create shadow directory for ATS2 files
init-shadow:
    mkdir -p shadow tools/ats-shadow
    @echo "Created shadow/ and tools/ats-shadow/ directories for ATS2 advisory verification files."
