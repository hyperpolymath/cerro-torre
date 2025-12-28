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

# Build the project
build:
    alr build

# Run tests
test:
    alr build
    @echo "Unit tests not yet implemented - see docs/MVP-PLAN.md"

# Clean build artifacts
clean:
    alr clean
    rm -rf obj/ lib/

# Format Ada code (uses gnatpp if available)
fmt:
    @if command -v gnatpp &> /dev/null; then \
        find src -name "*.adb" -o -name "*.ads" | xargs gnatpp -i3 -M100; \
    else \
        echo "gnatpp not found - install GNAT tools for formatting"; \
    fi

# Lint/check Ada code
lint:
    alr build -- -gnatwa -gnatwe

# Run SPARK proofs (requires gnatprove)
prove:
    @if command -v gnatprove &> /dev/null; then \
        gnatprove -P cerro_torre.gpr --level=2; \
    else \
        echo "gnatprove not found - requires SPARK Pro or Community"; \
    fi

# Build and run the CLI
run *args:
    alr run -- {{args}}

# Build the ATS2 shadow verifier
shadow-build:
    @if command -v patscc &> /dev/null; then \
        patscc -O2 -DATS_MEMALLOC_LIBC -o ct-shadow tools/ats-shadow/main.dats; \
    else \
        echo "patscc not found - install ATS2 for shadow verifier"; \
    fi

