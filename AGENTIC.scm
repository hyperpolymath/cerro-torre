;; SPDX-License-Identifier: MPL-2.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for Cerro Torre

(define agentic-config
  `((version . "1.0.0")
    (schema-version . "1.0")

    (claude-code
      ((model . "claude-opus-4-5-20251101")
       (tools . ("read" "edit" "bash" "grep" "glob" "write"))
       (permissions . "read-all")
       (context-files . ("STATE.scm" "META.scm" "ECOSYSTEM.scm" "AGENTIC.scm"
                         "NEUROSYM.scm" "PLAYBOOK.scm" "README.adoc"
                         "docs/MVP-PLAN.md" "docs/ARCHITECTURE.md"))))

    (patterns
      ((code-review . ((style . "thorough") (focus . ("memory-safety" "SPARK-annotations" "crypto-correctness"))))
       (refactoring . ((style . "conservative") (notes . "SPARK code changes require proof re-verification")))
       (testing . ((style . "comprehensive") (types . ("unit" "property-based" "integration"))))
       (documentation . ((style . "detailed") (format . "asciidoc")))))

    (constraints
      ((allowed-languages
         ((ada . "Core implementation, CLI, all security-critical code")
          (spark . "Formal verification annotations for core modules")
          (nickel . "Build configuration")
          (bash . "Build scripts, automation")
          (toml . "Manifest files, configuration")))
       (banned-languages
         ((typescript . "Use Ada instead")
          (go . "Use Ada or Rust instead")
          (python . "Only for SaltStack, not allowed here")
          (javascript . "Not needed for this project")
          (makefile . "Use Alire/GPRbuild instead")))))

    (task-guidance
      ((crypto-implementation
         ((priority . "high")
          (notes . "Crypto operations MUST use libsodium bindings. Never implement crypto from scratch.")
          (patterns . ("constant-time-comparison" "zeroing-secrets" "no-timing-leaks"))))
       (manifest-handling
         ((priority . "high")
          (notes . "Manifests are Turing-incomplete TOML. Parser must reject computation attempts.")
          (patterns . ("deterministic-ordering" "canonical-representation" "strict-validation"))))
       (cli-development
         ((priority . "medium")
          (notes . "CLI binary is ct. Error messages must be specific and actionable.")
          (patterns . ("specific-errors" "actionable-messages" "json-output-mode"))))
       (governance
         ((priority . "low")
          (notes . "Governance documents are in governance/. Do not modify without explicit request.")))))

    (file-organization
      ((source-layout
         ((src/core . "SPARK-verified modules (crypto, manifest, provenance, verify)")
          (src/cli . "CLI command implementations")
          (src/importers . "Debian, Fedora, Alpine importers")
          (src/exporters . "OCI, OSTree exporters")
          (src/build . "Build orchestration")
          (src/policy . "SELinux policy generation")))
       (scm-files
         ((STATE.scm . "Current project state, updated each session")
          (META.scm . "Architecture decisions, development practices")
          (ECOSYSTEM.scm . "Position in ecosystem, related projects")
          (AGENTIC.scm . "AI agent configuration (this file)")
          (NEUROSYM.scm . "Neurosymbolic integration config")
          (PLAYBOOK.scm . "Operational runbook")))))

    (session-protocol
      ((start . ("Read STATE.scm" "Check blockers" "Review critical-next-actions"))
       (during . ("Update STATE.scm as tasks complete" "Document decisions in META.scm if architectural"))
       (end . ("Ensure STATE.scm reflects completed work" "Update session-history" "Note new blockers"))))

    (safety
      ((crypto . "Never implement crypto primitives - always use libsodium")
       (secrets . "Never log or expose private keys, tokens, or credentials")
       (governance . "Do not modify governance documents without explicit request")
       (spark-proofs . "Do not remove SPARK annotations without justification")))))
