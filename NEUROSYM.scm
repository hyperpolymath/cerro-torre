;; SPDX-License-Identifier: MPL-2.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration configuration for Cerro Torre

(define neurosym-config
  `((version . "1.0.0")
    (schema-version . "1.0")

    (symbolic-layer
      ((type . "scheme")
       (reasoning . "deductive")
       (verification . "formal")
       (notes . "Cerro Torre emphasizes symbolic/formal verification over neural approaches.
                 SPARK proofs provide machine-checked guarantees.")

       (formal-verification
         ((tool . "gnatprove")
          (language . "SPARK")
          (target-modules . ("cerro_crypto" "cerro_manifest" "cerro_provenance" "cerro_verify"))
          (proof-level . "silver")
          (guarantees . ("no-buffer-overflow" "no-integer-overflow" "no-uninitialized-memory"
                         "functional-correctness-crypto"))))

       (symbolic-structures
         ((scm-files . "Scheme-based metadata files for machine-readable project state")
          (toml-manifests . "Turing-incomplete declarative package manifests")
          (trust-policies . "Symbolic rules for key trust and verification")
          (crypto-suites . "Formal specification of cryptographic algorithm sets")))))

    (neural-layer
      ((embeddings . #f)
       (fine-tuning . #f)
       (inference . #f)
       (notes . "Neural components are not used in Cerro Torre core. The emphasis is on
                 symbolic/formal methods for security guarantees."))

      (potential-applications
        ((vulnerability-detection . "Neural analysis of dependency graphs for anomalies")
         (policy-learning . "Learning trust policies from historical decisions")
         (documentation . "AI-assisted documentation generation")
         (code-review . "AI-assisted code review for Ada/SPARK patterns"))
        (status . "not-implemented")))

    (integration
      ((current-integration . "minimal")
       (notes . "AI agents read symbolic SCM files and documentation to understand project.
                 Agents produce symbolic outputs (code, specs) not neural artifacts.")

       (agent-symbolic-interaction
         ((reads . ("STATE.scm" "META.scm" "ECOSYSTEM.scm" "spec/*.json" "*.toml"))
          (writes . ("STATE.scm session updates" "Ada source code" "Documentation"))
          (reasoning-mode . "symbolic-only")))

       (ai-code-verification
         ((method . "SPARK-proofs")
          (notes . "Code produced by AI agents is verified using SPARK prover.
                    Neural generation does not bypass formal verification requirements.")
          (workflow . ("AI generates Ada/SPARK code"
                       "gnatprove checks SPARK annotations"
                       "Only proven code is accepted"))))))

    (ai-trust-model
      ((trust-level . "assistant")
       (notes . "AI agents are assistants, not authorities. All AI-produced code requires
                 formal verification before acceptance.")

       (allowed-ai-actions
         ("Code generation with SPARK annotations"
          "Documentation updates"
          "STATE.scm session updates"
          "Test generation"
          "Spec drafting"))

       (requires-human-approval
         ("Governance document changes"
          "Crypto implementation decisions"
          "Release signing"
          "Key management operations"
          "Architectural decisions"))

       (prohibited-ai-actions
         ("Signing releases or attestations"
          "Modifying trust policies without review"
          "Bypassing SPARK proof requirements"
          "Direct network operations in production"))))

    (reasoning-patterns
      ((primary . "deductive")
       (notes . "Deductive reasoning from specifications to implementations.
                 If the spec says X, the implementation must satisfy X.")
       (secondary . "abductive")
       (not-used . "inductive")
       (notes-inductive . "Inductive reasoning avoided for security properties - require proofs not patterns.")))))
