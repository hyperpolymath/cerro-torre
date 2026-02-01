;; SPDX-License-Identifier: MPL-2.0-or-later
;; PLAYBOOK.scm - Operational runbook for Cerro Torre

(define playbook
  `((version . "1.0.0")
    (schema-version . "1.0")

    (procedures
      ((build
         ((description . "Build the Cerro Torre CLI binary")
          (steps
            ((install-alire . "Install Alire package manager: https://alire.ada.dev/")
             (get-deps . "alr update")
             (build-dev . "alr build")
             (build-release . "alr build -- -XCERRO_BUILD_MODE=Release")
             (build-proof . "alr build -- -XCERRO_BUILD_MODE=Proof")))
          (notes . "Binary is named ct (short for Cerro Torre)")
          (output . "bin/ct")))

       (test
         ((description . "Run test suite")
          (steps
            ((unit-tests . "alr exec -- cerro_tests")
             (integration-tests . "./scripts/test-e2e.sh")
             (conformance-tests . "./scripts/test-canon.sh")))
          (notes . "Test infrastructure not yet fully implemented")
          (coverage-target . 80)))

       (spark-verify
         ((description . "Run SPARK formal verification")
          (steps
            ((check-syntax . "gnatprove -P cerro_torre.gpr --mode=check")
             (flow-analysis . "gnatprove -P cerro_torre.gpr --mode=flow")
             (proof . "gnatprove -P cerro_torre.gpr --mode=prove --level=2")))
          (notes . "Requires SPARK Pro license for full proof. Community edition has limits.")
          (target-modules . ("cerro_crypto" "cerro_manifest" "cerro_provenance"))))

       (release
         ((description . "Create a release")
          (steps
            ((version-bump . "Edit alire.toml version field")
             (changelog . "Update CHANGELOG.md")
             (tag . "git tag -s v0.x.x -m Release v0.x.x")
             (push . "git push origin main --tags")
             (build-release . "alr build -- -XCERRO_BUILD_MODE=Release")
             (sign-binary . "ct sign bin/ct -k release-key")
             (publish . "gh release create v0.x.x bin/ct")))
          (notes . "Threshold signing required for production releases")
          (approval-required . #t)))

       (rollback
         ((description . "Rollback a release")
          (steps
            ((identify . "Determine version to rollback to")
             (verify . "ct verify <previous-version>.ctp")
             (deploy . "ct push <previous-version>.ctp registry.cerro-torre.org/ct:latest")
             (announce . "Post to mailing list/chat")))
          (notes . "Rollback requires same signing threshold as release")))

       (debug
         ((description . "Debug common issues")
          (common-issues
            ((build-failure
               ((symptoms . "alr build fails")
                (check . ("GNAT version >= 13" "Alire version >= 2.0" "Dependencies resolved"))
                (fix . "alr update && alr build")))
             (proof-failure
               ((symptoms . "gnatprove reports unproven VCs")
                (check . ("SPARK annotations complete" "Preconditions sufficient" "Level >= 2"))
                (fix . "Add assertions, strengthen preconditions, or add loop invariants")))
             (crypto-zero
               ((symptoms . "SHA256 returns all zeros")
                (check . ("libsodium installed" "Bindings linked"))
                (fix . "This is expected until libsodium integration complete")))))))))

    (alerts
      ((build-failure . ((severity . "high") (trigger . "CI build fails on main") (response . "Investigate immediately")))
       (proof-regression . ((severity . "high") (trigger . "SPARK proof previously passed now fails") (response . "Do not merge")))
       (security-vuln . ((severity . "critical") (trigger . "Security vulnerability reported") (response . "Follow SECURITY.md")))
       (key-compromise . ((severity . "critical") (trigger . "Signing key potentially compromised") (response . "Revoke key immediately")))))

    (contacts
      ((project
         ((name . "Cerro Torre Cooperative")
          (email . "cerro-torre@example.org")
          (website . "https://cerro-torre.org")
          (repository . "https://github.com/hyperpolymath/cerro-torre")))
       (roles
         ((maintainers . "Technical Committee members")
          (security . "security@cerro-torre.org")
          (governance . "governance@cerro-torre.org")))))

    (runbooks
      ((key-rotation
         ((description . "Rotate signing keys")
          (frequency . "annual or on compromise")
          (steps
            ((generate-new . "ct keygen --id new-release-key-YYYY --suite CT-SIG-02")
             (approve . "Governance Committee approves via threshold signature")
             (publish . "Add public key to keys/ and transparency log")
             (transition . "Sign next 2 releases with both old and new key")
             (revoke-old . "Add old key to revocation list after transition")))))

       (incident-response
         ((description . "Respond to security incident")
          (steps
            ((assess . "Determine scope and severity")
             (contain . "Revoke affected keys, pause affected releases")
             (investigate . "Identify root cause")
             (remediate . "Fix vulnerability, re-sign affected packages")
             (communicate . "Notify affected users, publish post-mortem")
             (improve . "Update procedures to prevent recurrence")))))

       (new-maintainer
         ((description . "Onboard new maintainer")
          (steps
            ((contribution-history . "Verify 3+ months of sustained contributions")
             (covenant-review . "Ensure they affirm Palimpsest Covenant")
             (vote . "Existing maintainers vote on admission")
             (key-generation . "New maintainer generates signing key")
             (trust-assignment . "Add public key to keys/ with appropriate trust level")
             (access . "Grant repository access, mailing list membership")))))

       (release-signing
         ((description . "Sign a release with threshold signatures")
          (steps
            ((build . "Create release build")
             (verify . "Run full test suite and SPARK proofs")
             (initiate . "First signer creates signature request")
             (collect . "Collect k-of-n signatures from keyholders")
             (combine . "Combine partial signatures into threshold signature")
             (publish . "Publish release with combined signature")
             (log . "Submit to transparency log")))))))))
