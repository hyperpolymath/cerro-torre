--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  "Ship containers safely" - the distribution complement to Svalinn's
--  "run containers nicely".

package Cerro_CLI is

   --  Pack an OCI image into a verifiable .ctp bundle
   --  Usage: ct pack <image-ref> -o <output.ctp>
   --  Example: ct pack docker.io/library/nginx:1.26 -o nginx.ctp
   procedure Run_Pack;

   --  Verify a .ctp bundle with specific exit codes
   --  Usage: ct verify <bundle.ctp> [--policy <file>]
   --  Exit codes: 0=valid, 1=hash, 2=sig, 3=key, 4=policy, 10=malformed
   procedure Run_Verify;

   --  Print human-readable verification chain
   --  Usage: ct explain <bundle.ctp> [--signers|--layers]
   procedure Run_Explain;

   --  Generate a signing keypair
   --  Usage: ct keygen [--id <name>] [--suite <suite-id>]
   procedure Run_Keygen;

   --  Key management subcommands
   --  Usage: ct key <list|import|export|default> [args]
   procedure Run_Key;

   --  ======== v0.2 Commands (stubs) ========

   --  Fetch bundle from registry or create from OCI image
   --  Usage: ct fetch <ref> -o <output.ctp> [--create]
   procedure Run_Fetch;

   --  Push bundle to registry/mirror
   --  Usage: ct push <bundle.ctp> <destination>
   procedure Run_Push;

   --  Export bundles for offline transfer
   --  Usage: ct export <bundles...> -o <archive>
   procedure Run_Export;

   --  Import from offline archive
   --  Usage: ct import <archive> [--verify]
   procedure Run_Import;

end Cerro_CLI;
