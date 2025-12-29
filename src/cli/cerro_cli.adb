--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Command_Line;
with CT_Errors;

package body Cerro_CLI is

   use Ada.Text_IO;
   use Ada.Command_Line;

   ----------
   -- Pack --
   ----------

   procedure Run_Pack is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct pack <image-ref> -o <output.ctp>");
         Put_Line ("");
         Put_Line ("Create a verifiable .ctp bundle from an OCI image.");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct pack docker.io/library/nginx:1.26 -o nginx.ctp");
         Put_Line ("  ct pack oci:./local-image -o local.ctp");
         Put_Line ("  ct pack ghcr.io/org/app:v1 -o app.ctp -k my-key");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -o, --output <file>    Output path for .ctp bundle (required)");
         Put_Line ("  -k, --key <key-id>     Signing key to use (default: default key)");
         Put_Line ("  --suite <suite-id>     Crypto suite (default: CT-SIG-01)");
         Put_Line ("  --no-sign              Create unsigned bundle");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Image_Ref : constant String := Argument (2);
      begin
         Put_Line ("Packing image: " & Image_Ref);
         Put_Line ("");
         Put_Line ("(Not yet implemented)");
         Put_Line ("");
         Put_Line ("This command will:");
         Put_Line ("  1. Read OCI image metadata (via skopeo)");
         Put_Line ("  2. Generate canonical manifest.toml");
         Put_Line ("  3. Generate summary.json with all digests");
         Put_Line ("  4. Sign with specified key");
         Put_Line ("  5. Write .ctp bundle");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Pack;

   ------------
   -- Verify --
   ------------

   procedure Run_Verify is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct verify <bundle.ctp> [--policy <file>]");
         Put_Line ("");
         Put_Line ("Verify a .ctp bundle with specific exit codes.");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Verification succeeded");
         Put_Line ("  1   Hash mismatch (content tampered)");
         Put_Line ("  2   Signature invalid");
         Put_Line ("  3   Key not trusted by policy");
         Put_Line ("  4   Policy rejection (registry/base not allowed)");
         Put_Line ("  5   Missing required attestation");
         Put_Line ("  10  Malformed bundle");
         Put_Line ("  11  I/O error");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --policy <file>   Trust policy file");
         Put_Line ("  --offline         Skip transparency log checks");
         Put_Line ("  --verbose         Show detailed verification steps");
         Put_Line ("  --json            Output machine-readable JSON");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Path : constant String := Argument (2);
      begin
         Put_Line ("Verifying bundle: " & Bundle_Path);
         Put_Line ("");
         Put_Line ("(Not yet implemented)");
         Put_Line ("");
         Put_Line ("This command will:");
         Put_Line ("  1. Parse bundle structure");
         Put_Line ("  2. Verify all content hashes match summary");
         Put_Line ("  3. Verify signatures against policy");
         Put_Line ("  4. Return specific exit code for each failure type");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Verify;

   -------------
   -- Explain --
   -------------

   procedure Run_Explain is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct explain <bundle.ctp> [--signers|--layers]");
         Put_Line ("");
         Put_Line ("Print human-readable verification chain.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --signers   Show only signer information");
         Put_Line ("  --layers    Show only layer digests");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Path : constant String := Argument (2);
      begin
         Put_Line ("Explaining bundle: " & Bundle_Path);
         Put_Line ("");
         Put_Line ("(Not yet implemented)");
         Put_Line ("");
         Put_Line ("Output will show:");
         Put_Line ("  - Package info (name, version, suite)");
         Put_Line ("  - Provenance (source, fetch time)");
         Put_Line ("  - Content (manifest digest, layers)");
         Put_Line ("  - Signatures (key id, fingerprint, time)");
         Put_Line ("  - Trust chain status");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Explain;

   ------------
   -- Keygen --
   ------------

   procedure Run_Keygen is
   begin
      Put_Line ("Usage: ct keygen [--id <name>] [--suite <suite-id>]");
      Put_Line ("");
      Put_Line ("Generate a new signing keypair.");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --id <name>        Key identifier (default: auto-generated)");
      Put_Line ("  --suite <suite>    Crypto suite (default: CT-SIG-01)");
      Put_Line ("  --output <dir>     Output directory");
      Put_Line ("  --no-password      Don't encrypt private key (not recommended)");
      Put_Line ("");
      Put_Line ("Suites:");
      Put_Line ("  CT-SIG-01   Ed25519 (classical, default)");
      Put_Line ("  CT-SIG-02   Ed25519 + ML-DSA-87 (hybrid, v0.2)");
      Put_Line ("  CT-SIG-03   ML-DSA-87 (post-quantum only, v0.2)");
      Put_Line ("");
      Put_Line ("(Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Keygen;

   ---------
   -- Key --
   ---------

   procedure Run_Key is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct key <subcommand> [args]");
         Put_Line ("");
         Put_Line ("Key management subcommands:");
         Put_Line ("  list                   List all keys");
         Put_Line ("  import <file>          Import a public key");
         Put_Line ("  export <id> --public   Export public key");
         Put_Line ("  delete <id>            Remove a key");
         Put_Line ("  default <id>           Set default signing key");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct key list");
         Put_Line ("  ct key import upstream-nginx.pub");
         Put_Line ("  ct key export my-key --public > my-key.pub");
         Put_Line ("  ct key default my-key");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Subcommand : constant String := Argument (2);
      begin
         Put_Line ("Key subcommand: " & Subcommand);
         Put_Line ("(Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Key;

   -----------
   -- Fetch --
   -----------

   procedure Run_Fetch is
   begin
      Put_Line ("Usage: ct fetch <ref> -o <output.ctp> [--create]");
      Put_Line ("");
      Put_Line ("Pull a .ctp bundle from a registry, or create from OCI image.");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Fetch;

   ----------
   -- Push --
   ----------

   procedure Run_Push is
   begin
      Put_Line ("Usage: ct push <bundle.ctp> <destination>");
      Put_Line ("");
      Put_Line ("Publish a .ctp bundle to a registry or mirror.");
      Put_Line ("");
      Put_Line ("Destinations:");
      Put_Line ("  registry.io/name:tag    OCI registry");
      Put_Line ("  s3://bucket/path        S3-compatible store");
      Put_Line ("  git://host/repo         Git repository");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Push;

   ------------
   -- Export --
   ------------

   procedure Run_Export is
   begin
      Put_Line ("Usage: ct export <bundles...> -o <archive>");
      Put_Line ("");
      Put_Line ("Export bundles for offline transfer.");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  -o, --output <file>    Output archive path");
      Put_Line ("  --manifest <file>      File listing bundles to export");
      Put_Line ("  --include-keys         Include public keys for verification");
      Put_Line ("  --format <fmt>         Archive format: tar, tar.gz, tar.zst");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Export;

   ------------
   -- Import --
   ------------

   procedure Run_Import is
   begin
      Put_Line ("Usage: ct import <archive> [--verify]");
      Put_Line ("");
      Put_Line ("Import from offline archive.");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --verify            Verify each bundle after import");
      Put_Line ("  --policy <file>     Policy for verification");
      Put_Line ("  --keys-only         Only import keys, not bundles");
      Put_Line ("  --output-dir <dir>  Where to place imported bundles");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Import;

end Cerro_CLI;
