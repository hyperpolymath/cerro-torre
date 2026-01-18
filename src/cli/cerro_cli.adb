--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with CT_Errors;
with Cerro_Pack;
with Cerro_Verify;
with Cerro_Explain;

package body Cerro_CLI is

   use Ada.Text_IO;
   use Ada.Command_Line;

   ----------
   -- Pack --
   ----------

   procedure Run_Pack is
      Opts        : Cerro_Pack.Pack_Options;
      Output_Set  : Boolean := False;
      Verbose     : Boolean := False;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct pack <manifest.ctp> -o <output.ctp>");
         Put_Line ("");
         Put_Line ("Create a .ctp bundle from a manifest file.");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct pack ./hello.ctp -o hello-bundle.ctp");
         Put_Line ("  ct pack manifests/nginx.ctp -o nginx-bundle.ctp -v");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -o, --output <file>    Output path for .ctp bundle (required)");
         Put_Line ("  -s, --sources <dir>    Include source files from directory");
         Put_Line ("  -v, --verbose          Show detailed progress");
         Put_Line ("  --no-sign              Create unsigned bundle (default for MVP)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Parse arguments
      Opts.Manifest_Path := To_Unbounded_String (Argument (2));

      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "-o" or Arg = "--output" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Output_Path := To_Unbounded_String (Argument (I));
                     Output_Set := True;
                  end if;
               elsif Arg = "-s" or Arg = "--sources" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Source_Dir := To_Unbounded_String (Argument (I));
                  end if;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Opts.Verbose := True;
                  Verbose := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate required options
      if not Output_Set then
         Put_Line ("Error: Output path required (-o <file>)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Run pack
      if Verbose then
         Put_Line ("Packing manifest: " & To_String (Opts.Manifest_Path));
         Put_Line ("Output: " & To_String (Opts.Output_Path));
         Put_Line ("");
      end if;

      declare
         Result : constant Cerro_Pack.Pack_Result := Cerro_Pack.Create_Bundle (Opts);
      begin
         if Result.Success then
            Put_Line ("✓ Bundle created: " & To_String (Result.Bundle_Path));
            Set_Exit_Status (0);
         else
            Put_Line ("✗ Pack failed: " & To_String (Result.Error_Msg));
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end if;
      end;
   end Run_Pack;

   ------------
   -- Verify --
   ------------

   procedure Run_Verify is
      Opts    : Cerro_Verify.Verify_Options;
      Verbose : Boolean := False;
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

      --  Parse arguments
      Opts.Bundle_Path := To_Unbounded_String (Argument (2));

      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "--policy" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Policy_Path := To_Unbounded_String (Argument (I));
                  end if;
               elsif Arg = "--offline" then
                  Opts.Offline := True;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Opts.Verbose := True;
                  Verbose := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Run verification
      declare
         Result : constant Cerro_Verify.Verify_Result :=
            Cerro_Verify.Verify_Bundle (Opts);
         Exit_Code : constant Integer := Cerro_Verify.To_Exit_Code (Result.Code);
      begin
         if Verbose then
            Put_Line ("Bundle: " & To_String (Opts.Bundle_Path));
            Put_Line ("");
         end if;

         case Result.Code is
            when Cerro_Verify.OK =>
               Put_Line ("✓ Verification successful");
               if Length (Result.Package_Name) > 0 then
                  Put_Line ("  Package: " & To_String (Result.Package_Name) &
                            " " & To_String (Result.Package_Ver));
               end if;
               if Verbose then
                  Put_Line ("  Manifest hash: " & To_String (Result.Manifest_Hash));
               end if;

            when Cerro_Verify.Hash_Mismatch =>
               Put_Line ("✗ Hash mismatch - content may have been tampered");
               Put_Line ("  " & To_String (Result.Details));

            when Cerro_Verify.Signature_Invalid =>
               Put_Line ("✗ Signature verification failed");

            when Cerro_Verify.Key_Not_Trusted =>
               Put_Line ("✗ Signing key not trusted by policy");

            when Cerro_Verify.Policy_Rejection =>
               Put_Line ("✗ Bundle rejected by policy");
               Put_Line ("  " & To_String (Result.Details));

            when Cerro_Verify.Missing_Attestation =>
               Put_Line ("✗ Missing required attestation");

            when Cerro_Verify.Malformed_Bundle =>
               Put_Line ("✗ Malformed bundle");
               Put_Line ("  " & To_String (Result.Details));

            when Cerro_Verify.IO_Error =>
               Put_Line ("✗ I/O error");
               Put_Line ("  " & To_String (Result.Details));
         end case;

         Set_Exit_Status (Ada.Command_Line.Exit_Status (Exit_Code));
      end;
   end Run_Verify;

   -------------
   -- Explain --
   -------------

   procedure Run_Explain is
   begin
      Cerro_Explain.Run_Explain;
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

   ---------
   -- Run --
   ---------

   procedure Run_Run is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct run <bundle.ctp> [--runtime=<name>] [-- <args>]");
         Put_Line ("");
         Put_Line ("Run a verified bundle via configured runtime.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --runtime=<name>   Runtime to use (default from config)");
         Put_Line ("  --no-verify        Skip verification before run");
         Put_Line ("  -- <args>          Pass remaining args to runtime");
         Put_Line ("");
         Put_Line ("Runtimes:");
         Put_Line ("  svalinn            Svalinn (recommended)");
         Put_Line ("  podman             Podman");
         Put_Line ("  docker             Docker");
         Put_Line ("  nerdctl            containerd/nerdctl");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct run nginx.ctp");
         Put_Line ("  ct run nginx.ctp --runtime=svalinn");
         Put_Line ("  ct run nginx.ctp -- -p 8080:80 -d");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Path : constant String := Argument (2);
      begin
         Put_Line ("Running bundle: " & Bundle_Path);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Put_Line ("");
         Put_Line ("This command will:");
         Put_Line ("  1. Verify bundle (unless --no-verify)");
         Put_Line ("  2. Unpack to OCI layout");
         Put_Line ("  3. Delegate to runtime (svalinn/podman/docker)");
         Put_Line ("  4. Pass through runtime arguments");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Run;

   ------------
   -- Unpack --
   ------------

   procedure Run_Unpack is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct unpack <bundle.ctp> -o <dir> [--format=oci|docker]");
         Put_Line ("");
         Put_Line ("Extract bundle to OCI layout on disk.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -o, --output <dir>   Output directory (required)");
         Put_Line ("  --format=oci         OCI image layout (default)");
         Put_Line ("  --format=docker      Docker save format");
         Put_Line ("  --include-attestations  Copy attestations alongside");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct unpack nginx.ctp -o ./nginx-oci/");
         Put_Line ("  ct unpack nginx.ctp -o nginx.tar --format=docker");
         Put_Line ("");
         Put_Line ("Use with:");
         Put_Line ("  podman load < nginx.tar");
         Put_Line ("  nerdctl load < nginx.tar");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Path : constant String := Argument (2);
      begin
         Put_Line ("Unpacking bundle: " & Bundle_Path);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Unpack;

   ------------
   -- Doctor --
   ------------

   procedure Run_Doctor is
   begin
      Put_Line ("ct doctor - Check distribution pipeline health");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --quick   Just check essentials");
      Put_Line ("  --fix     Attempt to fix issues");
      Put_Line ("");
      Put_Line ("Checks performed:");
      Put_Line ("");
      Put_Line ("  Crypto backend:");
      Put_Line ("    [ ] libsodium available");
      Put_Line ("    [ ] liboqs available (for post-quantum)");
      Put_Line ("");
      Put_Line ("  Configuration:");
      Put_Line ("    [ ] Config file valid (~/.config/cerro/config.toml)");
      Put_Line ("    [ ] Policy file valid (~/.config/cerro/policy.json)");
      Put_Line ("    [ ] Default key configured");
      Put_Line ("");
      Put_Line ("  Keys:");
      Put_Line ("    [ ] Keys directory accessible");
      Put_Line ("    [ ] No expired keys");
      Put_Line ("    [ ] Private key decryptable");
      Put_Line ("");
      Put_Line ("  Registry access:");
      Put_Line ("    [ ] Can reach configured registries");
      Put_Line ("    [ ] Authentication valid");
      Put_Line ("");
      Put_Line ("  System:");
      Put_Line ("    [ ] Clock within tolerance");
      Put_Line ("    [ ] Content store healthy");
      Put_Line ("    [ ] Sufficient disk space");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Doctor;

   ------------
   -- Resign --
   ------------

   procedure Run_Resign is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct re-sign <bundle.ctp> -k <key-id> [options]");
         Put_Line ("");
         Put_Line ("Re-sign a bundle with a new key (preserves content).");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -k, --key <key-id>   New signing key (required)");
         Put_Line ("  --add-signature      Add signature, keep existing");
         Put_Line ("  --replace            Replace all signatures (default)");
         Put_Line ("  -o, --output <file>  Output path (default: overwrite)");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct re-sign nginx.ctp -k new-key-2026");
         Put_Line ("  ct re-sign nginx.ctp -k backup-key --add-signature");
         Put_Line ("  ct re-sign nginx.ctp -k new-key -o nginx-resigned.ctp");
         Put_Line ("");
         Put_Line ("Use cases:");
         Put_Line ("  - Key rotation (old key expiring)");
         Put_Line ("  - Multi-party signing (threshold policies)");
         Put_Line ("  - Countersigning (adding endorsements)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Path : constant String := Argument (2);
      begin
         Put_Line ("Re-signing bundle: " & Bundle_Path);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Resign;

   ----------
   -- Diff --
   ----------

   procedure Run_Diff is
   begin
      if Argument_Count < 3 then
         Put_Line ("Usage: ct diff <old.ctp> <new.ctp> [options]");
         Put_Line ("");
         Put_Line ("Human-readable diff between bundles.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --layers     Show only layer changes");
         Put_Line ("  --config     Show only config/env changes");
         Put_Line ("  --signers    Show only signature changes");
         Put_Line ("  --json       Output machine-readable JSON");
         Put_Line ("");
         Put_Line ("Output shows:");
         Put_Line ("  - Changed layers (added/removed/modified)");
         Put_Line ("  - Config differences (ENV, labels, entrypoint)");
         Put_Line ("  - Signature changes (new signers, removed)");
         Put_Line ("  - Attestation differences (SBOM, provenance)");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct diff nginx-1.25.ctp nginx-1.26.ctp");
         Put_Line ("  ct diff old.ctp new.ctp --layers");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Old_Bundle : constant String := Argument (2);
         New_Bundle : constant String := Argument (3);
      begin
         Put_Line ("Comparing bundles:");
         Put_Line ("  Old: " & Old_Bundle);
         Put_Line ("  New: " & New_Bundle);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Put_Line ("");
         Put_Line ("Sample output:");
         Put_Line ("");
         Put_Line ("  Layers:");
         Put_Line ("    ~ sha256:abc... -> sha256:def...  (base changed)");
         Put_Line ("    + sha256:123...                   (new layer)");
         Put_Line ("");
         Put_Line ("  Config:");
         Put_Line ("    ~ ENV[""VERSION""] = ""1.25"" -> ""1.26""");
         Put_Line ("");
         Put_Line ("  Signatures:");
         Put_Line ("    = Both signed by: cerro-official-2025");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Diff;

   -----------
   -- Index --
   -----------

   procedure Run_Index is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct index <directory> [options]");
         Put_Line ("");
         Put_Line ("Build searchable index of bundles.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --update    Update existing index");
         Put_Line ("  --output    Index file path (default: ./ct-index.json)");
         Put_Line ("");
         Put_Line ("Indexed fields:");
         Put_Line ("  - name, version, description");
         Put_Line ("  - source image digest");
         Put_Line ("  - signer key IDs and fingerprints");
         Put_Line ("  - SBOM presence, licenses");
         Put_Line ("  - build provenance (builder, date)");
         Put_Line ("  - base image lineage");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Dir_Path : constant String := Argument (2);
      begin
         Put_Line ("Indexing directory: " & Dir_Path);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Index;

   ------------
   -- Search --
   ------------

   procedure Run_Search is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct search <query> [options]");
         Put_Line ("");
         Put_Line ("Search bundles by metadata.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --signer <pattern>   Filter by signer key ID");
         Put_Line ("  --has-sbom           Only bundles with SBOM");
         Put_Line ("  --has-provenance     Only bundles with provenance");
         Put_Line ("  --digest <sha256>    By source image digest");
         Put_Line ("  --after <date>       Created after date");
         Put_Line ("  --before <date>      Created before date");
         Put_Line ("  --index <file>       Index file to search");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct search nginx");
         Put_Line ("  ct search --signer cerro-official-*");
         Put_Line ("  ct search --has-sbom --after 2025-01-01");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Query : constant String := Argument (2);
      begin
         Put_Line ("Searching for: " & Query);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Search;

   ------------
   -- Policy --
   ------------

   procedure Run_Policy is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct policy <subcommand> [args]");
         Put_Line ("");
         Put_Line ("Policy management subcommands:");
         Put_Line ("  init                   Create starter policy interactively");
         Put_Line ("  show                   Display current policy");
         Put_Line ("  add-signer <key-id>    Trust a signer");
         Put_Line ("  add-registry <pat>     Allow a registry pattern");
         Put_Line ("  deny <key-id> [date]   Add to deny-list");
         Put_Line ("  pin <bundle> <digest>  Pin bundle to specific digest");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct policy init");
         Put_Line ("  ct policy add-signer cerro-official-2025");
         Put_Line ("  ct policy add-registry 'docker.io/library/*'");
         Put_Line ("  ct policy deny compromised-key --after 2025-06-01");
         Put_Line ("  ct policy pin nginx.ctp sha256:abc123...");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Subcommand : constant String := Argument (2);
      begin
         Put_Line ("Policy subcommand: " & Subcommand);
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Policy;

   ----------
   -- Help --
   ----------

   procedure Run_Help is
   begin
      Cerro_Explain.Run_Help;
   end Run_Help;

   ---------
   -- Man --
   ---------

   procedure Run_Man is
   begin
      Cerro_Explain.Run_Man;
   end Run_Man;

   -------------
   -- Version --
   -------------

   procedure Run_Version is
   begin
      Cerro_Explain.Run_Version;
   end Run_Version;

end Cerro_CLI;
