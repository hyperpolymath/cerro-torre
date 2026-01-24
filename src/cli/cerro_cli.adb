--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with CT_Errors;
with CT_Registry;
with Cerro_Pack;
with Cerro_Verify;
with Cerro_Explain;
with Cerro_Trust_Store;
with Cerro_Runtime;

package body Cerro_CLI is

   use Ada.Text_IO;
   use Ada.Command_Line;
   use type Cerro_Verify.Verify_Code;

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

   procedure Print_Key_Info (Info : Cerro_Trust_Store.Key_Info) is
      Trust_Str : constant String :=
         (case Info.Trust is
            when Cerro_Trust_Store.Untrusted => "untrusted",
            when Cerro_Trust_Store.Marginal  => "marginal",
            when Cerro_Trust_Store.Full      => "full",
            when Cerro_Trust_Store.Ultimate  => "ultimate");
   begin
      Put_Line ("  " & Info.Key_Id (1 .. Info.Key_Id_Len));
      Put_Line ("    Fingerprint: " & Info.Fingerprint (1 .. 16) & "...");
      Put_Line ("    Trust: " & Trust_Str);
   end Print_Key_Info;

   procedure Run_Key is
      use Cerro_Trust_Store;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct key <subcommand> [args]");
         Put_Line ("");
         Put_Line ("Key management subcommands:");
         Put_Line ("  list                   List all keys in trust store");
         Put_Line ("  import <file>          Import a public key");
         Put_Line ("  import-hex <hex> <id>  Import key from hex string");
         Put_Line ("  export <id> -o <file>  Export public key");
         Put_Line ("  delete <id>            Remove a key");
         Put_Line ("  trust <id> <level>     Set trust level (untrusted/marginal/full/ultimate)");
         Put_Line ("  default [id]           Show or set default signing key");
         Put_Line ("  info <id>              Show key details");
         Put_Line ("");
         Put_Line ("Trust store: " & Get_Store_Path);
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct key list");
         Put_Line ("  ct key import upstream-nginx.pub");
         Put_Line ("  ct key trust nginx-upstream full");
         Put_Line ("  ct key default my-signing-key");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Initialize trust store
      Initialize;

      declare
         Subcommand : constant String := Argument (2);
      begin
         --  LIST
         if Subcommand = "list" then
            declare
               Count : constant Natural := Key_Count;
            begin
               if Count = 0 then
                  Put_Line ("No keys in trust store.");
                  Put_Line ("Import keys with: ct key import <file.pub>");
               else
                  Put_Line ("Keys in trust store (" & Natural'Image (Count) & "):");
                  Put_Line ("");
                  For_Each_Key (Print_Key_Info'Access);
               end if;
               Set_Exit_Status (0);
            end;

         --  IMPORT
         elsif Subcommand = "import" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key import <file.pub> [--id <name>]");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Path   : constant String := Argument (3);
               Key_Id : Unbounded_String := Null_Unbounded_String;
               Result : Store_Result;
            begin
               --  Check for --id flag
               if Argument_Count >= 5 and then Argument (4) = "--id" then
                  Key_Id := To_Unbounded_String (Argument (5));
               end if;

               if Length (Key_Id) > 0 then
                  Result := Import_Key (Path, To_String (Key_Id));
               else
                  Result := Import_Key (Path);
               end if;

               case Result is
                  when OK =>
                     Put_Line ("Key imported successfully.");
                     Put_Line ("Set trust level with: ct key trust <id> full");
                     Set_Exit_Status (0);
                  when Already_Exists =>
                     Put_Line ("Error: Key already exists in trust store.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Invalid_Key =>
                     Put_Line ("Error: Invalid key format (expected 64 hex chars).");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Invalid_Format =>
                     Put_Line ("Error: Invalid key ID format.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Not_Found =>
                     Put_Line ("Error: File not found: " & Path);
                     Set_Exit_Status (CT_Errors.Exit_IO_Error);
                  when IO_Error =>
                     Put_Line ("Error: Could not read file.");
                     Set_Exit_Status (CT_Errors.Exit_IO_Error);
               end case;
            end;

         --  IMPORT-HEX
         elsif Subcommand = "import-hex" then
            if Argument_Count < 4 then
               Put_Line ("Usage: ct key import-hex <hex-pubkey> <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Hex_Key : constant String := Argument (3);
               Key_Id  : constant String := Argument (4);
               Result  : Store_Result;
            begin
               Result := Import_Key_Hex (Hex_Key, Key_Id);
               case Result is
                  when OK =>
                     Put_Line ("Key '" & Key_Id & "' imported successfully.");
                     Set_Exit_Status (0);
                  when Already_Exists =>
                     Put_Line ("Error: Key '" & Key_Id & "' already exists.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Invalid_Key =>
                     Put_Line ("Error: Invalid hex key (expected 64 hex chars).");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when others =>
                     Put_Line ("Error: Could not import key.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end case;
            end;

         --  EXPORT
         elsif Subcommand = "export" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key export <key-id> -o <file>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id : constant String := Argument (3);
               Output : Unbounded_String := Null_Unbounded_String;
               Result : Store_Result;
               Info   : Key_Info;
            begin
               --  Check for -o flag
               if Argument_Count >= 5 and then Argument (4) = "-o" then
                  Output := To_Unbounded_String (Argument (5));
               end if;

               if Length (Output) > 0 then
                  Result := Export_Key (Key_Id, To_String (Output));
                  if Result = OK then
                     Put_Line ("Exported to: " & To_String (Output));
                     Set_Exit_Status (0);
                  else
                     Put_Line ("Error: Key not found: " & Key_Id);
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  end if;
               else
                  --  Print to stdout
                  Result := Get_Key (Key_Id, Info);
                  if Result = OK then
                     Put_Line (Info.Public_Key (1 .. Info.Pubkey_Len));
                     Set_Exit_Status (0);
                  else
                     Put_Line ("Error: Key not found: " & Key_Id);
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  end if;
               end if;
            end;

         --  DELETE
         elsif Subcommand = "delete" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key delete <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id : constant String := Argument (3);
               Result : Store_Result;
            begin
               Result := Delete_Key (Key_Id);
               if Result = OK then
                  Put_Line ("Key '" & Key_Id & "' deleted.");
                  Set_Exit_Status (0);
               else
                  Put_Line ("Error: Key not found: " & Key_Id);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end;

         --  TRUST
         elsif Subcommand = "trust" then
            if Argument_Count < 4 then
               Put_Line ("Usage: ct key trust <key-id> <level>");
               Put_Line ("Levels: untrusted, marginal, full, ultimate");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id    : constant String := Argument (3);
               Level_Str : constant String := Argument (4);
               Level     : Trust_Level;
               Result    : Store_Result;
            begin
               if Level_Str = "untrusted" then
                  Level := Untrusted;
               elsif Level_Str = "marginal" then
                  Level := Marginal;
               elsif Level_Str = "full" then
                  Level := Full;
               elsif Level_Str = "ultimate" then
                  Level := Ultimate;
               else
                  Put_Line ("Error: Invalid trust level: " & Level_Str);
                  Put_Line ("Valid levels: untrusted, marginal, full, ultimate");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               Result := Set_Trust (Key_Id, Level);
               if Result = OK then
                  Put_Line ("Trust level for '" & Key_Id & "' set to " & Level_Str);
                  Set_Exit_Status (0);
               else
                  Put_Line ("Error: Key not found: " & Key_Id);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end;

         --  DEFAULT
         elsif Subcommand = "default" then
            if Argument_Count < 3 then
               --  Show current default
               declare
                  Default : constant String := Get_Default_Key;
               begin
                  if Default'Length > 0 then
                     Put_Line ("Default signing key: " & Default);
                  else
                     Put_Line ("No default signing key set.");
                     Put_Line ("Set with: ct key default <key-id>");
                  end if;
                  Set_Exit_Status (0);
               end;
            else
               --  Set default
               declare
                  Key_Id : constant String := Argument (3);
                  Result : Store_Result;
               begin
                  Result := Set_Default_Key (Key_Id);
                  if Result = OK then
                     Put_Line ("Default signing key set to: " & Key_Id);
                     Set_Exit_Status (0);
                  else
                     Put_Line ("Error: Key not found: " & Key_Id);
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  end if;
               end;
            end if;

         --  INFO
         elsif Subcommand = "info" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key info <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id : constant String := Argument (3);
               Info   : Key_Info;
               Result : Store_Result;
            begin
               Result := Get_Key (Key_Id, Info);
               if Result = OK then
                  Put_Line ("Key: " & Info.Key_Id (1 .. Info.Key_Id_Len));
                  Put_Line ("Fingerprint: " & Info.Fingerprint (1 .. Info.Finger_Len));
                  Put_Line ("Public Key: " & Info.Public_Key (1 .. Info.Pubkey_Len));
                  Put_Line ("Trust Level: " &
                     (case Info.Trust is
                        when Untrusted => "untrusted",
                        when Marginal  => "marginal",
                        when Full      => "full",
                        when Ultimate  => "ultimate"));
                  if Info.Created_Len > 0 then
                     Put_Line ("Created: " & Info.Created (1 .. Info.Created_Len));
                  end if;
                  Set_Exit_Status (0);
               else
                  Put_Line ("Error: Key not found: " & Key_Id);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end;

         else
            Put_Line ("Unknown subcommand: " & Subcommand);
            Put_Line ("Run 'ct key' for usage.");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end if;
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
      use CT_Registry;
      use Ada.Directories;

      Bundle_Path : Unbounded_String := Null_Unbounded_String;
      Destination : Unbounded_String := Null_Unbounded_String;
      Verbose     : Boolean := False;
      Force       : Boolean := False;

   begin
      --  Parse arguments
      if Argument_Count < 3 then
         Put_Line ("Usage: ct push <bundle.ctp> <destination> [options]");
         Put_Line ("");
         Put_Line ("Publish a .ctp bundle to an OCI registry.");
         Put_Line ("");
         Put_Line ("Arguments:");
         Put_Line ("  <bundle.ctp>     Path to .ctp bundle file");
         Put_Line ("  <destination>    Registry reference (registry/repo:tag)");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -v, --verbose    Show detailed progress");
         Put_Line ("  -f, --force      Overwrite existing tag");
         Put_Line ("");
         Put_Line ("Authentication:");
         Put_Line ("  Reads from ~/.docker/config.json or CT_REGISTRY_AUTH env var");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct push nginx.ctp ghcr.io/hyperpolymath/nginx:v1.0");
         Put_Line ("  ct push hello.ctp docker.io/myuser/hello:latest");
         Put_Line ("  ct push app.ctp myregistry.io/apps/myapp:v2.1 -v");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Push succeeded");
         Put_Line ("  1   Authentication failed");
         Put_Line ("  2   Network error or registry unavailable");
         Put_Line ("  3   Bundle not found or malformed");
         Put_Line ("  10  Invalid arguments");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      Bundle_Path := To_Unbounded_String (Argument (2));
      Destination := To_Unbounded_String (Argument (3));

      --  Parse optional flags
      declare
         I : Positive := 4;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "-v" or Arg = "--verbose" then
                  Verbose := True;
               elsif Arg = "-f" or Arg = "--force" then
                  Force := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate bundle exists
      declare
         Bundle_Str : constant String := To_String (Bundle_Path);
      begin
         if not Ada.Directories.Exists (Bundle_Str) then
            Put_Line ("Error: Bundle not found: " & Bundle_Str);
            Set_Exit_Status (3);
            return;
         end if;

         if Verbose then
            Put_Line ("Bundle: " & Bundle_Str);
            Put_Line ("Destination: " & To_String (Destination));
            Put_Line ("");
         end if;
      end;

      --  Parse destination reference
      declare
         Ref : constant Image_Reference := Parse_Reference (To_String (Destination));
      begin
         if Length (Ref.Registry) = 0 or Length (Ref.Repository) = 0 then
            Put_Line ("Error: Invalid destination reference");
            Put_Line ("  Expected format: registry/repository:tag");
            Set_Exit_Status (10);
            return;
         end if;

         if Verbose then
            Put_Line ("Parsed destination:");
            Put_Line ("  Registry: " & To_String (Ref.Registry));
            Put_Line ("  Repository: " & To_String (Ref.Repository));
            Put_Line ("  Tag: " & To_String (Ref.Tag));
            Put_Line ("");
         end if;

         --  Step 1: Load credentials
         --  TODO: Read from ~/.docker/config.json or CT_REGISTRY_AUTH
         declare
            Auth : Auth_Credentials := (others => <>);
            Username_Env : constant String := Ada.Environment_Variables.Value ("CT_REGISTRY_USER", "");
            Password_Env : constant String := Ada.Environment_Variables.Value ("CT_REGISTRY_PASS", "");
         begin
            if Username_Env'Length > 0 then
               Auth.Method := Basic;
               Auth.Username := To_Unbounded_String (Username_Env);
               Auth.Password := To_Unbounded_String (Password_Env);

               if Verbose then
                  Put_Line ("Using credentials from environment");
               end if;
            else
               if Verbose then
                  Put_Line ("No credentials configured (attempting anonymous push)");
               end if;
            end if;

            --  Step 2: Create registry client
            declare
               Client : Registry_Client := Create_Client (
                  Registry => To_String (Ref.Registry),
                  Auth     => Auth);
            begin
               if Verbose then
                  Put_Line ("Connecting to registry: " & To_String (Client.Base_URL));
               end if;

               --  Step 3: Authenticate if credentials provided
               if Auth.Method /= None then
                  declare
                     Auth_Result : constant Registry_Error := Authenticate (
                        Client     => Client,
                        Repository => To_String (Ref.Repository),
                        Actions    => "push");
                  begin
                     if Auth_Result /= Success and Auth_Result /= Not_Implemented then
                        Put_Line ("✗ Authentication failed: " & Error_Message (Auth_Result));
                        Set_Exit_Status (1);
                        return;
                     end if;

                     if Verbose and Auth_Result = Success then
                        Put_Line ("✓ Authenticated");
                     end if;
                  end;
               end if;

               --  Step 4: Check if tag already exists (unless --force)
               if not Force then
                  if Manifest_Exists (Client, To_String (Ref.Repository), To_String (Ref.Tag)) then
                     Put_Line ("Error: Tag already exists: " & To_String (Ref.Tag));
                     Put_Line ("  Use --force to overwrite");
                     Set_Exit_Status (1);
                     return;
                  end if;
               end if;

               --  Step 5: Push blobs from bundle
               --  TODO: Extract blobs from .ctp tarball and push each layer
               --  For now, note that this step is not yet implemented

               if Verbose then
                  Put_Line ("Pushing blobs...");
                  Put_Line ("  (Blob upload not yet implemented - OCI layer extraction pending)");
               end if;

               --  Step 6: Push manifest
               --  TODO: Extract manifest from bundle, push to registry
               declare
                  Dummy_Manifest : OCI_Manifest;
                  Push_Res : Push_Result;
               begin
                  Dummy_Manifest.Schema_Version := 2;
                  Dummy_Manifest.Media_Type := To_Unbounded_String (OCI_Manifest_V1);

                  Push_Res := Push_Manifest (
                     Client     => Client,
                     Repository => To_String (Ref.Repository),
                     Tag        => To_String (Ref.Tag),
                     Manifest   => Dummy_Manifest);

                  if Push_Res.Error = Not_Implemented then
                     Put_Line ("");
                     Put_Line ("Push operation prepared but not yet implemented.");
                     Put_Line ("");
                     Put_Line ("Implementation roadmap:");
                     Put_Line ("  1. HTTP client integration (AWS.Client or similar)");
                     Put_Line ("  2. Extract OCI blobs from .ctp tarball");
                     Put_Line ("  3. Push each blob with chunked upload");
                     Put_Line ("  4. Push manifest with references to uploaded blobs");
                     Put_Line ("  5. Add audit log entry");
                     Put_Line ("");
                     Put_Line ("When implemented, this will:");
                     Put_Line ("  ✓ Upload all container layers to registry");
                     Put_Line ("  ✓ Upload manifest with attestations");
                     Put_Line ("  ✓ Return manifest digest for verification");
                     Put_Line ("");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                     return;
                  elsif Push_Res.Error /= Success then
                     Put_Line ("✗ Push failed: " & Error_Message (Push_Res.Error));

                     case Push_Res.Error is
                        when Auth_Failed | Auth_Required | Forbidden =>
                           Set_Exit_Status (1);
                        when Network_Error | Timeout | Server_Error =>
                           Set_Exit_Status (2);
                        when others =>
                           Set_Exit_Status (CT_Errors.Exit_General_Failure);
                     end case;
                     return;
                  else
                     Put_Line ("✓ Pushed to " & To_String (Destination));
                     Put_Line ("  Digest: " & To_String (Push_Res.Digest));

                     if Verbose then
                        Put_Line ("  URL: " & To_String (Push_Res.URL));
                     end if;

                     Set_Exit_Status (0);
                  end if;
               end;
            end;
         end;
      end;
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
      package Dir renames Ada.Directories;

      Bundle_Path   : Unbounded_String := Null_Unbounded_String;
      Runtime_Name  : Unbounded_String := Null_Unbounded_String;
      Skip_Verify   : Boolean := False;
      Detach        : Boolean := False;
      Extra_Args    : Unbounded_String := Null_Unbounded_String;
      Ports         : Unbounded_String := Null_Unbounded_String;
      Volumes       : Unbounded_String := Null_Unbounded_String;
      Found_Separator : Boolean := False;  --  Track if we found "--"

      procedure Show_Help is
      begin
         Put_Line ("Usage: ct run <bundle.ctp> [--runtime=<name>] [-- <args>]");
         Put_Line ("");
         Put_Line ("Run a verified bundle via configured runtime.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -h, --help         Show this help");
         Put_Line ("  --runtime=<name>   Runtime to use (default: auto-detect)");
         Put_Line ("  --no-verify        Skip verification before run");
         Put_Line ("  -d, --detach       Run container in background");
         Put_Line ("  -p <ports>         Port mapping (e.g., 8080:80)");
         Put_Line ("  -v <vol>           Volume mount (e.g., /host:/container)");
         Put_Line ("  -- <args>          Pass remaining args to container");
         Put_Line ("");
         Put_Line ("Runtimes (FOSS-first preference):");
         Put_Line ("  svalinn            Svalinn (recommended, formally verified)");
         Put_Line ("  podman             Podman (rootless)");
         Put_Line ("  nerdctl            containerd/nerdctl");
         Put_Line ("  docker             Docker (fallback)");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct run nginx.ctp");
         Put_Line ("  ct run nginx.ctp --runtime=podman");
         Put_Line ("  ct run nginx.ctp -p 8080:80 -d");
         Put_Line ("  ct run myapp.ctp -- --config /etc/app.conf");
      end Show_Help;

   begin
      --  Show help if no arguments or --help/-h specified
      if Argument_Count < 2 or else
         Argument (2) = "--help" or else
         Argument (2) = "-h"
      then
         Show_Help;
         Set_Exit_Status (CT_Errors.Exit_Success);
         return;
      end if;

      --  Parse arguments
      declare
         I : Positive := 2;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Found_Separator then
                  --  Everything after "--" goes to container
                  if Length (Extra_Args) > 0 then
                     Append (Extra_Args, " ");
                  end if;
                  Append (Extra_Args, Arg);

               elsif Arg = "--" then
                  Found_Separator := True;

               elsif Arg'Length > 10 and then
                     Arg (Arg'First .. Arg'First + 9) = "--runtime="
               then
                  Runtime_Name := To_Unbounded_String (Arg (Arg'First + 10 .. Arg'Last));

               elsif Arg = "--no-verify" then
                  Skip_Verify := True;

               elsif Arg = "-d" or Arg = "--detach" then
                  Detach := True;

               elsif Arg = "-p" and I < Argument_Count then
                  I := I + 1;
                  Ports := To_Unbounded_String (Argument (I));

               elsif Arg = "-v" and I < Argument_Count then
                  I := I + 1;
                  Volumes := To_Unbounded_String (Argument (I));

               elsif Arg (Arg'First) /= '-' and Length (Bundle_Path) = 0 then
                  Bundle_Path := To_Unbounded_String (Arg);
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate bundle path
      if Length (Bundle_Path) = 0 then
         Put_Line ("Error: No bundle path specified");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Str : constant String := To_String (Bundle_Path);
      begin
         if not Dir.Exists (Bundle_Str) then
            Put_Line ("Error: Bundle not found: " & Bundle_Str);
            Set_Exit_Status (CT_Errors.Exit_IO_Error);
            return;
         end if;

         Put_Line ("Running bundle: " & Bundle_Str);

         --  Step 1: Verify bundle (unless --no-verify)
         if not Skip_Verify then
            Put_Line ("  Verifying bundle...");
            declare
               Verify_Opts : Cerro_Verify.Verify_Options := (
                  Bundle_Path => Bundle_Path,
                  Policy_Path => Null_Unbounded_String,
                  Offline     => False,
                  Verbose     => False
               );
               Verify_Result : constant Cerro_Verify.Verify_Result :=
                  Cerro_Verify.Verify_Bundle (Verify_Opts);
            begin
               if not (Verify_Result.Code = Cerro_Verify.OK) then
                  Put_Line ("  Verification failed: " & To_String (Verify_Result.Details));
                  Set_Exit_Status (Ada.Command_Line.Exit_Status (
                     Cerro_Verify.To_Exit_Code (Verify_Result.Code)));
                  return;
               end if;
               Put_Line ("  ✓ Bundle verified: " &
                  To_String (Verify_Result.Package_Name) & " " &
                  To_String (Verify_Result.Package_Ver));
            end;
         else
            Put_Line ("  Skipping verification (--no-verify)");
         end if;

         --  Step 2: Detect runtime
         Cerro_Runtime.Detect_Runtimes;

         declare
            Selected_Runtime : Cerro_Runtime.Runtime_Kind;
         begin
            if Length (Runtime_Name) > 0 then
               Selected_Runtime := Cerro_Runtime.Parse_Runtime_Name (To_String (Runtime_Name));
               if not Cerro_Runtime.Is_Available (Selected_Runtime) then
                  Put_Line ("  Warning: " & To_String (Runtime_Name) &
                     " not available, falling back to preferred runtime");
                  Selected_Runtime := Cerro_Runtime.Get_Preferred_Runtime;
               end if;
            else
               Selected_Runtime := Cerro_Runtime.Get_Preferred_Runtime;
            end if;

            declare
               Runtime_Info : constant Cerro_Runtime.Runtime_Info :=
                  Cerro_Runtime.Get_Runtime_Info (Selected_Runtime);
            begin
               if not Runtime_Info.Available then
                  Put_Line ("Error: No container runtime available");
                  Put_Line ("  Please install one of: svalinn, podman, nerdctl, docker");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               Put_Line ("  Using runtime: " &
                  Cerro_Runtime.Runtime_Command (Selected_Runtime) &
                  " (" & To_String (Runtime_Info.Version) & ")");

               --  Step 3: Load and run
               --  For now, we'll use the bundle path directly as image reference
               --  In the future, we'll unpack to OCI and load properly
               Put_Line ("  Loading image...");

               declare
                  Load_Result : constant Cerro_Runtime.Run_Result :=
                     Cerro_Runtime.Load_Image (
                        Kind    => Selected_Runtime,
                        Tarball => Bundle_Str);
               begin
                  if not Load_Result.Success then
                     --  If load fails, try running directly (some runtimes support this)
                     Put_Line ("  Note: Direct load not supported, using image reference");
                  end if;
               end;

               Put_Line ("  Starting container...");

               declare
                  Run_Opts : Cerro_Runtime.Run_Options := (
                     Runtime     => Selected_Runtime,
                     Image_Path  => Bundle_Path,  --  Will need to map to loaded image
                     Detach      => Detach,
                     Ports       => Ports,
                     Volumes     => Volumes,
                     Environment => Null_Unbounded_String,
                     Extra_Args  => Extra_Args
                  );
                  Run_Result : constant Cerro_Runtime.Run_Result :=
                     Cerro_Runtime.Run_Container (Run_Opts);
               begin
                  if Run_Result.Success then
                     Put_Line ("  ✓ Container started");
                     if Length (Run_Result.Container_ID) > 0 then
                        Put_Line ("  Container ID: " & To_String (Run_Result.Container_ID));
                     end if;
                     Set_Exit_Status (CT_Errors.Exit_Success);
                  else
                     Put_Line ("  Error: " & To_String (Run_Result.Error_Message));
                     Set_Exit_Status (Ada.Command_Line.Exit_Status (Run_Result.Exit_Code));
                  end if;
               end;
            end;
         end;
      end;
   end Run_Run;

   ------------
   -- Unpack --
   ------------

   procedure Run_Unpack is
      package Dir renames Ada.Directories;

      type Output_Format is (Format_OCI, Format_Docker);

      Bundle_Path          : Unbounded_String := Null_Unbounded_String;
      Output_Path          : Unbounded_String := Null_Unbounded_String;
      Format               : Output_Format := Format_OCI;
      Include_Attestations : Boolean := False;

      procedure Show_Help is
      begin
         Put_Line ("Usage: ct unpack <bundle.ctp> -o <dir> [--format=oci|docker]");
         Put_Line ("");
         Put_Line ("Extract bundle to OCI layout on disk.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -h, --help           Show this help");
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
      end Show_Help;

   begin
      --  Show help if no arguments or --help/-h specified
      if Argument_Count < 2 or else
         Argument (2) = "--help" or else
         Argument (2) = "-h"
      then
         Show_Help;
         Set_Exit_Status (CT_Errors.Exit_Success);
         return;
      end if;

      --  Parse arguments
      declare
         I : Positive := 2;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if (Arg = "-o" or Arg = "--output") and I < Argument_Count then
                  I := I + 1;
                  Output_Path := To_Unbounded_String (Argument (I));

               elsif Arg'Length > 9 and then
                     Arg (Arg'First .. Arg'First + 8) = "--format="
               then
                  declare
                     Format_Str : constant String := Arg (Arg'First + 9 .. Arg'Last);
                  begin
                     if Format_Str = "oci" then
                        Format := Format_OCI;
                     elsif Format_Str = "docker" then
                        Format := Format_Docker;
                     else
                        Put_Line ("Error: Unknown format: " & Format_Str);
                        Put_Line ("  Supported formats: oci, docker");
                        Set_Exit_Status (CT_Errors.Exit_General_Failure);
                        return;
                     end if;
                  end;

               elsif Arg = "--include-attestations" then
                  Include_Attestations := True;

               elsif Arg (Arg'First) /= '-' and Length (Bundle_Path) = 0 then
                  Bundle_Path := To_Unbounded_String (Arg);
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate arguments
      if Length (Bundle_Path) = 0 then
         Put_Line ("Error: No bundle path specified");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      if Length (Output_Path) = 0 then
         Put_Line ("Error: Output path required (-o <dir>)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Str : constant String := To_String (Bundle_Path);
         Output_Str : constant String := To_String (Output_Path);
      begin
         if not Dir.Exists (Bundle_Str) then
            Put_Line ("Error: Bundle not found: " & Bundle_Str);
            Set_Exit_Status (CT_Errors.Exit_IO_Error);
            return;
         end if;

         Put_Line ("Unpacking bundle: " & Bundle_Str);
         Put_Line ("  Output: " & Output_Str);
         Put_Line ("  Format: " & (if Format = Format_OCI then "OCI" else "Docker"));

         --  Step 1: Verify bundle first
         Put_Line ("  Verifying bundle...");
         declare
            Verify_Opts : Cerro_Verify.Verify_Options := (
               Bundle_Path => Bundle_Path,
               Policy_Path => Null_Unbounded_String,
               Offline     => False,
               Verbose     => False
            );
            Verify_Result : constant Cerro_Verify.Verify_Result :=
               Cerro_Verify.Verify_Bundle (Verify_Opts);
         begin
            if not (Verify_Result.Code = Cerro_Verify.OK) then
               Put_Line ("  Verification failed: " & To_String (Verify_Result.Details));
               Set_Exit_Status (Ada.Command_Line.Exit_Status (
                  Cerro_Verify.To_Exit_Code (Verify_Result.Code)));
               return;
            end if;
            Put_Line ("  ✓ Bundle verified");
         end;

         --  Step 2: Create output directory (for OCI layout)
         if Format = Format_OCI then
            if not Dir.Exists (Output_Str) then
               Dir.Create_Path (Output_Str);
            end if;
         end if;

         --  Step 3: Extract bundle contents
         Put_Line ("  Extracting contents...");

         --  For now, use system tar command to extract
         --  Future: use Cerro_Tar for pure Ada extraction
         declare
            use GNAT.OS_Lib;
            Args    : Argument_List (1 .. 4);
            Success : Boolean;
         begin
            if Format = Format_OCI then
               --  Extract to directory
               Args (1) := new String'("-xf");
               Args (2) := new String'(Bundle_Str);
               Args (3) := new String'("-C");
               Args (4) := new String'(Output_Str);

               Spawn ("/usr/bin/tar", Args, Success);

               Free (Args (1));
               Free (Args (2));
               Free (Args (3));
               Free (Args (4));

               if Success then
                  Put_Line ("  ✓ Extracted to " & Output_Str);

                  --  Create OCI layout marker files if needed
                  declare
                     Layout_File : Ada.Text_IO.File_Type;
                     Index_Path  : constant String := Output_Str & "/oci-layout";
                  begin
                     if not Dir.Exists (Index_Path) then
                        Ada.Text_IO.Create (Layout_File, Ada.Text_IO.Out_File, Index_Path);
                        Ada.Text_IO.Put_Line (Layout_File, "{""imageLayoutVersion"": ""1.0.0""}");
                        Ada.Text_IO.Close (Layout_File);
                        Put_Line ("  Created oci-layout marker");
                     end if;
                  exception
                     when others =>
                        null;  --  Non-fatal if we can't create marker
                  end;

                  if Include_Attestations then
                     Put_Line ("  Attestations included in output");
                  end if;

                  Set_Exit_Status (CT_Errors.Exit_Success);
               else
                  Put_Line ("  Error: Extraction failed");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;

            else
               --  For Docker format, the .ctp is already a tar, we just verify and copy
               --  In production, we'd reformat to Docker-save compatible format
               Put_Line ("  Note: Docker format export requires conversion");
               Put_Line ("  For now, using direct tar copy");

               declare
                  Args2 : Argument_List (1 .. 2);
               begin
                  --  Just copy the bundle for now
                  Args2 (1) := new String'(Bundle_Str);
                  Args2 (2) := new String'(Output_Str);

                  Spawn ("/usr/bin/cp", Args2, Success);

                  Free (Args2 (1));
                  Free (Args2 (2));
               end;

               if Success then
                  Put_Line ("  ✓ Created " & Output_Str);
                  Put_Line ("");
                  Put_Line ("Load with:");
                  Put_Line ("  podman load < " & Output_Str);
                  Put_Line ("  docker load < " & Output_Str);
                  Set_Exit_Status (CT_Errors.Exit_Success);
               else
                  Put_Line ("  Error: Copy failed");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end if;
         end;
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
