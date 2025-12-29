--  Cerro Torre - Ship containers safely
--  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  "Ship containers safely" - the distribution complement to
--  Svalinn's "run containers nicely".

with Ada.Text_IO;
with Ada.Command_Line;
with Cerro_CLI;

procedure Cerro_Main is
   use Ada.Text_IO;
   use Ada.Command_Line;

   Version : constant String := "0.1.0-dev";
begin
   if Argument_Count = 0 then
      Put_Line ("ct - Ship containers safely");
      Put_Line ("");
      Put_Line ("Usage: ct <command> [options]");
      Put_Line ("");
      Put_Line ("Core commands:");
      Put_Line ("  pack <image> -o <file>    Pack OCI image into .ctp bundle");
      Put_Line ("  verify <bundle>           Verify a .ctp bundle");
      Put_Line ("  explain <bundle>          Show verification chain");
      Put_Line ("");
      Put_Line ("Key management:");
      Put_Line ("  keygen                    Generate signing keypair");
      Put_Line ("  key <subcommand>          Key management (list, import, export)");
      Put_Line ("");
      Put_Line ("Distribution (v0.2):");
      Put_Line ("  fetch <ref> -o <file>     Fetch bundle from registry");
      Put_Line ("  push <bundle> <dest>      Push bundle to registry");
      Put_Line ("  export <bundles> -o <ar>  Export for offline transfer");
      Put_Line ("  import <archive>          Import from offline archive");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --help, -h                Show this help");
      Put_Line ("  --version, -v             Show version");
      Put_Line ("");
      Put_Line ("Examples:");
      Put_Line ("  ct pack docker.io/library/nginx:1.26 -o nginx.ctp");
      Put_Line ("  ct verify nginx.ctp --policy strict.json");
      Put_Line ("  ct explain nginx.ctp");
      Put_Line ("");
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Command : constant String := Argument (1);
   begin
      if Command = "--help" or Command = "-h" then
         Put_Line ("ct " & Version);
         Put_Line ("Ship containers safely - supply-chain verified distribution");
         Put_Line ("");
         Put_Line ("Run 'ct' without arguments for usage.");
         Put_Line ("See: https://cerro-torre.org");

      elsif Command = "--version" or Command = "-v" then
         Put_Line ("ct " & Version);

      --  Core commands (MVP v0.1)
      elsif Command = "pack" then
         Cerro_CLI.Run_Pack;

      elsif Command = "verify" then
         Cerro_CLI.Run_Verify;

      elsif Command = "explain" then
         Cerro_CLI.Run_Explain;

      elsif Command = "keygen" then
         Cerro_CLI.Run_Keygen;

      elsif Command = "key" then
         Cerro_CLI.Run_Key;

      --  Distribution commands (v0.2)
      elsif Command = "fetch" then
         Cerro_CLI.Run_Fetch;

      elsif Command = "push" then
         Cerro_CLI.Run_Push;

      elsif Command = "export" then
         Cerro_CLI.Run_Export;

      elsif Command = "import" then
         Cerro_CLI.Run_Import;

      else
         Put_Line ("Unknown command: " & Command);
         Put_Line ("");
         Put_Line ("Run 'ct --help' for usage.");
         Set_Exit_Status (Failure);
      end if;
   end;
end Cerro_Main;
