--  Cerro Torre - Supply-chain-verified container distribution
--  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Command_Line;
with Cerro_CLI;

procedure Cerro_Main is
   use Ada.Text_IO;
   use Ada.Command_Line;
begin
   if Argument_Count = 0 then
      Put_Line ("Cerro Torre - Supply-chain-verified container distribution");
      Put_Line ("");
      Put_Line ("Usage: cerro <command> [options]");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  import <source>    Import package from upstream (debian:pkg, fedora:pkg)");
      Put_Line ("  build <manifest>   Build package from manifest");
      Put_Line ("  verify <manifest>  Verify manifest and provenance");
      Put_Line ("  export <package>   Export to OCI image or other format");
      Put_Line ("  inspect <package>  Show package details and provenance");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --help, -h         Show this help");
      Put_Line ("  --version, -v      Show version");
      Put_Line ("");
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Command : constant String := Argument (1);
   begin
      if Command = "--help" or Command = "-h" then
         Put_Line ("Cerro Torre v0.1.0-dev");
         Put_Line ("See: https://gitlab.com/cerro-torre/cerro-torre");

      elsif Command = "--version" or Command = "-v" then
         Put_Line ("cerro 0.1.0-dev");
         Put_Line ("Built with GNAT, SPARK verified core");

      elsif Command = "import" then
         Cerro_CLI.Run_Import;

      elsif Command = "build" then
         Cerro_CLI.Run_Build;

      elsif Command = "verify" then
         Cerro_CLI.Run_Verify;

      elsif Command = "export" then
         Cerro_CLI.Run_Export;

      elsif Command = "inspect" then
         Cerro_CLI.Run_Inspect;

      else
         Put_Line ("Unknown command: " & Command);
         Put_Line ("Run 'cerro --help' for usage.");
         Set_Exit_Status (Failure);
      end if;
   end;
end Cerro_Main;
