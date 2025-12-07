--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Command_Line;

package body Cerro_CLI is

   use Ada.Text_IO;
   use Ada.Command_Line;

   -----------
   -- Import --
   -----------

   procedure Run_Import is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: cerro import <source>");
         Put_Line ("");
         Put_Line ("Sources:");
         Put_Line ("  debian:<package>/<version>   Import from Debian");
         Put_Line ("  fedora:<package>-<version>   Import from Fedora");
         Put_Line ("  alpine:<package>=<version>   Import from Alpine");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  cerro import debian:hello/2.10-3");
         Put_Line ("  cerro import debian:nginx/1.26.0-1");
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
         Source : constant String := Argument (2);
      begin
         Put_Line ("Importing from: " & Source);
         Put_Line ("(Not yet implemented)");
         --  TODO: Parse source specifier
         --  TODO: Fetch source package
         --  TODO: Convert to CTP manifest
         --  TODO: Verify upstream signatures
         Set_Exit_Status (Failure);
      end;
   end Run_Import;

   -----------
   -- Build --
   -----------

   procedure Run_Build is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: cerro build <manifest.ctp>");
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
         Manifest_Path : constant String := Argument (2);
      begin
         Put_Line ("Building from manifest: " & Manifest_Path);
         Put_Line ("(Not yet implemented)");
         --  TODO: Parse manifest
         --  TODO: Verify provenance
         --  TODO: Fetch sources
         --  TODO: Execute build phases
         --  TODO: Generate attestations
         Set_Exit_Status (Failure);
      end;
   end Run_Build;

   ------------
   -- Verify --
   ------------

   procedure Run_Verify is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: cerro verify <manifest.ctp>");
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
         Manifest_Path : constant String := Argument (2);
      begin
         Put_Line ("Verifying manifest: " & Manifest_Path);
         Put_Line ("(Not yet implemented)");
         --  TODO: Parse manifest
         --  TODO: Check all hashes
         --  TODO: Verify signatures
         --  TODO: Validate provenance chain
         --  TODO: Check SBOM completeness
         Set_Exit_Status (Failure);
      end;
   end Run_Verify;

   ------------
   -- Export --
   ------------

   procedure Run_Export is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: cerro export [--format=oci|ostree|deb] <package>");
         Set_Exit_Status (Failure);
         return;
      end if;

      Put_Line ("Exporting package...");
      Put_Line ("(Not yet implemented)");
      --  TODO: Parse format option
      --  TODO: Load built package
      --  TODO: Generate output format
      --  TODO: Sign output
      Set_Exit_Status (Failure);
   end Run_Export;

   -------------
   -- Inspect --
   -------------

   procedure Run_Inspect is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: cerro inspect <package>");
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
         Package_Name : constant String := Argument (2);
      begin
         Put_Line ("Inspecting package: " & Package_Name);
         Put_Line ("(Not yet implemented)");
         --  TODO: Load manifest
         --  TODO: Display metadata
         --  TODO: Display provenance
         --  TODO: Display dependencies
         --  TODO: Display attestations
         Set_Exit_Status (Failure);
      end;
   end Run_Inspect;

end Cerro_CLI;
