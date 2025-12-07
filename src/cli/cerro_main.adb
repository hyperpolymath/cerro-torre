-------------------------------------------------------------------------------
--  Cerro_Main - Command Line Interface for Cerro Torre
--
--  Main entry point for the cerro command-line tool.
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Cerro_Main is

   ---------------------------------------------------------------------------
   --  Version Information
   ---------------------------------------------------------------------------

   Version : constant String := "0.1.0-dev";

   ---------------------------------------------------------------------------
   --  Help Text
   ---------------------------------------------------------------------------

   procedure Print_Help is
   begin
      Put_Line ("Cerro Torre - Supply Chain Verified Linux Distribution");
      Put_Line ("Version: " & Version);
      New_Line;
      Put_Line ("Usage: cerro <command> [options] [arguments]");
      New_Line;
      Put_Line ("Commands:");
      Put_Line ("  import <source>      Import package from upstream distribution");
      Put_Line ("                       Examples: debian:hello/2.10-3");
      Put_Line ("                                 fedora:nginx/1.26.0");
      New_Line;
      Put_Line ("  build <manifest>     Build package from .ctp manifest");
      Put_Line ("                       Example: cerro build manifests/hello.ctp");
      New_Line;
      Put_Line ("  verify <manifest>    Verify package manifest and signatures");
      Put_Line ("                       Example: cerro verify hello.ctp");
      New_Line;
      Put_Line ("  export <options>     Export package to output format");
      Put_Line ("    --format=oci       Export as OCI container image");
      Put_Line ("    --format=ostree    Export as OSTree commit");
      Put_Line ("    --format=deb       Export as Debian package");
      New_Line;
      Put_Line ("  sign <manifest>      Sign a package manifest");
      Put_Line ("    --key=<keyfile>    Path to Ed25519 private key");
      Put_Line ("    --type=<type>      Attestation type (build/maintainer)");
      New_Line;
      Put_Line ("  info <manifest>      Display package information");
      Put_Line ("  provenance <pkg>     Show complete provenance chain");
      New_Line;
      Put_Line ("Global Options:");
      Put_Line ("  --help, -h           Show this help message");
      Put_Line ("  --version, -v        Show version information");
      Put_Line ("  --verbose            Enable verbose output");
      Put_Line ("  --quiet              Suppress non-error output");
      New_Line;
      Put_Line ("For more information, see: https://cerro-torre.org/docs");
   end Print_Help;

   ---------------------------------------------------------------------------
   --  Version Display
   ---------------------------------------------------------------------------

   procedure Print_Version is
   begin
      Put_Line ("cerro " & Version);
      Put_Line ("Copyright 2024 Cerro Torre Cooperative");
      Put_Line ("License: Apache-2.0 OR MIT");
   end Print_Version;

   ---------------------------------------------------------------------------
   --  Command Handlers (Stubs)
   ---------------------------------------------------------------------------

   procedure Handle_Import (Source : String) is
   begin
      Put_Line ("Import command not yet implemented");
      Put_Line ("Would import from: " & Source);
   end Handle_Import;

   procedure Handle_Build (Manifest : String) is
   begin
      Put_Line ("Build command not yet implemented");
      Put_Line ("Would build: " & Manifest);
   end Handle_Build;

   procedure Handle_Verify (Manifest : String) is
   begin
      Put_Line ("Verify command not yet implemented");
      Put_Line ("Would verify: " & Manifest);
   end Handle_Verify;

   procedure Handle_Export (Package_Name : String; Format : String) is
   begin
      Put_Line ("Export command not yet implemented");
      Put_Line ("Would export: " & Package_Name & " as " & Format);
   end Handle_Export;

   procedure Handle_Info (Manifest : String) is
   begin
      Put_Line ("Info command not yet implemented");
      Put_Line ("Would show info for: " & Manifest);
   end Handle_Info;

   procedure Handle_Provenance (Package_Name : String) is
   begin
      Put_Line ("Provenance command not yet implemented");
      Put_Line ("Would show provenance for: " & Package_Name);
   end Handle_Provenance;

   ---------------------------------------------------------------------------
   --  Main Logic
   ---------------------------------------------------------------------------

begin
   --  Check for no arguments
   if Argument_Count = 0 then
      Print_Help;
      Set_Exit_Status (0);
      return;
   end if;

   --  Parse first argument as command
   declare
      Command : constant String := Argument (1);
   begin
      if Command = "--help" or Command = "-h" then
         Print_Help;

      elsif Command = "--version" or Command = "-v" then
         Print_Version;

      elsif Command = "import" then
         if Argument_Count < 2 then
            Put_Line ("Error: import requires a source argument");
            Put_Line ("Usage: cerro import <source>");
            Set_Exit_Status (1);
         else
            Handle_Import (Argument (2));
         end if;

      elsif Command = "build" then
         if Argument_Count < 2 then
            Put_Line ("Error: build requires a manifest argument");
            Put_Line ("Usage: cerro build <manifest>");
            Set_Exit_Status (1);
         else
            Handle_Build (Argument (2));
         end if;

      elsif Command = "verify" then
         if Argument_Count < 2 then
            Put_Line ("Error: verify requires a manifest argument");
            Put_Line ("Usage: cerro verify <manifest>");
            Set_Exit_Status (1);
         else
            Handle_Verify (Argument (2));
         end if;

      elsif Command = "export" then
         if Argument_Count < 2 then
            Put_Line ("Error: export requires package and format arguments");
            Put_Line ("Usage: cerro export --format=<format> <package>");
            Set_Exit_Status (1);
         else
            --  TODO: Proper argument parsing for --format
            Handle_Export (Argument (2), "oci");
         end if;

      elsif Command = "info" then
         if Argument_Count < 2 then
            Put_Line ("Error: info requires a manifest argument");
            Put_Line ("Usage: cerro info <manifest>");
            Set_Exit_Status (1);
         else
            Handle_Info (Argument (2));
         end if;

      elsif Command = "provenance" then
         if Argument_Count < 2 then
            Put_Line ("Error: provenance requires a package argument");
            Put_Line ("Usage: cerro provenance <package>");
            Set_Exit_Status (1);
         else
            Handle_Provenance (Argument (2));
         end if;

      else
         Put_Line ("Error: Unknown command '" & Command & "'");
         Put_Line ("Run 'cerro --help' for usage information");
         Set_Exit_Status (1);
      end if;
   end;

end Cerro_Main;
