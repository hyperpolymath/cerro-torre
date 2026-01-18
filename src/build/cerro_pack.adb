--  Cerro_Pack - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

with Ada.Text_IO;
with Ada.Directories;
with Cerro_Manifest;
with Cerro_Crypto;
with Cerro_Tar;

package body Cerro_Pack is

   package TIO renames Ada.Text_IO;
   package Dir renames Ada.Directories;

   ---------------------------------------------------------------------------
   --  Hash_File - Compute SHA-256 hash of file contents
   ---------------------------------------------------------------------------

   function Hash_File (Path : String) return String is
      File    : TIO.File_Type;
      Buffer  : String (1 .. 8192);
      Last    : Natural;
      Content : Unbounded_String := Null_Unbounded_String;
   begin
      if not Dir.Exists (Path) then
         return "";
      end if;

      --  Read file contents
      begin
         TIO.Open (File, TIO.In_File, Path);
         while not TIO.End_Of_File (File) loop
            TIO.Get_Line (File, Buffer, Last);
            Append (Content, Buffer (1 .. Last));
            Append (Content, ASCII.LF);
         end loop;
         TIO.Close (File);
      exception
         when others =>
            if TIO.Is_Open (File) then
               TIO.Close (File);
            end if;
            return "";
      end;

      --  Compute hash
      declare
         Hash : constant Cerro_Crypto.SHA256_Digest :=
            Cerro_Crypto.Compute_SHA256 (To_String (Content));
      begin
         return Cerro_Crypto.Bytes_To_Hex (Hash);
      end;
   end Hash_File;

   ---------------------------------------------------------------------------
   --  Write_Summary_Json - Generate summary.json with all content hashes
   ---------------------------------------------------------------------------

   procedure Write_Summary_Json
      (Path          : String;
       Manifest      : Cerro_Manifest.Manifest;
       Manifest_Hash : String)
   is
      File : TIO.File_Type;
   begin
      TIO.Create (File, TIO.Out_File, Path);

      TIO.Put_Line (File, "{");
      TIO.Put_Line (File, "  ""ctp_version"": ""1.0"",");
      TIO.Put_Line (File, "  ""package"": {");
      TIO.Put_Line (File, "    ""name"": """ & To_String (Manifest.Metadata.Name) & """,");
      TIO.Put_Line (File, "    ""version"": """ & To_String (Manifest.Metadata.Version.Upstream) & """");
      TIO.Put_Line (File, "  },");
      TIO.Put_Line (File, "  ""content"": {");
      TIO.Put_Line (File, "    ""manifest_sha256"": """ & Manifest_Hash & """");
      TIO.Put_Line (File, "  },");
      TIO.Put_Line (File, "  ""attestations"": []");
      TIO.Put_Line (File, "}");

      TIO.Close (File);
   exception
      when others =>
         if TIO.Is_Open (File) then
            TIO.Close (File);
         end if;
   end Write_Summary_Json;

   ---------------------------------------------------------------------------
   --  Add_Directory_To_Archive - Recursively add directory contents
   ---------------------------------------------------------------------------

   procedure Add_Directory_To_Archive
     (Writer      : in out Cerro_Tar.Tar_Writer;
      Dir_Path    : String;
      Prefix      : String;
      File_Count  : in out Natural)
   is
      use Dir;

      Search   : Search_Type;
      Dir_Ent  : Directory_Entry_Type;
   begin
      Start_Search (Search, Dir_Path, "*", (others => True));

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);

         declare
            Name : constant String := Simple_Name (Dir_Ent);
            Full_Path : constant String := Full_Name (Dir_Ent);
         begin
            --  Skip . and ..
            if Name /= "." and Name /= ".." then
               case Kind (Dir_Ent) is
                  when Ordinary_File =>
                     declare
                        Archive_Name : constant String := Prefix & "/" & Name;
                     begin
                        Cerro_Tar.Add_File_From_Disk (Writer, Archive_Name, Full_Path);
                        File_Count := File_Count + 1;
                     end;

                  when Directory =>
                     --  Recurse into subdirectory
                     Add_Directory_To_Archive
                       (Writer, Full_Path, Prefix & "/" & Name, File_Count);

                  when Special_File =>
                     null;  --  Skip special files
               end case;
            end if;
         end;
      end loop;

      End_Search (Search);
   exception
      when others =>
         null;  --  Directory access error - continue
   end Add_Directory_To_Archive;

   ---------------------------------------------------------------------------
   --  Create_Tar_Bundle - Create tar archive with manifest and summary
   ---------------------------------------------------------------------------

   procedure Create_Tar_Bundle
      (Output_Path   : String;
       Manifest_Path : String;
       Summary_Path  : String;
       Source_Dir    : String;
       Verbose       : Boolean;
       Success       : out Boolean;
       Error_Msg     : out Unbounded_String)
   is
      Archive_Path : constant String := Output_Path & ".ctp";
      Writer       : Cerro_Tar.Tar_Writer;
      Source_Count : Natural := 0;
   begin
      --  Create real POSIX ustar tar archive
      begin
         --  Delete existing archive if present
         if Dir.Exists (Archive_Path) then
            Dir.Delete_File (Archive_Path);
         end if;

         --  Create new tar archive
         Cerro_Tar.Create (Writer, Archive_Path);

         --  Add manifest.ctp (use base name in archive)
         Cerro_Tar.Add_File_From_Disk (Writer, "manifest.ctp", Manifest_Path);

         --  Add summary.json
         Cerro_Tar.Add_File_From_Disk (Writer, "summary.json", Summary_Path);

         --  Add source files if directory specified
         if Source_Dir'Length > 0 and then Dir.Exists (Source_Dir) then
            if Verbose then
               TIO.Put_Line ("Including sources from: " & Source_Dir);
            end if;
            Add_Directory_To_Archive (Writer, Source_Dir, "src", Source_Count);
            if Verbose then
               TIO.Put_Line ("Added" & Natural'Image (Source_Count) & " source files");
            end if;
         end if;

         --  Finalize archive (writes end-of-archive markers)
         Cerro_Tar.Close (Writer);

         Success := True;
         Error_Msg := To_Unbounded_String ("Bundle created: " & Archive_Path);
      exception
         when others =>
            if Cerro_Tar.Is_Open (Writer) then
               Cerro_Tar.Close (Writer);
            end if;
            Success := False;
            Error_Msg := To_Unbounded_String ("Failed to create tar archive");
      end;
   end Create_Tar_Bundle;

   ---------------------------------------------------------------------------
   --  Create_Bundle - Main entry point
   ---------------------------------------------------------------------------

   function Create_Bundle (Opts : Pack_Options) return Pack_Result is
      Manifest_Path_Str : constant String := To_String (Opts.Manifest_Path);
      Output_Path_Str   : constant String := To_String (Opts.Output_Path);
   begin
      --  Check manifest exists
      if not Dir.Exists (Manifest_Path_Str) then
         return (Success => False,
                 Bundle_Path => Null_Unbounded_String,
                 Error_Msg => To_Unbounded_String ("Manifest not found: " & Manifest_Path_Str));
      end if;

      --  Parse manifest
      declare
         Parse_Result : constant Cerro_Manifest.Parse_Result :=
            Cerro_Manifest.Parse_File (Manifest_Path_Str);
      begin
         if not Parse_Result.Success then
            return (Success => False,
                    Bundle_Path => Null_Unbounded_String,
                    Error_Msg => To_Unbounded_String ("Failed to parse manifest: " &
                                                       To_String (Parse_Result.Error_Msg)));
         end if;

         if Opts.Verbose then
            TIO.Put_Line ("Parsed manifest: " & To_String (Parse_Result.Value.Metadata.Name));
            TIO.Put_Line ("Version: " & To_String (Parse_Result.Value.Metadata.Version.Upstream));
         end if;

         --  Compute manifest hash
         declare
            Manifest_Hash : constant String := Hash_File (Manifest_Path_Str);
            Summary_Path  : constant String := "/tmp/ct-summary-" &
                            To_String (Parse_Result.Value.Metadata.Name) & ".json";
         begin
            if Opts.Verbose then
               TIO.Put_Line ("Manifest SHA256: " & Manifest_Hash);
            end if;

            --  Write summary.json
            Write_Summary_Json (Summary_Path, Parse_Result.Value, Manifest_Hash);

            if Opts.Verbose then
               TIO.Put_Line ("Created summary: " & Summary_Path);
            end if;

            --  Create bundle
            declare
               Tar_Success : Boolean;
               Tar_Error   : Unbounded_String;
               Source_Dir  : constant String := To_String (Opts.Source_Dir);
            begin
               Create_Tar_Bundle
                 (Output_Path   => Output_Path_Str,
                  Manifest_Path => Manifest_Path_Str,
                  Summary_Path  => Summary_Path,
                  Source_Dir    => Source_Dir,
                  Verbose       => Opts.Verbose,
                  Success       => Tar_Success,
                  Error_Msg     => Tar_Error);

               if Tar_Success then
                  return (Success => True,
                          Bundle_Path => To_Unbounded_String (Output_Path_Str & ".ctp"),
                          Error_Msg => Null_Unbounded_String);
               else
                  return (Success => False,
                          Bundle_Path => Null_Unbounded_String,
                          Error_Msg => Tar_Error);
               end if;
            end;
         end;
      end;
   end Create_Bundle;

end Cerro_Pack;
