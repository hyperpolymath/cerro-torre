-------------------------------------------------------------------------------
--  Cerro_Manifest - Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Cerro_Manifest is

   ---------------------------------------------------------------------------
   --  Is_Valid_Package_Name
   ---------------------------------------------------------------------------

   function Is_Valid_Package_Name (Name : String) return Boolean is
   begin
      --  Check length
      if Name'Length < 2 or Name'Length > Max_Package_Name_Length then
         return False;
      end if;

      --  First character must be lowercase letter
      if Name (Name'First) not in 'a' .. 'z' then
         return False;
      end if;

      --  Check all characters and boundaries
      for I in Name'Range loop
         declare
            C : constant Character := Name (I);
         begin
            --  Valid characters: a-z, 0-9, -, +, .
            if C not in 'a' .. 'z' | '0' .. '9' | '-' | '+' | '.' then
               return False;
            end if;

            --  Cannot start or end with -, +, or .
            if (I = Name'First or I = Name'Last) and
               C in '-' | '+' | '.'
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Is_Valid_Package_Name;

   ---------------------------------------------------------------------------
   --  Parse_Version
   ---------------------------------------------------------------------------

   function Parse_Version (S : String) return Version is
      Result : Version;
      Colon_Pos : Natural := 0;
      Dash_Pos  : Natural := 0;
   begin
      --  Find epoch separator ':'
      for I in S'Range loop
         if S (I) = ':' then
            Colon_Pos := I;
            exit;
         end if;
      end loop;

      --  Find revision separator '-' (last occurrence)
      for I in reverse S'Range loop
         if S (I) = '-' then
            Dash_Pos := I;
            exit;
         end if;
      end loop;

      --  Parse epoch if present
      if Colon_Pos > 0 then
         declare
            Epoch_Str : constant String := S (S'First .. Colon_Pos - 1);
         begin
            Result.Epoch := Natural'Value (Epoch_Str);
         exception
            when others =>
               Result.Epoch := 0;
         end;
      end if;

      --  Determine upstream version boundaries
      declare
         Upstream_First : constant Positive :=
            (if Colon_Pos > 0 then Colon_Pos + 1 else S'First);
         Upstream_Last : constant Positive :=
            (if Dash_Pos > Upstream_First then Dash_Pos - 1 else S'Last);
      begin
         Result.Upstream := To_Unbounded_String (S (Upstream_First .. Upstream_Last));

         --  Parse revision if present
         if Dash_Pos > Upstream_First then
            declare
               Rev_Str : constant String := S (Dash_Pos + 1 .. S'Last);
            begin
               Result.Revision := Positive'Value (Rev_Str);
            exception
               when others =>
                  Result.Revision := 1;
            end;
         end if;
      end;

      return Result;
   end Parse_Version;

   ---------------------------------------------------------------------------
   --  Version_To_String
   ---------------------------------------------------------------------------

   function Version_To_String (V : Version) return String is
      Epoch_Part : constant String :=
         (if V.Epoch > 0
          then Natural'Image (V.Epoch) (2 .. Natural'Image (V.Epoch)'Last) & ":"
          else "");
      Rev_Part : constant String :=
         "-" & Positive'Image (V.Revision) (2 .. Positive'Image (V.Revision)'Last);
   begin
      return Epoch_Part & To_String (V.Upstream) & Rev_Part;
   end Version_To_String;

   ---------------------------------------------------------------------------
   --  Compare_Versions
   ---------------------------------------------------------------------------

   function Compare_Versions (Left, Right : Version) return Integer is
   begin
      --  Compare epochs first
      if Left.Epoch < Right.Epoch then
         return -1;
      elsif Left.Epoch > Right.Epoch then
         return 1;
      end if;

      --  Compare upstream versions (simple string comparison for now)
      --  TODO: Implement proper version comparison (like dpkg --compare-versions)
      declare
         L : constant String := To_String (Left.Upstream);
         R : constant String := To_String (Right.Upstream);
      begin
         if L < R then
            return -1;
         elsif L > R then
            return 1;
         end if;
      end;

      --  Compare revisions
      if Left.Revision < Right.Revision then
         return -1;
      elsif Left.Revision > Right.Revision then
         return 1;
      end if;

      return 0;
   end Compare_Versions;

   ---------------------------------------------------------------------------
   --  Is_Valid_Hash
   ---------------------------------------------------------------------------

   function Is_Valid_Hash (H : Hash_Value) return Boolean is
      Len : constant Natural := Length (H.Digest);
   begin
      case H.Algorithm is
         when SHA256 =>
            return Len = 64;
         when SHA384 =>
            return Len = 96;
         when SHA512 =>
            return Len = 128;
         when Blake3 =>
            return Len = 64;
      end case;
   end Is_Valid_Hash;

   ---------------------------------------------------------------------------
   --  Parse_Manifest (Placeholder)
   ---------------------------------------------------------------------------

   procedure Parse_Manifest
      (Input   : String;
       Result  : out Manifest;
       Status  : out Parse_Result)
   is
      pragma Unreferenced (Input);
   begin
      --  TODO: Implement TOML parsing
      --  This is a placeholder that returns an empty manifest with error status

      Result := (
         Manifest_Version => Null_Unbounded_String,
         Name             => Null_Unbounded_String,
         Pkg_Version      => (Epoch => 0, Upstream => Null_Unbounded_String, Revision => 1),
         Summary          => Null_Unbounded_String,
         Description      => Null_Unbounded_String,
         Source           => (others => <>),
         Build            => (others => <>),
         Runtime_Deps     => String_Vectors.Empty_Vector,
         Files            => File_Vectors.Empty_Vector,
         Signatures       => Attestation_Vectors.Empty_Vector
      );

      Status := Invalid_Format;  -- Not yet implemented
   end Parse_Manifest;

   ---------------------------------------------------------------------------
   --  Validate_Manifest
   ---------------------------------------------------------------------------

   function Validate_Manifest (M : Manifest) return Boolean is
   begin
      --  Check required fields
      if Length (M.Name) = 0 then
         return False;
      end if;

      if not Is_Valid_Package_Name (To_String (M.Name)) then
         return False;
      end if;

      if Length (M.Summary) = 0 then
         return False;
      end if;

      if Length (M.Source.Upstream_URL) = 0 then
         return False;
      end if;

      if not Is_Valid_Hash (M.Source.Upstream_Hash) then
         return False;
      end if;

      return True;
   end Validate_Manifest;

   ---------------------------------------------------------------------------
   --  Manifest_To_String (Placeholder)
   ---------------------------------------------------------------------------

   function Manifest_To_String (M : Manifest) return String is
      pragma Unreferenced (M);
   begin
      --  TODO: Implement TOML serialization
      return "";
   end Manifest_To_String;

end Cerro_Manifest;
