-------------------------------------------------------------------------------
--  Cerro_Verify - Implementation
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Cerro_Verify is

   ---------------------------------------------------------------------------
   --  Verify_Manifest_File
   ---------------------------------------------------------------------------

   function Verify_Manifest_File (Path : String) return Package_Verification is
      Result : Package_Verification;
   begin
      --  Check file exists
      if not Ada.Directories.Exists (Path) then
         Result.Status := Parse_Error;
         return Result;
      end if;

      --  TODO: Read file and call Verify_Manifest_String
      Result.Status := Parse_Error;
      return Result;
   end Verify_Manifest_File;

   ---------------------------------------------------------------------------
   --  Verify_Manifest_String
   ---------------------------------------------------------------------------

   function Verify_Manifest_String (Content : String) return Package_Verification
   is
      Result  : Package_Verification;
      M       : Manifest;
      P_Stat  : Parse_Result;
   begin
      --  Parse manifest
      Parse_Manifest (Content, M, P_Stat);
      Result.Manifest_Result := P_Stat;

      if P_Stat /= Success then
         Result.Status := Parse_Error;
         return Result;
      end if;

      --  Validate manifest structure
      if not Validate_Manifest (M) then
         Result.Status := Invalid_Manifest;
         return Result;
      end if;

      --  Verify provenance chain
      Result.Prov_Result := Verify_Chain_Integrity (M);
      if Result.Prov_Result.Status /= Verified then
         Result.Status := Provenance_Error;
         return Result;
      end if;

      --  Count signatures
      Result.Total_Sigs := Natural (M.Signatures.Length);
      Result.Valid_Sigs := Count_Valid_Signatures (M);

      if Result.Valid_Sigs = 0 then
         Result.Status := Signature_Error;
         return Result;
      end if;

      Result.Status := Valid;
      return Result;
   end Verify_Manifest_String;

   ---------------------------------------------------------------------------
   --  Verify_Package_Directory
   ---------------------------------------------------------------------------

   function Verify_Package_Directory
      (Manifest_Path : String;
       Package_Root  : String) return Package_Verification
   is
      Result : Package_Verification;
   begin
      --  First verify manifest
      Result := Verify_Manifest_File (Manifest_Path);

      if Result.Status /= Valid then
         return Result;
      end if;

      --  TODO: Verify file contents against hashes in manifest
      --  For each file in manifest:
      --    1. Check file exists in Package_Root
      --    2. Compute hash
      --    3. Compare to expected hash
      Result.Files_Checked := 0;
      Result.Files_Valid := 0;

      return Result;
   end Verify_Package_Directory;

   ---------------------------------------------------------------------------
   --  Verify_With_Policy
   ---------------------------------------------------------------------------

   function Verify_With_Policy
      (Content : String;
       Policy  : Verification_Policy) return Package_Verification
   is
      Result : Package_Verification;
   begin
      --  Start with basic verification
      Result := Verify_Manifest_String (Content);

      if Result.Status /= Valid then
         return Result;
      end if;

      --  Apply policy checks
      if Policy.Require_Build_Sig then
         --  TODO: Check for build attestation
         null;
      end if;

      if Policy.Require_Maintainer_Sig then
         --  TODO: Check for maintainer attestation
         null;
      end if;

      if Result.Valid_Sigs < Policy.Min_Signatures then
         Result.Status := Signature_Error;
         return Result;
      end if;

      --  TODO: Check signature age against Max_Sig_Age_Days

      return Result;
   end Verify_With_Policy;

   ---------------------------------------------------------------------------
   --  Status_To_String
   ---------------------------------------------------------------------------

   function Status_To_String (S : Package_Status) return String is
   begin
      case S is
         when Valid =>
            return "Valid - all checks passed";
         when Parse_Error =>
            return "Error - manifest could not be parsed";
         when Invalid_Manifest =>
            return "Error - manifest fails validation";
         when Provenance_Error =>
            return "Error - provenance chain verification failed";
         when Signature_Error =>
            return "Error - signature verification failed";
         when Content_Mismatch =>
            return "Error - file contents do not match hashes";
         when Policy_Violation =>
            return "Error - security policy check failed";
      end case;
   end Status_To_String;

   ---------------------------------------------------------------------------
   --  Format_Verification_Result
   ---------------------------------------------------------------------------

   function Format_Verification_Result
      (V : Package_Verification) return String
   is
      pragma SPARK_Mode (Off);  -- String operations not in SPARK subset

      LF : constant Character := Character'Val (10);
   begin
      return
         "Verification Result" & LF &
         "===================" & LF &
         "Status: " & Status_To_String (V.Status) & LF &
         "Signatures: " & Natural'Image (V.Valid_Sigs) &
         " valid of " & Natural'Image (V.Total_Sigs) & " total" & LF &
         "Files: " & Natural'Image (V.Files_Valid) &
         " valid of " & Natural'Image (V.Files_Checked) & " checked";
   end Format_Verification_Result;

end Cerro_Verify;
