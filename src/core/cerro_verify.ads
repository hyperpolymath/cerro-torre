-------------------------------------------------------------------------------
--  Cerro_Verify - High-Level Verification API
--
--  This package provides the main entry points for verifying Cerro Torre
--  packages, combining manifest parsing, provenance checking, and
--  signature verification.
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Cerro_Manifest; use Cerro_Manifest;
with Cerro_Provenance; use Cerro_Provenance;

package Cerro_Verify is

   ---------------------------------------------------------------------------
   --  Overall Verification Status
   ---------------------------------------------------------------------------

   type Package_Status is
      (Valid,                     -- Package passes all checks
       Parse_Error,               -- Manifest could not be parsed
       Invalid_Manifest,          -- Manifest fails structural validation
       Provenance_Error,          -- Provenance chain verification failed
       Signature_Error,           -- Signature verification failed
       Content_Mismatch,          -- File contents don't match hashes
       Policy_Violation);         -- Security policy check failed

   type Package_Verification is record
      Status          : Package_Status := Parse_Error;
      Manifest_Result : Parse_Result := Invalid_Format;
      Prov_Result     : Verification_Result;
      Valid_Sigs      : Natural := 0;
      Total_Sigs      : Natural := 0;
      Files_Checked   : Natural := 0;
      Files_Valid     : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Main Verification Entry Points
   ---------------------------------------------------------------------------

   function Verify_Manifest_File (Path : String) return Package_Verification
      with Pre => Path'Length > 0;
   --  Verify a package from its manifest file path.
   --  @param Path Absolute path to the .ctp manifest file
   --  @return Verification result with detailed status

   function Verify_Manifest_String (Content : String) return Package_Verification
      with Pre => Content'Length > 0;
   --  Verify a package from manifest content string.
   --  @param Content The manifest file content as a string
   --  @return Verification result with detailed status

   function Verify_Package_Directory
      (Manifest_Path : String;
       Package_Root  : String) return Package_Verification
      with Pre => Manifest_Path'Length > 0 and Package_Root'Length > 0;
   --  Verify a package including file content checks.
   --  @param Manifest_Path Path to the .ctp manifest file
   --  @param Package_Root Root directory containing package files
   --  @return Verification result including file content verification

   ---------------------------------------------------------------------------
   --  Verification Policy
   ---------------------------------------------------------------------------

   type Verification_Policy is record
      Require_Build_Sig      : Boolean := True;   -- Require build attestation
      Require_Maintainer_Sig : Boolean := False;  -- Require maintainer attestation
      Min_Signatures         : Positive := 1;     -- Minimum valid signatures
      Max_Sig_Age_Days       : Natural := 365;    -- Max signature age (0 = unlimited)
      Check_File_Contents    : Boolean := True;   -- Verify file hashes
      Allow_Unknown_Signers  : Boolean := False;  -- Accept unknown signing keys
   end record;

   Default_Policy : constant Verification_Policy := (others => <>);

   function Verify_With_Policy
      (Content : String;
       Policy  : Verification_Policy) return Package_Verification
      with Pre => Content'Length > 0;
   --  Verify a package using a custom verification policy.
   --  @param Content The manifest file content
   --  @param Policy Custom verification requirements
   --  @return Verification result

   ---------------------------------------------------------------------------
   --  Result Formatting
   ---------------------------------------------------------------------------

   function Status_To_String (S : Package_Status) return String;
   --  Get human-readable status description.
   --  @param S The status value
   --  @return Description string

   function Format_Verification_Result
      (V : Package_Verification) return String;
   --  Format a complete verification result for display.
   --  @param V The verification result
   --  @return Formatted multi-line report

   ---------------------------------------------------------------------------
   --  Quick Check Functions
   ---------------------------------------------------------------------------

   function Is_Valid (V : Package_Verification) return Boolean is
      (V.Status = Valid)
      with Inline;
   --  Check if verification passed.
   --  @param V The verification result
   --  @return True if package is valid

   function Has_Valid_Signatures (V : Package_Verification) return Boolean is
      (V.Valid_Sigs > 0)
      with Inline;
   --  Check if package has any valid signatures.
   --  @param V The verification result
   --  @return True if at least one signature is valid

end Cerro_Verify;
