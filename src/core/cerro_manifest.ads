-------------------------------------------------------------------------------
--  Cerro_Manifest - Package Manifest Types and Parsing
--
--  This package defines the core data types for Cerro Torre Package (CTP)
--  manifests and provides parsing/validation functionality.
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar;

package Cerro_Manifest is

   ---------------------------------------------------------------------------
   --  Package Name Validation
   ---------------------------------------------------------------------------

   Max_Package_Name_Length : constant := 128;

   subtype Package_Name_Length is Natural range 2 .. Max_Package_Name_Length;

   function Is_Valid_Package_Name (Name : String) return Boolean
      with Global => null;
   --  Validate a package name according to Cerro Torre naming rules:
   --  - Starts with lowercase letter
   --  - Contains only: a-z, 0-9, -, +, .
   --  - Length between 2 and 128 characters
   --  - Does not start or end with -, +, or .

   ---------------------------------------------------------------------------
   --  Version Types
   ---------------------------------------------------------------------------

   type Version is record
      Epoch    : Natural := 0;
      Upstream : Unbounded_String;
      Revision : Positive := 1;
   end record;

   function Parse_Version (S : String) return Version
      with Pre => S'Length > 0;
   --  Parse a version string in format [epoch:]upstream[-revision]

   function Version_To_String (V : Version) return String;
   --  Convert version back to string representation

   function Compare_Versions (Left, Right : Version) return Integer
      with Post => Compare_Versions'Result in -1 .. 1;
   --  Compare two versions. Returns:
   --  -1 if Left < Right
   --   0 if Left = Right
   --   1 if Left > Right

   ---------------------------------------------------------------------------
   --  Hash Types
   ---------------------------------------------------------------------------

   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3);

   type Hash_Value is record
      Algorithm : Hash_Algorithm := SHA256;
      Digest    : Unbounded_String;
   end record;

   function Is_Valid_Hash (H : Hash_Value) return Boolean;
   --  Validate that the hash digest has correct length for its algorithm

   ---------------------------------------------------------------------------
   --  Provenance Types
   ---------------------------------------------------------------------------

   type Patch_Entry is record
      Name        : Unbounded_String;
      Hash        : Hash_Value;
      Origin      : Unbounded_String;
      Description : Unbounded_String;
   end record;

   package Patch_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Patch_Entry);

   type Provenance is record
      Upstream_URL       : Unbounded_String;
      Upstream_Hash      : Hash_Value;
      Upstream_Signature : Unbounded_String;  -- Optional
      Upstream_Signer    : Unbounded_String;  -- Optional
      Imported_From      : Unbounded_String;  -- e.g., "debian:nginx/1.26.0-1"
      Import_Date        : Ada.Calendar.Time;
      Patches            : Patch_Vectors.Vector;
   end record;

   ---------------------------------------------------------------------------
   --  File Entry Types
   ---------------------------------------------------------------------------

   type File_Type is
      (Executable, Library, Header, Configuration,
       Documentation, Data, License);

   type File_Entry is record
      Path      : Unbounded_String;
      Hash      : Hash_Value;
      Mode      : Unbounded_String;  -- e.g., "0755"
      File_Kind : File_Type := Data;
   end record;

   package File_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => File_Entry);

   ---------------------------------------------------------------------------
   --  Dependency Types
   ---------------------------------------------------------------------------

   package String_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Unbounded_String);

   ---------------------------------------------------------------------------
   --  Signature Types
   ---------------------------------------------------------------------------

   type Attestation_Type is (Build, Maintainer, Audit, Security);

   type Attestation is record
      Kind        : Attestation_Type := Build;
      Signer      : Unbounded_String;
      Public_Key  : Unbounded_String;  -- Format: "ed25519:base64..."
      Signature   : Unbounded_String;  -- Base64-encoded
      Timestamp   : Ada.Calendar.Time;
   end record;

   package Attestation_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Attestation);

   ---------------------------------------------------------------------------
   --  Build Configuration
   ---------------------------------------------------------------------------

   type Build_System is (Autotools, CMake, Meson, Make, Custom);

   type Build_Config is record
      System           : Build_System := Autotools;
      Dependencies     : String_Vectors.Vector;
      Configure_Cmd    : Unbounded_String;
      Build_Cmd        : Unbounded_String;
      Install_Cmd      : Unbounded_String;
   end record;

   ---------------------------------------------------------------------------
   --  Main Manifest Type
   ---------------------------------------------------------------------------

   type Manifest is record
      Manifest_Version : Unbounded_String;
      Name             : Unbounded_String;
      Pkg_Version      : Version;
      Summary          : Unbounded_String;
      Description      : Unbounded_String;
      Source           : Provenance;
      Build            : Build_Config;
      Runtime_Deps     : String_Vectors.Vector;
      Files            : File_Vectors.Vector;
      Signatures       : Attestation_Vectors.Vector;
   end record;

   ---------------------------------------------------------------------------
   --  Manifest Operations
   ---------------------------------------------------------------------------

   type Parse_Result is (Success, Invalid_Format, Missing_Field, Invalid_Hash);

   procedure Parse_Manifest
      (Input   : String;
       Result  : out Manifest;
       Status  : out Parse_Result)
      with Pre => Input'Length > 0;
   --  Parse a TOML manifest string into a Manifest record

   function Validate_Manifest (M : Manifest) return Boolean;
   --  Perform comprehensive validation of a parsed manifest

   function Manifest_To_String (M : Manifest) return String;
   --  Serialize a manifest back to TOML format

end Cerro_Manifest;
