-------------------------------------------------------------------------------
--  Cerro_Provenance - Provenance Chain Verification
--
--  This package handles verification of complete package lineage,
--  ensuring cryptographic integrity from upstream source through
--  all transformations.
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Cerro_Manifest; use Cerro_Manifest;
with Cerro_Crypto;   use Cerro_Crypto;

package Cerro_Provenance is

   ---------------------------------------------------------------------------
   --  Verification Status
   ---------------------------------------------------------------------------

   type Verification_Status is
      (Verified,                  -- All checks passed
       Upstream_Hash_Mismatch,    -- Upstream source hash doesn't match
       Upstream_Signature_Invalid,-- Upstream PGP signature failed
       Patch_Hash_Mismatch,       -- One or more patches have wrong hash
       Chain_Broken,              -- Provenance chain has gaps
       Signature_Invalid,         -- Package signature verification failed
       Signature_Expired,         -- Signature timestamp too old
       Unknown_Signer);           -- Signer not in trust store

   type Verification_Result is record
      Status       : Verification_Status := Chain_Broken;
      Failed_Item  : Natural := 0;  -- Index of failed item (0 = none)
      Message      : String (1 .. 256);
      Message_Len  : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Provenance Verification
   ---------------------------------------------------------------------------

   function Verify_Upstream_Hash
      (Prov : Provenance;
       Data : String) return Boolean
      with Global => null,
           Pre    => Data'Length > 0;
   --  Verify that the upstream source data matches the recorded hash.
   --  @param Prov The provenance record containing expected hash
   --  @param Data The actual upstream source content
   --  @return True if hash matches

   function Verify_Patch_Hashes
      (Prov         : Provenance;
       Patch_Data   : String_Vectors.Vector) return Verification_Result
      with Global => null,
           Pre    => Natural (Patch_Data.Length) =
                     Natural (Prov.Patches.Length);
   --  Verify all patches have correct hashes.
   --  @param Prov The provenance record with expected patch hashes
   --  @param Patch_Data Vector of patch file contents (same order as Prov.Patches)
   --  @return Verification result with status and failed index if any

   function Verify_Chain_Integrity (M : Manifest) return Verification_Result
      with Global => null;
   --  Perform comprehensive provenance chain verification.
   --  Checks:
   --  - All required provenance fields present
   --  - Hash formats are valid
   --  - Timestamps are reasonable
   --  @param M The manifest to verify
   --  @return Verification result

   ---------------------------------------------------------------------------
   --  Signature Verification
   ---------------------------------------------------------------------------

   function Verify_Manifest_Signature
      (M          : Manifest;
       Sig_Index  : Positive) return Boolean
      with Global => null,
           Pre    => Sig_Index <= Natural (M.Signatures.Length);
   --  Verify a specific signature on the manifest.
   --  @param M The manifest
   --  @param Sig_Index Index of the attestation to verify
   --  @return True if signature is valid

   function Count_Valid_Signatures (M : Manifest) return Natural
      with Global => null;
   --  Count how many signatures on the manifest are valid.
   --  @param M The manifest
   --  @return Number of valid signatures

   function Has_Required_Attestations (M : Manifest) return Boolean
      with Global => null;
   --  Check if manifest has minimum required attestations.
   --  Currently requires at least one valid build attestation.
   --  @param M The manifest
   --  @return True if requirements met

   ---------------------------------------------------------------------------
   --  Trust Store Operations
   ---------------------------------------------------------------------------

   type Trust_Level is (Untrusted, Known, Trusted, Core);

   function Get_Signer_Trust (Public_Key : String) return Trust_Level
      with Global => null,
           Pre    => Public_Key'Length > 0;
   --  Look up trust level for a signing key.
   --  @param Public_Key The Ed25519 public key in "ed25519:base64" format
   --  @return Trust level for this key

   ---------------------------------------------------------------------------
   --  Provenance Report Generation
   ---------------------------------------------------------------------------

   function Generate_Provenance_Report (M : Manifest) return String;
   --  Generate a human-readable provenance report.
   --  @param M The manifest
   --  @return Formatted provenance report text

end Cerro_Provenance;
