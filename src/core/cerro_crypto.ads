-------------------------------------------------------------------------------
--  Cerro_Crypto - Cryptographic Operations for Cerro Torre
--
--  This package provides SPARK-verified cryptographic primitives including
--  hashing and digital signature verification.
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Cerro_Crypto is

   ---------------------------------------------------------------------------
   --  Hash Types
   ---------------------------------------------------------------------------

   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3);

   --  Fixed-size digest arrays
   type Digest_256 is array (1 .. 32) of Unsigned_8;
   type Digest_384 is array (1 .. 48) of Unsigned_8;
   type Digest_512 is array (1 .. 64) of Unsigned_8;

   --  Ed25519 signature and key sizes
   subtype Signature_Bytes is Positive range 1 .. 64;
   subtype Public_Key_Bytes is Positive range 1 .. 32;

   type Ed25519_Signature is array (1 .. 64) of Unsigned_8;
   type Ed25519_Public_Key is array (1 .. 32) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  Hash Functions
   ---------------------------------------------------------------------------

   function SHA256_Hash (Data : String) return Digest_256
      with Global => null,
           Pre    => Data'Length <= Natural'Last - 64,
           Post   => SHA256_Hash'Result'Length = 32;
   --  Compute SHA-256 hash of the input data.
   --  @param Data The input string to hash
   --  @return 32-byte digest

   function SHA384_Hash (Data : String) return Digest_384
      with Global => null,
           Pre    => Data'Length <= Natural'Last - 128,
           Post   => SHA384_Hash'Result'Length = 48;
   --  Compute SHA-384 hash of the input data.
   --  @param Data The input string to hash
   --  @return 48-byte digest

   function SHA512_Hash (Data : String) return Digest_512
      with Global => null,
           Pre    => Data'Length <= Natural'Last - 128,
           Post   => SHA512_Hash'Result'Length = 64;
   --  Compute SHA-512 hash of the input data.
   --  @param Data The input string to hash
   --  @return 64-byte digest

   ---------------------------------------------------------------------------
   --  Hex Encoding
   ---------------------------------------------------------------------------

   function Digest_To_Hex (Digest : Digest_256) return String
      with Global => null,
           Post   => Digest_To_Hex'Result'Length = 64;
   --  Convert a 256-bit digest to lowercase hexadecimal string.
   --  @param Digest The binary digest
   --  @return 64-character hex string

   function Hex_To_Digest_256 (Hex : String) return Digest_256
      with Global => null,
           Pre    => Hex'Length = 64 and then Is_Valid_Hex (Hex);
   --  Parse a hexadecimal string into a 256-bit digest.
   --  @param Hex The 64-character hex string
   --  @return Binary digest

   function Is_Valid_Hex (S : String) return Boolean
      with Global => null;
   --  Check if a string contains only valid hexadecimal characters.
   --  @param S The string to validate
   --  @return True if all characters are 0-9, a-f, or A-F

   ---------------------------------------------------------------------------
   --  Signature Verification
   ---------------------------------------------------------------------------

   function Verify_Ed25519
      (Message    : String;
       Signature  : Ed25519_Signature;
       Public_Key : Ed25519_Public_Key) return Boolean
      with Global => null,
           Pre    => Message'Length > 0;
   --  Verify an Ed25519 signature.
   --  @param Message The signed message
   --  @param Signature The 64-byte signature
   --  @param Public_Key The 32-byte public key
   --  @return True if signature is valid

   ---------------------------------------------------------------------------
   --  Constant-Time Comparison
   ---------------------------------------------------------------------------

   function Constant_Time_Equal (Left, Right : Digest_256) return Boolean
      with Global => null;
   --  Compare two digests in constant time to prevent timing attacks.
   --  @param Left First digest
   --  @param Right Second digest
   --  @return True if digests are equal

end Cerro_Crypto;
