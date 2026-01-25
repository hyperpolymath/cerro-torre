--  Cerro Torre Crypto OpenSSL - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Directories;
with Ada.Calendar;
with GNAT.OS_Lib;
with Ada.Strings.Fixed;
with Interfaces;

package body Cerro_Crypto_OpenSSL is

   use Ada.Text_IO;
   use Ada.Calendar;
   use GNAT.OS_Lib;
   use Ada.Strings.Fixed;
   use Interfaces;

   function Get_Unique_ID return String is
      Now : constant Time := Clock;
      Seconds_Since_Epoch : constant Duration := Now - Time_Of (1970, 1, 1);
   begin
      return Trim (Duration'Image (Seconds_Since_Epoch), Ada.Strings.Both);
   end Get_Unique_ID;

   ---------------------
   -- Key Generation --
   ---------------------

   procedure Generate_Ed25519_Keypair
      (Private_Key : out Ed25519_Private_Key;
       Public_Key  : out Ed25519_Public_Key;
       Success     : out Boolean)
   is
      Temp_Dir      : constant String := "/tmp/cerro_keygen_" &
                                         Get_Unique_ID;
      Private_File  : constant String := Temp_Dir & "/private.pem";
      Public_File   : constant String := Temp_Dir & "/public.pem";
      Raw_Priv_File : constant String := Temp_Dir & "/private.raw";
      Raw_Pub_File  : constant String := Temp_Dir & "/public.raw";
      Args          : Argument_List_Access;
      Success_Flag  : Boolean;
   begin
      Success := False;

      --  Create temp directory
      begin
         Ada.Directories.Create_Directory (Temp_Dir);
      exception
         when others =>
            return;
      end;

      --  Step 1: Generate Ed25519 private key
      Args := Argument_String_To_List ("genpkey -algorithm ED25519 -out " & Private_File);
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 2: Extract public key
      Args := Argument_String_To_List ("pkey -in " & Private_File &
                                       " -pubout -out " & Public_File);
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 3: Convert private key to raw format
      Args := Argument_String_To_List ("pkey -in " & Private_File &
                                       " -outform DER -out " & Raw_Priv_File);
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 4: Convert public key to raw format
      Args := Argument_String_To_List ("pkey -pubin -in " & Public_File &
                                       " -outform DER -out " & Raw_Pub_File);
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 5: Read raw keys (skip DER header, last 32/64 bytes are key material)
      declare
         Priv_F : File_Type;
         Pub_F  : File_Type;
         Byte   : Character;
         Priv_Size : Natural;
         Pub_Size  : Natural;
      begin
         --  Read private key
         Open (Priv_F, In_File, Raw_Priv_File);
         Priv_Size := Natural (Ada.Directories.Size (Raw_Priv_File));

         --  Skip DER header (first bytes), read last 64 bytes
         if Priv_Size >= 64 then
            for I in 1 .. Priv_Size - 64 loop
               Get (Priv_F, Byte);
            end loop;

            for I in Private_Key'Range loop
               Get (Priv_F, Byte);
               Private_Key (I) := Unsigned_8 (Character'Pos (Byte));
            end loop;
         end if;

         Close (Priv_F);

         --  Read public key
         Open (Pub_F, In_File, Raw_Pub_File);
         Pub_Size := Natural (Ada.Directories.Size (Raw_Pub_File));

         --  Skip DER header, read last 32 bytes
         if Pub_Size >= 32 then
            for I in 1 .. Pub_Size - 32 loop
               Get (Pub_F, Byte);
            end loop;

            for I in Public_Key'Range loop
               Get (Pub_F, Byte);
               Public_Key (I) := Unsigned_8 (Character'Pos (Byte));
            end loop;
         end if;

         Close (Pub_F);

         Success := (Priv_Size >= 64 and Pub_Size >= 32);
      exception
         when others =>
            Success := False;
      end;

      --  Cleanup
      Ada.Directories.Delete_Tree (Temp_Dir);

   exception
      when others =>
         Success := False;
         if Ada.Directories.Exists (Temp_Dir) then
            Ada.Directories.Delete_Tree (Temp_Dir);
         end if;
   end Generate_Ed25519_Keypair;

   ----------------------
   -- Signing Operations --
   ----------------------

   procedure Sign_Ed25519
      (Message     : String;
       Private_Key : Ed25519_Private_Key;
       Signature   : out Ed25519_Signature;
       Success     : out Boolean)
   is
      Temp_Dir      : constant String := "/tmp/cerro_sign_" &
                                         Get_Unique_ID;
      Key_File      : constant String := Temp_Dir & "/key.pem";
      Message_File  : constant String := Temp_Dir & "/message.bin";
      Sig_File      : constant String := Temp_Dir & "/signature.bin";
      Raw_Key_File  : constant String := Temp_Dir & "/key.raw";
      Args          : Argument_List_Access;
      Success_Flag  : Boolean;
   begin
      Success := False;

      --  Create temp directory
      begin
         Ada.Directories.Create_Directory (Temp_Dir);
      exception
         when others =>
            return;
      end;

      --  Step 1: Write private key in raw format
      declare
         F : File_Type;
      begin
         Create (F, Out_File, Raw_Key_File);
         for Byte of Private_Key loop
            Put (F, Character'Val (Byte));
         end loop;
         Close (F);
      end;

      --  Step 2: Convert raw key to PEM format
      Args := Argument_String_To_List ("pkey -inform DER -in " & Raw_Key_File &
                                       " -out " & Key_File);
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 3: Write message to file
      declare
         F : File_Type;
      begin
         Create (F, Out_File, Message_File);
         Put (F, Message);
         Close (F);
      end;

      --  Step 4: Sign the message
      Args := Argument_String_To_List ("pkeyutl -sign -inkey " & Key_File &
                                       " -in " & Message_File &
                                       " -out " & Sig_File);
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 5: Read signature (should be exactly 64 bytes)
      declare
         F    : File_Type;
         Byte : Character;
         Size : Natural;
      begin
         Open (F, In_File, Sig_File);
         Size := Natural (Ada.Directories.Size (Sig_File));

         if Size = 64 then
            for I in Signature'Range loop
               Get (F, Byte);
               Signature (I) := Unsigned_8 (Character'Pos (Byte));
            end loop;
            Success := True;
         else
            Success := False;
         end if;

         Close (F);
      exception
         when others =>
            Success := False;
      end;

      --  Cleanup
      Ada.Directories.Delete_Tree (Temp_Dir);

   exception
      when others =>
         Success := False;
         if Ada.Directories.Exists (Temp_Dir) then
            Ada.Directories.Delete_Tree (Temp_Dir);
         end if;
   end Sign_Ed25519;

   -----------------------
   -- Key Serialization --
   -----------------------

   function Byte_To_Hex (B : Unsigned_8) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      High      : constant Natural := Natural (B / 16);
      Low       : constant Natural := Natural (B mod 16);
   begin
      return Hex_Chars (High + 1) & Hex_Chars (Low + 1);
   end Byte_To_Hex;

   function Hex_To_Byte (Hex : String) return Unsigned_8 is
      function Hex_Digit (C : Character) return Unsigned_8 is
      begin
         if C >= '0' and C <= '9' then
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('0'));
         elsif C >= 'a' and C <= 'f' then
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('a') + 10);
         elsif C >= 'A' and C <= 'F' then
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('A') + 10);
         else
            return 0;
         end if;
      end Hex_Digit;
   begin
      return Hex_Digit (Hex (Hex'First)) * 16 + Hex_Digit (Hex (Hex'First + 1));
   end Hex_To_Byte;

   function Private_Key_To_Hex (Key : Ed25519_Private_Key) return String is
      Result : String (1 .. 128);
      Pos    : Positive := 1;
   begin
      for Byte of Key loop
         Result (Pos .. Pos + 1) := Byte_To_Hex (Byte);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Private_Key_To_Hex;

   procedure Hex_To_Private_Key
      (Hex         : String;
       Private_Key : out Ed25519_Private_Key;
       Success     : out Boolean)
   is
   begin
      Success := True;
      for I in Private_Key'Range loop
         declare
            Offset : constant Natural := (I - 1) * 2;
         begin
            Private_Key (I) := Hex_To_Byte (Hex (Hex'First + Offset .. Hex'First + Offset + 1));
         end;
      end loop;
   exception
      when others =>
         Success := False;
   end Hex_To_Private_Key;

   function Public_Key_To_Hex (Key : Ed25519_Public_Key) return String is
      Result : String (1 .. 64);
      Pos    : Positive := 1;
   begin
      for Byte of Key loop
         Result (Pos .. Pos + 1) := Byte_To_Hex (Byte);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Public_Key_To_Hex;

   procedure Hex_To_Public_Key
      (Hex        : String;
       Public_Key : out Ed25519_Public_Key;
       Success    : out Boolean)
   is
   begin
      Success := True;
      for I in Public_Key'Range loop
         declare
            Offset : constant Natural := (I - 1) * 2;
         begin
            Public_Key (I) := Hex_To_Byte (Hex (Hex'First + Offset .. Hex'First + Offset + 1));
         end;
      end loop;
   exception
      when others =>
         Success := False;
   end Hex_To_Public_Key;

   function Signature_To_Hex (Sig : Ed25519_Signature) return String is
      Result : String (1 .. 128);
      Pos    : Positive := 1;
   begin
      for Byte of Sig loop
         Result (Pos .. Pos + 1) := Byte_To_Hex (Byte);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Signature_To_Hex;

   procedure Hex_To_Signature
      (Hex       : String;
       Signature : out Ed25519_Signature;
       Success   : out Boolean)
   is
   begin
      Success := True;
      for I in Signature'Range loop
         declare
            Offset : constant Natural := (I - 1) * 2;
         begin
            Signature (I) := Hex_To_Byte (Hex (Hex'First + Offset .. Hex'First + Offset + 1));
         end;
      end loop;
   exception
      when others =>
         Success := False;
   end Hex_To_Signature;

end Cerro_Crypto_OpenSSL;
