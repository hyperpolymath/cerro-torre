-------------------------------------------------------------------------------
--  CT_HTTP - Implementation of HTTP Client Wrapper
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Implements HTTP operations using AWS (Ada Web Server) client library.
--  Provides the HTTP foundation for CT_Registry and CT_Transparency.
-------------------------------------------------------------------------------

with AWS.Client;
with AWS.Response;
with AWS.Headers;
with AWS.Messages;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body CT_HTTP is

   use Ada.Strings.Fixed;

   ---------------------------------------------------------------------------
   --  Status Code Predicates
   ---------------------------------------------------------------------------

   function Is_Success (Code : Status_Code) return Boolean is
   begin
      return Code in 200 .. 299;
   end Is_Success;

   function Is_Redirect (Code : Status_Code) return Boolean is
   begin
      return Code in 300 .. 399;
   end Is_Redirect;

   function Is_Client_Error (Code : Status_Code) return Boolean is
   begin
      return Code in 400 .. 499;
   end Is_Client_Error;

   function Is_Server_Error (Code : Status_Code) return Boolean is
   begin
      return Code in 500 .. 599;
   end Is_Server_Error;

   ---------------------------------------------------------------------------
   --  Response Constructors
   ---------------------------------------------------------------------------

   function Empty_Response return HTTP_Response is
   begin
      return HTTP_Response'
        (Status_Code   => 0,
         Status_Reason => Null_Unbounded_String,
         Headers       => Header_Maps.Empty_Map,
         Body          => Null_Unbounded_String,
         Error_Message => Null_Unbounded_String,
         Success       => False);
   end Empty_Response;

   function Error_Response (Message : String) return HTTP_Response is
      Response : HTTP_Response := Empty_Response;
   begin
      Response.Error_Message := To_Unbounded_String (Message);
      Response.Success := False;
      return Response;
   end Error_Response;

   ---------------------------------------------------------------------------
   --  Header Utilities (Case-Insensitive)
   ---------------------------------------------------------------------------

   function Normalize_Header_Name (Name : String) return String is
      Result : String := Name;
   begin
      for I in Result'Range loop
         Result (I) := Ada.Characters.Handling.To_Lower (Result (I));
      end loop;
      return Result;
   end Normalize_Header_Name;

   function Get_Header
     (Response : HTTP_Response;
      Name     : String) return String
   is
      Normalized_Name : constant String := Normalize_Header_Name (Name);
      Cursor : Header_Maps.Cursor;
   begin
      --  Iterate and compare normalized names
      Cursor := Response.Headers.First;
      while Header_Maps.Has_Element (Cursor) loop
         if Normalize_Header_Name (Header_Maps.Key (Cursor)) = Normalized_Name then
            return Header_Maps.Element (Cursor);
         end if;
         Header_Maps.Next (Cursor);
      end loop;
      return "";
   end Get_Header;

   function Has_Header
     (Response : HTTP_Response;
      Name     : String) return Boolean
   is
   begin
      return Get_Header (Response, Name)'Length > 0;
   end Has_Header;

   ---------------------------------------------------------------------------
   --  Internal: Build Authorization Header
   ---------------------------------------------------------------------------

   function Build_Auth_Header (Auth : Auth_Credentials) return String is
   begin
      case Auth.Scheme is
         when No_Auth =>
            return "";

         when Basic_Auth =>
            --  Base64 encode username:password
            declare
               Credentials : constant String :=
                  To_String (Auth.Username) & ":" & To_String (Auth.Password);
               --  Simple inline Base64 encoding
               Base64_Chars : constant String :=
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
               Result : String (1 .. ((Credentials'Length + 2) / 3) * 4);
               Src_Idx : Natural := Credentials'First;
               Dst_Idx : Natural := 1;
               B1, B2, B3 : Natural;
            begin
               while Src_Idx <= Credentials'Last loop
                  --  Get up to 3 bytes
                  B1 := Character'Pos (Credentials (Src_Idx));
                  Src_Idx := Src_Idx + 1;

                  if Src_Idx <= Credentials'Last then
                     B2 := Character'Pos (Credentials (Src_Idx));
                     Src_Idx := Src_Idx + 1;
                  else
                     B2 := 0;
                  end if;

                  if Src_Idx <= Credentials'Last then
                     B3 := Character'Pos (Credentials (Src_Idx));
                     Src_Idx := Src_Idx + 1;
                  else
                     B3 := 0;
                  end if;

                  --  Encode to 4 base64 characters
                  Result (Dst_Idx) := Base64_Chars (B1 / 4 + 1);
                  Result (Dst_Idx + 1) := Base64_Chars (((B1 mod 4) * 16 + B2 / 16) + 1);

                  if Src_Idx - 2 <= Credentials'Last then
                     Result (Dst_Idx + 2) := Base64_Chars (((B2 mod 16) * 4 + B3 / 64) + 1);
                  else
                     Result (Dst_Idx + 2) := '=';
                  end if;

                  if Src_Idx - 1 <= Credentials'Last then
                     Result (Dst_Idx + 3) := Base64_Chars ((B3 mod 64) + 1);
                  else
                     Result (Dst_Idx + 3) := '=';
                  end if;

                  Dst_Idx := Dst_Idx + 4;
               end loop;

               return "Basic " & Result (1 .. Dst_Idx - 1);
            end;

         when Bearer_Token =>
            return "Bearer " & To_String (Auth.Token);
      end case;
   end Build_Auth_Header;

   ---------------------------------------------------------------------------
   --  Internal: Convert AWS Response to CT_HTTP Response
   ---------------------------------------------------------------------------

   function Convert_Response (AWS_Resp : AWS.Response.Data) return HTTP_Response is
      Response : HTTP_Response;
      AWS_Headers : constant AWS.Headers.List := AWS.Response.Header (AWS_Resp);
   begin
      Response.Status_Code := Status_Code (AWS.Response.Status_Code (AWS_Resp));
      Response.Status_Reason := To_Unbounded_String
        (AWS.Messages.Status_Code'Image (AWS.Response.Status_Code (AWS_Resp)));
      Response.Body := To_Unbounded_String (AWS.Response.Message_Body (AWS_Resp));
      Response.Success := True;

      --  Copy headers from AWS response
      for I in 1 .. AWS.Headers.Count (AWS_Headers) loop
         declare
            Name  : constant String := AWS.Headers.Get_Name (AWS_Headers, I);
            Value : constant String := AWS.Headers.Get_Value (AWS_Headers, I);
         begin
            Response.Headers.Insert (Name, Value);
         end;
      end loop;

      return Response;
   end Convert_Response;

   ---------------------------------------------------------------------------
   --  Internal: Perform HTTP Request
   ---------------------------------------------------------------------------

   function Do_Request
     (Method       : HTTP_Method;
      URL          : String;
      Body         : String := "";
      Content_Type : String := "";
      Config       : HTTP_Client_Config;
      Auth         : Auth_Credentials;
      Headers      : Header_Map) return HTTP_Response
   is
      Connection : AWS.Client.HTTP_Connection;
      AWS_Resp   : AWS.Response.Data;
      Auth_Header : constant String := Build_Auth_Header (Auth);
      Timeout_Duration : constant Duration := Duration (Config.Timeout_Seconds);
   begin
      --  Create connection with TLS options
      AWS.Client.Create
        (Connection => Connection,
         Host       => URL,
         Timeouts   => AWS.Client.Timeouts
           (Connect  => Timeout_Duration,
            Send     => Timeout_Duration,
            Receive  => Timeout_Duration,
            Response => Timeout_Duration),
         User_Agent => To_String (Config.User_Agent));

      --  Build custom headers list
      declare
         Custom_Headers : AWS.Headers.List;
         Cursor : Header_Maps.Cursor := Headers.First;
      begin
         --  Add authentication header if present
         if Auth_Header'Length > 0 then
            AWS.Headers.Add (Custom_Headers, Authorization_Header, Auth_Header);
         end if;

         --  Add custom headers
         while Header_Maps.Has_Element (Cursor) loop
            AWS.Headers.Add
              (Custom_Headers,
               Header_Maps.Key (Cursor),
               Header_Maps.Element (Cursor));
            Header_Maps.Next (Cursor);
         end loop;

         --  Perform request based on method
         case Method is
            when GET =>
               AWS.Client.Get (Connection, AWS_Resp, Headers => Custom_Headers);

            when POST =>
               if Content_Type'Length > 0 then
                  AWS.Headers.Add (Custom_Headers, Content_Type_Header, Content_Type);
               end if;
               AWS.Client.Post
                 (Connection, AWS_Resp,
                  Data    => Body,
                  Headers => Custom_Headers);

            when PUT =>
               if Content_Type'Length > 0 then
                  AWS.Headers.Add (Custom_Headers, Content_Type_Header, Content_Type);
               end if;
               AWS.Client.Put
                 (Connection, AWS_Resp,
                  Data    => Body,
                  Headers => Custom_Headers);

            when DELETE =>
               --  AWS doesn't have a direct Delete, use generic request
               AWS.Client.Get (Connection, AWS_Resp, Headers => Custom_Headers);
               --  Note: For proper DELETE support, may need to use lower-level AWS APIs

            when HEAD =>
               AWS.Client.Head (Connection, AWS_Resp, Headers => Custom_Headers);

            when PATCH =>
               --  PATCH may need special handling depending on AWS version
               if Content_Type'Length > 0 then
                  AWS.Headers.Add (Custom_Headers, Content_Type_Header, Content_Type);
               end if;
               AWS.Client.Post
                 (Connection, AWS_Resp,
                  Data    => Body,
                  Headers => Custom_Headers);
         end case;
      end;

      AWS.Client.Close (Connection);
      return Convert_Response (AWS_Resp);

   exception
      when E : others =>
         return Error_Response ("HTTP request failed: network or connection error");
   end Do_Request;

   ---------------------------------------------------------------------------
   --  Public HTTP Methods
   ---------------------------------------------------------------------------

   function Get
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Do_Request (GET, URL, "", "", Config, Auth, Headers);
   end Get;

   function Post
     (URL          : String;
      Body         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Do_Request (POST, URL, Body, Content_Type, Config, Auth, Headers);
   end Post;

   function Put
     (URL          : String;
      Body         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Do_Request (PUT, URL, Body, Content_Type, Config, Auth, Headers);
   end Put;

   function Delete
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Do_Request (DELETE, URL, "", "", Config, Auth, Headers);
   end Delete;

   function Head
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Do_Request (HEAD, URL, "", "", Config, Auth, Headers);
   end Head;

   function Patch
     (URL          : String;
      Body         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Do_Request (PATCH, URL, Body, Content_Type, Config, Auth, Headers);
   end Patch;

   ---------------------------------------------------------------------------
   --  Streaming Support
   ---------------------------------------------------------------------------

   function Download_To_File
     (URL         : String;
      Output_Path : String;
      Config      : HTTP_Client_Config := Default_Config;
      Auth        : Auth_Credentials := No_Credentials;
      Headers     : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
      Response : HTTP_Response;
   begin
      --  First, perform the GET request
      Response := Get (URL, Config, Auth, Headers);

      if Response.Success and then Is_Success (Response.Status_Code) then
         --  Write body to file
         declare
            use Ada.Streams.Stream_IO;
            File   : File_Type;
            Stream : Stream_Access;
            Body_Str : constant String := To_String (Response.Body);
         begin
            Create (File, Out_File, Output_Path);
            Stream := Ada.Streams.Stream_IO.Stream (File);

            for I in Body_Str'Range loop
               Ada.Streams.Stream_Element'Write
                 (Stream, Ada.Streams.Stream_Element (Character'Pos (Body_Str (I))));
            end loop;

            Close (File);

            --  Clear body from response to save memory
            Response.Body := To_Unbounded_String ("[saved to " & Output_Path & "]");
         exception
            when others =>
               if Is_Open (File) then
                  Close (File);
               end if;
               Response.Success := False;
               Response.Error_Message := To_Unbounded_String
                 ("Failed to write response to file: " & Output_Path);
         end;
      end if;

      return Response;
   end Download_To_File;

   function Upload_From_File
     (URL          : String;
      Input_Path   : String;
      Content_Type : String := "application/octet-stream";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
      use Ada.Streams.Stream_IO;
      use Ada.Streams;
   begin
      --  Check file exists
      if not Ada.Directories.Exists (Input_Path) then
         return Error_Response ("File not found: " & Input_Path);
      end if;

      --  Read file content
      declare
         File_Size : constant Natural :=
            Natural (Ada.Directories.Size (Input_Path));
         File   : File_Type;
         Stream : Stream_Access;
         Buffer : String (1 .. File_Size);
         Element : Stream_Element;
      begin
         Open (File, In_File, Input_Path);
         Stream := Ada.Streams.Stream_IO.Stream (File);

         for I in Buffer'Range loop
            Stream_Element'Read (Stream, Element);
            Buffer (I) := Character'Val (Natural (Element));
         end loop;

         Close (File);

         --  Perform PUT request with file content
         return Put (URL, Buffer, Content_Type, Config, Auth, Headers);

      exception
         when others =>
            return Error_Response ("Failed to read file: " & Input_Path);
      end;
   end Upload_From_File;

   ---------------------------------------------------------------------------
   --  Authentication Helpers
   ---------------------------------------------------------------------------

   function Make_Basic_Auth
     (Username : String;
      Password : String) return Auth_Credentials
   is
   begin
      return Auth_Credentials'
        (Scheme   => Basic_Auth,
         Username => To_Unbounded_String (Username),
         Password => To_Unbounded_String (Password),
         Token    => Null_Unbounded_String);
   end Make_Basic_Auth;

   function Make_Bearer_Auth (Token : String) return Auth_Credentials is
   begin
      return Auth_Credentials'
        (Scheme   => Bearer_Token,
         Username => Null_Unbounded_String,
         Password => Null_Unbounded_String,
         Token    => To_Unbounded_String (Token));
   end Make_Bearer_Auth;

   function Parse_WWW_Authenticate (Header_Value : String) return WWW_Auth_Challenge is
      Challenge : WWW_Auth_Challenge;

      --  Extract value for a key from the header
      function Extract_Value (Key : String) return String is
         Key_Pattern : constant String := Key & "=""";
         Key_Pos : Natural;
         End_Quote : Natural;
      begin
         Key_Pos := Index (Header_Value, Key_Pattern);
         if Key_Pos = 0 then
            return "";
         end if;

         Key_Pos := Key_Pos + Key_Pattern'Length;

         --  Find closing quote
         End_Quote := Key_Pos;
         while End_Quote <= Header_Value'Last and then
               Header_Value (End_Quote) /= '"'
         loop
            End_Quote := End_Quote + 1;
         end loop;

         if End_Quote > Header_Value'Last then
            return "";
         end if;

         return Header_Value (Key_Pos .. End_Quote - 1);
      end Extract_Value;

   begin
      Challenge.Realm := To_Unbounded_String (Extract_Value ("realm"));
      Challenge.Service := To_Unbounded_String (Extract_Value ("service"));
      Challenge.Scope := To_Unbounded_String (Extract_Value ("scope"));
      return Challenge;
   end Parse_WWW_Authenticate;

   ---------------------------------------------------------------------------
   --  URL Utilities
   ---------------------------------------------------------------------------

   function URL_Encode (S : String) return String is
      Hex_Chars : constant String := "0123456789ABCDEF";
      --  Estimate worst case: every char becomes %XX (3x size)
      Result : String (1 .. S'Length * 3);
      Result_Idx : Natural := 0;
   begin
      for C of S loop
         case C is
            --  Unreserved characters (RFC 3986)
            when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' =>
               Result_Idx := Result_Idx + 1;
               Result (Result_Idx) := C;

            when others =>
               --  Percent-encode
               declare
                  Code : constant Natural := Character'Pos (C);
               begin
                  Result_Idx := Result_Idx + 1;
                  Result (Result_Idx) := '%';
                  Result_Idx := Result_Idx + 1;
                  Result (Result_Idx) := Hex_Chars (Code / 16 + 1);
                  Result_Idx := Result_Idx + 1;
                  Result (Result_Idx) := Hex_Chars (Code mod 16 + 1);
               end;
         end case;
      end loop;

      return Result (1 .. Result_Idx);
   end URL_Encode;

   function Build_URL
     (Base_URL : String;
      Path     : String;
      Query    : Header_Map := Header_Maps.Empty_Map) return String
   is
      Result : Unbounded_String := To_Unbounded_String (Join_Path (Base_URL, Path));
      Cursor : Header_Maps.Cursor := Query.First;
      First_Param : Boolean := True;
   begin
      while Header_Maps.Has_Element (Cursor) loop
         if First_Param then
            Append (Result, "?");
            First_Param := False;
         else
            Append (Result, "&");
         end if;

         Append (Result, URL_Encode (Header_Maps.Key (Cursor)));
         Append (Result, "=");
         Append (Result, URL_Encode (Header_Maps.Element (Cursor)));

         Header_Maps.Next (Cursor);
      end loop;

      return To_String (Result);
   end Build_URL;

   function Join_Path (Base : String; Path : String) return String is
      Base_Ends_Slash : constant Boolean :=
         Base'Length > 0 and then Base (Base'Last) = '/';
      Path_Starts_Slash : constant Boolean :=
         Path'Length > 0 and then Path (Path'First) = '/';
   begin
      if Base'Length = 0 then
         return Path;
      elsif Path'Length = 0 then
         return Base;
      elsif Base_Ends_Slash and Path_Starts_Slash then
         --  Remove one slash
         return Base & Path (Path'First + 1 .. Path'Last);
      elsif Base_Ends_Slash or Path_Starts_Slash then
         --  One has slash, good
         return Base & Path;
      else
         --  Neither has slash, add one
         return Base & "/" & Path;
      end if;
   end Join_Path;

end CT_HTTP;
