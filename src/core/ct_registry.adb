-------------------------------------------------------------------------------
--  CT_Registry - Implementation of OCI Distribution Specification Client
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Implements registry operations per OCI Distribution Specification.
--  Currently provides stub implementations pending HTTP client integration.
--
--  Future Integration Plan:
--    1. Add AWS.Client or similar HTTP library dependency
--    2. Implement OAuth2 token exchange for authentication
--    3. Add chunked upload support for large blobs
--    4. Integrate with Cerro_Crypto for digest verification
--
--  Security Considerations:
--    - Always verify TLS certificates in production
--    - Never log credentials or tokens
--    - Verify digests on all downloaded content
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  SPARK mode off pending HTTP client bindings

with Ada.Strings.Fixed;
with CT_HTTP;

package body CT_Registry is

   use Ada.Strings.Fixed;

   ---------------------------------------------------------------------------
   --  Internal Constants
   ---------------------------------------------------------------------------

   API_Version : constant String := "v2";

   ---------------------------------------------------------------------------
   --  Client Creation
   ---------------------------------------------------------------------------

   function Create_Client
     (Registry : String;
      Auth     : Auth_Credentials := (others => <>)) return Registry_Client
   is
      Client : Registry_Client;
      Reg    : String := Registry;
   begin
      --  Normalize registry URL
      if Registry'Length >= 8 and then
         Registry (Registry'First .. Registry'First + 7) = "https://"
      then
         Client.Base_URL := To_Unbounded_String (Registry);
      elsif Registry'Length >= 7 and then
            Registry (Registry'First .. Registry'First + 6) = "http://"
      then
         --  Allow HTTP for localhost/testing only
         Client.Base_URL := To_Unbounded_String (Registry);
      else
         --  Prepend https://
         Client.Base_URL := To_Unbounded_String ("https://" & Registry);
      end if;

      Client.Auth := Auth;
      Client.User_Agent := To_Unbounded_String (Default_User_Agent);
      Client.Timeout_Ms := 30_000;
      Client.Verify_TLS := True;

      return Client;
   end Create_Client;

   ---------------------------------------------------------------------------
   --  Authentication
   ---------------------------------------------------------------------------

   function Authenticate
     (Client     : in out Registry_Client;
      Repository : String;
      Actions    : String := "pull") return Registry_Error
   is
      Test_URL : constant String := To_String (Client.Base_URL) & "/v2/";
   begin
      --  Step 1: Try anonymous access to /v2/
      declare
         use CT_HTTP;
         Response : HTTP_Response;
      begin
         Response := Get (URL => Test_URL);

         --  If 200, no auth needed
         if Response.Success and then Response.Status_Code = 200 then
            Client.Auth.Method := None;
            return Success;
         end if;

         --  If not 401, something else is wrong
         if Response.Status_Code /= 401 then
            return Network_Error;
         end if;

         --  Step 2: Parse WWW-Authenticate header
         declare
            WWW_Auth : constant String := Get_Header (Response, WWW_Authenticate_Header);
            Challenge : WWW_Auth_Challenge;
         begin
            if WWW_Auth'Length = 0 then
               return Auth_Failed;
            end if;

            Challenge := Parse_WWW_Authenticate (WWW_Auth);

            --  Step 3: Exchange credentials for token
            if Length (Client.Auth.Username) = 0 then
               return Auth_Required;
            end if;

            --  Build token endpoint URL
            declare
               Token_URL : constant String := To_String (Challenge.Realm) &
                  "?service=" & To_String (Challenge.Service) &
                  "&scope=repository:" & Repository & ":" & Actions;
               Token_Auth : constant Auth_Credentials := Make_Basic_Auth (
                  To_String (Client.Auth.Username),
                  To_String (Client.Auth.Password));
               Token_Response : HTTP_Response;
            begin
               Token_Response := Get (
                  URL  => Token_URL,
                  Auth => Token_Auth);

               if not Token_Response.Success or else
                  not Is_Success (Token_Response.Status_Code)
               then
                  return Auth_Failed;
               end if;

               --  TODO: Parse JSON response to extract "token" field
               --  For now, just store the whole body as token (will fail, but shows structure)
               Client.Auth.Token := Token_Response.Body;
               Client.Auth.Method := Bearer;

               --  Placeholder until JSON parsing implemented
               return Not_Implemented;
            end;
         end;
      end;
   end Authenticate;

   function Is_Authenticated (Client : Registry_Client) return Boolean is
   begin
      return Length (Client.Auth.Token) > 0 or else
             (Client.Auth.Method = Basic and then
              Length (Client.Auth.Username) > 0);
   end Is_Authenticated;

   ---------------------------------------------------------------------------
   --  Manifest Operations
   ---------------------------------------------------------------------------

   function Pull_Manifest
     (Client     : Registry_Client;
      Repository : String;
      Reference  : String) return Pull_Result
   is
      Result : Pull_Result;
      URL    : constant String := To_String (Client.Base_URL) &
                                  "/v2/" & Repository & "/manifests/" & Reference;
   begin
      --  Build Accept header for OCI manifest types
      declare
         use CT_HTTP;
         Headers : Header_Map;
         Auth    : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Add Accept header for manifest media types
         Headers.Insert (Accept_Header,
            OCI_Manifest_V1 & ", " &
            OCI_Index_V1 & ", " &
            Docker_Manifest_V2 & ", " &
            Docker_Manifest_List);

         --  Add authentication if available
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Perform GET request
         Response := Get (
            URL     => URL,
            Auth    => Auth,
            Headers => Headers);

         --  Handle response
         if not Response.Success then
            Result.Error := Network_Error;
            return Result;
         elsif Response.Status_Code = 401 or Response.Status_Code = 403 then
            Result.Error := Auth_Failed;
            return Result;
         elsif Response.Status_Code = 404 then
            Result.Error := Not_Found;
            return Result;
         elsif not Is_Success (Response.Status_Code) then
            Result.Error := Server_Error;
            return Result;
         end if;

         --  Success - store raw JSON for signature verification
         Result.Raw_Json := Response.Body;

         --  Extract Docker-Content-Digest header if present
         declare
            Digest_Header : constant String := Get_Header (Response, Docker_Content_Digest);
         begin
            if Digest_Header'Length > 0 then
               Result.Digest := To_Unbounded_String (Digest_Header);
            else
               --  Calculate digest from body
               Result.Digest := To_Unbounded_String (
                  Manifest_Digest (To_String (Response.Body)));
            end if;
         end;

         --  TODO: Parse JSON body into OCI_Manifest structure
         --  For now, mark success with empty manifest
         Result.Manifest.Schema_Version := 2;
         Result.Manifest.Media_Type := To_Unbounded_String (OCI_Manifest_V1);

         Result.Error := Success;
         return Result;
      end;
   end Pull_Manifest;

   function Push_Manifest
     (Client        : Registry_Client;
      Repository    : String;
      Tag           : String;
      Manifest      : OCI_Manifest;
      Manifest_Json : String := "") return Push_Result
   is
      pragma Unreferenced (Client, Repository, Tag, Manifest);
      Result : Push_Result;
      Json   : constant String := (if Manifest_Json'Length > 0
                                   then Manifest_Json
                                   else Manifest_To_Json (Manifest));
      pragma Unreferenced (Json);
   begin
      --  TODO: Implement manifest push
      --
      --  Expected request:
      --    PUT /v2/{repository}/manifests/{tag}
      --    Content-Type: application/vnd.oci.image.manifest.v1+json
      --    Authorization: Bearer {token}
      --    Body: {manifest JSON}
      --
      --  Response should include Docker-Content-Digest header

      Result.Error := Not_Implemented;
      return Result;
   end Push_Manifest;

   function Manifest_Exists
     (Client     : Registry_Client;
      Repository : String;
      Reference  : String) return Boolean
   is
      URL : constant String := To_String (Client.Base_URL) &
                                "/v2/" & Repository & "/manifests/" & Reference;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Add authentication if available
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Perform HEAD request (efficient, no body)
         Response := Head (
            URL  => URL,
            Auth => Auth);

         --  Return true if 200 OK, false otherwise
         return Response.Success and then Response.Status_Code = 200;
      end;
   end Manifest_Exists;

   function Delete_Manifest
     (Client     : Registry_Client;
      Repository : String;
      Digest     : String) return Registry_Error
   is
      pragma Unreferenced (Client, Repository, Digest);
   begin
      --  TODO: Implement DELETE /v2/{repository}/manifests/{digest}
      --  Note: Most registries require digest, not tag, for deletion
      return Not_Implemented;
   end Delete_Manifest;

   ---------------------------------------------------------------------------
   --  Blob Operations
   ---------------------------------------------------------------------------

   function Pull_Blob
     (Client      : Registry_Client;
      Repository  : String;
      Digest      : String;
      Output_Path : String := "") return Blob_Result
   is
      Result : Blob_Result;
      URL    : constant String := To_String (Client.Base_URL) &
                                  "/v2/" & Repository & "/blobs/" & Digest;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Download blob (streaming to file if path provided)
         if Output_Path'Length > 0 then
            Response := Download_To_File (
               URL         => URL,
               Output_Path => Output_Path,
               Auth        => Auth);
         else
            Response := Get (
               URL  => URL,
               Auth => Auth);
         end if;

         --  Handle errors
         if not Response.Success then
            Result.Error := Network_Error;
            return Result;
         elsif Response.Status_Code = 404 then
            Result.Error := Not_Found;
            return Result;
         elsif not Is_Success (Response.Status_Code) then
            Result.Error := Server_Error;
            return Result;
         end if;

         --  Success
         Result.Content := Response.Body;
         Result.Digest := To_Unbounded_String (Digest);

         --  TODO: Verify digest matches downloaded content
         --  For now, assume digest is correct (caller should verify)

         Result.Error := Success;
         return Result;
      end;
   end Pull_Blob;

   function Push_Blob
     (Client     : Registry_Client;
      Repository : String;
      Content    : String;
      Media_Type : String := OCI_Layer_Gzip) return Push_Result
   is
      pragma Unreferenced (Client, Repository, Content, Media_Type);
      Result : Push_Result;
   begin
      --  TODO: Implement monolithic blob upload
      --
      --  Flow:
      --  1. POST /v2/{repository}/blobs/uploads/
      --     Get Location header for upload URL
      --
      --  2. PUT {upload-url}?digest=sha256:{digest}
      --     Content-Type: {media_type}
      --     Body: {content}
      --
      --  OR: Single POST with ?digest= for small blobs

      Result.Error := Not_Implemented;
      return Result;
   end Push_Blob;

   function Push_Blob_From_File
     (Client      : Registry_Client;
      Repository  : String;
      File_Path   : String;
      Media_Type  : String := OCI_Layer_Gzip;
      Chunk_Size  : Positive := 5_242_880) return Push_Result
   is
      pragma Unreferenced (Client, Repository, File_Path, Media_Type, Chunk_Size);
      Result : Push_Result;
   begin
      --  TODO: Implement chunked blob upload
      --
      --  Flow:
      --  1. POST /v2/{repository}/blobs/uploads/
      --     Get Location header
      --
      --  2. For each chunk:
      --     PATCH {location}
      --     Content-Range: {start}-{end}
      --     Content-Length: {chunk-size}
      --     Body: {chunk}
      --
      --  3. PUT {location}?digest=sha256:{digest}
      --     Content-Length: 0

      Result.Error := Not_Implemented;
      return Result;
   end Push_Blob_From_File;

   function Blob_Exists
     (Client     : Registry_Client;
      Repository : String;
      Digest     : String) return Boolean
   is
      pragma Unreferenced (Client, Repository, Digest);
   begin
      --  TODO: Implement HEAD /v2/{repository}/blobs/{digest}
      return False;
   end Blob_Exists;

   function Mount_Blob
     (Client      : Registry_Client;
      Target_Repo : String;
      Source_Repo : String;
      Digest      : String) return Registry_Error
   is
      pragma Unreferenced (Client, Target_Repo, Source_Repo, Digest);
   begin
      --  TODO: Implement cross-repo blob mount
      --
      --  Request:
      --    POST /v2/{target}/blobs/uploads/?mount={digest}&from={source}
      --
      --  If blob exists in source and client has access:
      --    Returns 201 Created with Location header
      --  Otherwise falls back to regular upload

      return Not_Implemented;
   end Mount_Blob;

   ---------------------------------------------------------------------------
   --  Tag Operations
   ---------------------------------------------------------------------------

   function List_Tags
     (Client     : Registry_Client;
      Repository : String;
      Page_Size  : Positive := 100) return Tags_Result
   is
      pragma Unreferenced (Client, Repository, Page_Size);
      Result : Tags_Result;
   begin
      --  TODO: Implement tag listing
      --
      --  Request:
      --    GET /v2/{repository}/tags/list?n={page_size}
      --
      --  Response:
      --    {"name": "repo", "tags": ["v1.0", "v1.1", "latest"]}
      --
      --  Pagination via Link header for next page

      Result.Error := Not_Implemented;
      return Result;
   end List_Tags;

   ---------------------------------------------------------------------------
   --  Image Reference Parsing
   ---------------------------------------------------------------------------

   function Parse_Reference (Ref : String) return Image_Reference
   is
      Result     : Image_Reference;
      At_Pos     : Natural := 0;
      Colon_Pos  : Natural := 0;
      Slash_Pos  : Natural := 0;
      Work       : String := Ref;
      Work_Start : Positive := Work'First;
      Work_End   : Natural := Work'Last;
   begin
      --  Parse format: [registry/]repository[:tag][@digest]
      --
      --  Examples:
      --    "nginx" -> docker.io/library/nginx:latest
      --    "nginx:1.25" -> docker.io/library/nginx:1.25
      --    "ghcr.io/user/repo:v1.0" -> as-is
      --    "ghcr.io/user/repo@sha256:abc" -> as-is

      --  First, extract digest if present (after @)
      for I in reverse Work'Range loop
         if Work (I) = '@' then
            At_Pos := I;
            Result.Digest := To_Unbounded_String (Work (I + 1 .. Work'Last));
            Work_End := I - 1;
            exit;
         end if;
      end loop;

      --  Extract tag if present (after last : but only after last /)
      Slash_Pos := 0;
      for I in reverse Work_Start .. Work_End loop
         if Work (I) = '/' then
            Slash_Pos := I;
            exit;
         end if;
      end loop;

      for I in reverse (if Slash_Pos > 0 then Slash_Pos else Work_Start) .. Work_End loop
         if Work (I) = ':' then
            Colon_Pos := I;
            Result.Tag := To_Unbounded_String (Work (I + 1 .. Work_End));
            Work_End := I - 1;
            exit;
         end if;
      end loop;

      --  Determine if first component is registry or repository
      --  Registry indicators: contains '.', ':', or is 'localhost'
      Slash_Pos := 0;
      for I in Work_Start .. Work_End loop
         if Work (I) = '/' then
            Slash_Pos := I;
            exit;
         end if;
      end loop;

      if Slash_Pos > 0 then
         declare
            First_Part : constant String := Work (Work_Start .. Slash_Pos - 1);
            Has_Dot    : Boolean := False;
            Has_Colon  : Boolean := False;
         begin
            for C of First_Part loop
               if C = '.' then Has_Dot := True; end if;
               if C = ':' then Has_Colon := True; end if;
            end loop;

            if Has_Dot or Has_Colon or First_Part = "localhost" then
               --  First part is registry
               Result.Registry := To_Unbounded_String (First_Part);
               Result.Repository := To_Unbounded_String (Work (Slash_Pos + 1 .. Work_End));
            else
               --  No registry, use default
               Result.Registry := To_Unbounded_String (Default_Registry);
               Result.Repository := To_Unbounded_String (Work (Work_Start .. Work_End));
            end if;
         end;
      else
         --  No slash at all - simple name like "nginx"
         Result.Registry := To_Unbounded_String (Default_Registry);
         --  Docker Hub library images
         Result.Repository := To_Unbounded_String ("library/" & Work (Work_Start .. Work_End));
      end if;

      --  Default tag if none specified and no digest
      if Length (Result.Tag) = 0 and Length (Result.Digest) = 0 then
         Result.Tag := To_Unbounded_String (Default_Tag);
      end if;

      return Result;
   end Parse_Reference;

   function To_String (Ref : Image_Reference) return String is
      Result : Unbounded_String;
   begin
      --  Format: registry/repository[:tag][@digest]
      if Length (Ref.Registry) > 0 and then
         To_String (Ref.Registry) /= Default_Registry
      then
         Append (Result, Ref.Registry);
         Append (Result, "/");
      end if;

      Append (Result, Ref.Repository);

      if Length (Ref.Tag) > 0 and then
         To_String (Ref.Tag) /= Default_Tag
      then
         Append (Result, ":");
         Append (Result, Ref.Tag);
      end if;

      if Length (Ref.Digest) > 0 then
         Append (Result, "@");
         Append (Result, Ref.Digest);
      end if;

      return To_String (Result);
   end To_String;

   ---------------------------------------------------------------------------
   --  Manifest Serialization
   ---------------------------------------------------------------------------

   function Manifest_To_Json (M : OCI_Manifest) return String is
   begin
      --  TODO: Implement proper JSON serialization
      --  For now, return minimal valid manifest
      return "{""schemaVersion"": 2, ""mediaType"": """ &
             To_String (M.Media_Type) & """}";
   end Manifest_To_Json;

   function Parse_Manifest (Json : String) return Pull_Result is
      pragma Unreferenced (Json);
      Result : Pull_Result;
   begin
      --  TODO: Implement JSON parsing
      --  Parse manifest JSON into OCI_Manifest structure
      Result.Error := Not_Implemented;
      return Result;
   end Parse_Manifest;

   function Manifest_Digest (Json : String) return String is
      pragma Unreferenced (Json);
   begin
      --  TODO: Integrate with Cerro_Crypto.SHA256
      --  Compute sha256 of canonical JSON
      return "sha256:0000000000000000000000000000000000000000000000000000000000000000";
   end Manifest_Digest;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Verify_Digest
     (Content : String;
      Digest  : String) return Boolean
   is
      pragma Unreferenced (Content, Digest);
   begin
      --  TODO: Integrate with Cerro_Crypto
      --  1. Extract algorithm from digest prefix (sha256:, sha512:, etc.)
      --  2. Compute hash of content
      --  3. Compare with digest value
      return False;
   end Verify_Digest;

   function Error_Message (E : Registry_Error) return String is
   begin
      case E is
         when Success =>
            return "Operation completed successfully";
         when Not_Implemented =>
            return "Feature not yet implemented";
         when Network_Error =>
            return "Network connection failed";
         when Auth_Required =>
            return "Authentication required";
         when Auth_Failed =>
            return "Authentication failed: invalid credentials";
         when Not_Found =>
            return "Resource not found (404)";
         when Forbidden =>
            return "Access denied (403)";
         when Digest_Mismatch =>
            return "Content digest verification failed";
         when Invalid_Manifest =>
            return "Invalid manifest format";
         when Rate_Limited =>
            return "Rate limit exceeded (429)";
         when Server_Error =>
            return "Registry server error";
         when Push_Failed =>
            return "Failed to push content";
         when Unsupported_Media_Type =>
            return "Media type not supported by registry";
         when Timeout =>
            return "Request timed out";
      end case;
   end Error_Message;

end CT_Registry;
