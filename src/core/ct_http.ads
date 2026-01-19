-------------------------------------------------------------------------------
--  CT_HTTP - HTTP Client Wrapper for Cerro Torre
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Provides a thin wrapper around AWS.Client for HTTP operations needed by
--  CT_Registry (OCI Distribution Spec) and CT_Transparency (Rekor/tlog).
--
--  Features:
--    - GET/POST/PUT/DELETE/HEAD requests
--    - Bearer token and Basic authentication headers
--    - TLS certificate verification control
--    - Configurable timeouts
--    - Response handling (status, headers, body)
--
--  Security Considerations:
--    - TLS verification enabled by default (disable only for testing)
--    - Credentials should never be logged
--    - All sensitive data in headers must be protected
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package CT_HTTP
   with SPARK_Mode => Off  --  External library bindings
is
   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  HTTP methods supported
   type HTTP_Method is (GET, POST, PUT, DELETE, HEAD, PATCH);

   --  Authentication schemes
   type Auth_Scheme is
     (No_Auth,       --  No authentication
      Basic_Auth,    --  HTTP Basic Authentication (username:password)
      Bearer_Token); --  OAuth2 Bearer Token

   --  Authentication credentials
   type Auth_Credentials is record
      Scheme   : Auth_Scheme := No_Auth;
      Username : Unbounded_String;  --  For Basic_Auth
      Password : Unbounded_String;  --  For Basic_Auth
      Token    : Unbounded_String;  --  For Bearer_Token
   end record;

   --  No credentials constant for convenience
   No_Credentials : constant Auth_Credentials :=
     (Scheme   => No_Auth,
      Username => Null_Unbounded_String,
      Password => Null_Unbounded_String,
      Token    => Null_Unbounded_String);

   --  Header map type
   package Header_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   subtype Header_Map is Header_Maps.Map;

   --  Client configuration
   type HTTP_Client_Config is record
      User_Agent     : Unbounded_String;
      Timeout_Seconds : Positive := 30;
      Verify_TLS     : Boolean := True;
      Follow_Redirects : Boolean := True;
      Max_Redirects  : Positive := 5;
   end record;

   --  Default configuration
   Default_Config : constant HTTP_Client_Config :=
     (User_Agent       => To_Unbounded_String ("cerro-torre/0.2"),
      Timeout_Seconds  => 30,
      Verify_TLS       => True,
      Follow_Redirects => True,
      Max_Redirects    => 5);

   ---------------------------------------------------------------------------
   --  HTTP Response
   ---------------------------------------------------------------------------

   --  HTTP status code categories
   subtype Status_Code is Natural range 100 .. 599;

   function Is_Success (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Success'Result = (Code in 200 .. 299);

   function Is_Redirect (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Redirect'Result = (Code in 300 .. 399);

   function Is_Client_Error (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Client_Error'Result = (Code in 400 .. 499);

   function Is_Server_Error (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Server_Error'Result = (Code in 500 .. 599);

   --  HTTP response structure
   type HTTP_Response is record
      Status_Code   : CT_HTTP.Status_Code := 0;
      Status_Reason : Unbounded_String;
      Headers       : Header_Map;
      Body          : Unbounded_String;
      Error_Message : Unbounded_String;  --  Set if request failed
      Success       : Boolean := False;  --  True if request completed
   end record;

   --  Create empty/error response
   function Empty_Response return HTTP_Response
   with Global => null;

   function Error_Response (Message : String) return HTTP_Response
   with Global => null;

   ---------------------------------------------------------------------------
   --  Header Utilities
   ---------------------------------------------------------------------------

   --  Get header value (case-insensitive lookup)
   function Get_Header
     (Response : HTTP_Response;
      Name     : String) return String
   with Global => null;

   --  Check if header exists
   function Has_Header
     (Response : HTTP_Response;
      Name     : String) return Boolean
   with Global => null;

   --  Common header names
   Content_Type_Header       : constant String := "Content-Type";
   Content_Length_Header     : constant String := "Content-Length";
   Authorization_Header      : constant String := "Authorization";
   Accept_Header             : constant String := "Accept";
   User_Agent_Header         : constant String := "User-Agent";
   Location_Header           : constant String := "Location";
   WWW_Authenticate_Header   : constant String := "WWW-Authenticate";
   Docker_Content_Digest     : constant String := "Docker-Content-Digest";

   ---------------------------------------------------------------------------
   --  HTTP Request Functions
   ---------------------------------------------------------------------------

   --  Perform GET request
   function Get
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform POST request with body
   function Post
     (URL          : String;
      Body         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform PUT request with body
   function Put
     (URL          : String;
      Body         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform DELETE request
   function Delete
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform HEAD request (response body will be empty)
   function Head
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform PATCH request with body
   function Patch
     (URL          : String;
      Body         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   ---------------------------------------------------------------------------
   --  Streaming/Large Body Support
   ---------------------------------------------------------------------------

   --  Download to file (for large blobs)
   function Download_To_File
     (URL         : String;
      Output_Path : String;
      Config      : HTTP_Client_Config := Default_Config;
      Auth        : Auth_Credentials := No_Credentials;
      Headers     : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and Output_Path'Length > 0;

   --  Upload from file (for large blobs)
   function Upload_From_File
     (URL          : String;
      Input_Path   : String;
      Content_Type : String := "application/octet-stream";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and Input_Path'Length > 0;

   ---------------------------------------------------------------------------
   --  Authentication Helpers
   ---------------------------------------------------------------------------

   --  Create Basic auth credentials
   function Make_Basic_Auth
     (Username : String;
      Password : String) return Auth_Credentials
   with Global => null;

   --  Create Bearer token credentials
   function Make_Bearer_Auth (Token : String) return Auth_Credentials
   with Global => null;

   --  Parse WWW-Authenticate header (for Docker Registry v2 auth flow)
   --  Returns realm, service, and scope from the header
   type WWW_Auth_Challenge is record
      Realm   : Unbounded_String;
      Service : Unbounded_String;
      Scope   : Unbounded_String;
   end record;

   function Parse_WWW_Authenticate (Header_Value : String) return WWW_Auth_Challenge
   with Global => null;

   ---------------------------------------------------------------------------
   --  URL Utilities
   ---------------------------------------------------------------------------

   --  URL encode a string (for query parameters)
   function URL_Encode (S : String) return String
   with Global => null;

   --  Build URL with query parameters
   function Build_URL
     (Base_URL : String;
      Path     : String;
      Query    : Header_Map := Header_Maps.Empty_Map) return String
   with Global => null;

   --  Join URL path segments
   function Join_Path (Base : String; Path : String) return String
   with Global => null;

end CT_HTTP;
