--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
--  Palimpsest-Covenant: 1.0

package Cerro_CLI is

   --  Import a package from an upstream source
   --  Usage: cerro import debian:hello/2.10-3
   procedure Run_Import;

   --  Build a package from its manifest
   --  Usage: cerro build manifests/hello.ctp
   procedure Run_Build;

   --  Verify a manifest and its provenance chain
   --  Usage: cerro verify manifests/hello.ctp
   procedure Run_Verify;

   --  Export a built package to OCI or other format
   --  Usage: cerro export --format=oci hello:2.10-3
   procedure Run_Export;

   --  Inspect a package's details and provenance
   --  Usage: cerro inspect hello
   procedure Run_Inspect;

end Cerro_CLI;
