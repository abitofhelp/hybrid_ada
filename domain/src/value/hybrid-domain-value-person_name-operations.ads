pragma Ada_2022;
--  ==========================================================================
--  Hybrid.Domain.Value.Person_Name.Operations - Port-generic API
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Notes:
--    - Ada 2022; aspects preferred over pragmas when available.
--    - 78-character separators enforced.
--  ==========================================================================

with Hybrid.Domain.Foundation.Ports.Result_Port;
with Hybrid.Domain.Error;

package Hybrid.Domain.Value.Person_Name.Operations is
   generic
      with package R is new
        Hybrid.Domain.Foundation.Ports.Result_Port
          (T      => Hybrid.Domain.Value.Person_Name.Person_Name,
           E      => Hybrid.Domain.Error.Domain_Error,
           others => <>);
   package API is
      function Create (Raw : String) return R.Result
      with Inline;
   end API;
end Hybrid.Domain.Value.Person_Name.Operations;
