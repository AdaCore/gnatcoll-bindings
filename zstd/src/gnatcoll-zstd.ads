------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Direct binding to ZSTD API. Note that streaming API binding is located in
--  GNATCOLL.ZSTD.Streams.

with Interfaces.C;
with System;

package GNATCOLL.ZSTD is

   package C renames Interfaces.C;
   use all type C.unsigned_long_long;

   ZSTD_Error : exception;

   -------------
   -- Version --
   -------------

   function ZSTD_Version_Number return C.unsigned;
   pragma Import (C, ZSTD_Version_Number, "ZSTD_versionNumber");
   --  Return runtime library version, the value is (MAJOR*100*100 +
   --  MINOR*100 + RELEASE).

   function ZSTD_Version_String return String;
   --  Return runtime library version, like "1.4.5". Requires v1.3.0+.

   ----------------
   -- Simple API --
   ----------------

   function ZSTD_Compress
     (Dst               : System.Address;
      Dst_Capacity      : C.size_t;
      Src               : System.Address;
      Src_Size          : C.size_t;
      Compression_Level : Integer) return C.size_t;
   pragma Import (C, ZSTD_Compress, "ZSTD_compress");
   --  Compresses Src content as a single ZSTD compressed frame into already
   --  allocated Dst. Note that compression runs faster if Dst_Capacity is
   --  superior or equal than ZSTD_compressBound (Src_Size).
   --
   --  Return the compressed size written into Dst or an error code (see
   --  ZSTD_Is_Error).

   function ZSTD_Decompress
     (Dst             : System.Address;
      Dst_Capacity    : C.size_t;
      Src             : System.Address;
      Compressed_Size : C.size_t) return C.size_t;
   pragma Import (C, ZSTD_Decompress, "ZSTD_decompress");
   --  Decompress Src content into Dst. Compressed_Size is the size of Src and
   --  must be the exact size of some number compress/skippable frames.
   --  Dst_Capacity should be superior to the content original size. If an
   --  upper bound for the original size cannot be guessed then stream API
   --  should be used.
   --  Return the number of bytes decompressed into Dst or an error code
   --  (see ZSTD_Is_Error).

   ZSTD_CONTENTSIZE_UNKNOWN : constant C.unsigned_long_long := 0 - 1;
   ZSTD_CONTENTSIZE_ERROR   : constant C.unsigned_long_long := 0 - 2;

   function ZSTD_Get_Frame_Content_Size
     (Src : System.Address; Src_Size : C.size_t) return C.unsigned_long_long;
   pragma
     Import (C, ZSTD_Get_Frame_Content_Size, "ZSTD_Get_Frame_Content_Size");
   --  Return the decompressed size of the frame of size pointed by Src.
   --  Src_Size should be at least the size of a frame header.
   --
   --  Return ZSTD_CONTENTSIZE_UNKNOWN if the size cannot be determined.
   --  Return ZSTD_CONTENTSIZE_ERROR if an error occured

   function ZSTD_Find_Frame_Compressed_Size
     (Src : System.Address; Src_Size : C.size_t) return C.size_t;
   pragma
     Import
       (C, ZSTD_Find_Frame_Compressed_Size, "ZSTD_findFrameCompressedSize");
   --  Return the compressed size of the first frame starting at Src. The
   --  returned value is suitable as Src_Size parameter for ZSTD_Decompress.
   --  An error code is returned if the input if invalid.

   ----------------------
   -- Helper functions --
   ----------------------

   function ZSTD_Compress_Bound (Src_Size : C.size_t) return C.size_t;
   pragma Import (C, ZSTD_Compress_Bound, "ZSTD_compressBound");
   --  Maximum compressed size in worst case single-pass scenario.
   --  When invoking ZSTD_Compress or any other one-pass compression
   --  function, it's recommended to provide Dst_Size >=
   --  ZSTD_Compress_Bound(Src_Size) as it eliminates one potential failure
   --  scenario.

   function ZSTD_Is_Error (Code : C.size_t) return Boolean;
   pragma Inline (ZSTD_Is_Error);
   --  Most ZSTD_* functions returning a size_t value can be tested for error,
   --  using ZSTD_Is_Error.

   function ZSTD_Get_Error_Name (Code : C.size_t) return String;
   --  Provides readable string from an error code

   function ZSTD_Min_C_Level return Integer;
   pragma Import (C, ZSTD_Min_C_Level, "ZSTD_Min_C_Level");
   --  Minimum negative compression level allowed

   function ZSTD_Max_C_Level return Integer;
   pragma Import (C, ZSTD_Max_C_Level, "ZSTD_Max_C_Level");
   --  Maximum compression level available

   function ZSTD_Default_C_Level return Integer;
   pragma Import (C, ZSTD_Default_C_Level, "ZSTD_Default_C_Level");
   --  Default compression level

   ----------------------
   -- Explicit context --
   ----------------------

   --  Compression context

   --  When compressing many times,
   --  it is recommended to allocate a context just once,
   --  and re-use it for each successive compression operation.
   --  This will make workload friendlier for system's memory.
   --  Note : re-using context is just a speed / resource optimization.
   --         It doesn't change the compression ratio, which remains identical.
   --  Note 2 : In multi-threaded environments,
   --           use one different context per thread for parallel execution.

   type ZSTD_CCtx is private;
   Null_ZSTD_CCtx : constant ZSTD_CCtx;

   function ZSTD_Create_CCtx return ZSTD_CCtx;
   pragma Import (C, ZSTD_Create_CCtx, "ZSTD_createCCtx");

   function ZSTD_Free_CCtx (CCtx : ZSTD_CCtx) return C.size_t;
   pragma Import (C, ZSTD_Free_CCtx, "ZSTD_freeCCtx");

   function ZSTD_Compress_CCtx
     (CCtx              : ZSTD_CCtx;
      Dst               : System.Address;
      Dst_Capacity      : C.size_t;
      Src               : System.Address;
      Src_Size          : C.size_t;
      Compression_Level : Integer) return C.size_t;
   pragma Import (C, ZSTD_Compress_CCtx, "ZSTD_compressCCtx");
   --  Same as ZSTD_Compress(), using an explicit ZSTD_CCtx.
   --  Important : in order to behave similarly to `ZSTD_Compress()`,
   --  this function compresses at requested compression level,
   --  __ignoring any other parameter__ .
   --  If any advanced parameter was set using the advanced API,
   --  they will all be reset. Only `compression_Level` remains.

   --  Decompression context

   --  When decompressing many times,
   --  it is recommended to allocate a context only once,
   --  and re-use it for each successive compression operation.
   --  This will make workload friendlier for system's memory.
   --  Use one context per thread for parallel execution.

   type ZSTD_DCtx is private;
   Null_ZSTD_DCtx : constant ZSTD_DCtx;

   function ZSTD_Create_DCtx return ZSTD_DCtx;
   pragma Import (C, ZSTD_Create_DCtx, "ZSTD_createDCtx");

   function ZSTD_Free_DCtx (DCtx : ZSTD_DCtx) return C.size_t;
   pragma Import (C, ZSTD_Free_DCtx, "ZSTD_freeDCtx");

   function ZSTD_Decompress_DCtx
     (DCtx         : ZSTD_DCtx;
      Dst          : System.Address;
      Dst_Capacity : C.size_t;
      Src          : System.Address;
      Src_Size     : C.size_t) return C.size_t;
   pragma Import (C, ZSTD_Decompress_DCtx, "ZSTD_decompressDCtx");
   --    Same as ZSTD_Decompress(),
   --    requires an allocated ZSTD_DCtx.
   --    Compatible with sticky parameters.

   ------------------------------
   -- Advanced compression API --
   ------------------------------

   type ZSTD_Strategy is
     (ZSTD_fast,
      ZSTD_dfast,
      ZSTD_greedy,
      ZSTD_lazy,
      ZSTD_lazy2,
      ZSTD_btlazy2,
      ZSTD_btopt,
      ZSTD_btultra,
      ZSTD_btultra2);

   for ZSTD_Strategy use
     (ZSTD_fast     => 1,
      ZSTD_dfast    => 2,
      ZSTD_greedy   => 3,
      ZSTD_lazy     => 4,
      ZSTD_lazy2    => 5,
      ZSTD_btlazy2  => 6,
      ZSTD_btopt    => 7,
      ZSTD_btultra  => 8,
      ZSTD_btultra2 => 9);

   --  Compression parameters
   type ZSTD_C_Parameter is
     (ZSTD_C_Compression_Level,
      --  Set compression parameters according to pre-defined cLevel table.

      -------------------------------------
      -- Advanced compression parameters --
      -------------------------------------
      --  It's possible to pin down compression parameters to some specific
      --  values. In which case, these values are no longer dynamically
      --  selected by the compressor. See ZSTD reference documentation for
      --  more details on each parameter

      ZSTD_C_Window_Log,
      --  Maximum allowed back-reference distance, expressed as power of 2.
      ZSTD_C_Hash_Log,
      --  Size of the initial probe table, as a power of 2.
      ZSTD_C_Chain_Log,
      --  Size of the multi-probe search table, as a power of 2.
      ZSTD_C_Search_Log,
      --  Number of search attempts, as a power of 2.
      ZSTD_C_Min_Match,
      --  Minimum size of searched matches.
      ZSTD_C_Target_Length,
      --  Impact of this field depends on strategy.
      --  For strategies btopt, btultra & btultra2:
      --      Length of Match considered "good enough" to stop search.
      --      Larger values make compression stronger, and slower.
      --  For strategy fast:
      --      Distance between match sampling.
      --      Larger values make compression faster, and weaker.
      --  Special: value 0 means "use default targetLength".
      ZSTD_C_Strategy,
      --  See ZSTD_strategy enum definition.
      --  The higher the value of selected strategy, the more complex it is,
      --  resulting in stronger and slower compression.
      --  Special: value 0 means "use default strategy".

      --  LDM mode parameters
      ZSTD_C_Enable_Long_Distance_Matching,
      --  Enable long distance matching.
      ZSTD_C_Ldm_Hash_Log,
      --  Size of the table for long distance matching, as a power of 2.
      ZSTD_C_Ldm_Min_Match,
      --  Minimum match size for long distance matcher.
      ZSTD_C_Ldm_Bucket_Size_Log,
      --  Log size of each bucket in the LDM hash table for collision
      --  resolution.
      ZSTD_C_Ldm_Hash_Rate_Log,
      --  Frequency of inserting/looking up entries into the LDM hash table.

      --  Frame parameters
      ZSTD_C_Content_Size_Flag,
      --  Content size will be written into frame header whenever known
      ZSTD_C_Checksum_Flag,
      --  A 32-bits checksum of content is written at end of frame
      ZSTD_C_Dict_ID_Flag,
      --  When applicable, dictionary's ID is written into frame header

      --------------------------------
      -- Multi-threading parameters --
      --------------------------------
      --  These parameters are only active if multi-threading is enabled
      --  (compiled with build macro ZSTD_MULTITHREAD). Otherwise, trying to
      --  set any other value than default (0) will be a no-op and return an
      --  error. In a situation where it's unknown if the linked library
      --  supports multi-threading or not, setting ZSTD_c_nbWorkers to any
      --  value >= 1 and consulting the return value provides a quick way to
      --  check this property.

      ZSTD_C_Nb_Workers,
      --   Select how many threads will be spawned to compress in parallel.
      ZSTD_C_Job_Size,
      --  Size of a compression job.
      ZSTD_C_Overlap_Log
      --  Control the overlap size, as a fraction of window size.
     );

   for ZSTD_C_Parameter use
     (ZSTD_C_Compression_Level             => 100,
      ZSTD_C_Window_Log                    => 101,
      ZSTD_C_Hash_Log                      => 102,
      ZSTD_C_Chain_Log                     => 103,
      ZSTD_C_Search_Log                    => 104,
      ZSTD_C_Min_Match                     => 105,
      ZSTD_C_Target_Length                 => 106,
      ZSTD_C_Strategy                      => 107,
      ZSTD_C_Enable_Long_Distance_Matching => 160,
      ZSTD_C_Ldm_Hash_Log                  => 161,
      ZSTD_C_Ldm_Min_Match                 => 162,
      ZSTD_C_Ldm_Bucket_Size_Log           => 163,
      ZSTD_C_Ldm_Hash_Rate_Log             => 164,
      ZSTD_C_Content_Size_Flag             => 200,
      ZSTD_C_Checksum_Flag                 => 201,
      ZSTD_C_Dict_ID_Flag                  => 202,
      ZSTD_C_Nb_Workers                    => 400,
      ZSTD_C_Job_Size                      => 401,
      ZSTD_C_Overlap_Log                   => 402);

   type ZSTD_Bounds is record
      Error       : C.size_t;
      Lower_Bound : Integer;
      Upper_Bound : Integer;
   end record;

   function ZSTD_C_Param_Get_Bounds
     (C_Param : ZSTD_C_Parameter) return ZSTD_Bounds;
   pragma Import (C, ZSTD_C_Param_Get_Bounds, "ZSTD_cParam_getBounds");
   --  Get parameter bounds (lower and upper)

   function ZSTD_CCtx_Set_Parameter
     (CCtx : ZSTD_CCtx; Param : ZSTD_C_Parameter; Value : Integer)
      return C.size_t;
   pragma Import (C, ZSTD_CCtx_Set_Parameter, "ZSTD_CCtx_setParameter");
   --  Set one compression parameter.
   --  All parameters have valid bounds. Bounds can be queried using
   --  ZSTD_cParam_getBounds(). Providing a value beyond bound will either
   --  clamp it, or trigger an error (depending on parameter).
   --  Setting a parameter is generally only possible during frame
   --  initialization (before starting compression). See ZSTD documentation
   --  for exceptions.
   --  Return an error code (which can be tested using ZSTD_isError()).

   function ZSTD_CCtx_Set_Pledged_Src_Size
     (cctx : ZSTD_CCtx; Pledged_Src_Size : C.unsigned_long_long)
      return C.size_t;
   pragma
     Import (C, ZSTD_CCtx_Set_Pledged_Src_Size, "ZSTD_CCtx_setPledgedSrcSize");
   --  Set the total input data size to be compressed as a single frame.
   --  Value will be written in frame header, unless if explicitly forbidden
   --  using ZSTD_c_contentSizeFlag.
   --  This value will also be controlled at end of frame, and trigger an
   --  error if not respected. See ZSTD documentation for more details.
   --  Return an error code (which can be tested using ZSTD_isError()).

   type ZSTD_Reset_Directive is
     (ZSTD_Reset_Session_Only,
      ZSTD_Reset_Parameters,
      ZSTD_Reset_Session_And_Parameters);

   for ZSTD_Reset_Directive use
     (ZSTD_Reset_Session_Only           => 1,
      ZSTD_Reset_Parameters             => 2,
      ZSTD_Reset_Session_And_Parameters => 3);

   function ZSTD_CCtx_Reset
     (cctx : ZSTD_CCtx; Reset : ZSTD_Reset_Directive) return C.size_t;
   pragma Import (C, ZSTD_CCtx_Reset, "ZSTD_CCtx_reset");
   --  Reset a compression context.
   --  There are 2 different things that can be reset, independently or
   --  jointly : the session and the parameters.
   --  Return an error code (which can be tested using ZSTD_isError()).

   function ZSTD_Compress2
     (CCtx         : ZSTD_CCtx;
      Dst          : System.Address;
      Dst_Capacity : C.size_t;
      Src          : System.Address;
      Src_Size     : C.size_t) return C.size_t;
   pragma Import (C, ZSTD_Compress2, "ZSTD_compress2");
   --  Behave the same as ZSTD_compressCCtx(), but compression parameters
   --  are set using the advanced API. ZSTD_compress2() always starts a new
   --  frame.
   --  Return compressed size written into Dst or an error code if it fails
   --  (which can be tested using ZSTD_isError()).

   --------------------------------
   -- Advanced decompression API --
   --------------------------------

   type ZSTD_D_Parameter is (ZSTD_D_Window_Log_Max);

   for ZSTD_D_Parameter use (ZSTD_D_Window_Log_Max => 100);

   function ZSTD_D_Param_Get_Bounds
     (D_Param : ZSTD_D_Parameter) return ZSTD_Bounds;
   pragma Import (C, ZSTD_D_Param_Get_Bounds, "ZSTD_dParam_getBounds");
   --  Same as ZSTD_C_Param_Get_Bounds for decompression parameters.

   function ZSTD_DCtx_Set_Parameter
     (DCtx : ZSTD_DCtx; Param : ZSTD_D_Parameter; Value : Integer)
      return C.size_t;
   pragma Import (C, ZSTD_DCtx_Set_Parameter, "ZSTD_DCtx_setParameter");
   --  Same as ZSTD_CCtx_Set_Parameter for decompression parameters.

   function ZSTD_DCtx_Reset
     (DCtx : ZSTD_DCtx; Reset : ZSTD_Reset_Directive) return C.size_t;
   pragma Import (C, ZSTD_DCtx_Reset, "ZSTD_DCtx_reset");
   --  Reset a decompression context.
   --  There are 2 different things that can be reset, independently or
   --  jointly : the session and the parameters.
   --  Return an error code (which can be tested using ZSTD_isError()).

private

   type ZSTD_CCtx is new System.Address;
   Null_ZSTD_CCtx : constant ZSTD_CCtx := ZSTD_CCtx (System.Null_Address);

   type ZSTD_DCtx is new System.Address;
   Null_ZSTD_DCtx : constant ZSTD_DCtx := ZSTD_DCtx (System.Null_Address);

end GNATCOLL.ZSTD;
