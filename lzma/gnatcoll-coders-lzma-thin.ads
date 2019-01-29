------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

--  This package provides thin binding to LZMA compression/decompression

with Interfaces.C; use Interfaces;
with System;

package GNATCOLL.Coders.LZMA.Thin is

   type lzma_allocator is record
      alloc : access function
        (opaque : System.Address;
         nmemb  : C.size_t;
         size   : C.size_t) return System.Address;
      free : access procedure (opaque : System.Address; ptr : System.Address);
      opaque : System.Address;
   end record with Convention => C;
   --  alloc is a pointer to a custom memory allocation function
   --
   --  If you don't want a custom allocator, but still want
   --  custom free(), set this to NULL and liblzma will use
   --  the standard malloc().
   --
   --  \param       opaque  lzma_allocator.opaque (see below)
   --  \param       nmemb   Number of elements like in calloc(). liblzma
   --                       will always set nmemb to 1, so it is safe to
   --                       ignore nmemb in a custom allocator if you like.
   --                       The nmemb argument exists only for
   --                       compatibility with zlib and libbzip2.
   --  \param       size    Size of an element in bytes.
   --                       liblzma never sets this to zero.
   --
   --  \return      Pointer to the beginning of a memory block of
   --               `size' bytes, or NULL if allocation fails
   --               for some reason. When allocation fails, functions
   --               of liblzma return LZMA_MEM_ERROR.
   --
   --  The allocator should not waste time zeroing the allocated buffers.
   --  This is not only about speed, but also memory usage, since the
   --  operating system kernel doesn't necessarily allocate the requested
   --  memory in physical memory until it is actually used. With small
   --  input files, liblzma may actually need only a fraction of the
   --  memory that it requested for allocation.
   --
   --  \note        LZMA_MEM_ERROR is also used when the size of the
   --               allocation would be greater than SIZE_MAX. Thus,
   --               don't assume that the custom allocator must have
   --               returned NULL if some function from liblzma
   --               returns LZMA_MEM_ERROR.

   type lzma_stream is record
      next_in        : access Stream_Element          := null;
      avail_in       : aliased C.size_t               := 0;
      total_in       : aliased Unsigned_64            := 0;
      next_out       : access Stream_Element          := null;
      avail_out      : aliased C.size_t               := 0;
      total_out      : aliased Unsigned_64            := 0;
      allocator      : access constant lzma_allocator := null;
      internal       : System.Address                 := System.Null_Address;
      reserved_ptr1  : System.Address                 := System.Null_Address;
      reserved_ptr2  : System.Address                 := System.Null_Address;
      reserved_ptr3  : System.Address                 := System.Null_Address;
      reserved_ptr4  : System.Address                 := System.Null_Address;
      reserved_int1  : aliased Unsigned_64            := 0;
      reserved_int2  : aliased Unsigned_64            := 0;
      reserved_int3  : aliased C.size_t               := 0;
      reserved_int4  : aliased C.size_t               := 0;
      reserved_enum1 : aliased C.int                  := 0;
      reserved_enum2 : aliased C.int                  := 0;
   end record with Convention => C;
   --  Passing data to and from liblzma
   --
   --  The lzma_stream structure is used for
   --   - passing pointers to input and output buffers to liblzma;
   --   - defining custom memory hander functions; and
   --   - holding a pointer to coder-specific internal data structures.
   --
   --  Typical usage:
   --
   --   - After allocating lzma_stream (on stack or with malloc()), it must be
   --     initialized to LZMA_STREAM_INIT (see LZMA_STREAM_INIT for details).
   --
   --   - Initialize a coder to the lzma_stream, for example by using
   --     lzma_easy_encoder() or lzma_auto_decoder(). Some notes:
   --      - In contrast to zlib, strm->next_in and strm->next_out are
   --        ignored by all initialization functions, thus it is safe
   --        to not initialize them yet.
   --      - The initialization functions always set strm->total_in and
   --        strm->total_out to zero.
   --      - If the initialization function fails, no memory is left allocated
   --        that would require freeing with lzma_end() even if some memory was
   --        associated with the lzma_stream structure when the initialization
   --        function was called.
   --
   --   - Use lzma_code() to do the actual work.
   --
   --   - Once the coding has been finished, the existing lzma_stream can be
   --     reused. It is OK to reuse lzma_stream with different initialization
   --     function without calling lzma_end() first. Old allocations are
   --     automatically freed.
   --
   --   - Finally, use lzma_end() to free the allocated memory. lzma_end()
   --     never frees the lzma_stream structure itself.
   --
   --  Application may modify the values of total_in and total_out as it wants.
   --  They are updated by liblzma to match the amount of data read and
   --  written but aren't used for anything else except as a possible return
   --  values from lzma_get_progress().

   subtype lzma_check is C.unsigned;
   --  Type of the integrity check (Check ID)
   --
   --  The .xz format supports multiple types of checks that are calculated
   --  from the uncompressed data. They vary in both speed and ability to
   --  detect errors.

   LZMA_CHECK_NONE   : constant lzma_check := 0;
   --  No Check is calculated.
   --  Size of the Check field: 0 bytes

   LZMA_CHECK_CRC32  : constant lzma_check := 1;
   --  CRC32 using the polynomial from the IEEE 802.3 standard
   --  Size of the Check field: 4 bytes

   LZMA_CHECK_CRC64  : constant lzma_check := 4;
   --  CRC64 using the polynomial from the ECMA-182 standard
   --  Size of the Check field: 8 bytes

   LZMA_CHECK_SHA256 : constant lzma_check := 10;
   --  SHA-256
   --  Size of the Check field: 32 bytes

   type lzma_ret is
     (LZMA_OK,
      --  Operation completed successfully
      --
      LZMA_STREAM_END,
      --  End of stream was reached
      --
      --  In encoder, LZMA_SYNC_FLUSH, LZMA_FULL_FLUSH, or
      --  LZMA_FINISH was finished. In decoder, this indicates
      --  that all the data was successfully decoded.
      --
      --  In all cases, when LZMA_STREAM_END is returned, the last
      --  output bytes should be picked from strm->next_out.
      --
      LZMA_NO_CHECK,
      --  Input stream has no integrity check
      --
      --  This return value can be returned only if the
      --  LZMA_TELL_NO_CHECK flag was used when initializing
      --  the decoder. LZMA_NO_CHECK is just a warning, and
      --  the decoding can be continued normally.
      --
      --  It is possible to call lzma_get_check() immediately after
      --  lzma_code has returned LZMA_NO_CHECK. The result will
      --  naturally be LZMA_CHECK_NONE, but the possibility to call
      --  lzma_get_check() may be convenient in some applications.
      --
      LZMA_UNSUPPORTED_CHECK,
      --  Cannot calculate the integrity check
      --
      --  The usage of this return value is different in encoders
      --  and decoders.
      --
      --  Encoders can return this value only from the initialization
      --  function. If initialization fails with this value, the
      --  encoding cannot be done, because there's no way to produce
      --  output with the correct integrity check.
      --
      --  Decoders can return this value only from lzma_code() and
      --  only if the LZMA_TELL_UNSUPPORTED_CHECK flag was used when
      --  initializing the decoder. The decoding can still be
      --  continued normally even if the check type is unsupported,
      --  but naturally the check will not be validated, and possible
      --  errors may go undetected.
      --
      --  With decoder, it is possible to call lzma_get_check()
      --  immediately after lzma_code() has returned
      --  LZMA_UNSUPPORTED_CHECK. This way it is possible to find
      --  out what the unsupported Check ID was.

      LZMA_GET_CHECK,
      --  Integrity check type is now available
      --
      --  This value can be returned only by the lzma_code() function
      --  and only if the decoder was initialized with the
      --  LZMA_TELL_ANY_CHECK flag. LZMA_GET_CHECK tells the
      --  application that it may now call lzma_get_check() to find
      --  out the Check ID. This can be used, for example, to
      --  implement a decoder that accepts only files that have
      --  strong enough integrity check.

      LZMA_MEM_ERROR,
      --  Cannot allocate memory
      --
      --  Memory allocation failed, or the size of the allocation
      --  would be greater than SIZE_MAX.
      --
      --  Due to internal implementation reasons, the coding cannot
      --  be continued even if more memory were made available after
      --  LZMA_MEM_ERROR.

      LZMA_MEMLIMIT_ERROR,
      --  Memory usage limit was reached
      --
      --  Decoder would need more memory than allowed by the
      --  specified memory usage limit. To continue decoding,
      --  the memory usage limit has to be increased with
      --  lzma_memlimit_set().

      LZMA_FORMAT_ERROR,
      --  File format not recognized
      --
      --  The decoder did not recognize the input as supported file
      --  format. This error can occur, for example, when trying to
      --  decode .lzma format file with lzma_stream_decoder,
      --  because lzma_stream_decoder accepts only the .xz format.

      LZMA_OPTIONS_ERROR,
      --  Invalid or unsupported options
      --
      --  Invalid or unsupported options, for example
      --   - unsupported filter(s) or filter options; or
      --   - reserved bits set in headers (decoder only).
      --
      --  Rebuilding liblzma with more features enabled, or
      --  upgrading to a newer version of liblzma may help.

      LZMA_DATA_ERROR,
      --  Data is corrupt
      --
      --  The usage of this return value is different in encoders
      --  and decoders. In both encoder and decoder, the coding
      --  cannot continue after this error.
      --
      --  Encoders return this if size limits of the target file
      --  format would be exceeded. These limits are huge, thus
      --  getting this error from an encoder is mostly theoretical.
      --  For example, the maximum compressed and uncompressed
      --  size of a .xz Stream is roughly 8 EiB (2^63 bytes).
      --
      --  Decoders return this error if the input data is corrupt.
      --  This can mean, for example, invalid CRC32 in headers
      --  or invalid check of uncompressed data.

      LZMA_BUF_ERROR,
      --  No progress is possible
      --
      --  This error code is returned when the coder cannot consume
      --  any new input and produce any new output. The most common
      --  reason for this error is that the input stream being
      --  decoded is truncated or corrupt.
      --
      --  This error is not fatal. Coding can be continued normally
      --  by providing more input and/or more output space, if
      --  possible.
      --
      --  Typically the first call to lzma_code() that can do no
      --  progress returns LZMA_OK instead of LZMA_BUF_ERROR. Only
      --  the second consecutive call doing no progress will return
      --  LZMA_BUF_ERROR. This is intentional.
      --
      --  With zlib, Z_BUF_ERROR may be returned even if the
      --  application is doing nothing wrong, so apps will need
      --  to handle Z_BUF_ERROR specially. The above hack
      --  guarantees that liblzma never returns LZMA_BUF_ERROR
      --  to properly written applications unless the input file
      --  is truncated or corrupt. This should simplify the
      --  applications a little.

      LZMA_PROG_ERROR
      --  Programming error
      --
      --  This indicates that the arguments given to the function are
      --  invalid or the internal state of the decoder is corrupt.
      --    - Function arguments are invalid or the structures
      --      pointed by the argument pointers are invalid
      --      e.g. if strm->next_out has been set to NULL and
      --      strm->avail_out > 0 when calling lzma_code().
      --    - lzma_* functions have been called in wrong order
      --      e.g. lzma_code() was called right after lzma_end().
      --    - If errors occur randomly, the reason might be flaky
      --      hardware.
      --
      --  If you think that your code is correct, this error code
      --  can be a sign of a bug in liblzma. See the documentation
      --  how to report bugs.
     ) with Convention => C;
   --  Return values used by several functions in liblzma
   --
   --  Check the descriptions of specific functions to find out which return
   --  values they can return. With some functions the return values may have
   --  more specific meanings than described here; those differences are
   --  described per-function basis.

   LZMA_PRESET_EXTREME : constant := 2 ** 31;
   --  Extreme compression preset
   --
   --  This flag modifies the preset to make the encoding significantly slower
   --  while improving the compression ratio only marginally. This is useful
   --  when you don't mind wasting time to get as small result as possible.
   --
   --  This flag doesn't affect the memory usage requirements of the decoder
   --  (at least not significantly). The memory usage of the encoder may be
   --  increased a little but only at the lowest preset levels (0-3).

   function lzma_easy_encoder
     (strm   : access lzma_stream;
      preset : Unsigned_32;
      check  : lzma_check) return lzma_ret
     with Import, Convention => C, External_Name => "lzma_easy_encoder";
   --  Initialize .xz Stream encoder using a preset number
   --
   --  This function is intended for those who just want to use the basic
   --  features if liblzma (that is, most developers out there).
   --
   --  \param    strm    Pointer to lzma_stream that is at least initialized
   --                    with LZMA_STREAM_INIT.
   --  \param    preset  Compression preset to use. A preset consist of level
   --                    number and zero or more flags. Usually flags aren't
   --                    used, so preset is simply a number [0, 9] which match
   --                    the options -0 ... -9 of the xz command line tool.
   --                    Additional flags can be be set using bitwise-or with
   --                    the preset level number, e.g. 6 | LZMA_PRESET_EXTREME.
   --  \param    check   Integrity check type to use. See check.h for available
   --                    checks. The xz command line tool defaults to
   --                    LZMA_CHECK_CRC64, which is a good choice if you are
   --                    unsure. LZMA_CHECK_CRC32 is good too as long as the
   --                    uncompressed file is not many gigabytes.
   --
   --  \return     - LZMA_OK: Initialization succeeded. Use lzma_code() to
   --                encode your data.
   --              - LZMA_MEM_ERROR: Memory allocation failed.
   --              - LZMA_OPTIONS_ERROR: The given compression preset is not
   --                supported by this build of liblzma.
   --              - LZMA_UNSUPPORTED_CHECK: The given check type is not
   --                supported by this liblzma build.
   --              - LZMA_PROG_ERROR: One or more of the parameters have values
   --                that will never be valid. For example, strm == NULL.
   --
   --  If initialization fails (return value is not LZMA_OK), all the memory
   --  allocated for *strm by liblzma is always freed. Thus, there is no need
   --  to call lzma_end() after failed initialization.
   --
   --  If initialization succeeds, use lzma_code() to do the actual encoding.
   --  Valid values for `action' (the second argument of lzma_code()) are
   --  LZMA_RUN, LZMA_SYNC_FLUSH, LZMA_FULL_FLUSH, and LZMA_FINISH. In future,
   --  there may be compression levels or flags that don't support
   --  LZMA_SYNC_FLUSH.

   type lzma_filter is null record; -- Not supported in this bind version

   ----------------------------
   -- Multithreading options --
   ----------------------------

   type lzma_mt is record
      flags      : Unsigned_32 := 0; -- No flags are currently supported.
      threads    : Unsigned_32 := 0; -- Number of worker threads to use
      block_size : Unsigned_64 := 0;
      --  Maximum uncompressed size of a Block.
      --  The encoder will start a new .xz Block every block_size bytes.
      --  Using LZMA_FULL_FLUSH or LZMA_FULL_BARRIER with lzma_code()
      --  the caller may tell liblzma to start a new Block earlier.
      --  With LZMA2, a recommended block size is 2-4 times the LZMA2
      --  dictionary size. With very small dictionaries, it is recommended
      --  to use at least 1 MiB block size for good compression ratio, even
      --  if this is more than four times the dictionary size. Note that
      --  these are only recommendations for typical use cases; feel free
      --  to use other values. Just keep in mind that using a block size
      --  less than the LZMA2 dictionary size is waste of RAM.
      --  Set this to 0 to let liblzma choose the block size depending
      --  on the compression options. For LZMA2 it will be 3*dict_size
      --  or 1 MiB, whichever is more.
      --  For each thread, about 3 * block_size bytes of memory will be
      --  allocated. This may change in later liblzma versions. If so,
      --  the memory usage will probably be reduced, not increased.

      timeout : Unsigned_32 := 0;
      --  Timeout to allow lzma_code() to return early
      --  Multithreading can make liblzma to consume input and produce
      --  output in a very bursty way: it may first read a lot of input
      --  to fill internal buffers, then no input or output occurs for
      --  a while.
      --  In single-threaded mode, lzma_code() won't return until it has
      --  either consumed all the input or filled the output buffer. If
      --  this is done in multithreaded mode, it may cause a call
      --  lzma_code() to take even tens of seconds, which isn't acceptable
      --  in all applications.
      --  To avoid very long blocking times in lzma_code(), a timeout
      --  (in milliseconds) may be set here. If lzma_code() would block
      --  longer than this number of milliseconds, it will return with
      --  LZMA_OK. Reasonable values are 100 ms or more. The xz command
      --  line tool uses 300 ms.
      --  If long blocking times are fine for you, set timeout to a special
      --  value of 0, which will disable the timeout mechanism and will make
      --  lzma_code() block until all the input is consumed or the output
      --  buffer has been filled.
      --  Even with a timeout, lzma_code() might sometimes take
      --  somewhat long time to return. No timing guarantees
      --  are made.

      preset : Unsigned_32 := 0;
      --  Compression preset (level and possible flags)
      --  The preset is set just like with lzma_easy_encoder().
      --  The preset is ignored if filters below is non-NULL.

      filters : access constant lzma_filter;
      --  Filter chain (alternative to a preset)
      --  If this is NULL, the preset above is used. Otherwise the preset
      --  is ignored and the filter chain specified here is used.

      check : lzma_check := 0;
      --  Integrity check type
      --  See check.h for available checks. The xz command line tool
      --  defaults to LZMA_CHECK_CRC64, which is a good choice if you
      --  are unsure.

      reserved_enum1 : C.int := 0;
      reserved_enum2 : C.int := 0;
      reserved_enum3 : C.int := 0;
      reserved_int1  : Unsigned_32 := 0;
      reserved_int2  : Unsigned_32 := 0;
      reserved_int3  : Unsigned_32 := 0;
      reserved_int4  : Unsigned_32 := 0;
      reserved_int5  : Unsigned_64 := 0;
      reserved_int6  : Unsigned_64 := 0;
      reserved_int7  : Unsigned_64 := 0;
      reserved_int8  : Unsigned_64 := 0;
      reserved_ptr1  : System.Address := System.Null_Address;
      reserved_ptr2  : System.Address := System.Null_Address;
      reserved_ptr3  : System.Address := System.Null_Address;
      reserved_ptr4  : System.Address := System.Null_Address;
      --  Reserved space to allow possible future extensions without
      --  breaking the ABI. You should not touch these, because the names
      --  of these variables may change. These are and will never be used
      --  with the currently supported options, so it is safe to leave these
      --  uninitialized.
   end record with Convention => C;

   function lzma_stream_encoder_mt
     (strm    : access lzma_stream;
      options : access constant lzma_mt) return lzma_ret
   with Import, Convention => C, External_Name => "lzma_stream_encoder_mt";
  --  Initialize multithreaded .xz Stream encoder
  --
  --  This provides the functionality of lzma_easy_encoder() and
  --  lzma_stream_encoder() as a single function for multithreaded use.
  --
  --  The supported actions for lzma_code() are LZMA_RUN, LZMA_FULL_FLUSH,
  --  LZMA_FULL_BARRIER, and LZMA_FINISH. Support for LZMA_SYNC_FLUSH might be
  --  added in the future.
  --
  --  strm    Pointer to properly prepared lzma_stream
  --  options Pointer to multithreaded compression options
  --
  --  Returns - LZMA_OK
  --          - LZMA_MEM_ERROR
  --          - LZMA_UNSUPPORTED_CHECK
  --          - LZMA_OPTIONS_ERROR
  --          - LZMA_PROG_ERROR

   function lzma_auto_decoder
     (strm     : access lzma_stream;
      memlimit : Unsigned_64;
      flags    : Unsigned_32) return lzma_ret
     with Import, Convention => C, External_Name => "lzma_auto_decoder";
  --  Decode .xz Streams and .lzma files with autodetection
  --
  --  This decoder autodetects between the .xz and .lzma file formats, and
  --  calls lzma_stream_decoder() or lzma_alone_decoder() once the type
  --  of the input file has been detected.
  --
  --  \param       strm        Pointer to properly prepared lzma_stream
  --  \param       memlimit    Memory usage limit as bytes. Use UINT64_MAX
  --                           to effectively disable the limiter.
  --  \param       flags       Bitwise-or of flags, or zero for no flags.
  --
  --  \return      - LZMA_OK: Initialization was successful.
  --               - LZMA_MEM_ERROR: Cannot allocate memory.
  --               - LZMA_OPTIONS_ERROR: Unsupported flags
  --               - LZMA_PROG_ERROR

   subtype lzma_action is C.unsigned;
   --  The `action' argument for lzma_code()
   --
   --  After the first use of LZMA_SYNC_FLUSH, LZMA_FULL_FLUSH,
   --  LZMA_FULL_BARRIER, or LZMA_FINISH, the same `action' must is used until
   --  lzma_code() returns LZMA_STREAM_END. Also, the amount of input (that is,
   --  strm->avail_in) must not be modified by the application until
   --  lzma_code() returns LZMA_STREAM_END. Changing the `action' or modifying
   --  the amount of input will make lzma_code() return LZMA_PROG_ERROR.

   LZMA_RUN : constant lzma_action := 0;
   --  Continue coding
   --
   --  Encoder: Encode as much input as possible. Some internal
   --  buffering will probably be done (depends on the filter
   --  chain in use), which causes latency: the input used won't
   --  usually be decodeable from the output of the same
   --  lzma_code() call.
   --
   --  Decoder: Decode as much input as possible and produce as
   --  much output as possible.

   LZMA_SYNC_FLUSH : constant lzma_action := 1;
   --  Make all the input available at output
   --
   --  Normally the encoder introduces some latency.
   --  LZMA_SYNC_FLUSH forces all the buffered data to be
   --  available at output without resetting the internal
   --  state of the encoder. This way it is possible to use
   --  compressed stream for example for communication over
   --  network.
   --
   --  Only some filters support LZMA_SYNC_FLUSH. Trying to use
   --  LZMA_SYNC_FLUSH with filters that don't support it will
   --  make lzma_code() return LZMA_OPTIONS_ERROR. For example,
   --  LZMA1 doesn't support LZMA_SYNC_FLUSH but LZMA2 does.
   --
   --  Using LZMA_SYNC_FLUSH very often can dramatically reduce
   --  the compression ratio. With some filters (for example,
   --  LZMA2), fine-tuning the compression options may help
   --  mitigate this problem significantly (for example,
   --  match finder with LZMA2).
   --
   --  Decoders don't support LZMA_SYNC_FLUSH.

   LZMA_FULL_FLUSH : constant lzma_action := 2;
   --  Finish encoding of the current Block
   --
   --  All the input data going to the current Block must have
   --  been given to the encoder (the last bytes can still be
   --  pending in *next_in). Call lzma_code() with LZMA_FULL_FLUSH
   --  until it returns LZMA_STREAM_END. Then continue normally
   --  with LZMA_RUN or finish the Stream with LZMA_FINISH.
   --
   --  This action is currently supported only by Stream encoder
   --  and easy encoder (which uses Stream encoder). If there is
   --  no unfinished Block, no empty Block is created.

   LZMA_FULL_BARRIER : constant lzma_action := 4;
   --  Finish encoding of the current Block
   --
   --  This is like LZMA_FULL_FLUSH except that this doesn't
   --  necessarily wait until all the input has been made
   --  available via the output buffer. That is, lzma_code()
   --  might return LZMA_STREAM_END as soon as all the input
   --  has been consumed (avail_in == 0).
   --
   --  LZMA_FULL_BARRIER is useful with a threaded encoder if
   --  one wants to split the .xz Stream into Blocks at specific
   --  offsets but doesn't care if the output isn't flushed
   --  immediately. Using LZMA_FULL_BARRIER allows keeping
   --  the threads busy while LZMA_FULL_FLUSH would make
   --  lzma_code() wait until all the threads have finished
   --  until more data could be passed to the encoder.
   --
   --  With a lzma_stream initialized with the single-threaded
   --  lzma_stream_encoder() or lzma_easy_encoder(),
   --  LZMA_FULL_BARRIER is an alias for LZMA_FULL_FLUSH.

   LZMA_FINISH : constant lzma_action := 3;  -- /usr/include/lzma/base.h:345
   --  Finish the coding operation
   --
   --  All the input data must have been given to the encoder
   --  (the last bytes can still be pending in next_in).
   --  Call lzma_code() with LZMA_FINISH until it returns
   --  LZMA_STREAM_END. Once LZMA_FINISH has been used,
   --  the amount of input must no longer be changed by
   --  the application.
   --
   --  When decoding, using LZMA_FINISH is optional unless the
   --  LZMA_CONCATENATED flag was used when the decoder was
   --  initialized. When LZMA_CONCATENATED was not used, the only
   --  effect of LZMA_FINISH is that the amount of input must not
   --  be changed just like in the encoder.

   function lzma_code
     (strm : access lzma_stream; action : lzma_action) return lzma_ret
     with Import, Convention => C, External_Name => "lzma_code";
  --  Encode or decode data
  --
  --  Once the lzma_stream has been successfully initialized (e.g. with
  --  lzma_stream_encoder()), the actual encoding or decoding is done
  --  using this function. The application has to update strm->next_in,
  --  strm->avail_in, strm->next_out, and strm->avail_out to pass input
  --  to and get output from liblzma.
  --
  --  See the description of the coder-specific initialization function to find
  --  out what `action' values are supported by the coder.

   procedure lzma_end (strm : access lzma_stream)
     with Import, Convention => C, External_Name => "lzma_end";
   --  Free memory allocated for the coder data structures
   --
   --  \param       strm    Pointer to lzma_stream that is at least initialized
   --                       with LZMA_STREAM_INIT.
   --
   --  After lzma_end(strm), strm->internal is guaranteed to be NULL. No other
   --  members of the lzma_stream structure are touched.
   --
   --  \note        zlib indicates an error if application end()s unfinished
   --               stream structure. liblzma doesn't do this, and assumes that
   --               application knows what it is doing.

end GNATCOLL.Coders.LZMA.Thin;
