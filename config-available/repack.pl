/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
			 CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_config_repack, []).
:- use_module(swish(lib/cron)).
:- use_module(swish(lib/storage)).

/** <module> Configure repacking of gitty store

This config file schedules optimization  of   the  versioned  file store
(_gitty_). This process combines   files  from `data/storage/XX/YY/SHA1`
into a pack file in `data/storage/pack`.  Packing reduces disk usage and
scanning time, typically by a large margin because the average object in
the store is much smaller than a disk allocation unit.

The options fine tune the process:

  - min_files(Count)
  Specifies the minimum amount of file objects that must be
  present before considering creating a pack.
  - small_pack(Bytes)
  Add the new files and possibly other small packs together
  into a new pack for all packs that are smaller than the
  given size.

@bug Although the repacking can be performed  safely on a life system it
is currently incompatible with sharing  the   same  gitty  store between
multiple  SWISH  instances.  If  you  share  a  store  between  multiple
instances it is possible to use  packing by stopping all-but-one-server,
repack and restart the other servers.
*/

:- initialization
    http_schedule_maintenance(weekly(sunday, 03:10),
                              storage_repack([ min_files(1_000),
                                               small_pack(10_000_000)
                                             ])).
