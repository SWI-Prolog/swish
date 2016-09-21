/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(download,
	  [ download_button/2			% +Data, +Options
	  ]).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(http/mimetype)).

/** <module> Provide data for downloading
*/

%%	download_button(+Data:string, +Options)
%
%	Emit a button in the SWISH   output window for downloading Data.
%	The provided data is associated with   the button and (thus) not
%	stored on the server.  A small tests indicates this works fairly
%	well up to several tens of megabytes.
%
%	Options:
%
%	  - name(+Name)
%	  (Base-)Name of the file created (default: 'swish-download')
%	  - ext(+Ext)
%	  Extension for the file (default: 'dat')
%	  - encoding(+Enc)
%	  Encoding to use.  One of `utf8` or `octet`.  default is `utf8`
%
%	@see https://en.wikipedia.org/wiki/Data_URI_scheme

download_button(Data, Options) :-
	option(filename(FileName), Options, 'swish-download.dat'),
	file_mime_type(FileName, Major/Minor),
	atomics_to_string([Major, Minor], /, ContentType),
	option(encoding(Enc), Options, utf8),
	encode_data(Enc, Data, CharSet, EncData),
	pengine_output(
	    json{action:downloadButton,
		 content_type:ContentType,
		 data:EncData,
		 filename:FileName,
		 charset:CharSet
		}).

encode_data(utf8,  Data, "charset=UTF-8", Data).
encode_data(octet, Data, "base64", Data64) :-
	string_codes(Data, Codes),
	phrase(base64(Codes), Codes64),
	string_codes(Data64, Codes64).
