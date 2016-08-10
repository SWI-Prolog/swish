/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
