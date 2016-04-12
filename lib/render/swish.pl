/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(swish_render_swish,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module('../render').
:- use_module('../storage').

:- register_renderer(swish, "Render references to SWISH objects").

/** <module> SWISH renderer

Render references to SWISH objects
*/

%%	term_rendering(+File, +Vars, +Options)//
%
%	Renders a link to a SWISH file

term_rendering(File, _Vars, _Options) -->
	{ atom(File),
	  file_name_extension(_, Ext, File),
	  swish_extension(Ext),
	  storage_file(File)
	},
	html(a([class(store),href('/p/'+File)], File)).

swish_extension(pl).
swish_extension(swinb).
