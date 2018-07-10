/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cwi.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2018, VU University Amsterdam
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

:- module(swish_config_branding, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).

/** <module> Brand the SWISH installation

This config file changes the page  title   and  SWISH  logo. Branding is
achieved by proving Prolog hooks for the page generation. Icon resources
may be placed in  `config-enabled/web/icons`.   Icons  placed there that
precedence over icons in the `web/icons` directory of SWISH itself. Note
however that many small icons are compiled into the minified CSS file.

The branding in this file is for the VRE4EIC project that supported part
of the development of SWISH.
*/

:- multifile
    swish_config:title//1,
    swish_config:logo//1.

%!  swish_config:title(+Options)//
%
%   Emit the title element.

swish_config:title(_Options) -->
    { http_absolute_location(icons('favicon.ico'), FavIcon, []),
      http_absolute_location(icons('swish-touch-icon.png'), TouchIcon, [])
    },
    html([ title('VRE4EIC -- enhanced Virtual Research Environment'),
           link([ rel('shortcut icon'),
                  href(FavIcon)
                ]),
           link([ rel('apple-touch-icon'),
                  href(TouchIcon)
                ])
         ]).

%!  swish_config:logo(+Options)//
%
%   Insert the logo into the web page.
%
%   @arg Options is the option list passed from initializing SWISH.

swish_config:logo(_Options) -->
    { http_absolute_location(icons('vre4eic.png'), Icon, [])
    },
    html(a([ href('https://www.vre4eic.eu/'),
             style('padding-top:7px; padding-left:5px; white-space: nowrap'),
             class('navbar-brand')
           ],
           [ img([ style('margin-right:5px;display:inline;\c
                          position:relative;top:-2px;\c
                          width:42px;height:42px;'),
                   src(Icon)
                 ]),
             b([ style('font-size:24px; color:black;')
               ],
               [ span('VRE'),
                 span([style('color:#a92330')], '4'),
                 span('EIC')
               ])
           ])).

