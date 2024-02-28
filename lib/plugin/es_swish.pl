/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2024, SWI-Prolog Solutions b.v.
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

:- module(es_swish,
          [ es_create_index/0,
            es_add_file/1,                        % +File
            es_add/2,                             % +Offset, +Limit
            es_add_since/1,                       % +Time
            es_query/2
          ]).
:- use_module(elastic).
:- use_module('../storage').
:- use_module('../authenticate').
:- use_module(library(base64)).
:- use_module(library(broadcast)).
:- use_module(library(solution_sequences)).
:- use_module(library(apply)).
:- use_module(library(pprint)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(http/http_server)).

/** <module> SWISH integration of Elastic Search

This module hooks into storage.pl  to   provide  full  text and metadata
search over the file storage. This  module   listens  to  saved files to
index new or updated files.
*/

%!  es_create_index
%
%   Create an Elastic Search index to represent commits.

es_create_index :-
    commit_properties(Properties),
    es_create_index(swish, Properties).

%!  es_add_since(+Time)
%
%   Update entries that have  been  added   or  modified  less than Time
%   seconds ago.

es_add_since(Time) :-
    get_time(Now),
    Since is Now - Time,
    forall(call_nth((storage_meta_data(File, Meta),
                     Meta.time > Since),
                    Nth),
           es_add_file(File, 0, Nth)).

%!  es_add(+Offset, +Limit) is det.
%
%   Add files to the index iven an offset and limit.
%
%   @tbd: see whether the bulk insert option of Elastic search provides
%   a significant speedup. See
%   https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html)

es_add(Offset, Limit) :-
    forall(call_nth(limit(Limit, offset(Offset, storage_file(File))), Nth),
           es_add_file(File, Offset, Nth)).

es_add_file(File, Offset, N) :-
    Nth is Offset+N,
    format(user_error, '\r~D ~w ... \e[K', [Nth, File]),
    Error = error(_,_),
    catch(es_add_file(File), Error,
          print_message(warning, Error)).

%!  es_add_file(+File) is det.
%
%   Add a file to the `swish` index.
%
%   @error May throw Gitty, Redis or Elastic errors.  Bulk adding
%   should typically ignore these.

es_add_file(File) :-
    storage_file(File, Data, Meta),
    !,
    file_name_extension(_Base, Ext, File),
    atom_concat('p/', File, IdPath),
    base64url(IdPath, Id0),
    str_limit(Id0, 511, Id), % Elastic _id is limited to 512 bytes
    es_add(swish, Meta.put(_{ content: Data,
                              type:Ext
                            }), _Reply,
           [ id(Id)
           ]).
es_add_file(File) :-
    print_message(warning, gitty(no_file(File))).

str_limit(Id, Limit, Id) :-
    string_length(Id, Len),
    Len =< Limit,
    !.
str_limit(Id0, Limit, Id) :-
    sub_string(Id0, 0, Limit, _, Id).

%!  es_query(+Query, -Result) is det.
%
%   Run a query against the Elastic database.

es_query(For, Result) :-
    es_search(swish, For, Result).

commit_properties(#{ name: #{type: keyword},      % File name

                     author: #{type: keyword},    % Ownership
                     avatar: #{type: keyword},
                     commit: #{type: keyword},
                     identity: #{type: keyword},
                     email: #{type: keyword},
                     profile_id: #{type: keyword},
                     peer: #{type: ip, ignore_malformed:true},

                     title: #{type: text},        % Search metadata
                     description: #{type: text},
                     commit_message: #{type: text},
                     type: #{type: keyword},
                     tags: #{type: keyword},
                     example: #{type: boolean},

                     data: #{type: keyword},      % Content
                     previous: #{type: keyword},
                     content: #{type: text},
                     time: #{ type: date,
                              format:"epoch_second"
                            },
                                                  % Access permissions
                     modify: #{type: keyword},    % list of any, login or owner
                     public: #{type: boolean}
                   }).


		 /*******************************
		 *        SEARCH PLUGIN		*
		 *******************************/

:- multifile
    web_storage:search_sources_hook/2.

%!  web_storage:search_sources_hook(+Query, -Results) is det.
%
%   Run full text and/or metadata search over the stored files.

web_storage:search_sources_hook(Query, Result) :-
    (   catch_with_backtrace(es_search_sources_hook(Query, Result), E, true)
    ->  (   var(E)
        ->  true
        ;   message_to_string(E, Msg),
            Result = #{ error: Msg },
            (   debugging(elastic)
            ->  print_term(Result, [nl(true)])
            ;   true
            )
        )
    ;   Result = #{ error: "failed" }
    ).

es_search_sources_hook(Query, Result) :-
    (   debugging(elastic)
    ->  print_term(Query, [nl(true)])
    ;   true
    ),
    partition(on_content, Query.query, OnContent, OnMeta),
    phrase(es_filter(Query, OnMeta), Filter),
    phrase(es_content(OnContent), Match),
    es_ordering(OnContent, OnMeta, Query, Field, Dir),
    dict_pairs(OrderBy, #, [Field - #{order:Dir}]),
    ESQuery = #{query:
                #{bool:
                  #{ must: Match,
                     filter: Filter
                   }
                 },
                '_source': false,        % do not include source
                fields:
                [ name,                  % select fields
                  time,
                  author,
                  avatar,
                  tags
                ],
                track_total_hits: true,  % default
                highlight:
                #{ fields:
                   #{ content: #{}}
                 },
                sort: [
                    OrderBy
                ],
                from: Query.get(offset, 0),
                size: Query.get(limit, 10)
               },
    (   debugging(elastic)
    ->  print_term(ESQuery, [nl(true)])
    ;   true
    ),
    es_query(ESQuery, Matches),
    es_to_swish(Matches, Result).

on_content(word(_)).
on_content(string(_)).
on_content(regex(_,_)).

%!  es_content(+Query)//
%
%   Get the content filtering from Query

es_content([]) -->
    [].
es_content([H|T]) -->
    es_content_1(H),
    es_content(T).

es_content_1(word(W)) -->
    [ #{match: #{content: W}} ].
es_content_1(string(W)) -->
    [ #{match_phrase: #{content: W}} ].
es_content_1(regex(RE,Flags)) -->
    field_filter(regex(RE,Flags), content).

%!  es_filter(+Query, OnMeta)//
%
%   Obtain the meta-data filtering from the query.

es_filter(Query, OnMeta) -->
    es_owner_filter(Query, OnMeta),
    es_tag_filter(OnMeta),
    es_type_filter(OnMeta),
    es_name_filter(OnMeta).

es_owner_filter(Query, OnMeta) -->		% private search
    { memberchk(user("me"), OnMeta),
      user_property(Query.auth, identity(Id))
    },
    !,
    [ #{term: #{identity: Id}} ].
es_owner_filter(Query, OnMeta) -->		% private search
    { memberchk(user("me"), OnMeta),
      NickName = Query.auth.get(display_name)
    },
    !,
    field_filter(NickName, author),
    public_filter.
es_owner_filter(Query, OnMeta) -->		% private search
    { memberchk(user("me"), OnMeta),
      user_property(Query.auth, avatar(Avatar))
    },
    !,
    [ #{term: #{avatar: Avatar}} ],
    public_filter.
es_owner_filter(_Query, OnMeta) -->
    { memberchk(user(User), OnMeta),
      User \== "me"
    },
    field_filter(User, author),
    public_filter.
es_owner_filter(_, _) -->
    public_filter.

public_filter -->
    [ #{term: #{public: true}} ].

es_tag_filter(Query) -->
    { memberchk(tag(Tag), Query) },
    !,
    field_filter(Tag, tags).
es_tag_filter(_) -->
    [].

es_name_filter(Query) -->
    { memberchk(name(Name), Query) },
    !,
    field_filter(Name, tags).
es_name_filter(_) -->
    [].

es_type_filter(Query) -->
    { memberchk(type(Type), Query) },
    !,
    [ #{term: #{type: Type}}].
es_type_filter(_) -->
    [].

%!  field_filter(+Spec, +Field)//
%
%   Add a filter on the given meta-data

field_filter(regex(RE,Flags), Field) -->
    !,
    { convlist(re_pair, Flags, REOpts),
      dict_pairs(REDict, #, [Field - REProps]),
      dict_pairs(REProps, #, [value-RE|REOpts])
    },
    [ #{regexp: REDict} ].
field_filter(String, Field) -->
    { dict_pairs(WCDict, #, [Field - WCProps]),
      dict_pairs(WCProps, #, [value-String, case_insensitive-true])
    },
    [ #{wildcard: WCDict} ].

re_pair(i, case_insensitive-true).

%!  es_ordering(+OnContent, +OnMeta, +Query, -Field, -Dir) is det.

es_ordering(_OnContent, _OnMeta, Query, Field, Dir) :-
    _{ order_by:Field, order: Dir} :< Query,
    !.
es_ordering(_OnContent, _OnMeta, Query, Field, Dir) :-
    _{ order_by:Field} :< Query,
    !,
    (   Field == time
    ->  Dir = desc
    ;   Dir = asc
    ).
es_ordering(OnContent, _OnMeta, _Query, '_score', desc) :-
    OnContent \== [],
    !.
es_ordering(_OnContent, _OnMeta, _Query, 'time', desc).

%!  es_to_swish(+ESResuls, -Results) is det.
%
%   Extract our compatible results from the Elastic result set.

es_to_swish(ESResult, #{ matches:Matches,
                         total:Total,
                         cpu:CPU,
                         cache:false
                       }) :-
    Hits = ESResult.hits,
    CPU is ESResult.get(took, 0)/1000.0,
    Total = Hits.total.value,
    maplist(es_to_swish_hit, Hits.hits, Matches).

es_to_swish_hit(ESHit, Match) :-
    dict_pairs(ESHit.fields, _, Pairs),
    convlist(to_single, Pairs, RPairs),
    add_highlight(ESHit, RPairs, RPairs1),
    dict_pairs(Match, #, RPairs1).

to_single(tags-Tags, tags-Tags) :- !.
to_single(Field-[Value], Field-Value) :- !.

add_highlight(ESHit, Pairs, [highlight-Lines|Pairs]) :-
    Lines = ESHit.get(highlight).get(content),
    !.
add_highlight(_, Pairs, Pairs).


		 /*******************************
		 *          TYPEAHEAD		*
		 *******************************/

:- multifile web_storage:typeahead_hooked/1.

web_storage:typeahead_hooked(file).
web_storage:typeahead_hooked(store_content).

:- multifile swish_search:typeahead/4.       % +Set, +Query, -Match, +Options

%!  swish_search:typeahead(+Set, +Query, -Result, +Options) is nondet.
%
%   Search for typeahead of the top-right search box.

swish_search:typeahead(file, For, FileInfo, _Options) :-
    atom_concat(For, *, Pattern),
    ESQuery = #{query:
                #{bool:
                  #{ must:
                     [ #{query_string: #{
                             query: Pattern,
                             fields: [name, title, tags]
                         }}
                     ],
                     filter: [ #{term: #{public: true}} ]
                   }
                 },
                '_source': false,
                fields:
                [ name,                  % select fields
                  time,
                  author,
                  avatar,
                  tags
                ],
                sort:[#{time: #{order:desc}}],
                from: 0,
                size: 10
               },
    es_query(ESQuery, Matches),
    es_to_swish(Matches, Result),
    member(Hit, Result.matches),
    File = Hit.name,
    FileInfo = Hit.put(_{type:"store", file:File}).

swish_search:typeahead(store_content, Text, FileInfo, Options) :-
    http_current_request(Request),
    authenticate(Request, Auth),
    limit(25, se_typeahead(store_content, Text, FileInfo,
                           Options.put(auth,Auth))).

se_typeahead(store_content, Text, FileInfo, Options) :-
    option(auth(Auth), Options),
    phrase(es_owner_filter(#{auth:Auth.put(Options)}, [user("me")]), Filter),
    Query = #{query:
              #{bool:
                #{ must:
                   [ #{match: #{content: Text}}
                   ],
                   filter: Filter
                 }
               },
              '_source': false,        % do not include source
              fields:
              [ name,                  % select fields
                time,
                author,
                avatar,
                tags
              ],
              track_total_hits: true,  % default
              highlight:
              #{ fields:
                 #{ content:
                    #{ number_of_fragments: 1
                     }
                  },
                 encoder: html,
                 boundary_chars: '\n'
               },
              sort:[#{time: #{order:desc}}],
              from:0, size:10
             },
    (   debugging(elastic)
    ->  print_term(Query, [nl(true)])
    ;   true
    ),
    es_query(Query, Matches),
    es_to_swish(Matches, Result),
    member(Hit, Result.matches),
    File = Hit.name,
    member(Line, Hit.highlight),
    FileInfo = Hit.put(_{type:"store", file:File,
                         line: 0, text:Line, query:Text,
                         encoder: html
                        }).


		 /*******************************
		 *           UPDATES		*
		 *******************************/

% Listen to new and updated  files  to   update  the  index. This should
% normally be fine to keep the index   up-to-date as this is executed by
% the swish instance that makes the change.   Only  a badly timed crash,
% i.e., between adding to the  store   and  this  broadcast request, can
% cause a new commit to be lost.

% In addition, es_add_since/1 may be  used   to  simply update all files
% added or modified since some time period. This is slow though.

:- listen(swish(created(File, _Commit)),
          es_add_file(File)).
:- listen(swish(updated(File, _Commit)),
          es_add_file(File)).
