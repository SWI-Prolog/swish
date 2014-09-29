:- use_module(library(git)).
:- use_module('../lib/gitty').

git_history(Dir, File, Limit, Objects) :-
	git_shortlog(Dir, ShortLog, [git_path(File),limit(Limit)]),
	maplist(git_content(Dir,File), ShortLog, Objects).

git_content(Dir, File, ShortLog, git{dir:Dir,file:File,commit:Commit,data:Data}) :-
	git_log_data(commit_hash, ShortLog, Commit),
	setup_call_cleanup(
	    git_open_file(Dir, File, Commit, In),
	    read_string(In, _, Data),
	    close(In)).

git_log(Dir, File) :-
	git_history(Dir, File, 100, Objects),
	append(_, [New,Old|_], Objects),
	(   data_diff(Old.data, New.data, Diffs),
	    maplist(udiff_string, Diffs, Strings)
	->  atomics_to_string(Strings, '\n', String),
	    format(user_error, '~s~n', String)
	;   gtrace,
	    data_diff(Old.data, New.data, Diffs),
	    maplist(udiff_string, Diffs, Strings)
	->  atomics_to_string(Strings, '\n', String),
	    format(user_error, '~s~n', String)
	).
