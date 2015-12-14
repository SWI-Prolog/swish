% Reading and writing
% -------------------

hello_world :-
    writeln('Hello World!'),
    sleep(1),
    hello_world.

read_and_write :-
    prompt(_, 'Type a term or \'stop\''),
    read(Something),
    (   Something == stop
    ->  true
    ;   writeln(Something),
        read_and_write
    ).


/** <examples>

?- hello_world.
?- read_and_write.

*/
