:- use_module(server).

% Using `localhost:3050`, we only bind to localhost interface!
% Use plain `3050` (or any port number you like) to make the server
% accessible from all network interfaces.

%:- initialization server(localhost:3050).
:- initialization server(3050).
