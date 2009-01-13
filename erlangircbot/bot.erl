-module(bot).
-author("jessta@gmail.com").
-export([start/2,connect/2,loop/3,parse_line/2,rss/2,erowid/1,meme/1]).

-import(xmerl).
-import(inet).
-import(timer).
-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xs).

-define(nickname, "jigglypuff").
-define(channel, "#jigglytest").
-define(emomessage,"and now to an end, I leave austnet. goodbye to all my austnet friends forever").
-define(say, ".say").
-define(rss,".rss").
-define(meme,".meme").
-define(weather,".weather").
-define(ragequit,".ragequit").
-define(erowid,".erowid").
-define(kick,".kick").

% Connect to an IRC server with a given Host and Port.  Set up the TCP option to
% give us messages on a line-by-line basis.
start(Host,Port) ->
	register(jigglypuff, spawn (bot, connect, [Host,Port])),
	%register(jigglypuffrss, spawn_link(bot,rss,[?channel,[]])),
	register(jigglypufferowid, spawn_link(bot,erowid,[?channel])),
	register(jigglypuffmeme, spawn_link(bot,meme,[?channel])),
	erlang:monitor(process, jigglypuffmeme),
	%erlang:monitor(process, jigglypuffrss),
	erlang:monitor(process, jigglypufferowid).
	
connect(Host, Port) ->
	{ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
	process_flag(trap_exit, true),
	% According to RFC1459, we need to tell the server our nickname and username
	gen_tcp:send(Sock, "NICK " ++ ?nickname ++ "\r\n"),
	gen_tcp:send(Sock, "USER " ++ ?nickname ++ " blah blah blah blah\r\n"),	
	loop(Sock,Host,Port).
	
% Now that we're connected, receive TCP messages and parse them.
loop(Sock,Host,Port) ->
	receive
		{tcp, Sock, Data} ->
			io:format("[~w] Received: ~s", [Sock, Data]),
			StrippedData = string:strip(Data, both,$\n),
			StrippedData2 = string:strip(StrippedData, both,$\r),
			parse_line(Sock, string:tokens(StrippedData2, ": ")),
			loop(Sock,Host,Port);
			
		{tcp_closed, Sock} ->
			timer:sleep(20000),
			connect(Host,Port);
		
		{'DOWN', Ref, process,{RegName, Node}, Reason} ->
			case RegName of
				jigglypuffmeme ->
					io:format("lost meme: ~s",[Reason]),
					register(jigglypuffmeme, spawn_link(bot,meme,[?channel])),
					erlang:monitor(process, jigglypuffmeme),
					io:format("respawned meme"),
					loop(Sock,Host,Port);
				%jigglypuffrss ->
				%	io:format("lost rss: ~s",[Reason]),
				%	register(jigglypuffrss, spawn_link(bot,rss,[?channel,[]])),
				%	erlang:monitor(process, jigglypuffrss),
				%	io:format("respawned rss"),
				%	loop(Sock,Host,Port);
				Etc ->
					io:format("~s when down",RegName),
					loop(Sock,Host,Port)
			end;
			
		{privmsg,Channel,Msg} ->
			io:format("~p~n",[Msg]),
			ok = irc_privmsg(Sock,Channel,Msg),
			loop(Sock,Host,Port);
			
		upgrade ->
			jigglypuffmeme ! upgrade,
			jigglypuffrss ! upgrade,
			bot:loop(Sock,Host,Port);
		yay ->
			io:format("pizza, changes seen"),
			loop(Sock,Host,Port);
		quit ->
			io:format("[~w] Received quit message, exiting...~n", [Sock]),
			gen_tcp:close(Sock),
			exit(stopped);
		Etc ->
			io:format("~p~n",[Etc]),
			loop(Sock,Host,Port)
	end.

% The following is an example of the message this fun intends to parse.  Here we see
% the limitation that tokenizing the string on both :'s and spaces puts on us.
% [#Port<0.124>] Received: :jroes!jroes@mask-2EDB8BDB.net PRIVMSG #jroes-test :jroes-test: wassup?

parse_line(Sock, [User,"PRIVMSG",Channel,?nickname,?say|Parameters]) ->
	irc_privmsg(Sock, Channel, string:join(Parameters," "));
	
parse_line(Sock, [User,"PRIVMSG",Channel,?nickname,?erowid,Query])->
	jigglypufferowid ! {erowid,Query};

parse_line(Sock, [User,"PRIVMSG",Channel,?nickname,?ragequit]) ->
	Nick = lists:nth(1, string:tokens(User, "!")),
	irc_quit(Sock,Channel,?emomessage);

%parse_line(Sock, [User,"PRIVMSG",Channel,?rss]) ->
%	rss(Sock,Channel);
	
parse_line(Sock, [User,"PRIVMSG",Channel,?nickname,?meme]) ->
	jigglypuffmeme ! {meme,kgo};
	
%parse_line(Sock, [User,"PRIVMSG",Channel,?meme]) ->
%	meme(Sock,Channel);	

%parse_line(Sock, [User,"PRIVMSG",Channel,?nickname|_]) ->
%	Nick = lists:nth(1, string:tokens(User, "!")),
%	irc_privmsg(Sock, Channel, "You talkin to me, " ++ Nick ++ "?");

parse_line(Sock, [User,"PRIVMSG",Channel,?nickname,?kick,ToKick]) ->
	Nick = lists:nth(1, string:tokens(User, "!")),
	case Nick of
		"jessta" -> irc_kick(Sock, Channel,ToKick,"kicked!");
		_ -> true
	end;

% If the second token is "376", then join our channel.  376 indicates End of MOTD.
parse_line(Sock, [_,"376"|_]) ->
	gen_tcp:send(Sock, "JOIN :" ++ ?channel ++ "\r\n");
	%spawn(bot,rss,Sock,channel);

% The server will periodically send PINGs and expect you to PONG back to make sure
% you haven't lost the connection.
parse_line(Sock, ["PING"|Rest]) ->
	gen_tcp:send(Sock, "PONG " ++ Rest ++ "\r\n");

parse_line(_, _) ->
	0.

% This just helps us write a PRIVMSG back to a client without having to type
% the newlines and :'s ourselves so much.  It'll be more useful later.
irc_privmsg(Sock, To, Message) ->
	gen_tcp:send(Sock, "PRIVMSG " ++ To ++ " :" ++ Message ++ "\r\n").

irc_quit(Sock, Channel, Message) ->
	gen_tcp:send(Sock, "QUIT :" ++ Channel ++ " " ++ Message ++"\r\n").

%irc_privmsg(Sock, To, Message) ->
%	io:format("Send: ~s ~s ~s ~s ~s",["PRIVMSG",To," :",Message,"\r\n"]).

irc_kick(Sock,User,Channel,Message) ->
	gen_tcp:send(Sock, "KICK " ++ Channel ++ " " ++ User ++" :"++ Message ++"\r\n").


% stolen from http://www.trapexit.org/index.php/How_to_write_an_RSS_aggregator#Getting_information_from_an_RSS_feed.
getElementsByTagName([H|T], Item) when H#xmlElement.name == Item ->
    [H | getElementsByTagName(T, Item)];
    
getElementsByTagName([H|T], Item) when record(H, xmlElement) ->
    getElementsByTagName(H#xmlElement.content, Item) ++
      getElementsByTagName(T, Item);  
      
getElementsByTagName(X, Item) when record(X, xmlElement) ->
    getElementsByTagName(X#xmlElement.content, Item);
    
getElementsByTagName([_|T], Item) ->
    getElementsByTagName(T, Item);
    
getElementsByTagName([], _) ->
    [].
    
printItems(Items) ->
    F = fun(Item) -> printItem(Item) end,
    lists:map(F, Items).

printItem(Item) ->
    %irc_privmsg(Sock,Channel,string:join([textOf(first(Item, link)),textOf(first(Item, title))]," - ")),
    %io:format("~s ~n", string:join([textOf(first(Item, link)),textOf(first(Item, title))]," - ")),
    io_lib:format("title: ~s~n ~s ~n", [textOf(first(Item, title)),textOf(first(Item, link))]).

first(Item, Tag) ->
    hd([X || X <- Item#xmlElement.content,
	     X#xmlElement.name == Tag]).

textOf(Item) ->
    lists:flatten([X#xmlText.value || X <- Item#xmlElement.content,
				      element(1,X) == xmlText]).
				      
compareRss(List, Item) ->
	F = fun(ListItem) -> string:equal(textOf(first(ListItem,link)),textOf(first(Item,link))) end,
	lists:filter(F,List).
				      
rss(Channel,PreviousItems) ->
	receive
		stop ->
			io:format("Stopping RSS ~n",[]),
			exit(normal);
		upgrade ->
			bot:rss(Channel,PreviousItems)
		
	after 20000 ->
		inets:start(),
		%{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = http:request("http://www.reddit.com/.rss"),
		case http:request("http://roll.jessta.id.au/rss") of
			{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
				{Doc,Misc} = xmerl_scan:string(Body),
				CurrentItems = lists:reverse(getElementsByTagName(Doc,item)),
				NewItems = [X || X <- CurrentItems,  compareRss(PreviousItems,X) ==[]],
				jigglypuff ! {privmsg,Channel,lists:flatten(lists:flatten(printItems(NewItems)))},
				rss(Channel,CurrentItems);
			{ok, {{Version,_,ReasonPhrase}, Headers, Body}} ->
				io:format("error fetching rss feed"),
				rss(Channel,PreviousItems);
			{error,ReasonPhrase} ->
				io:format("error fetching rss feed: ~s",[ReasonPhrase]),
				rss(Channel,PreviousItems)
		end
	end.
		%printItems(Sock,Channel,getElementsByTagName(Doc, item)).
% stolen from http://www.trapexit.org/index.php/How_to_write_an_RSS_aggregator#Getting_information_from_an_RSS_feed.

meme(Channel) ->
	
	receive 
		{meme,kgo} ->
			inets:start(),
			case http:request("http://meme.boxofjunk.ws/moar.html?lines=1") of 
				{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
					jigglypuff ! {privmsg,Channel,[Body]};
				{ok,{{Version,_,ReasonPhrase}, Headers,Body}} ->
					jigglypuff ! {privmsg,Channel,["Error getting meme"]}
			end,
			meme(Channel);
		stop ->
			io:format("Stopping Meme ~n",[]),
			exit(normal);
		upgrade ->
			bot:meme(Channel)
		
	after 600000 ->
		case random:uniform(3) of
			1 ->
				case http:request("http://lolwut.boxofjunk.net/moar.html?lines=1") of 
				{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
					jigglypuff ! {privmsg,Channel,Body};
				{ok,{{Version,_,ReasonPhrase}, Headers,Body}} ->
					jigglypuff ! {privmsg,Channel,"Error getting meme"}
				end;
			2 ->
				true;
			3 ->
				true
		end,
		meme(Channel)
	end.

erowid(Channel) ->
	receive
		{erowid,Query} ->
			inets:start(),
			case http:request("http://www.erowid.org/chemicals/"++ Query ++"/") of
				{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
					%case http:request("http://www.erowid.org/chemicals/"++ Query ++"/"++ Query ++"_dose.shtml") of
					%	{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
					%		DosageStart = string:str(Body,"Threshold</td><td class=\"chart3\">")+33,
					%		DosageEnd = string:str(string:sub_string(Body,DosageStart),"</td>") + DosageStart-2,
							%get doage information
					%	{ok,{{Version,_,ReasonPhrase}, Headers,Body}} ->
							%no dosage information
					%end,
					DescriptionStart = string:str(Body,"<div class=\"sum-description\">")+29,
					DescriptionEnd = string:str(string:sub_string(Body,DescriptionStart),"</div>") + DescriptionStart-2,
					Description = string:sub_string(Body, DescriptionStart,DescriptionEnd),
					jigglypuff ! {privmsg,Channel,Description};
				{ok,{{Version,_,ReasonPhrase}, Headers,Body}} ->
					jigglypuff ! {privmsg,Channel,"unable to find erowid entry"}
			end,
			erowid(Channel);
		stop ->
			io:format("Stopping erowid ~n",[]),
			exit(normal);
		upgrade ->
			bot:erowid(Channel)
	end.
	
	%<ol class="search-results-body">
%<div class="search-result"><li><a href="http://www.erowid.org/chemicals/lsd/">LSD (Acid) Vault</a>
	
