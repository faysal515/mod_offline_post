-module(mod_offline_post).

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 send_notice/1]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

start(Host, Opts) ->
	?INFO_MSG("Starting mod_offline_post", [] ),
	register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
	ok.

init(Host, _Opts) ->
	inets:start(),
	ssl:start(),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 100),
	ok.

stop(Host) ->
	?INFO_MSG("Stopping mod_offline_post", [] ),
	ejabberd_hooks:delete(offline_message_hook, Host,
			      ?MODULE, send_notice, 10),
	ok.

send_notice({Action,Packet}) ->
	From = Packet#message.from,
	To = Packet#message.to,
	
	FromUsername = binary_to_list(From#jid.luser),
	ToUsername = binary_to_list(To#jid.luser),
	
	Body = binary_to_list(xmpp:get_text(Packet#message.body)),
	
	%% Convert back to xmlel
	El = xmpp:encode(Packet),
	
	
	PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url,fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
	
	%% Configure your own options passed to module
	AppId = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, app_id, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
	MasterKey = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, master_key, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
	
	FinalData = string:join(["{", "\"to\":", "\"", ToUsername, "\",", "\"from\":", "\"", FromUsername, "\",", "\"body\":", "\"", Body, "\"",  "}"], ""),
        Request = {binary_to_list(PostUrl), [{"X-Parse-Application-Id", binary_to_list(AppId)}, {"X-Parse-Master-key", binary_to_list(MasterKey)}], "application/json", FinalData},
	httpc:request(post, Request,[],[]),

	ok.
