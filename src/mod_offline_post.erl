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
	From = element(5,Packet),
	To = element(6,Packet),
	
	From1 = From#jid.luser,
	To1 = To#jid.luser,
	
	Body = binary_to_list(element(3,lists:nth(1,element(8,Packet)))),
	
	PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url,fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
	AppId = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, app_id, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
	ApiKey = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, api_key, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
	
	Data = string:join(["to=", binary_to_list(To1), "&from=", binary_to_list(From1), "&body=", Body], ""),
	Request = {binary_to_list(PostUrl), [{"X-Parse-Application-Id", binary_to_list(AppId)}, {"X-Parse-REST-API-Key", binary_to_list(ApiKey)}], "application/x-www-form-urlencoded", Data},
	httpc:request(post, Request,[],[]),
			
	ok.