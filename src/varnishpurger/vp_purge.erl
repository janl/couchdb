% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(vp_purge).
-behaviour(gen_server).

-include("../couchdb/couch_db.hrl").

-export([start/1, stop/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start(Varnish) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Varnish, []).

stop() ->
    gen_server:call(?MODULE, stop).

purge(Id, Url, DbName, Socket) ->
    BinUrl = ?l2b(Url),
    Request = <<"PURGE /",DbName/binary,"/",Id/binary," HTTP/1.1\r\nHost: ",
        BinUrl/binary,"\r\n\r\n">>,
    % TODO verify socket availability / check errors
    ok = gen_tcp:send(Socket, ?b2l(Request)).

handle_change(#doc_info{id=Id}, {Url, DbName, Socket} = Acc) ->
    % purge async, so we can handle the next change right away
    spawn_link(fun() -> purge(Id, Url, DbName, Socket) end),
    {ok, Acc}.

wait_for_change(Db, UpdateSeq, Url, Socket) ->
    NewUpdateSeq = case wait_db_updated() of
    updated ->
        case couch_server:open(Db#db.name, []) of
        {ok, Db} ->
            couch_db:changes_since(
                Db,
                main_only,
                UpdateSeq,
                fun handle_change/2,
                [{dir, fwd}],
                {Url, Db#db.name, Socket}),
            couch_db:close(Db),
            couch_db:get_update_seq(Db); % keep new high watermark 
        _DbOpenFail ->
            ?LOG_ERROR("Varnishpurger can't open database: '~s'",
                [Db#db.name])
        end;
    _Else ->
        ok
    end,
    wait_for_change(Db, NewUpdateSeq, Url, Socket).

split_url(Url) -> string:tokens(Url, ":").

register_for_updates(Db, Url) ->
    UpdateSeq = couch_db:get_update_seq(Db),
    couch_db:close(Db), % release the fd until we do get changes
    [Ip, Port] = split_url(Url),
    case gen_tcp:connect(Ip, list_to_integer(Port), [binary, {packet, 0}]) of
    {ok, Socket} ->
        % set up reciving process for update notifications
        Pid = spawn_link(fun() ->
            wait_for_change(Db, UpdateSeq, Url, Socket)
        end),

        % register for updates
        {ok, _} = couch_db_update_notifier:start_link(
            fun({_, DbName2}) when DbName2 == Db#db.name ->
                Pid ! db_updated;
            (_) ->
                ok
            end
        );
    {error, Reason} ->
        ?LOG_ERROR("Varnishpurger can't connect to Varnish host: '~s' '~s'",
            [Url, Reason])
    end.

init({DbName, Url}) ->
    case couch_server:open(?l2b(DbName), []) of
    {ok, Db} -> register_for_updates(Db, Url);
    _ -> ?LOG_ERROR("Varnishpurger can't open db: ~s", [DbName])
    end,
    {ok, {}}.

% waits for a db_updated msg, if there are multiple msgs, collects them.
% Duplicated code from couch_changes.erl -- this should be generalized
wait_db_updated() ->
    receive
    db_updated ->
        get_rest_db_updated()
    end.

get_rest_db_updated() ->
    receive
    db_updated ->
        get_rest_db_updated()
    after 0 ->
        updated
    end.

terminate(_Reason, _State) ->
    ok.

handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.

handle_cast(foo, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
