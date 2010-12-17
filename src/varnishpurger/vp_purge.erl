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
    Request = ?b2l(
        <<"PURGE /",
        DbName/binary,
        "/",
        Id/binary,
        " HTTP/1.1\r\nHost: ",
        BinUrl/binary,"\r\n\r\n">>),
    ok = gen_tcp:send(Socket, Request).

handle_change(#doc_info{id=Id}, {Url, DbName, Socket} = Acc) ->
    spawn_link(fun() -> purge(Id, Url, DbName, Socket) end),
    {ok, Acc}.

wait_for_change(DbName, UpdateSeq, Url, Socket) ->
    NewUpdateSeq = case wait_db_updated() of
    updated ->
        case couch_server:open(DbName, []) of
        {ok, Db} ->
            couch_db:changes_since(
                Db,
                main_only,
                UpdateSeq,
                fun handle_change/2,
                [{dir, fwd}],
                {Url, DbName, Socket}),
            NewUpdateSeq2 = couch_db:get_update_seq(Db),
            couch_db:close(Db),
            NewUpdateSeq2;
        _DbOpenFail ->
            ?LOG_ERROR("Varnishpurger can't open database: '~s'", [DbName])
        end;
    _Else ->
        ok
    end,
    wait_for_change(DbName, NewUpdateSeq, Url, Socket).

init({DbNameList, Url}) ->
    DbName = ?l2b(DbNameList),
    case couch_server:open(DbName, []) of
    {ok, Db} ->
        UpdateSeq = couch_db:get_update_seq(Db),
        couch_db:close(Db),

        [Ip, Port] = string:tokens(Url, ":"),
        {ok, Socket} = gen_tcp:connect(Ip, list_to_integer(Port),
            [binary, {packet, 0}]),

        Pid = spawn(fun() -> wait_for_change(DbName, UpdateSeq, Url, Socket) end),
        {ok, _Notify} = couch_db_update_notifier:start_link(
            fun({_, DbName2}) when DbName2 == Db#db.name ->
                Pid ! db_updated;
            (_) ->
                ok
            end
        );
    _ -> ?LOG_ERROR("Varnishpurger can't open db: ~s", [DbName])
    end,
    {ok, {}}.

% waits for a db_updated msg, if there are multiple msgs, collects them.
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
