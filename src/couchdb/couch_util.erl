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

-module(couch_util).

-export([priv_dir/0, start_driver/1, normpath/1]).
-export([should_flush/0, should_flush/1, to_existing_atom/1]).
-export([rand32/0, implode/2, collate/2, collate/3]).
-export([abs_pathname/1,abs_pathname/2, trim/1, ascii_lower/1]).
-export([encodeBase64Url/1, decodeBase64Url/1]).
-export([to_hex/1, parse_term/1, dict_find/3]).
-export([file_read_size/1, get_nested_json_value/2, json_user_ctx/1]).
-export([proplist_apply_field/2, json_apply_field/2]).
-export([to_binary/1, to_integer/1, to_list/1, url_encode/1]).
-export([json_encode/1, json_decode/1]).
-export([verify/2,simple_call/2,shutdown_sync/1]).
-export([compressible_att_type/1]).
-export([get_value/2, get_value/3]).
-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
-export([reorder_results/2]).
-export([url_strip_password/1]).
-export([to_utf8/1,from_utf8/1, from_utf16be/1]).


-include("couch_db.hrl").
-include_lib("kernel/include/file.hrl").

% arbitrarily chosen amount of memory to use before flushing to disk
-define(FLUSH_MAX_MEM, 10000000).

priv_dir() ->
    case code:priv_dir(couch) of
        {error, bad_name} ->
            % small hack, in dev mode "app" is couchdb. Fixing requires
            % renaming src/couch to src/couch. Not really worth the hassle.
            % -Damien
            code:priv_dir(couchdb);
        Dir -> Dir
    end.

start_driver(LibDir) ->
    case erl_ddll:load_driver(LibDir, "couch_icu_driver") of
    ok ->
        ok;
    {error, already_loaded} ->
        ok = erl_ddll:reload_driver(LibDir, "couch_icu_driver");
    {error, Error} ->
        exit(erl_ddll:format_error(Error))
    end.

% Normalize a pathname by removing .. and . components.
normpath(Path) ->
    normparts(filename:split(Path), []).

normparts([], Acc) ->
    filename:join(lists:reverse(Acc));
normparts([".." | RestParts], [_Drop | RestAcc]) ->
    normparts(RestParts, RestAcc);
normparts(["." | RestParts], Acc) ->
    normparts(RestParts, Acc);
normparts([Part | RestParts], Acc) ->
    normparts(RestParts, [Part | Acc]).

% works like list_to_existing_atom, except can be list or binary and it
% gives you the original value instead of an error if no existing atom.
to_existing_atom(V) when is_list(V) ->
    try list_to_existing_atom(V) catch _:_ -> V end;
to_existing_atom(V) when is_binary(V) ->
    try list_to_existing_atom(?b2l(V)) catch _:_ -> V end;
to_existing_atom(V) when is_atom(V) ->
    V.

shutdown_sync(Pid) when not is_pid(Pid)->
    ok;
shutdown_sync(Pid) ->
    MRef = erlang:monitor(process, Pid),
    try
        catch unlink(Pid),
        catch exit(Pid, shutdown),
        receive
        {'DOWN', MRef, _, _, _} ->
            ok
        end
    after
        erlang:demonitor(MRef, [flush])
    end.
    

simple_call(Pid, Message) ->
    MRef = erlang:monitor(process, Pid),
    try
        Pid ! {self(), Message},
        receive
        {Pid, Result} ->
            Result;
        {'DOWN', MRef, _, _, Reason} ->
            exit(Reason)
        end
    after
        erlang:demonitor(MRef, [flush])
    end.

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.


parse_term(Bin) when is_binary(Bin) ->
    parse_term(binary_to_list(Bin));
parse_term(List) ->
    {ok, Tokens, _} = erl_scan:string(List ++ "."),
    erl_parse:parse_term(Tokens).

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
    {value, {Key,Value}} ->
        Value;
    false ->
        Default
    end.

get_nested_json_value({Props}, [Key|Keys]) ->
    case couch_util:get_value(Key, Props, nil) of
    nil -> throw({not_found, <<"missing json key: ", Key/binary>>});
    Value -> get_nested_json_value(Value, Keys)
    end;
get_nested_json_value(Value, []) ->
    Value;
get_nested_json_value(_NotJSONObj, _) ->
    throw({not_found, json_mismatch}).

proplist_apply_field(H, L) ->
    {R} = json_apply_field(H, {L}),
    R.

json_apply_field(H, {L}) ->
    json_apply_field(H, L, []).
json_apply_field({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
    {[{Key, NewValue}|Acc]}.

json_user_ctx(#db{name=DbName, user_ctx=Ctx}) ->
    {[{<<"db">>, DbName},
            {<<"name">>,Ctx#user_ctx.name},
            {<<"roles">>,Ctx#user_ctx.roles}]}.
    

% returns a random integer
rand32() ->
    crypto:rand_uniform(0, 16#100000000).

% given a pathname "../foo/bar/" it gives back the fully qualified
% absolute pathname.
abs_pathname(" " ++ Filename) ->
    % strip leading whitspace
    abs_pathname(Filename);
abs_pathname([$/ |_]=Filename) ->
    Filename;
abs_pathname(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    {Filename2, Args} = separate_cmd_args(Filename, ""),
    abs_pathname(Filename2, Cwd) ++ Args.

abs_pathname(Filename, Dir) ->
    Name = filename:absname(Filename, Dir ++ "/"),
    OutFilename = filename:join(fix_path_list(filename:split(Name), [])),
    % If the filename is a dir (last char slash, put back end slash
    case string:right(Filename,1) of
    "/" ->
        OutFilename ++ "/";
    "\\" ->
        OutFilename ++ "/";
    _Else->
        OutFilename
    end.

% if this as an executable with arguments, seperate out the arguments
% ""./foo\ bar.sh -baz=blah" -> {"./foo\ bar.sh", " -baz=blah"}
separate_cmd_args("", CmdAcc) ->
    {lists:reverse(CmdAcc), ""};
separate_cmd_args("\\ " ++ Rest, CmdAcc) -> % handle skipped value
    separate_cmd_args(Rest, " \\" ++ CmdAcc);
separate_cmd_args(" " ++ Rest, CmdAcc) ->
    {lists:reverse(CmdAcc), " " ++ Rest};
separate_cmd_args([Char|Rest], CmdAcc) ->
    separate_cmd_args(Rest, [Char | CmdAcc]).

% lowercases string bytes that are the ascii characters A-Z.
% All other characters/bytes are ignored.
ascii_lower(String) ->
    ascii_lower(String, []).

ascii_lower([], Acc) ->
    lists:reverse(Acc);
ascii_lower([Char | RestString], Acc) when Char >= $A, Char =< $B ->
    ascii_lower(RestString, [Char + ($a-$A) | Acc]);
ascii_lower([Char | RestString], Acc) ->
    ascii_lower(RestString, [Char | Acc]).

% Is a character whitespace?
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_Else) -> false.


% removes leading and trailing whitespace from a string
trim(String) ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).

% takes a heirarchical list of dirs and removes the dots ".", double dots
% ".." and the corresponding parent dirs.
fix_path_list([], Acc) ->
    lists:reverse(Acc);
fix_path_list([".."|Rest], [_PrevAcc|RestAcc]) ->
    fix_path_list(Rest, RestAcc);
fix_path_list(["."|Rest], Acc) ->
    fix_path_list(Rest, Acc);
fix_path_list([Dir | Rest], Acc) ->
    fix_path_list(Rest, [Dir | Acc]).


implode(List, Sep) ->
    implode(List, Sep, []).

implode([], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc));
implode([H], Sep, Acc) ->
    implode([], Sep, [H|Acc]);
implode([H|T], Sep, Acc) ->
    implode(T, Sep, [Sep,H|Acc]).


drv_port() ->
    case get(couch_drv_port) of
    undefined ->
        Port = open_port({spawn, "couch_icu_driver"}, []),
        put(couch_drv_port, Port),
        Port;
    Port ->
        Port
    end.

collate(A, B) ->
    collate(A, B, []).

collate(A, B, Options) when is_binary(A), is_binary(B) ->
    Operation =
    case lists:member(nocase, Options) of
        true -> 1; % Case insensitive
        false -> 0 % Case sensitive
    end,
    SizeA = byte_size(A),
    SizeB = byte_size(B),
    Bin = <<SizeA:32/native, A/binary, SizeB:32/native, B/binary>>,
    [Result] = erlang:port_control(drv_port(), Operation, Bin),
    % Result is 0 for lt, 1 for eq and 2 for gt. Subtract 1 to return the
    % expected typical -1, 0, 1
    Result - 1.

should_flush() ->
    should_flush(?FLUSH_MAX_MEM).

should_flush(MemThreshHold) ->
    {memory, ProcMem} = process_info(self(), memory),
    BinMem = lists:foldl(fun({_Id, Size, _NRefs}, Acc) -> Size+Acc end,
        0, element(2,process_info(self(), binary))),
    if ProcMem+BinMem > 2*MemThreshHold ->
        garbage_collect(),
        {memory, ProcMem2} = process_info(self(), memory),
        BinMem2 = lists:foldl(fun({_Id, Size, _NRefs}, Acc) -> Size+Acc end,
            0, element(2,process_info(self(), binary))),
        ProcMem2+BinMem2 > MemThreshHold;
    true -> false end.

encodeBase64Url(Url) ->
    Url1 = iolist_to_binary(re:replace(base64:encode(Url), "=+$", "")),
    Url2 = iolist_to_binary(re:replace(Url1, "/", "_", [global])),
    iolist_to_binary(re:replace(Url2, "\\+", "-", [global])).

decodeBase64Url(Url64) ->
    Url1 = re:replace(iolist_to_binary(Url64), "-", "+", [global]),
    Url2 = iolist_to_binary(
        re:replace(iolist_to_binary(Url1), "_", "/", [global])
    ),
    Padding = ?l2b(lists:duplicate((4 - size(Url2) rem 4) rem 4, $=)),
    base64:decode(<<Url2/binary, Padding/binary>>).

dict_find(Key, Dict, DefaultValue) ->
    case dict:find(Key, Dict) of
    {ok, Value} ->
        Value;
    error ->
        DefaultValue
    end.


file_read_size(FileName) ->
    case file:read_file_info(FileName) of
        {ok, FileInfo} ->
            FileInfo#file_info.size;
        Error -> Error
    end.

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch
        _ ->
            list_to_binary(io_lib:format("~p", [V]))
    end;
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) ->
    list_to_binary(io_lib:format("~p", [V])).

to_integer(V) when is_integer(V) ->
    V;
to_integer(V) when is_list(V) ->
    erlang:list_to_integer(V);
to_integer(V) when is_binary(V) ->
    erlang:list_to_integer(binary_to_list(V)).

to_list(V) when is_list(V) ->
    V;
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) ->
    lists:flatten(io_lib:format("~p", [V])).

url_encode(Bin) when is_binary(Bin) ->
    url_encode(binary_to_list(Bin));
url_encode([H|T]) ->
    if
    H >= $a, $z >= H ->
        [H|url_encode(T)];
    H >= $A, $Z >= H ->
        [H|url_encode(T)];
    H >= $0, $9 >= H ->
        [H|url_encode(T)];
    H == $_; H == $.; H == $-; H == $: ->
        [H|url_encode(T)];
    true ->
        case lists:flatten(io_lib:format("~.16.0B", [H])) of
        [X, Y] ->
            [$%, X, Y | url_encode(T)];
        [X] ->
            [$%, $0, X | url_encode(T)]
        end
    end;
url_encode([]) ->
    [].

json_encode(V) ->
    Handler =
    fun({L}) when is_list(L) ->
        {struct,L};
    (Bad) ->
        exit({json_encode, {bad_term, Bad}})
    end,
    (mochijson2:encoder([{handler, Handler}]))(V).

json_decode(V) ->
    try (mochijson2:decoder([{object_hook, fun({struct,L}) -> {L} end}]))(V)
    catch
        _Type:_Error ->
            throw({invalid_json,V})
    end.

verify([X|RestX], [Y|RestY], Result) ->
    verify(RestX, RestY, (X bxor Y) bor Result);
verify([], [], Result) ->
    Result == 0.

verify(<<X/binary>>, <<Y/binary>>) ->
    verify(?b2l(X), ?b2l(Y));
verify(X, Y) when is_list(X) and is_list(Y) ->
    case length(X) == length(Y) of
        true ->
            verify(X, Y, 0);
        false ->
            false
    end;
verify(_X, _Y) -> false.

compressible_att_type(MimeType) when is_binary(MimeType) ->
    compressible_att_type(?b2l(MimeType));
compressible_att_type(MimeType) ->
    TypeExpList = re:split(
        couch_config:get("attachments", "compressible_types", ""),
        ", ?",
        [{return, list}]
    ),
    lists:any(
        fun(TypeExp) ->
            Regexp = "^\\s*" ++
                re:replace(TypeExp, "\\*", ".*", [{return, list}]) ++ "\\s*$",
            case re:run(MimeType, Regexp, [caseless]) of
            {match, _} ->
                true;
            _ ->
                false
            end
        end,
        [T || T <- TypeExpList, T /= []]
    ).

-spec md5(Data::(iolist() | binary())) -> Digest::binary().
md5(Data) ->
    try crypto:md5(Data) catch error:_ -> erlang:md5(Data) end.

-spec md5_init() -> Context::binary().
md5_init() ->
    try crypto:md5_init() catch error:_ -> erlang:md5_init() end.

-spec md5_update(Context::binary(), Data::(iolist() | binary())) ->
    NewContext::binary().
md5_update(Ctx, D) ->
    try crypto:md5_update(Ctx,D) catch error:_ -> erlang:md5_update(Ctx,D) end.

-spec md5_final(Context::binary()) -> Digest::binary().
md5_final(Ctx) ->
    try crypto:md5_final(Ctx) catch error:_ -> erlang:md5_final(Ctx) end.

% linear search is faster for small lists, length() is 0.5 ms for 100k list
reorder_results(Keys, SortedResults) when length(Keys) < 100 ->
    [couch_util:get_value(Key, SortedResults) || Key <- Keys];
reorder_results(Keys, SortedResults) ->
    KeyDict = dict:from_list(SortedResults),
    [dict:fetch(Key, KeyDict) || Key <- Keys].

url_strip_password(Url) ->
    re:replace(Url,
        "http(s)?://([^:]+):[^@]+@(.*)$",
        "http\\1://\\2:*****@\\3",
        [{return, list}]).

% borrowed from xmerl_ucs.erl
to_utf8(List) when is_list(List) -> lists:flatmap(fun to_utf8/1, List);
to_utf8(Ch) -> char_to_utf8(Ch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UTF-8 support
%%% Possible errors encoding UTF-8:
%%%	- Non-character values (something other than 0 .. 2^31-1).
%%%	- Surrogate pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
%%% Possible errors decoding UTF-8:
%%%	- 10xxxxxx or 1111111x as initial byte.
%%%	- Insufficient number of 10xxxxxx octets following an initial octet of
%%%	multi-octet sequence.
%%% 	- Non-canonical encoding used.
%%%	- Surrogate-pair code encoded as UTF-8.
%%%	- 16#FFFE or 16#FFFF character in string.
char_to_utf8(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch < 128 ->
	    %% 0yyyyyyy
	    [Ch];
       Ch < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    [16#C0 + (Ch bsr 6),
	     128+(Ch band 16#3F)];
       Ch < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
		    [16#E0 + (Ch bsr 12),
		     128+((Ch bsr 6) band 16#3F),
		     128+(Ch band 16#3F)]
	    end;
       Ch < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    [16#F0+(Ch bsr 18),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#F8+(Ch bsr 24),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#FC+(Ch bsr 30),
	     128+((Ch bsr 24) band 16#3F),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)]
    end.

from_utf8(Bin) when is_binary(Bin) -> from_utf8(binary_to_list(Bin));
from_utf8(List) -> 
    case expand_utf8(List) of
	{Result,0} -> Result;
	{_Res,_NumBadChar} ->
	    exit({ucs,{bad_utf8_character_code}})
    end.

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
					 C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when 16#80 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
					    C2 band 16#C0 =:= 16#80,
					    C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when 16#800 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
					       C2 band 16#C0 =:= 16#80,
					       C3 band 16#C0 =:= 16#80,
					       C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when 16#10000 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc),Bad}.


from_utf16be(Bin) when is_binary(Bin) -> from_utf16be(Bin,[],[]).
from_utf16be(<<Ch:16/big-unsigned-integer, Rest/binary>>, Acc, Tail)
  when Ch < 16#D800; Ch > 16#DFFF ->
    if Ch < 16#FFFE -> from_utf16be(Rest,[Ch|Acc],Tail) end;
from_utf16be(<<Hi:16/big-unsigned-integer, Lo:16/big-unsigned-integer,
	       Rest/binary>>, Acc, Tail)
  when Hi >= 16#D800, Hi < 16#DC00, Lo >= 16#DC00, Lo =< 16#DFFF ->
    %% Surrogate pair
    Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    from_utf16be(Rest, [Ch|Acc], Tail);
from_utf16be(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_utf16be(Bin,Acc,Tail) ->
    io:format("ucs Error: Bin=~p~n     Acc=~p~n     Tail=~p~n",[Bin,Acc,Tail]),
    {error,not_utf16be}.
