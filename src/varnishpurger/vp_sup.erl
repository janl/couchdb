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

-module(vp_sup).
-behaviour(supervisor).
-include("../couchdb/couch_db.hrl").

-export([init/1,start_link/0]).

init(ChildSpecs) ->
    {ok, {{one_for_one, 10, 3}, ChildSpecs}}.

start_link() ->
    MakeChildSpecs = fun({Db, _} = VarnishSpec) -> 
        Id = Db ++ ?b2l(couch_uuids:random()),
        {Id, % allow multiple purgers per db
         {vp_purge, start, [VarnishSpec]},
         permanent,
         brutal_kill,
         worker,
         [vp_purge]}
    end,
    case couch_config:get("varnishpurger") of
    {error, Reason} ->
        ?LOG_INFO("Can't read varnishpurger configuration: '~s'", [Reason]),
        ignore;
    [] ->
        ?LOG_DEBUG("Varnishpurger configuration is empty.", []),
        ignore;
    Config ->
        ChildSpecs = lists:map(MakeChildSpecs, Config),
        supervisor:start_link({local, varnishpurger}, vp_sup, ChildSpecs)
    end.
