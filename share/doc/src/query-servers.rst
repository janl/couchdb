.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

.. default-domain:: js

=============
Query servers
=============

.. _queryserver_js:

JavaScript
==========

.. note:: While every design function has access to all JavaScript objects,
   the table below describes appropriate usage cases. For example,
   you may use :func:`emit` in :ref:`listfun`, but :func:`getRow` is not permitted during :ref:`mapfun`.

+--------------------------------+---------------------------------------------+
| JS Function                    | Reasonable to use in design doc functions   |
+================================+=============================================+
| :func:`emit`                   | :ref:`mapfun`                               |
+--------------------------------+---------------------------------------------+
| :func:`getRow`                 | :ref:`listfun`                              |
+--------------------------------+---------------------------------------------+
| :data:`JSON`                   | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`isArray`                | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`log`                    | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`provides`               | :ref:`showfun`, :ref:`listfun`              |
+--------------------------------+---------------------------------------------+
| :func:`registerType`           | :ref:`showfun`, :ref:`listfun`              |
+--------------------------------+---------------------------------------------+
| :func:`require`                | every except :ref:`reducefun`               |
+--------------------------------+---------------------------------------------+
| :func:`send`                   | :ref:`listfun`                              |
+--------------------------------+---------------------------------------------+
| :func:`start`                  | :ref:`listfun`                              |
+--------------------------------+---------------------------------------------+
| :func:`sum`                    | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`toJSON`                 | any                                         |
+--------------------------------+---------------------------------------------+

Design functions context
------------------------

Each design function executes within special context of predefined objects,
modules and functions:


.. function:: emit(key, value)

   Puts `key`-`value` pair into inner stack for further passing to CouchDB
   when map function is done.

   :param key: View's key.
   :param value: Associated value with `key`.

   .. code-block:: javascript

      function(doc){
        emit(doc._id, doc._rev);
      }


.. function:: getRow()

   Extracts next row from the related view result.

   :return: View result row
   :rtype: object

   .. code-block:: javascript

      function(head, req){
        send('[');
        row = getRow();
        if (row){
          send(toJSON(row));
          while(row = getRow()){
            send(',');
            send(toJSON(row));
          }
        }
        return ']';
      }


.. data:: JSON

   `JSON2 <https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=blob;f=share/server/json2.js>`_
   object.


.. function:: isArray(obj)

   Helper to check is provided value `array` or not

   :param obj: Any Javascript value
   :return: ``true`` is `obj` is `array` typed, ``false`` otherwise.
   :rtype: boolean


.. function:: log(message)

   :param message: Message to log in at CouchDB `INFO` level.

   .. code-block:: javascript

      function(doc){
        log('Procesing doc ' + doc['_id']);
        emit(doc['_id'], null);
      }

   On map function run in CouchDB logs (e.g. at `/var/log/couchdb/couch.log`)
   you may find next record:

   .. code-block:: text

      [Sat, 03 Nov 2012 17:38:02 GMT] [info] [<0.7543.0>] OS Process #Port<0.3289> Log :: Processing doc 8d300b86622d67953d102165dbe99467


.. function:: provides(key, func)

   Registers callable handler for specified MIME key.

   :param key: MIME key previously defined by :func:`registerType`
   :param func: MIME type handler.


.. function:: registerType(key, *mimes)

   Registers list of MIME types by associated `key`.

   :param key: MIME types
   :param mimes: MIME types enumeration.

   Predefined mappings (`key`-`array`):

   - **all**: ``*/*``
   - **text**: ``text/plain; charset=utf-8``, ``txt``
   - **html**: ``text/html; charset=utf-8``
   - **xhtml**: ``application/xhtml+xml``, ``xhtml``
   - **xml**: ``application/xml``, ``text/xml``, ``application/x-xml``
   - **js**: ``text/javascript``, ``application/javascript``,
     ``application/x-javascript``
   - **css**: ``text/css``
   - **ics**: ``text/calendar``
   - **csv**: ``text/csv``
   - **rss**: ``application/rss+xml``
   - **atom**: ``application/atom+xml``
   - **yaml**: ``application/x-yaml``, ``text/yaml``
   - **multipart_form**: ``multipart/form-data``
   - **url_encoded_form**: ``application/x-www-form-urlencoded``
   - **json**: ``application/json``, ``text/x-json``


.. function:: require(path)

   Loads CommonJS module by specified `path`. Path shouldn't starts with slash.

   :param path: CommonJS module path started from design document root.
   :return: Exported statements.


.. function:: send(chunk)

   Sends a single string `chunk` in response.

   :param chunk: Text chunk

   .. code-block:: javascript

      function(head, req){
        send('Hello,');
        send(' ');
        send('Couch');
        return !
      }


.. function:: start(init_resp)

   Initiates chunked response. As an option, custom
   :ref:`response <response_object>` object may be sent at this point.
   For `list`-functions only!

   .. note::
   
      Only at this point list functions may set response `HTTP code` and
      `headers`. Also, you need to run this function before :func:`send`,
      :func:`getRow` or `return` statement or query server will implicitly call
      this function with empty object (``{}``).

   .. code-block:: javascript

      function(head, req){
        start({
          "code": 302,
          "headers": {
            "Location": "http://couchdb.apache.org"
          }
        });
        return "Relax!";
      }


.. function:: sum(arr)

   Summarize `arr` items.

   :param arr: Array of numbers.
   :rtype: number


.. function:: toJSON(obj)

   Encodes `obj` to JSON string. Actually is a proxy to ``JSON.stringify``
   method.

   :param obj: JSON encodable object.
   :return: JSON string.



CommonJS Modules
----------------

`CommonJS Modules <http://wiki.commonjs.org/wiki/Modules/1.1.1>`_ is the one of
major CouchDB feature introduced in 0.11.0 version that allows to create modular
design functions without needs to duplicate a lot of same functionality.

Example of CommonJS module that checks user permissions:

.. code-block:: javascript

    function user_context(userctx, secobj){
      var is_admin = function(){
        return userctx.indexOf('_admin') != -1;
      }
      var is_db_admin = function(){
        if (is_admin() || !secobj){
          return true;
        }
        if (secobj.admins.names.indexOf(userctx.name) != -1){
          return true;
        }
        for (var idx in userctx.roles){
          if (secobj.admins.roles.indexOf(userctx.roles[idx]) != -1){
            return true;
          }
        }
        return false;
      }
      var is_db_member = function(){
        if (is_admin() || is_db_admin() || !secobj){
          return true;
        }
        if (secobj.members.names.indexOf(userctx.name) != -1){
          return true;
        }
        for (var idx in userctx.roles){
          if (secobj.members.roles.indexOf(userctx.roles[idx]) != -1){
            return true;
          }
        }
        return false;
      }
      var has_all_roles = function(roles){
        for (var idx in roles){
          if (userctx.roles.indexOf(roles[idx]) == -1){
            return false;
          }
        }
        return true;
      }
      var has_any_role = function(roles){
        for (var idx in roles){
          if (userctx.roles.indexOf(roles[idx]) != -1){
            return true;
          }
        }
        return false;
      }
      return {
        'is_admin': is_admin,
        'is_db_admin': is_db_admin,
        'is_db_member': is_db_member,
        'has_all_roles': has_all_roles,
        'has_any_role': has_any_role
      }
    }

    exports['user'] = user_context

Each module has access to additional global variables:

- **module** (`object`): Contains information about stored module.

  - **id** (`string`): Module id that is actually JSON path in ddoc context.
  - **current** (`code`): Compiled module code object.
  - **parent** (`object`): Parent frame.
  - **exports** (`object`): Export statements.

- **exports** (`object`): Shortcut to ``module.exports`` object.

Lets place module above within design document under `lib/validate` path.
Now we could use it in our design functions:

.. code-block:: javascript

    function(newdoc, olddoc, userctx, secobj){
      user = require('lib/validate').user(userctx, secobj);
      if (user.is_admin()){
        return true;
      }
      if (newdoc.author != olddoc.author){
        throw({'forbidden': 'unable to update `author` field'});
      }
    }


.. _queryserver_erlang:

Erlang
======

.. warning::

   Erlang query server runs out of sandbox feature like JavaScript has to!
   This means, that Erlang code has full access to your OS, file system and
   network which may leads to security issues. While Erlang functions are
   faster than JavaScript ones, you need to be careful with running them,
   especially if they  wasn't written by your own hands.

   Keep in mind: don't trust every code - review it first before running.


.. note::

   Due to security restriction, Erlang query server is disabled by default.
   To enable it you'll need to edit your `local.ini` to include a
   ``native_query_servers`` section:

   .. code-block:: ini

      [native_query_servers]
      erlang = {couch_native_process, start_link, []}

   And don't forget to restart CouchDB after that and use ``language: "erlang"``
   property in your Erlang design documents.


.. function:: Emit(Id, Value)

   Emits `key`-`value` pair to view indexer process.

   .. code-block:: erlang

      fun({Doc}) ->
        <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
        V = proplists:get_value(<<"_id">>, Doc, null),
        Emit(<<K>>, V)
      end.


.. function:: FoldRows(Fun, Acc)

   Helper to iterate over all rows in list function.

   :param Fun: Function object.
   :param Acc: Previous returned value by `Fun`.

   .. code-block:: erlang

      fun(Head, {Req}) ->
        Fun = fun({Row}, Acc) ->
          Id = couch_util:get_value(<<"id">>, Row),
          Send(list_to_binary(io_lib:format("Previous doc id: ~p~n", [Acc]))),
          Send(list_to_binary(io_lib:format("Current  doc id: ~p~n", [Id]))),
          {ok, Id}
        end,
        FoldRows(Fun, nil),
        ""
      end.


.. function:: GetRow()

   Retrieves next row from related view result.

   .. code-block:: erlang

      %% FoldRows background implementation.
      %% https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=blob;f=src/couchdb/couch_native_process.erl;hb=HEAD#l368
      %%
      foldrows(GetRow, ProcRow, Acc) ->
        case GetRow() of
          nil ->
            {ok, Acc};
          Row ->
            case (catch ProcRow(Row, Acc)) of
              {ok, Acc2} ->
                foldrows(GetRow, ProcRow, Acc2);
              {stop, Acc2} ->
                {ok, Acc2}
            end
      end.

.. function:: Log(Msg)

   :param Msg: Message to log in at CouchDB `INFO` level.

   .. code-block:: erlang

      fun({Doc}) ->
        <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
        V = proplists:get_value(<<"_id">>, Doc, null),
        Log(lists:flatten(io_lib:format("Hello from ~s doc!", [V]))),
        Emit(<<K>>, V)
      end.

   On map function run in CouchDB logs (e.g. at `/var/log/couchdb/couch.log`)
   you may find next record:

   .. code-block:: text

      [Sun, 04 Nov 2012 11:33:58 GMT] [info] [<0.9144.2>] Hello from 8d300b86622d67953d102165dbe99467 doc!


.. function:: Send(Chunk)

   Sends a single string `Chunk` in response.

   .. code-block:: erlang

      fun(Head, {Req}) ->
        Send("Hello,"),
        Send(" "),
        Send("Couch"),
        "!"
      end.

   Function above produces next response:

   .. code-block:: text

      Hello, Couch!


.. function:: Start(Headers)

   :param Headers: Proplist of :ref:`response object<response_object>`.

   Initialize :ref:`listfun` response. At this point response code and headers
   may be defined. For example, next function redirect to CouchDB web site:

   .. code-block:: erlang

      fun(Head, {Req}) ->
        Start({[{<<"code">>, 302},
                {<<"headers">>, {[
                  {<<"Location">>, <<"http://couchdb.apache.org">>}]
                }}
              ]}),
        "Relax!"
      end.


