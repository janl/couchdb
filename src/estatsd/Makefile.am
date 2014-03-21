## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

estatsdlibdir = $(localerlanglibdir)/estatsd-1.0
estatsdebindir = $(estatsdlibdir)/ebin

estatsdebin_DATA = $(compiled_files)

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

source_files = \
	src/estatsd.app.src \
	src/estatsd_app.erl \
	src/estatsd_sup.erl \
	src/estatsd_couch.erl \
	src/estatsd_server.erl \
	src/estatsd.erl


compiled_files = \
	ebin/estatsd_app.beam \
	ebin/estatsd_sup.beam \
	ebin/estatsd_couch.beam \
	ebin/estatsd_server.beam \
	ebin/estatsd.app \
	ebin/estatsd.beam

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	cp $< $@

ebin/%.beam: src/%.erl $(include_files)
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/src/couchdb \
		-o ebin/ $(ERLC_FLAGS) ${TEST} $<;
