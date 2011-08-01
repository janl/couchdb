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

jsonpointerebindir = $(localerlanglibdir)/jsonpointer-0.0.1/ebin

jsonpointer_file_collection = \
  src/jsonpointer.erl \
  src/jsonpointer_app.src

jsonpointerebin_make_generated_file_list = \
  ebin/jsonpointer.beam \
  ebin/jsonpointer.app

jsonpointerebin_DATA = \
    $(jsonpointerebin_make_generated_file_list)

EXTRA_DIST =  \
    $(jsonpointer_file_collection)

CLEANFILES = \
    $(jsonpointerebin_make_generated_file_list) jsonpointer.beam jsonpointer.app

# ebin/%.app: src%.app.src
#   cp $< $@
# 
# %.beam: %.erl
#   $(ERLC) $(ERLC_FLAGS) $<

all:
	rebar compile
	cp ebin/* .
