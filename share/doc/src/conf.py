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

# http://sphinx.pocoo.org/config.html

import sys, os

extensions = ["sphinx.ext.todo"]

source_suffix = ".rst"

master_doc = "index"

nitpicky = True

# CouchDB ##############################################################
# These will be over-ridden during make dist but are listed here
# to ensure that the readthedocs.org build has version numbers present
# The short X.Y version.
#version = "@@"
version = "latest"
# The full version, including alpha/beta/rc tags.
# release = None
#release = "@@"
release = "latest"

#project = "@@"
project = u'Apache CouchDB'

#copyright = "@@"
copyright = u'2012, Apache Software Foundation'

highlight_language = "json"

pygments_style = "sphinx"

# @@ design CouchDB theme
# @@ http://jinja.pocoo.org/docs/ looks nice

html_theme = "default"

# inherit from project title
#html_short_title = "@@"

html_logo = ""

html_favicon = ""

html_use_opensearch = ""

html_add_permalinks = True

text_newlines = "native"

# use this for excluding docs from final build
# unused_docs = ["src/tmp/*.rst"]

# latex_documents = [(
# 	"index",
# 	"CouchDB.tex",
# 	"",
# 	"",
# 	"manual",
# 	True
# )]

# #latex_elements[(
# #	"a4paper" # @@ do we want this?
# #)]

# texinfo_documents = [(
# 	"index",
# 	"CouchDB",
# 	"",
# 	"",
# 	"dir_entry",
# 	"description",
# 	"category",
# 	"manual",
# 	True
# )]

rst_epilog = """
.. |psf| replace:: Apache Software Foundation
"""