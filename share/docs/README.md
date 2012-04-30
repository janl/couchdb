# CouchDB Docs

This repo include DocBook source for Apache CouchDB API docs and release
guides.

## Requirements

In order to build the documentation you need the following Perl
modules installed:

Carp
File::Basename
Getopt::Long
IO::File
Module::Load;
Module::Util
XML::Parser::PerlSAX
Image::Info
DateTime

To use the publishing system, you need the following Perl modules:

Archive::Zip
Cwd
File::Rsync

In short:

    $ [sudo] cpan -i Carp File::Basename IO:File Module::Load Module::Util XML::Parser::PerlSAX Image::Info Archive::Zip Cwd File::Rsync DateTime

## Format Requirements

To actually build the target formats and assemble the documentation,
you also need:

xsltproc (http://xmlsoft.org/XSLT/downloads.html)
FOP 1.x (http://xmlgraphics.apache.org/fop/)
Zip
GNU make

## Building

You can build all of the available documentation (in the respective
formats) from the top level of the repository using:

    $ make everything

If you want to make a specific document (in all formats), change to
the directory and make everything:

    $ cd couchdb-manual/
    $ make everything

To make a specific format for a specific document, change to the
directory and specify the target. For example, to make a PDF of the
couchbase-api document:

    $ cd couchdb-manual
    $ make couchbase-api.pdf

## Publishing

Publishing of the documentation creates and assembles the correct
documentation and bundles ready for distribution/download.

Then, from the top level of the main CouchDocs repo: 

    $ make publish

The process also creates an index file (index.html), and copies the
configured documents into the directory specified in the Makefile
(PUBLISH_DIR)

## License

Creative Commons 3.0.
