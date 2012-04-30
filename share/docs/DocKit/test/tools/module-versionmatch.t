#!/usr/bin/perl

use warnings;
use strict;
use lib '../../bin';
use CouchDocs;
use Test::Simple tests => 22;

my $cd = new CouchDocs();

my $nv = {
};

my $iv = { 
   iv => '1.5',
   };

my $ov = { 
   ov => '1.5',
   };

my $ivov = { 
   iv => '1.5',
   ov => '2.4',
   };

ok( $cd->meta_verify_version('1.0',$nv) == 1, 'No version 1.0');
ok( $cd->meta_verify_version('2.9',$nv) == 1, 'No version 2.9');
ok( $cd->meta_verify_version('',$nv) == 1, 'No version ');

ok( $cd->meta_verify_version('1.0',$iv) == 0, 'inversion 1.0');
ok( $cd->meta_verify_version('1.5',$iv) == 1, 'inversion 1.5');
ok( $cd->meta_verify_version('1.0-1.9',$iv) == 1, 'inversion 1.0-1.5');
ok( $cd->meta_verify_version('1.0-1.9',$iv) == 1, 'inversion 1.0-1.5');

ok( $cd->meta_verify_version('1.0',$ov) == 1, 'outversion 1.0');
ok( $cd->meta_verify_version('1.5',$ov) == 0, 'outversion 1.5 (false)');
ok( $cd->meta_verify_version('1.0-1.9',$ov) == 1, 'outversion 1.0-1.9');
ok( $cd->meta_verify_version('1.6-2.4',$ov) == 0, 'outversion 1.6-2.4 (false)');

ok( $cd->meta_verify_version('1.0',$ivov) == 0, 'ivov 1.0 (false)');
ok( $cd->meta_verify_version('1.5',$ivov) == 1, 'ivov 1.5');
ok( $cd->meta_verify_version('1.0-2.5',$ivov) == 1, 'ivov 1.0-2.5');
ok( $cd->meta_verify_version('0.0-1.4',$ivov) == 0, 'ivov 0.0-1.4 (false)');
ok( $cd->meta_verify_version('2.5-3.0',$ivov) == 0, 'ivov 2.5-3.0 (false)');

ok( $cd->meta_verify_version('1.1-1.4',$ivov) == 0, 'range 1.1-1.4 (false)');
ok( $cd->meta_verify_version('1.1-1.9',$ivov) == 1, 'range 1.1-1.9');
ok( $cd->meta_verify_version('1.4,1.9',$ivov) == 1, 'range 1.4,1.9');
ok( $cd->meta_verify_version('1.5,1.9',$ivov) == 1, 'range 1.5,1.9');
ok( $cd->meta_verify_version('1.1,2.5,1.1-1.4',$ivov) == 0, 'range 1.1,2.5,1.1-1.4 (false)');
ok( $cd->meta_verify_version('1.1,2.5,1.4-3.0',$ivov) == 1, 'range 1.1,2.5,1.4-3.0');

