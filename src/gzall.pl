#!/usr/bin/perl

# This script should be uploaded to the web server.

use warnings;
use strict;
use File::Find;

find (\&wanted, ("."));

sub wanted
{
    if (/(.*\.(?:html|css|js)$)/i) {
        print "Compressing $File::Find::name\n";
        system ("gzip --best --force -c $_ > $_.gz");        
    }
}

##if (! -f "$_.gz") {
## system ("gzip --keep --best --force $_");
