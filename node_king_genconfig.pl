#!/usr/bin/perl -w
#
use strict;
use warnings;
use Data::UUID;


my $ug = new Data::UUID;

my %nodes = ();
my $NodeCount = 10;

open(CONFIG, '>node_king.config') and do {
  print CONFIG qq|%{uuid/bitstring, name/bitstring, ip/bitstring, port/integer, type/integer, id/integer, self/boolean}\n|;
  for (my $in = 0; $in < $NodeCount; ++$in) {
    my $uuid = $ug->create_str();
    my $name = sprintf("nodeking%03d", int($in + 1));
    my $ip = 'localhost';
    my $port = 9090 + int($in);
    my $type = int(rand(3));
    my $id = int(rand(10));
    my $self = 'false';

    $self = 'true' if $in == 5;
    $nodes{$id} = 0 unless exists $nodes{$id};
    ++$nodes{$id};

    print CONFIG qq|{"$uuid", "$name", "$ip", $port, $type, $id, $self}.\n|;
  }
  close(CONFIG);
};


foreach my $id(sort keys %nodes) {
  my $value = $nodes{$id};
  next unless defined $value;
  print qq|$id = $value\n|;
}

