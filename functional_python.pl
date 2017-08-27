#!/usr/bin/env perl
#
sub limit {
    my $app = shift;
    my $rac = shift;

    return ($app + $rac/$app)/2;
}

sprintf("The result is %.3f", limit(1,5));
