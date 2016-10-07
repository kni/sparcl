$| = 1;
use v5.10;
use strict;
use warnings;

use Test::More "no_plan";
use Data::Dumper;

use Sparcl;


my $takeStr = takeStr("INFO");
is_deeply([ $takeStr->("INFOTAIL") ], ["Done", "INFO", "TAIL"], "takeStr");

my $takeNat = takeNat();
is_deeply([ $takeNat->("100TAIL") ], ["Done", 100, "TAIL"], "takeNat");


is_deeply([ Bind(takeNat(), \&takeN)->("4INFOTAIL") ], ["Done", "INFO", "TAIL"], "Bind");

is_deeply([ apR(takeStr('$'), takeNat())->("\$4TAIL") ], ["Done", 4, "TAIL"], "apR");

is_deeply([ apL(takeNat(), takeStr("INFO"))->("4INFOTAIL") ], ["Done", 4, "TAIL"], "apL");


is_deeply(
	[ many(takeStr('$'), takeNat(), takeStr("\r\n"), takeStr("INFO"), takeStr("\r\n"))->("\$4\r\nINFO\r\nTAIL") ],
	[ "Done", ["\$", 4, "\r\n", "INFO", "\r\n"], "TAIL" ],
	"many"
);



my $foo = Bind(
		apR(takeStr('$'), takeNat()),
		sub {
			my ($n) = @_;
			apR(
				takeStr("\r\n"),
				apL( takeN($n), takeStr("\r\n"))
			)
		}
	);

is_deeply( [ $foo->("\$4\r\nINFO\r\nTAIL") ], ["Done", "INFO", "TAIL"], "foo");


is_deeply( [ choice( takeStr("PING"), takeStr("PONG"), takeStr("INFO"), takeStr("END") )->("INFO") ], ["Done", "INFO", ""], "choice");
