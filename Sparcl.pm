package Sparcl;

use strict;
use warnings;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(
	takeStr takeN takeNat
	Bind apR apL many choice
);


# Генераторы персеров


# Вернуть парсер указанной строки.
sub takeStr {
	my ($s) = @_;
	my $s_l = length($s);
	sub {
		my ($ss) = @_;
		my $ss_l = length($ss);

		if ($ss_l >= $s_l) {
			my $sss = substr $ss, 0, $s_l;
			if ($sss eq $s) {
				substr $ss, 0, $s_l, "";
				return "Done", $s, $ss;
			} else {
				return "Fail";
			}
		} else {
			return "Partial";
		}
	}
}

# Вернуть парсер n символов.
sub takeN {
	my ($s_l) = @_;
	sub {
		my ($ss) = @_;
		my $ss_l = length($ss);
		if ($ss_l >= $s_l) {
			my $sss = substr $ss, 0, $s_l;
			substr $ss, 0, $s_l, "";
			return "Done", $sss, $ss;
		} else {
			return "Partial";
		}
	};
}

# Вернуть парсер натурального числа.
sub takeNat {
	sub {
		my ($ss) = @_;
		$ss or return "Partial";
		if ($ss =~ m/^(\d+)/){
			if ($') {
				return "Done", $1, $';
			} else {
				return "Partial";
			}
		} else {
			return "Fail";
		}
	};
};




# Комбинаторы парсеров.

sub Bind {
	my ($p, $pg) = @_;
	sub {
		my ($ss) = @_;
		my ($done, $r, $t) = $p->($ss);
		if ($done eq "Done") {
			my ($done, $r, $t) = $pg->($r)->($t);
			if ($done eq "Done") {
				return $done, $r, $t;
			} else {
				return $done;
			}
		} else {
			return $done;
		}
	};
}



sub apR {
	my ($p1, $p2) = @_;
	sub {
		my ($ss) = @_;
		my ($done1, $r1, $t1) = $p1->($ss);
		if ($done1 eq "Done") {
			my ($done2, $r2, $t2) = $p2->($t1);
			if ($done2 eq "Done") {
				return $done2, $r2, $t2;
			} else {
				return $done2;
			}
		} else {
			return $done1;
		}
	};
}



sub apL {
	my ($p1, $p2) = @_;
	sub {
		my ($ss) = @_;
		my ($done1, $r1, $t1) = $p1->($ss);
		if ($done1 eq "Done") {
			my ($done2, $r2, $t2) = $p2->($t1);
			if ($done2 eq "Done") {
				return $done2, $r1, $t2;
			} else {
				return $done2;
			}
		} else {
			return $done1;
		}
	};
}



sub many {
	my ($f, @f) = @_;
	sub {
		my ($ss) = @_;
		my ($done, $r, $t) = $f->($ss);
		if ($done eq "Done") {
			if (@f) {
				my @r = ($r);
				my $ss = $t;
				my ($done, $r, $t) = many(@f)->($ss);
				if ($done eq "Done") {
					push @r, (ref $r ? @$r : $r);
					return $done, \@r, $t;
				} else {
					return $done;
				}
			} else {
				return $done, $r, $t;
			}
		} else {
			return $done;
		}
	};
}



# Какой парсер подойдет (по порядку).
sub choice {
	my (@p) = @_;
	sub {
		my ($ss) = @_;
		my $is_Partial = 0;
		foreach my $p (@p) {
			my ($done, $r, $t) = $p->($ss);
			if ($done eq "Done") {
				return $done, $r, $t;
			} elsif ($done eq "Partial") {
				$is_Partial = 1;
			}
		}
		if ($is_Partial) {
			return "Partial";
		} else {
			return "Fail";
		}
	};
}


1;
