# Copyright (c) 1999 James H. Woodyatt.  All rights reserved.
#
# Redistribution and use in source and machine-readable forms, with or without
# modification, are permitted provided that the conditions and terms of its
# accompanying LICENSE are met.

require 5.004;
use strict;

package Conjury::C::GNU;

BEGIN {
	use Conjury::C;
	use vars qw(@ISA);
	@ISA = qw(Conjury::C);
}

package Conjury::C::GNU::Compiler;
use vars qw(@ISA);
use Carp qw(croak);
use Conjury::Core qw(cast_error %Option);

my %gnu_cc_argmatch;

BEGIN {
	@ISA = qw(Conjury::C::Compiler);

	$gnu_cc_argmatch{new} =
	  join '|', qw(Program Options Language Journal No_Scanner);
}

my $gcc_scanner = sub {
	my ($program, $c_file, $parameters) = @_;

	my $verbose = exists $Option{'verbose'};
	my $command = "$program";
	for (@$parameters) {
		$command .= ' ';
		$command .= /\s/ ? "'$_'" : $_;
	}
	$command .= " -M $c_file";

	print "scanning $c_file ...\n";
	print "$command\n" if $verbose;

	cast_error "Unable to scan file '$c_file'"
	  unless open(PIPE, "$command |");
	my @make_rule = <PIPE>;
	close PIPE || cast_error 'Scan failed.';

	print @make_rule if $verbose;
	
	for (@make_rule) {
		chomp;
		s/\s*\\?$//;
		s/^\S+:\s*//;
	}

	return split('\s+', join(' ', @make_rule));
};

sub new {
	my ($class, %arg) = @_;
	croak 'Conjury::C::GNU::Compiler::new-- argument mismatch'
	  unless (!((@_ - 1) % 2)
			  && !grep $_ !~ /\A($gnu_cc_argmatch{new})\Z/, keys(%arg));

	$class = ref($class) if ref($class);

	my $program = $arg{Program};
	$program = 'gcc' unless defined($program);
	croak 'Conjury::C::GNU::Compiler::new-- argument mismatch {Program}'
	  unless defined($program) && !ref($program);

	my $language = $arg{Language};
	$language = 'c' unless defined($language);
	$language = lc($language);
	croak 'Conjury::C::GNU::Compiler::new-- argument mismatch {Language}'
	  unless defined($language) && !ref($language);
	$program =~ s/gcc/g++/ if $language eq 'c++';

	my $no_scanner = $arg{No_Scanner};
	my $journal = $arg{Journal};
	my $scanner = undef;

	$scanner = sub { &$gcc_scanner($program, @_) }
	  if (defined($journal) && !defined($no_scanner));

	my $suffix_rule = sub {
		my $name = shift;
		$name .= '.o' unless $name =~ s/\.(c|C|cc|cxx|c\+\+|m|i|ii|s|S)\Z/.o/;
		return $name;
	};

	my $self = eval {
	  Conjury::C::Compiler->new
		(Program => $program,
		 Suffix_Rule => $suffix_rule,
		 Options => $arg{Options},
		 Scanner => $scanner,
		 Journal => $journal);
	};
	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }

	$self->{Language} = $language;

	bless $self, $class;
}


package Conjury::C::GNU::Linker;
use vars qw(@ISA);
use Carp qw(croak);

my %gnu_ld_argmatch;

BEGIN {
	@ISA = qw(Conjury::C::Linker);

	$gnu_ld_argmatch{new} =
	  join '|', qw(Program Options Language Journal);
}

sub new {
	my ($class, %arg) = @_;
	croak 'Conjury::C::GNU::Linker::new-- argument mismatch'
	  unless (!((@_ - 1) % 2)
			  && !grep $_ !~ /\A($gnu_ld_argmatch{new})\Z/, keys(%arg));

	$class = ref($class) if ref($class);

	my $program = $arg{Program};
	$program = 'gcc' unless defined($program);
	croak 'Conjury::C::GNU::Linker::new-- argument mismatch {Program}'
	  unless defined($program) && !ref($program);

	my $language = $arg{Language};
	$language = 'c' unless defined($language);
	$language = lc($language);
	croak 'Conjury::C::GNU::Linker::new-- argument mismatch {Language}'
	  unless defined($language) && !ref($language);
	$program =~ s/gcc/g++/ if $language eq 'c++';

	my $self = eval {
	  Conjury::C::Linker->new
		(Program => $program,
		 Options => $arg{Options},
		 Journal => $arg{Journal});
	};
	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }

	$self->{Language} = $language;

	if ($language eq 'objective-c') {
		my $opt_trailer = $self->{Options}->[1];
		push @$opt_trailer, '-lobjc';
	}

	bless $self, $class;
}

package Conjury::C::GNU::Archiver;
use vars qw(@ISA);

@ISA = qw(Conjury::C::Archiver);

1;

__END__

=head1 NAME

Conjury::C::GNU -- Perl Conjury with the GNU C/C++ tools

=head1 SYNOPSIS

  c_compiler Vendor => 'GNU',
    Language => I<language>,
    No_Scanner => 1,
    Program => I<program>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Journal => I<journal>;

  c_linker Vendor => 'GNU',
    Language => I<language>,
    Program => I<program>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Journal => I<journal>;

=head1 DESCRIPTION

The optional 'Program', 'Options' and 'Journal' arguments to the
GNU-specific specializations of the C<c_compiler> and C<c_linker>
functions are simply passed through unmodified to the base class
constructor.

The optional 'Language' argument specifies the langauge for which
the compiler or linker should be invoked.  The C language is the
default if not otherwise specified.  The value is case-insensitive
and may be any one of the following: 'c', 'c++' or 'objective-c'.

The optional 'No_Scanner' argument in the C<c_compiler>
specialization specifies that the processing overhead of scanning
all the source files for their dependency trees is unnecessary.
If you are only building from clean source file hierarchies (with
no existing products from previous runs), then the construction
time of large builds may be improved with this option.

=head1 SEE ALSO

More documentation can be found in L<Conjury>, L<cast>, L<Conjury::Core>
and L<Conjury::C>.

=head1 AUTHOR

James Woodyatt <jhw@wetware.com>
