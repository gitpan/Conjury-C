# Copyright (c) 1999 James H. Woodyatt.  All rights reserved.
#
# Redistribution and use in source and machine-readable forms, with or without
# modification, are permitted provided that the conditions and terms of its
# accompanying LICENSE are met.

require 5.004;
use strict;

package Conjury::C;
use Conjury::Core qw(%Option);

BEGIN {
	use Exporter ();
	use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
	use vars qw($Default_Compiler $Default_Linker $Default_Archiver);
	use subs qw(c_compiler c_linker c_archiver c_object c_executable
				c_static_library);

	$VERSION = 1.002;
	@ISA = qw(Exporter);
	@EXPORT = qw(&c_compiler &c_linker &c_archiver &c_object
				 &c_executable &c_static_library);
}


package Conjury::C::Compiler;
use Conjury::Core qw(%Option $Current_Context &cast_error &cast_warning
					 &fetch_spells);
use Carp qw(croak);
use Config;

my %cc_argmatch;

BEGIN {
	use vars qw($AUTOLOAD %Default);
	
	$Default{Flag_Map} = { map { $_ => "-$_" } qw(I D c o) };

	$Default{Suffix_Rule} = sub {
		my $name = shift;
		$name =~ s/\.c\Z/$Config{_o}/i
		  || cast_error "C source file '$name' doesn't have a .c extension.";
		return $name;
	};

	$cc_argmatch{new} =
	  join '|', qw(Program Options Flag_Map Suffix_Rule Journal Scanner);

	$cc_argmatch{object_spell} = 
	  join '|', qw(Directory Options Source Factors Includes Defines);
}

sub new {
	my ($class, %arg) = @_;
	croak 'Conjury::C::Compiler::new-- argument mismatch'
	  unless ((@_ - 1) % 2 == 0
			  && !grep $_ !~ /\A($cc_argmatch{new})\Z/, keys(%arg));

	$class = ref($class) if ref($class);

	my $program = $arg{Program};
	$program = $Config{cc} unless defined($program);
	croak 'Conjury::C::Compiler::new-- argument mismatch {Program}'
	  unless !ref($program);

	my $options = $arg{Options};
	croak 'Conjury::C::Compiler::new-- argument mismatch {Options}'
	  unless (!defined($options) || ref($options) eq 'ARRAY');
	$options = defined($options) ? [ @$options ] : [ ];

	my $flag_map = $arg{Flag_Map};
	croak 'Conjury::C::Compiler::new-- argument mismatch {Flag_Map}'
	  unless(!defined($flag_map) || ref($flag_map) eq 'HASH');
	my %my_flag_map = %{$Default{Flag_Map}};
	if (defined($flag_map)) {
		my ($key, $value);
		while (($key, $value) = each %$flag_map) {
			$my_flag_map{$key} = $value;
		}
	}
	$flag_map = \%my_flag_map;

	my $suffix_rule = $arg{Suffix_Rule};
	$suffix_rule = $Default{Suffix_Rule} unless defined($suffix_rule);
	croak 'Conjury::C::Compiler::new-- argument mismatch {Suffix_Rule}'
	  unless ref($suffix_rule) eq 'CODE';

	my $journal = $arg{Journal};
	croak 'Conjury::C::Compiler::new-- argument mismatch {Journal}'
	  unless (!defined($journal) || ref($journal));

	my $scanner = $arg{Scanner};
	croak 'Conjury::C::Compiler::new-- argument mismatch {Scanner}'
	  unless (!defined($scanner) || ref($scanner) eq 'CODE');
	
	my $self = { };
	bless $self, $class;

	$self->{Program} = $program;
	$self->{Options} = $options;
	$self->{Flag_Map} = $flag_map;
	$self->{Suffix_Rule} = $suffix_rule;
	$self->{Journal} = $journal;
	$self->{Scanner} = $scanner;

	return $self;
}

sub DESTROY { }

sub AUTOLOAD {
	my $field = $AUTOLOAD;
	$field =~ s/.*:://;
	my ($self, $set) = @_;

	croak "Conjury::C::Compiler::$field-- argument mismatch"
	  unless ((@_ == 1 || @_ == 2) && ref($self));
	croak "Conjury::C::Compiler::$field-- no field exists"
	  unless exists($self->{$field});
	
	$self->{$field} = $set if defined($set);
	return $self->{$field};
}

my $cc_new_profile = sub {
	my ($prefix, $factors, $c_file, $scanner, $parameters) = @_;
	
	my $profile;
	my $profile_str = "Conjury::C::Compiler $prefix";
	my @c_file_factors = fetch_spells Name => $c_file;
	if (@c_file_factors) {
		push @$factors, @c_file_factors;
		$profile = $profile_str;
	}
	else {
		$profile = sub {
			my $result = $profile_str;
			
			my @c_file_stat = stat $c_file;
			if (@c_file_stat) {
				$result .= " $c_file_stat[9]";
			}
			else {
				cast_error
				  "No spells for '$c_file' ...is it a missing source file?";
			}
			
			my @dependencies;
			if (defined($scanner)) {
				print "Scanning: $c_file\n" if exists($Option{'verbose'});
				@dependencies = &$scanner($c_file, $parameters);
			}

			for my $file (@dependencies) {
				next if $file eq $c_file;
				@c_file_stat = stat $file;
				if (@c_file_stat) {
					$result .= " $c_file_stat[9]";
				}
				else {
					cast_warning
					  "File '$c_file' depends on missing file '$file'.";
				}
			}

			return $result;
		};
	}

	return $profile;
};

sub object_spell {
	my $self = shift;
	my %arg = @_;
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch'
	  unless (ref($self) && !(@_ % 2)
			  && !grep $_ !~ /\A($cc_argmatch{object_spell})\Z/,
			  keys(%arg));

	my $directory = $arg{Directory};
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch {Directory}'
	  unless (!defined($directory) || !ref($directory));

	my $c_file = $arg{Source};
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch {Source}'
	  unless (defined($c_file) && !ref($c_file));

	my $factors_ref = $arg{Factors};
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch {Factors}'
	  unless (!defined($factors_ref) || ref($factors_ref) eq 'ARRAY');
	my @factors = defined($factors_ref) ? @$factors_ref : ();

	my $flag_map = $self->{Flag_Map};

	my $defines = $arg{Defines};
	$defines = { } unless defined($defines);
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch {Defines}'
	  unless ref($defines) eq 'HASH';
	my $flag_D = $flag_map->{D};
	my @define_list = map {
		my ($name, $value) = ($_, $defines->{$_});
		croak 'Conjury::C::Compiler::object_spell-- definition value mismatch'
		  unless (defined($value) && !ref($value));
		$value eq '1' ? "$flag_D$name" : "$flag_D$name=$value";
	} sort(keys(%$defines));
	$defines = \@define_list;

	my $includes = $arg{Includes};
	$includes = [ ] unless defined($includes);
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch {Includes}'
	  unless ref($includes) eq 'ARRAY';
	my $flag_I = $flag_map->{I};
	my @include_list = map "$flag_I$_", @$includes;
	$includes = \@include_list;

	my $options = $arg{Options};
	croak 'Conjury::C::Compiler::object_spell-- argument mismatch {Options}'
	  unless (!defined($options) || ref($options) eq 'ARRAY');
	$options = defined($options) ? [ @$options ] : [ ];

	my $cc_options = $self->{Options};
	unshift @$options, @$cc_options;

	my $program = $self->{Program};
	my $journal = $self->{Journal};
	my $scanner = $self->{Scanner};

	my $suffix_rule = $self->{Suffix_Rule};
	my $o_file = &$suffix_rule($c_file);

	$o_file = File::Spec->catfile($directory, $o_file)
	  if defined($directory);
	$o_file = File::Spec->canonpath($o_file);

	my $flag_c = $flag_map->{c};
	my $flag_o = $flag_map->{o};

	my @parameters = (@$options, @$defines, @$includes);
	my @command = ($program, @parameters, $flag_o, $o_file, $flag_c, $c_file);

	my $command_str = join ' ', @command;

	my $profile = &$cc_new_profile($command_str, \@factors, $c_file, $scanner,
								   \@parameters);

	return Conjury::Core::Spell->new
	  (Product => $o_file,
	   Factors => \@factors,
	   Profile => $profile,
	   Action => \@command,
	   Journal => $journal);
}


package Conjury::C::Linker;
use Conjury::Core qw(%Option $Current_Context &cast_error &fetch_spells);
use Carp qw(croak);
use Config;

 my %ld_argmatch;

BEGIN {
	use vars qw($AUTOLOAD %Default);

	my $verbose = exists $Option{'verbose'};

	$Default{Flag_Map} = { map { $_ => "-$_" } qw(o L l) };

	$Default{Bind_Rule} = sub {
		my ($library, $binding) = @_;
		cast_error "No support for '$binding' binding."
		  unless ($binding eq '' || $binding =~ /\A(static|dynamic)\Z/);

		my @result = ("lib${library}$Config{_a}");
		unshift @result, "lib${library}.$Config{so}"
		  unless $binding eq 'static';
		return @result;
	};

	$Default{Search_Rule} = sub {
		my ($library, $bind_rule, $binding, @search) = @_;

		my @files = &$bind_rule($library, $binding);
		my @factors = ( );
		
	  outer:
		for my $dir (@search) {
			for my $file (@files) {
				@factors =
				  fetch_spells(Name => File::Spec->catfile($dir, $file));
				last outer if @factors;
			}
		}

		if ($verbose) {
			print "Conjury::C::Default{Search_Rule}--\n";
			print "  library='$library'\n";
			print "  binding='$binding'\n";
			print "  search=[", join(',', (map "'$_'", @search)), "]\n";
			print "  factors=[",
			  join(',', (map "'$_->Product'", @factors)), "]\n";
		}

		return @factors;
	};

	$ld_argmatch{new} =
	  join '|', qw(Program Options Flag_Map Bind_Rule Search_Rule Journal);

	$ld_argmatch{executable_spell} =
	  join '|', qw(Name Options Order Factors);

	$Default{Order_Key_Match_} =
	  join '|', qw(Objects Libraries Searches);
}

sub new {
	my ($class, %arg) = @_;
	croak 'Conjury::C::Linker::new-- argument mismatch'
	  unless ((@_ - 1) % 2 == 0
			  && !grep $_ !~ /\A($ld_argmatch{new})\Z/, keys(%arg));

	$class = ref($class) if ref($class);

	my $program = $arg{Program};
	$program = $Config{ld} unless defined($program);
	croak 'Conjury::C::Linker::new-- argument mismatch {Program}'
	  unless !ref($program);

	my $options = $arg{Options};
	croak 'Conjury::C::Linker::new-- argument mismatch {Options}'
	  unless (!defined($options) || ref($options) eq 'ARRAY');
	if (defined($options)) {
		$options = [ [ @$options ], [ ] ] if (!ref($options->[0]));
		croak 'Conjury::C::Linker::new-- argument mismatch {Options} [length]'
		  unless (@$options == 2);
	}
	else {
		$options = [ [ ], [ ] ];
	}

	my $flag_map = $arg{Flag_Map};
	croak 'Conjury::C::Linker::new-- argument mismatch {Includes}'
	  unless(!defined($flag_map) || ref($flag_map) eq 'HASH');
	my %my_flag_map = %{$Default{Flag_Map}};
	if (defined($flag_map)) {
		my ($key, $value);
		while (($key, $value) = each %$flag_map) {
			$my_flag_map{$key} = $value;
		}
	}
	$flag_map = \%my_flag_map;
	my $flag_L = $flag_map->{L};

	my $bind_rule = $arg{Bind_Rule};
	$bind_rule = $Default{Bind_Rule} unless defined($bind_rule);
	croak 'Conjury::C::Linker::new-- argument mismatch {Bind_Rule}'
	  unless ref($bind_rule) eq 'CODE';

	my $search_rule = $arg{Search_Rule};
	$search_rule = $Default{Search_Rule} unless defined($search_rule);
	croak 'Conjury::C::Linker::new-- argument mismatch {Search_Rule}'
	  unless ref($search_rule) eq 'CODE';

	my $journal = $arg{Journal};
	croak 'Conjury::C::Linker::new-- argument mismatch {Journal}'
	  unless (!defined($journal) || ref($journal));

	my $self = { };
	bless $self, $class;

	$self->{Program} = $program;
	$self->{Options} = $options;
	$self->{Flag_Map} = $flag_map;
	$self->{Bind_Rule} = $bind_rule;
	$self->{Search_Rule} = $search_rule;
	$self->{Journal} = $journal;
	$self->{Order_Key_Match_} = $Default{Order_Key_Match_};

	return $self;
}

sub DESTROY { }

sub AUTOLOAD {
	my $field = $AUTOLOAD;
	$field =~ s/.*:://;
	my ($self, $set) = @_;

	croak "Conjury::C::Linker::$field-- argument mismatch"
	  unless ((@_ == 1 || @_ == 2) && ref($self));
	croak "Conjury::C::Linker::$field-- no field exists"
	  unless exists($self->{$field});
	
	$self->{$field} = $set if defined($set);
	return $self->{$field};
}

sub process_ {
	my ($self, $traveller, $key, $value) = @_;

	my $order_key_match = $self->{Order_Key_Match_};

	croak 'Conjury::C::Linker::process_-- unrecognized order key'
	  unless (!ref($key)
			  && $key =~ /\A($order_key_match)\Z/);

	$key = "process_${key}_";
	eval { $self->$key($traveller, $value) };
	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	undef;
}

sub process_Objects_ {
	my ($self, $traveller, $value) = @_;

	croak 'Conjury::C::Linker::process_Objects_-- argument mismatch'
	  unless (ref($value) eq 'ARRAY' && !grep ref, @$value);
		
	for (@$value) {
		push @{$traveller->{parameters}}, $_;
		push @{$traveller->{factors}}, $_;
	}

	undef;
}

sub process_Libraries_ {
	my ($self, $traveller, $value) = @_;

	croak 'Conjury::C::Linker::process_Libraries_-- argument mismatch'
	  unless (ref($value) eq 'ARRAY' && !grep ref, @$value);
			
	my $bind_rule = $self->{Bind_Rule};
	my $search_rule = $self->{Search_Rule};
	my $flag_map = $self->{Flag_Map};
	my $flag_l = $flag_map->{l};
	my $binding = $traveller->{binding};
	$binding = '' unless defined($binding);
	my $searches = $traveller->{searches};
	$searches = [ ] unless defined($searches);

	for (@$value) {
		my @spells = &$search_rule($_, $bind_rule, $binding, @$searches);
		push @{$traveller->{parameters}}, "$flag_l$_";
		push @{$traveller->{factors}}, @spells;
	}

	undef;
}

sub process_Searches_ {
	my ($self, $traveller, $value) = @_;

	croak 'Conjury::C::Linker::process_Searches_-- argument mismatch'
	  unless (ref($value) eq 'ARRAY' && !grep ref, @$value);

	my $flag_map = $self->{Flag_Map};
	my $flag_L = $flag_map->{L};

	$traveller->{searches} = [ ] unless exists($traveller->{searches});

	for (@$value) {
		push @{$traveller->{searches}}, $_;
		push @{$traveller->{parameters}}, "$flag_L$_";
	}

	undef;
}

sub executable_spell {
	my $self = shift;
	my %arg = @_;
	croak 'Conjury::C::Linker::executable_spell-- argument mismatch'
	  unless (ref($self) && !(@_ % 2)
			  && !grep $_ !~ /\A($ld_argmatch{executable_spell})\Z/,
			  keys(%arg));

	my $directory = $arg{Directory};
	croak 'Conjury::C::Linker::executable_spell'
	  . ' -- argument mismatch {Directory}'
	  unless (!defined($directory) || !ref($directory));

	my $name = $arg{Name};
	croak 'Conjury::C::Linker::executable_spell-- argument mismatch {Name}'
	  unless (defined($name) && !ref($name));
	my $product = "$name$Config{_exe}";

	my $options = $arg{Options};
	croak 'Conjury::C::Linker::new-- argument mismatch {Options}'
	  unless (!defined($options) || ref($options) eq 'ARRAY');
	if (defined($options)) {
		$options = [ [ @$options ], [ ] ] if (!ref($options->[0]));
		croak 'Conjury::C::Linker::new-- argument mismatch {Options} [length]'
		  unless (@$options == 2);
	}
	else {
		$options = [ [ ], [ ] ];
	}
	
	my $ld_options = $self->{Options};
	unshift @{$options->[0]}, @{$ld_options->[0]};
	unshift @{$options->[1]}, @{$ld_options->[1]};

	my $order_ref = $arg{Order};
	croak 'Conjury::C::Linker::executable_spell-- argument mismatch[1] {Order}'
	  unless (ref($order_ref) eq 'ARRAY' && @$order_ref > 1
			  && !(@$order_ref % 2));
	my @order = @$order_ref;

	my $factors_ref = $arg{Factors};
	croak 'Conjury::C::Linker::executable_spell-- argument mismatch {Factors}'
		unless (!defined($factors_ref) || ref($factors_ref) eq 'ARRAY');
	my @factors = defined($factors_ref) ? @$factors_ref : ();

	my @parameters = ( );
	my %traveller;
	$traveller{parameters} = \@parameters;
	$traveller{factors} = \@factors;

	while (@order) {
		eval { $self->process_(\%traveller, splice(@order, 0, 2)) };
		if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	}

	my $program = $self->{Program};
	my $journal = $self->{Journal};

	$product = File::Spec->catfile($directory, $product)
	  if defined($directory);
	$product = File::Spec->canonpath($product);

	my $flag_map = $self->{Flag_Map};
	my $flag_o = $flag_map->{o};

	my @command = ($program, @{$options->[0]}, $flag_o, $product, @parameters,
				   @{$options->[1]});
	my $profile = 'Conjury::C::Linker ' . join(' ', @command);

	return Conjury::Core::Spell->new
	  (Product => $product,
	   Factors => \@factors,
	   Profile => $profile,
	   Action => \@command,
	   Journal => $journal);
}


package Conjury::C::Archiver;
use Conjury::Core qw(%Option &cast_error);
use Carp qw(croak);
use Config;

my %ar_argmatch;

BEGIN {
	use vars qw($AUTOLOAD %Default);

	$Default{Flag_Map} = { map { $_ => "-$_" } qw(r) };

	$ar_argmatch{new} =
	  join '|', qw(Program Options Flag_Map Journal);

	$ar_argmatch{library_spell} =
	  join '|', qw(Directory Name Options Objects Factors);
}

sub new {
	my ($class, %arg) = @_;
	croak 'Conjury::C::Archiver::new-- argument mismatch'
	  unless ((@_ - 1) % 2 == 0
			  && !grep $_ !~ /\A($ar_argmatch{new})\Z/, keys(%arg));

	$class = ref($class) if ref($class);

	my $program = $arg{Program};
	$program = $Config{ar} unless defined($program);
	croak 'Conjury::C::Archiver::new-- argument mismatch {Program}'
	  unless !ref($program);

	my $options = $arg{Options};
	croak 'Conjury::C::Archiver::new-- argument mismatch {Options}'
	  unless (!defined($options) || ref($options) eq 'ARRAY');
	$options = defined($options) ? [ @$options ] : [ ];

	my $flag_map = $arg{Flag_Map};
	croak 'Conjury::C::Archiver::new-- argument mismatch {Flag_Map}'
	  unless(!defined($flag_map) || ref($flag_map) eq 'HASH');
	my %my_flag_map = %{$Default{Flag_Map}};
	if (defined($flag_map)) {
		my ($key, $value);
		while (($key, $value) = each %$flag_map) {
			$my_flag_map{$key} = $value;
		}
	}
	$flag_map = \%my_flag_map;

	my $journal = $arg{Journal};
	croak 'Conjury::C::Archiver::new-- argument mismatch {Journal}'
	  unless (!defined($journal) || ref($journal));

	my $self = { };
	bless $self, $class;

	$self->{Program} = $program;
	$self->{Options} = $options;
	$self->{Flag_Map} = $flag_map;
	$self->{Journal} = $journal;

	return $self;
}

sub DESTROY { }

sub AUTOLOAD {
	my $field = $AUTOLOAD;
	$field =~ s/.*:://;
	my ($self, $set) = @_;

	croak "Conjury::C::Archiver::$field-- argument mismatch"
	  unless ((@_ == 1 || @_ == 2) && ref($self));
	croak "Conjury::C::Archiver::$field-- no field exists"
	  unless exists($self->{$field});
	
	$self->{$field} = $set if defined($set);
	return $self->{$field};
}

sub library_spell {
	my $self = shift;
	my %arg = @_;
	croak 'Conjury::C::Archiver::library_spell-- argument mismatch'
	  unless (ref($self) && !(@_ % 2)
			  && !grep $_ !~ /\A($ar_argmatch{library_spell})\Z/,
			  keys(%arg));

	my $directory = $arg{Directory};
	croak 'Conjury::C::Archiver::library_spell-- argument mismatch {Directory}'
	  unless (!defined($directory) || !ref($directory));

	my $name = $arg{Name};
	croak 'Conjury::C::Archiver::library_spell-- argument mismatch {Product}'
	  unless (defined($name) && !ref($name));

	my $options = $arg{Options};
	croak 'Conjury::C::Archiver::object_spell-- argument mismatch {Options}'
	  unless (!defined($options) || ref($options) eq 'ARRAY');
	$options = defined($options) ? [ @$options ] : [ ];

	my $ar_options = $self->{Options};
	unshift @$options, @$ar_options;

	my $objects = $arg{Objects};
	croak 'Conjury::C::Archiver::library_spell-- argument mismatch {Objects}'
	  unless (ref($objects) eq 'ARRAY' && !grep ref, @$objects);

	my $factors_ref = $arg{Factors};
	croak 'Conjury::C::Archiver::library_spell-- argument mismatch {Factors}'
	  unless (!defined($factors_ref) || ref($factors_ref) eq 'ARRAY');
	my @factors = defined($factors_ref) ? @$factors_ref : ();

	my $program = $self->{Program};
	my $journal = $self->{Journal};

	my $product = "lib${name}$Config{_a}";
	$product = File::Spec->catfile($directory, $product)
	  if defined($directory);
	$product = File::Spec->canonpath($product);

	my $flag_map = $self->{Flag_Map};
	my $flag_r = $flag_map->{r};

	my @command = ($program, $flag_r, @$options, $product, @$objects);
	my $command_str = join ' ', @command;

	my $action = sub {
		print "$command_str\n";

		my $result;
		if (!exists($Option{'preview'})) {
			unlink $product;
			$result = system @command;
		}

		return $result;
	};

	my $profile = "Conjury::C::Archiver $command_str";

	push @factors, @$objects;

	return Conjury::Core::Spell->new
	  (Product => $product,
	   Factors => \@factors,
	   Profile => $profile,
	   Action => $action,
	   Journal => $journal);
}


package Conjury::C;
use Carp qw(croak);

sub c_compiler {
	my %arg = @_;

	my $vendor = $arg{Vendor};
	delete $arg{Vendor};
	
	my $cc;
	if (defined($vendor)) {
		my $package = "Conjury::C::$vendor";
		eval "require $package";
		die $@ if $@;

		my $class = "${package}::Compiler";
		$cc = eval { $class->new(%arg) };
	}
	else {
		$cc = eval { Conjury::C::Compiler->new(%arg) };
	}

	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	return $cc;
}

sub c_linker {
	my %arg = @_;

	my $vendor = $arg{Vendor};
	delete $arg{Vendor};
	
	my $cc;
	if (defined($vendor)) {
		my $package = "Conjury::C::$vendor";
		eval "require $package";
		die $@ if $@;

		my $class = "${package}::Linker";
		$cc = eval { $class->new(%arg) };
	}
	else {
		$cc = eval { Conjury::C::Linker->new(%arg) };
	}

	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	return $cc;
}

sub c_archiver {
	my %arg = @_;

	my $vendor = $arg{Vendor};
	delete $arg{Vendor};
	
	my $cc;
	if (defined($vendor)) {
		my $package = "Conjury::C::$vendor";
		eval "require $package";
		die $@ if $@;

		my $class = "${package}::Archiver";
		$cc = eval { $class->new(%arg) };
	}
	else {
		$cc = eval { Conjury::C::Archiver->new(%arg) };
	}

	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	return $cc;
}

sub c_object {
	my %arg = @_;

	my $compiler = $arg{Compiler};
	delete $arg{Compiler};

	if (!defined($compiler)) {
		$Default_Compiler = Conjury::C::Compiler->new
		  unless defined($Default_Compiler);
		$compiler = $Default_Compiler;
	}

	croak 'Conjury::C::c_object-- argument mismatch {Compiler}'
	  unless ref($compiler);

	my $spell = eval { $compiler->object_spell(%arg) };
	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	return wantarray ? @{$spell->Product} : $spell;
}

sub c_executable {
	my %arg = @_;

	my $linker = $arg{Linker};
	delete $arg{Linker};

	if (!defined($linker)) {
		$Default_Linker = Conjury::C::Linker->new
		  unless defined($Default_Linker);
		$linker = $Default_Linker;
	}

	croak 'Conjury::C::c_executable-- argument mismatch {Linker}'
	  unless ref($linker);

	my $spell = eval { $linker->executable_spell(%arg) };
	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	return wantarray ? @{$spell->Product} : $spell;
}

sub c_static_library {
	my %arg = @_;

	my $archiver = $arg{Archiver};
	delete $arg{Archiver};

	if (!defined($archiver)) {
		$Default_Archiver = Conjury::C::Archiver->new
		  unless defined($Default_Archiver);
		$archiver = $Default_Archiver;
	}

	croak 'Conjury::C::c_static_library-- argument mismatch {Archiver}'
	  unless ref($archiver);

	my $spell = eval { $archiver->library_spell(%arg) };
	if ($@) { $@ =~ s/ at \S+ line \d+\n//; croak $@; }
	return wantarray ? @{$spell->Product} : $spell;
}

1;

__END__

=head1 NAME

Conjury::C - Perl Conjury with C/C++ compilers, linkers and archivers

=head1 SYNOPSIS

  c_object
    Source => I<source-file>,
    Directory => I<directory>,
    Includes => [ I<dir1>, I<dir2>, ... ],
    Defines => { I<var1> => I<val1>, I<var2> => I<val2>, ... },
    Compiler => I<compiler>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Factors => [ I<factor1>, I<factor2>, ... ];

  c_executable
    Directory => I<directory>,
    Name => I<output-filename>,
    Order => [
	Searches => [ I<dir1>, I<dir2>, ... ],
	Objects => [ I<obj1>, I<obj2>, ... ],
	Libraries => [ I<lib1>, I<lib2>, ...] ],
    Linker => I<linker>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Factors => [ I<factor1>, I<factors2>, ... ]

  c_static_library
    Directory => I<directory>,
    Name => I<output-filename>,
    Objects => [ I<obj1>, I<obj2>, ...],
    Archiver => I<archiver>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Factors => [ I<factor1>, I<factors2>, ... ]

  c_compiler
    Program => I<program>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Journal => I<journal>,
    Flag_Map => { 'I' => I<include-flag>, 'D' => I<define-flag>,
	          'c' => I<source-flag>, 'o' => I<object-flag> },
    Suffix_Rule => sub { my ($name) = @_; ... return $result },
    Scanner => sub { my ($c_file, $args) = @_; ... return @result };

  c_compiler Vendor => I<vendor>, ...

  c_linker
    Program => I<program>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Journal => I<journal>,
    Flag_Map => { 'l' => I<link-flag>, 'L' => I<search-flag>,
	          'o' => I<output-flag> },
    Bind_Rule => sub { my ($lib, $bind) = @_;
	               ... return @result },
    Search_Rule => sub { my ($lib, $rule, $bind, @search) = @_;
	                 ... return @factors };

  c_linker Vendor => I<vendor>, ...

  c_archiver
    Program => I<program>,
    Options => [ I<opt1>, I<opt2>, ... ],
    Journal => I<journal>,
    Flag_Map => { 'r' => I<replace-flag> };

  c_archiver Vendor => I<vendor>, ...

=head1 DESCRIPTION

Spells for compiling and linking C/C++ software are constructed using
C<c_object>, C<c_executable> and C<c_static_library>.  The compiler,
linker and archiver used in the resulting actions are the same compiler,
linker and archiver that were used to build Perl itself, unless otherwise
specified.

Specializations of the general classes of compiler, linker and archiver
can be created using C<c_compiler>, C<c_linker> and C<c_archiver>.

=head2 Compiling Objects From C/C++ Source

Use the C<c_object> function to create a spell for compiling an object
file from a C or C++ source file.  The name of the resulting object file
will be derived by removing the .c suffix and replacing it with the
suffix for object files on the current platform.

Use the C<c_compiler> function to create an object to represent a
specialization of the C/C++ compiler tool class.

=over 4

=item c_object

This function returns a spell for the action to compile a source file
into an object file.

The 'Source' argument is required and specifies the name of the source
file to compile into an object file.

The optional 'Directory' argument specifies the directory in which the
object file should be produced.

The optional 'Includes' argument specifies a list of directories that
should be used by the compiler to search for header files.

Preprocessor variables may be specified with the optional 'Defines'
argument by placing the names of variables to set into the keys of a
hash, and defining the values as needed.

The optional 'Compiler' argument specifies that a particular
specialization of the C/C++ compiler tool be used (instead of the
default compiler).

The optional 'Options' argument specifies a list of options with which
to invoke the compiler for this particular object file.

The optional 'Factors' argument specifies additional factors to
associate with the spell created.

=item c_compiler

This function returns an object representing a specialization of the
C/C++ compiler class.  If the 'Vendor' argument is specified, then
the vendor-specific subclass of the C<Conjury::C::Compiler>
class parse the argument list.  Otherwise, the specialization is
applied to the base class used for the default compiler.

The optional 'Program' argument specifies the name of the program to
invoke the compiler.

The optional 'Options' argument specifies the list of options with
which the compiler should be invoked for all object files.

The optional 'Journal' argument specifies the journal object that
should be used in creating all spells.

The optional 'Flag_Map' argument specifies a hash that sets the flag
character for various standard options to C/C++ compilers for setting
the include path, the preprocessor definitions, the source file and
the output file.

The optional 'Suffix_Rule' argument specifies a subroutine that
converts the name of a source file into the corresponding object file.

The optional 'Scanner' argument specifies a subroutine that finds the
list of files that a source file references with '#include' directives.

=back

=head2 Linking Executables From Objects

Use the C<c_executable> function to create a spell for linking an
executable program from a list of object files.

Use the C<c_linker> function to create an object to represent a
specialization of the C/C++ linker tool class.

=over 4

=item c_executable

This function returns a spell for the action to link a list of object
files and libraries into an executable file.

The optional 'Directory' argument specifies the directory where the
executable program is to be produced.  If unspecified, the directory
of the current context is used.

The 'Name' argument is required and specifies the name of the executable
program.  Any filename extensions the system requires for executable
program files will be appended automatically.

The 'Order' argument is a required list of verb-object pairs
corresponding to orders for the linker to use in producing the
executable.  The 'Searches' verb specifies a list of directories to add
to the library search path; the 'Objects' verb specifies a list of
object files to link (either the spells that produce them, or the names
of spells that will produce them); the 'Libraries' verb specifies the
list of libraries to link.  Order verbs may be specified in whatever
sequence makes sense to the linker.

The optional 'Linker' argument specifies that a particular
specialization of the C/C++ linker tool be used (instead of the
default linker).

The optional 'Options' argument specifies a list of options with which
to invoke the linker for this particular object file.

The optional 'Factors' argument specifies additional factors to
associate with the spell created.

=item c_linker

This function returns an object representing a specialization of the
C/C++ linker class.  If the 'Vendor' argument is specified, then
the vendor-specific subclass of the C<Conjury::C::Linker>
class parse the argument list.  Otherwise, the specialization is
applied to the base class used for the default linker.

The optional 'Program' argument specifies the name of the program to
invoke the linker.

The optional 'Options' argument specifies the list of options with
which the linker should be invoked for all product files.

The optional 'Journal' argument specifies the journal object that
should be used in creating all spells.

The optional 'Flag_Map' argument specifies a hash that sets the flag
character for various standard options to C/C++ linkers for setting
the search path, the output file and the libraries to link.

The optional 'Bind_Rule' argument specifies a subroutine that takes
a library name and a binding specification and returns a list of
library filenames for which the binding specification applies.
Typical binding specifications include 'static' and 'dynamic'-- and
the unnamed specification, which typically implies "either dynamic or
static as required."

The optional 'Search_Rule' argument specifies a subroutine that
takes a library name, a binding rule (see above), a binding
specification (also, see above), and a list of the current
directories in the library search path (at the current point in the
sequence of orders), and returns the list of factors which produce
the library files.

=back

=head2 Creating Static Archives Of Objects

Use the C<c_static_library> function to create a spell for composing a
static library from a list of object files.

Use the C<c_archiver> function to create an object to represent a
specialization of the C/C++ archiver tool class.

=over 4

=item c_static_library

This function returns a spell for the action to archive a list of object
files in a static library file.

The optional 'Directory' argument specifies the directory where the
static library file is to be produced.  If unspecified, the directory
of the current context is used.

The 'Name' argument is required and specifies the name of the library
file.  Any filename extensions and prefixes the system requires for
static library files will be added automatically.

The 'Objects' argument is required and specifies the list of objects
(and/or spells that produce the objects) to be archived into the static
library file.

The optional 'Archiver' argument specifies that a particular
specialization of the C/C++ archiver tool be used (instead of the
default archiver).

The optional 'Options' argument specifies a list of options with which
to invoke the archiver for this particular object file.

The optional 'Factors' argument specifies additional factors to
associate with the spell created.

=item c_archiver

This function returns an object representing a specialization of the
C/C++ archiver class.  If the 'Vendor' argument is specified, then
the vendor-specific subclass of the C<Conjury::C::Archiver>
class parse the argument list.  Otherwise, the specialization is
applied to the base class used for the default archiver.

The optional 'Program' argument specifies the name of the program to
invoke the archiver.

The optional 'Options' argument specifies the list of options with
which the archiver should be invoked for all static library files.

The optional 'Journal' argument specifies the journal object that
should be used in creating all spells.

The optional 'Flag_Map' argument specifies a hash that sets the flag
character for various standard options to C/C++ archivers, including
the flag for replacing existing objects with new ones.

=back

=head1 SEE ALSO

The introduction to Perl Conjury is in L<Conjury>.  Other useful documents
include L<cast> and L<Conjury::Core>.

The specialization for the GNU tools is documented in L<Conjury::C::GNU>.

=head1 AUTHOR

James Woodyatt <F<jhw@wetware.com>>
