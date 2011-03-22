package ExtUtils::InstallPaths;
use 5.006;
use strict;
use warnings;

use File::Spec ();
use Carp ();

my %attributes = (
	installdirs     => 'site',
	install_base    => undef,
	prefix          => undef,
	verbose         => 0,
	blib            => 'blib',
	create_packlist => 1,
	module_name     => undef,
	destdir         => undef
);

my %explicit_accessors = map { $_ => 1 } qw/installdirs install_path/;

for my $attribute (grep { not exists $explicit_accessors{$_} } keys %attributes) {
	no strict qw/refs/;
	*{$attribute} = sub {
		my $self = shift;
		$self->{$attribute} = shift if @_;
		return $self->{$attribute};
	};
}

sub new {
	my ($class, %args) = @_;
	my $c = $args{config};
	my $p = default_install_paths({ config => $c });
	$p->{install_path} ||= {};
	for my $attribute (keys %attributes) {
		$p->{$attribute} = exists $args{$attribute} ? $args{$attribute} : $attributes{$attribute};
	}
	return bless $p, $class;
}

sub config {
	my $self = shift;
	my $c = ref($self) ? $self->{config} : 'ExtUtils::Config';
	return $c->all_config unless @_;

	my $key = shift;
	return $c->get($key) unless @_;

	my $val = shift;
	return $c->set($key => $val);
}

sub default_install_paths {
	my $self = shift;

	my $c = $self->{config};
	my $p = { config => $c };

	my @libstyle = $c->get('installstyle') ?
		File::Spec->splitdir($c->get('installstyle')) : qw(lib perl5);
	my $arch     = $c->get('archname');
	my $version  = $c->get('version');

	my $bindoc  = $c->get('installman1dir') || undef;
	my $libdoc  = $c->get('installman3dir') || undef;

	my $binhtml = $c->get('installhtml1dir') || $c->get('installhtmldir') || undef;
	my $libhtml = $c->get('installhtml3dir') || $c->get('installhtmldir') || undef;

	$p->{install_sets} = {
		core   => {
			lib     => $c->get('installprivlib'),
			arch    => $c->get('installarchlib'),
			bin     => $c->get('installbin'),
			script  => $c->get('installscript'),
			bindoc  => $bindoc,
			libdoc  => $libdoc,
			binhtml => $binhtml,
			libhtml => $libhtml,
		},
		site   => {
			lib     => $c->get('installsitelib'),
			arch    => $c->get('installsitearch'),
			bin     => $c->get('installsitebin') || $c->get('installbin'),
			script  => $c->get('installsitescript') || $c->get('installsitebin') || $c->get('installscript'),
			bindoc  => $c->get('installsiteman1dir') || $bindoc,
			libdoc  => $c->get('installsiteman3dir') || $libdoc,
			binhtml => $c->get('installsitehtml1dir') || $binhtml,
			libhtml => $c->get('installsitehtml3dir') || $libhtml,
		},
		vendor => {
			lib     => $c->get('installvendorlib'),
			arch    => $c->get('installvendorarch'),
			bin     => $c->get('installvendorbin') || $c->get('installbin'),
			script  => $c->get('installvendorscript') || $c->get('installvendorbin') || $c->get('installscript'),
			bindoc  => $c->get('installvendorman1dir') || $bindoc,
			libdoc  => $c->get('installvendorman3dir') || $libdoc,
			binhtml => $c->get('installvendorhtml1dir') || $binhtml,
			libhtml => $c->get('installvendorhtml3dir') || $libhtml,
		},
	};

	$p->{original_prefix} = {
		core   => $c->get('installprefixexp') || $c->get('installprefix') || $c->get('prefixexp') || $c->get('prefix') || '',
		site   => $c->get('siteprefixexp'),
		vendor => $c->get('usevendorprefix') ? $c->get('vendorprefixexp') : '',
	};
	$p->{original_prefix}{site} ||= $p->{original_prefix}{core};

	# Note: you might be tempted to use $Config{installstyle} here
	# instead of hard-coding lib/perl5, but that's been considered and
	# (at least for now) rejected.  `perldoc Config` has some wisdom
	# about it.
	$p->{install_base_relpaths} = {
		lib     => ['lib', 'perl5'],
		arch    => ['lib', 'perl5', $arch],
		bin     => ['bin'],
		script  => ['bin'],
		bindoc  => ['man', 'man1'],
		libdoc  => ['man', 'man3'],
		binhtml => ['html'],
		libhtml => ['html'],
	};

	$p->{prefix_relpaths} = {
		core => {
			lib        => [@libstyle],
			arch       => [@libstyle, $version, $arch],
			bin        => ['bin'],
			script     => ['bin'],
			bindoc     => ['man', 'man1'],
			libdoc     => ['man', 'man3'],
			binhtml    => ['html'],
			libhtml    => ['html'],
		},
		vendor => {
			lib        => [@libstyle],
			arch       => [@libstyle, $version, $arch],
			bin        => ['bin'],
			script     => ['bin'],
			bindoc     => ['man', 'man1'],
			libdoc     => ['man', 'man3'],
			binhtml    => ['html'],
			libhtml    => ['html'],
		},
		site => {
			lib        => [@libstyle, 'site_perl'],
			arch       => [@libstyle, 'site_perl', $version, $arch],
			bin        => ['bin'],
			script     => ['bin'],
			bindoc     => ['man', 'man1'],
			libdoc     => ['man', 'man3'],
			binhtml    => ['html'],
			libhtml    => ['html'],
		},
	};

	return $p;
}

sub installdirs {
	my $self = shift;
	if (@_) {
		if ($_[0] !~ /^(core|site|vendor)$/) {
			Carp::croak('ERROR: ', $_[0] eq 'perl'
			  ? 'Perhaps you meant installdirs to be "core" rather than "perl"?'
			  : 'installdirs must be one of "core", "site", or "vendor"');
		}
		$self->{installdirs} = shift;
	}
	return $self->{installdirs};
}

sub log_verbose {
	my $self = shift;
	print @_ if $self->verbose;
	return;
}

sub _merge_arglist {
	my( $self, $opts1, $opts2 ) = @_;

	$opts1 ||= {};
	$opts2 ||= {};
	my %new_opts = %$opts1;
	while (my ($key, $val) = each %$opts2) {
		if (exists $opts1->{$key}) {
			if (ref($val) eq 'HASH') {
				while (my ($k, $v) = each %$val) {
					$new_opts{$key}{$k} = $v unless exists $opts1->{$key}{$k};
				}
			}
		} else {
			$new_opts{$key} = $val
		}
	}

	return \%new_opts;
}

# Given a file type, will return true if the file type would normally
# be installed when neither install-base nor prefix has been set.
# I.e. it will be true only if the path is set from Config.pm or
# set explicitly by the user via install-path.
sub is_default_installable {
	my $self = shift;
	my $type = shift;
	my $installable = $self->install_destination($type) && ( $self->install_path($type) || $self->install_sets($self->installdirs)->{$type});
	return $installable ? 1 : 0;
}

sub install_path {
	my $self = shift;

	my $map = $self->{install_path};
	return $map unless @_;

	my $type = shift;
	Carp::croak('Type argument missing') unless defined $type ;
	
	if (@_) {
		my $new_value = shift;
		if (!defined $new_value) {
			# delete existing value if $value is literal undef()
			delete $map->{$type};
			return;
		}
		else {
			# set value if $value is a valid relative path
			return $map->{$type} = $new_value;
		}
	}
	# return existing value if no new $value is given
	return unless exists $map->{$type};
	return $map->{$type};
}

sub install_sets {
	# Usage: install_sets('site'), install_sets('site', 'lib'),
	#   or install_sets('site', 'lib' => $value);
	my ($self, $dirs, $key, $value) = @_;
	$dirs = $self->installdirs unless defined $dirs;
	# update property before merging with defaults
	if ( @_ == 4 && defined $dirs && defined $key) {
		# $value can be undef; will mask default
		$self->{install_sets}{$dirs}{$key} = $value;
	}
	my $map = $self->_merge_arglist(
		$self->{install_sets},
		$self->default_install_paths->{install_sets}
	);
	if (defined $dirs and defined $key) {
		return $map->{$dirs}{$key};
	}
	elsif (defined $dirs) {
		return $map->{$dirs};
	}
	else {
		Carp::croak('Can\'t determine installdirs for install_sets()');
	}
}

sub original_prefix {
	# Usage: original_prefix(), original_prefix('lib'),
	#   or original_prefix('lib' => $value);
	my ($self, $key, $value) = @_;
	# update property before merging with defaults
	if ( @_ == 3 && defined $key) {
		# $value can be undef; will mask default
		$self->{original_prefix}{$key} = $value;
	}
	my $map = $self->_merge_arglist(
		$self->{original_prefix},
		$self->default_install_paths->{original_prefix}
	);
	return $map unless defined $key;
	return $map->{$key}
}

sub install_base_relpaths {
	# Usage: install_base_relpaths(), install_base_relpaths('lib'),
	#   or install_base_relpaths('lib' => $value);
	my $self = shift;
	if ( @_ > 1 ) { # change values before merge
		$self->_set_relpaths($self->{install_base_relpaths}, @_);
	}
	my $map = $self->_merge_arglist(
		$self->{install_base_relpaths},
		$self->default_install_paths->{install_base_relpaths}
	);
	return $map unless @_;
	my $relpath = $map->{$_[0]};
	return defined $relpath ? File::Spec->catdir( @$relpath ) : undef;
}

# Defaults to use in case the config install paths cannot be prefixified.
sub prefix_relpaths {
	# Usage: prefix_relpaths('site'), prefix_relpaths('site', 'lib'),
	#   or prefix_relpaths('site', 'lib' => $value);
	my $self = shift;
	my $installdirs = shift || $self->installdirs or Carp::croak('Can\'t determine installdirs for prefix_relpaths()');
	if ( @_ > 1 ) { # change values before merge
		$self->{prefix_relpaths}{$installdirs} ||= {};
		$self->_set_relpaths($self->{prefix_relpaths}{$installdirs}, @_);
	}
	my $map = $self->_merge_arglist(
		$self->{prefix_relpaths}{$installdirs},
		$self->default_install_paths->{prefix_relpaths}{$installdirs}
	);
	return $map unless @_;
	my $relpath = $map->{$_[0]};
	return defined $relpath ? File::Spec->catdir( @$relpath ) : undef;
}

sub _set_relpaths {
	my $self = shift;
	my( $map, $type, $value ) = @_;

	Carp::croak('Type argument missing') unless defined $type;

	# set undef if $value is literal undef()
	if (not defined $value) {
		$map->{$type} = undef;
		return;
	}
	# set value if $value is a valid relative path
	else {
		Carp::croak('Value must be a relative path') if File::Spec::Unix->file_name_is_absolute($value);

		my @value = split( /\//, $value );
		return $map->{$type} = \@value;
	}
}

# Translated from ExtUtils::MM_Any::init_INSTALL_from_PREFIX
sub prefix_relative {
	my ($self, $type) = @_;
	my $installdirs = $self->installdirs;

	my $relpath = $self->install_sets($installdirs)->{$type};

	return $self->_prefixify($relpath, $self->original_prefix($installdirs), $type);
}

# Translated from ExtUtils::MM_Unix::prefixify()
sub _prefixify {
	my($self, $path, $sprefix, $type) = @_;

	my $rprefix = $self->prefix;
	$rprefix .= '/' if $sprefix =~ m{/$};

	$self->log_verbose("  prefixify $path from $sprefix to $rprefix\n") if defined $path && length $path;

	if (not defined $path or length $path == 0 ) {
		$self->log_verbose("  no path to prefixify, falling back to default.\n");
		return $self->_prefixify_default( $type, $rprefix );
	} elsif( !File::Spec->file_name_is_absolute($path) ) {
		$self->log_verbose("    path is relative, not prefixifying.\n");
	} elsif( $path !~ s{^\Q$sprefix\E\b}{}s ) {
		$self->log_verbose("    cannot prefixify, falling back to default.\n");
		return $self->_prefixify_default( $type, $rprefix );
	}

	$self->log_verbose("    now $path in $rprefix\n");

	return $path;
}

sub _prefixify_default {
	my $self = shift;
	my $type = shift;
	my $rprefix = shift;

	my $default = $self->prefix_relpaths($self->installdirs, $type);
	if( !$default ) {
		$self->log_verbose("    no default install location for type '$type', using prefix '$rprefix'.\n");
		return $rprefix;
	} else {
		return $default;
	}
}

sub install_destination {
	my ($self, $type) = @_;

	return $self->install_path($type) if $self->install_path($type);

	if ( $self->install_base ) {
		my $relpath = $self->install_base_relpaths($type);
		return $relpath ? File::Spec->catdir($self->install_base, $relpath) : undef;
	}

	if ( $self->prefix ) {
		my $relpath = $self->prefix_relative($type);
		return $relpath ? File::Spec->catdir($self->prefix, $relpath) : undef;
	}

	return $self->install_sets($self->installdirs)->{$type};
}

sub install_types {
	my $self = shift;

	my %types;
	if ( $self->install_base ) {
		%types = %{$self->install_base_relpaths};
	} elsif ( $self->prefix ) {
		%types = %{$self->prefix_relpaths};
	} else {
		%types = %{$self->install_sets($self->installdirs)};
	}

	%types = (%types, %{$self->install_path});

	my @types = sort keys %types;
	return @types;
}

sub install_map {
	my ($self, $blib) = @_;
	$blib ||= $self->blib;

	my( %map, @skipping );
	foreach my $type ($self->install_types) {
		my $localdir = File::Spec->catdir( $blib, $type );
		next unless -e $localdir;

		# the line "...next if (($type eq 'bindoc'..." was one of many changes introduced for
		# improving HTML generation on ActivePerl, see https://rt.cpan.org/Public/Bug/Display.html?id=53478
		# Most changes were ok, but this particular line caused test failures in t/manifypods.t on windows,
		# therefore it is commented out.

		# ********* next if (($type eq 'bindoc' || $type eq 'libdoc') && not $self->is_unixish);

		if (my $dest = $self->install_destination($type)) {
			$map{$localdir} = $dest;
		} else {
			push @skipping, $type;
		}
	}

	$self->log_warn(
		"WARNING: Can't figure out install path for types: @skipping\n" .
		"Files will not be installed.\n"
	) if @skipping;

	# Write the packlist into the same place as ExtUtils::MakeMaker.
	if ($self->create_packlist and my $module_name = $self->module_name) {
		my $archdir = $self->install_destination('arch');
		my @ext = split /::/, $module_name;
		$map{write} = File::Spec->catfile($archdir, 'auto', @ext, '.packlist');
	}

	# Handle destdir
	if (length(my $destdir = $self->destdir || '')) {
		foreach (keys %map) {
			# Need to remove volume from $map{$_} using splitpath, or else
			# we'll create something crazy like C:\Foo\Bar\E:\Baz\Quux
			# VMS will always have the file separate than the path.
			my ($volume, $path, $file) = File::Spec->splitpath( $map{$_}, 0 );

			# catdir needs a list of directories, or it will create something
			# crazy like volume:[Foo.Bar.volume.Baz.Quux]
			my @dirs = File::Spec->splitdir($path);

			# First merge the directories
			$path = File::Spec->catdir($destdir, @dirs);

			# Then put the file back on if there is one.
			if ($file ne '') {
			    $map{$_} = File::Spec->catfile($path, $file)
			} else {
			    $map{$_} = $path;
			}
		}
	}

	$map{read} = '';  # To keep ExtUtils::Install quiet

	return \%map;
}

# ABSTRACT: foo

1;
