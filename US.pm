package Geo::Coder::US;

=head1 NAME

Geo::Coder::US - Geocode (estimate latitude and longitude for) any US address

=head1 SYNOPSIS

  use Geo::Coder::US;

  Geo::Coder::US->set_db( "geocoder.db" );

  my @matches = Geo::Coder::US->geocode(
		    "1600 Pennsylvania Ave., Washington, DC" );

  my @matches = Geo::Coder::US->geocode(
		    "42nd & Broadway New York NY" )

  my ($ora) = Geo::Coder::US->geocode(
		    "1005 Gravenstein Hwy N, 95472" );

  print "O'Reilly is located at $ora->{lat} degrees north, "
                               "$ora->{long} degrees east.\n";


=head1 DESCRIPTION

Geo::Coder::US provides a complete facility for geocoding US addresses,
that is, estimating the latitude and longitude of any street address
or intersection in the United States. Geo::Coder::US relies on US
Census Bureau TIGER/Line data. Using Geo::TigerLine to parse this
data, and DB_File to store a highly compressed distillation of it, the
Geo::Coder::US suite also implements a reasonably forgiving US address
parser, which it uses to break addresses into normalized components
suitable for looking up in its database.

You can find a live demo of this code at L<http://geocoder.us/>. The
demo.cgi script is included in eg/ directory distributed with this module,
along with a whole bunch of other goodies. See L<Geo::Coder::US::Import>
for how to build your own Geo::Coder::US database.

Consider using a web service to access this geocoder over the Internet,
rather than going to all the trouble of building a database yourself.
See eg/soap-client.pl, eg/xmlrpc-client.pl, and eg/rest-client.pl for
different examples of working clients for the rpc.geocoder.us geocoder
web service.

=head1 METHODS

In general, the only methods you are likely to need to call on
Geo::Coder::US are set_db() and geocode(). The following documentation
is included for completeness's sake, and for the benefit of developers
interested in using bits of the module's internals.

Note: Calling conventions for address and intersection specifiers are
discussed in the following section on CALLING CONVENTIONS.

=over 4

=cut

use 5.6.1;
use Geo::Coder::US::Codes;
use DB_File;
use strict;
use warnings;

our $VERSION = '0.20';

our %Addr_Match = (
    type    => join("|", keys %Geo::Coder::US::Codes::_Street_Type_List),
    number  => qr/\d+-?\d*/,
    state   => join("|", %Geo::Coder::US::Codes::State_Code),
    direct  => join("|", %Geo::Coder::US::Codes::Directional), 
    dircode => join("|", keys %Geo::Coder::US::Codes::Direction_Code), 
    zip	    => qr/\d{5}(?:-\d{4})?/,
    corner  => qr/(?:\band\b|\bat\b|&|\@)/i,
    unit    => qr/(?:pmb|ste|suite|dept|apt|room)\W+\w+/i,
);

{
    use re 'eval';
    $Addr_Match{street} = qr/
	(?:($Addr_Match{direct})\W+		(?{ $_{prefix} = $^N }))?
	(?:
	  ([^,]+)				(?{ $_{street} = $^N })
	  (?:[^\w,]+($Addr_Match{type})\b	(?{ $_{type}   = $^N }))
	  (?:[^\w,]+($Addr_Match{direct})\b	(?{ $_{suffix} = $^N }))?
	 |
	  ([^,]*\d)				(?{ $_{street} = $^N })
	  ($Addr_Match{direct})\b		(?{ $_{suffix} = $^N })
	 |
	  ([^,]+?)				(?{ $_{street} = $^N })
	  (?:[^\w,]+($Addr_Match{type})\b	(?{ $_{type}   = $^N }))?
	  (?:[^\w,]+($Addr_Match{direct})\b	(?{ $_{suffix} = $^N }))?
	)
	/ix;

    $Addr_Match{place} = qr/
	(?:
	    ([^\d,]+?)\W+			(?{ $_{city}   = $^N })
	    ($Addr_Match{state})\W*		(?{ $_{state}  = $^N })
	)?
	(?:($Addr_Match{zip})			(?{ $_{zip}    = $^N }))?
	/ix;

    $Addr_Match{address} = qr/^\W*
	(  $Addr_Match{number})\W*		(?{ $_{number} = $^N })
	   $Addr_Match{street}\W+
	(?:$Addr_Match{unit}\W+)?
	   $Addr_Match{place}
	\W*$/ix;

    $Addr_Match{intersection} = qr/^\W*
	   $Addr_Match{street}\W*?	
	    (?{ @_{qw{prefix1 street1 type1 suffix1}}
		= delete @_{qw{prefix street type suffix }} })

	\s+$Addr_Match{corner}\s+

	   $Addr_Match{street}\W+
	    (?{ @_{qw{prefix2 street2 type2 suffix2}}
		= delete @_{qw{prefix street type suffix }} })

	   $Addr_Match{place}
	\W*$/ix;
}

our %Normalize_Map = (
    prefix  => \%Geo::Coder::US::Codes::Directional,
    prefix1 => \%Geo::Coder::US::Codes::Directional,
    prefix2 => \%Geo::Coder::US::Codes::Directional,
    suffix  => \%Geo::Coder::US::Codes::Directional,
    suffix1 => \%Geo::Coder::US::Codes::Directional,
    suffix2 => \%Geo::Coder::US::Codes::Directional,
    type    => \%Geo::Coder::US::Codes::Street_Type,
    type1   => \%Geo::Coder::US::Codes::Street_Type,
    type2   => \%Geo::Coder::US::Codes::Street_Type,
    state   => \%Geo::Coder::US::Codes::State_Code,
);

use constant SNAP_DISTANCE => 0.00015; 
    # distance to snap intersection points, in degrees
    # 0.00005 = ~7 meters
    # 0.0001  = ~14 meters

our ( %DB, $DBO );

sub db { \%DB }

sub db_file { $DBO }

sub set_db {
    my ($class, $file, $writable) = @_;
    return $DBO if $DBO and not $writable;
    my $mode = $writable ? O_CREAT|O_RDWR : O_RDONLY; 
    $DB_BTREE->{compare} = sub { lc $_[0] cmp lc $_[1] };
    $DBO = tie %DB, "DB_File", $file, $mode, 0666, $DB_BTREE;
    return $DBO;
}

=item Geo::Coder::US->geocode( $string )

Given a string containing a street address or intersection, return a
list of specifiers including latitude and longitude for all matching
entities in the database. To keep from churning over the entire database,
the given address string must contain either a city and state, or a ZIP
code (or both), or geocode() will return undef.

geocode() will attempt to normalize directional prefixes and suffixes,
street types, and state abbreviations, as well as substitute TIGER/Line's
idea of the "primary street name", if an alternate street name was
provided instead.

If geocode() can parse the address, but not find a match in the database,
it will return a hashref containing the parsed and normalized address
or intersection, but without the "lat" and "long" keys specifying the
location. If geocode() cannot even parse the address, it will return
undef. B<Be sure to check> for the existence of "lat" and "long" keys
in the hashes returned from geocode() B<before> attempting to use the
values! This serves to distinguish between addresses that cannot be
found versus addresses that are completely unparseable.

geocode() attempts to be as forgiving as possible when geocoding an
address.  If you say "Mission Ave" and all it knows about is "Mission St",
then "Mission St" is what you'll get back. If you leave off directional
identifiers, geocode() will return address geocoded in all the variants
it can find, i.e. both "N Main St" I<and> "S Main St".

Don't be surprised if geocoding an intersection returns more than one
lat/long pair for a single intersection. If one of the streets curves
greatly or doglegs even slightly, this will be the likely outcome.

geocode() is probably the method you want to use. See more in the
following section on the structure of the returned address and
intersection specifiers.

=cut

sub geocode {
    my ($class, $addr) = @_;
    my @results;
    
    my $part = $class->parse_location($addr);

    return unless $part 
	and ($part->{zip} or ($part->{city} and $part->{state}));

    if ( exists $part->{street1} ) {
	@results = $class->geocode_intersection($part);
    } else {
	@results = $class->geocode_ranges($part);
    }

    return @results ? @results : $part;
}

=item Geo::Coder::US->normalize_address( $spec )

Takes an address or intersection specifier, and normalizes its components,
stripping out all leading and trailing whitespace and punctuation,
and substituting official abbreviations for prefix, suffix, type, and
state values. Also, city names that are prefixed with a directional
abbreviation (e.g. N, NE, etc.) have the abbreviation expanded. The
normalized specifier is returned.

=cut

sub normalize_address {
    my ($class, $part) = @_;
    
    # strip off punctuation
    defined($_) && s/^\s+|\s+$|[^\w\s\-]//gos for values %$part;

    while (my ($key, $map) = each %Normalize_Map) {
	$part->{$key} = $map->{lc $part->{$key}}
              if  exists $part->{$key}
	      and exists $map->{lc $part->{$key}};
    }

    $part->{$_} = ucfirst lc $part->{$_} 
	for grep(exists $part->{$_}, qw( type type1 type2 ));

    # attempt to expand directional prefixes on place names
    $part->{city} =~ s/^($Addr_Match{dircode})\s+(?=\S)
		      /\u$Geo::Coder::US::Codes::Direction_Code{$1} /iosx
		      if $part->{city};

    # strip ZIP+4
    $part->{zip} =~ s/-.*$//os if $part->{zip};

    return $part;
}

=item Geo::Coder::US->parse_location( $string )

Parses any address or intersection string and returns the appropriate
specifier, by calling parse_intersection() or parse_address() as needed.

=cut

sub parse_location {
    my ($class, $addr) = @_;
    if ($addr =~ /$Addr_Match{corner}/ios) {
	$class->parse_intersection($addr);
    } else {
	$class->parse_address($addr);
    }
}

=item Geo::Coder::US->parse_address( $address_string )

Parses a street address into an address specifier, returning undef if
the address cannot be parsed. You probably want to use parse_location()
instead.

=cut

sub parse_address {
    my ($class, $addr) = @_;
    local %_;

    if ($addr =~ /$Addr_Match{address}/ios) {
	my %part = %_;
	### the next line is just to make fossil tests work
	$part{$_} ||= undef for qw{prefix type suffix city state zip};
	return $class->normalize_address(\%part);
    }
}

=item Geo::Coder::US->parse_intersection( $intersection_string )

Parses an intersection string into an intersection specifier, returning
undef if the address cannot be parsed. You probably want to use
parse_location() instead.

=cut

sub parse_intersection {
    my ($class, $addr) = @_;
    local %_;

    if ($addr =~ /$Addr_Match{intersection}/ios) {
	my %part = %_;
	### the next line is just to make fossil tests work
	$part{$_} ||= undef 
	    for qw{prefix1 type1 suffix1 prefix2 type2 suffix2 city state zip};

	if ( $part{type2} and $part{type2} =~ s/s\W*$//ios ) {
	    if ( $part{type2} =~ /^$Addr_Match{type}$/ios && ! $part{type1} ) {
		$part{type1} = $part{type2};
	    } else {
		$part{type2} .= "s";
	    }
	}

	return $class->normalize_address(\%part);
    }
}

=item Geo::Coder::US->filter_ranges( $spec, @candidates )

Filters a list of address specifiers (presumably from the database)
against a query specifier, filtering by prefix, type, suffix, or primary
name if possible. Returns a list of matching specifiers. filter_ranges()
will ignore a filtering step if it would result in no specifiers being
returned. You probably won't need to use this.

=cut

sub filter_ranges {
    my ($class, $args, @addrs) = @_;
    my @filter;

    for my $field (qw( prefix type suffix )) {
	next unless $args->{$field};
	@filter = grep { lc $_->{$field} eq lc $args->{$field} } @addrs;
	@addrs = @filter if @filter;
    }

    return @addrs;
}

=item Geo::Coder::US->find_ranges( $address_spec )

Given a normalized address specifier, return all the address ranges
in the database that appear to cover that address. find_ranges()
ignores prefix, suffix, and type fields in the specifier for search
purposes, and then filters against them ex post facto. The intention
for find_ranges() to find the closest match possible in preference to
returning nothing. You probably want to use geocode_ranges() instead,
which will call find_ranges() for you.

=cut

sub find_streets {
    my ($class, $args) = @_;
    my (@zips, @streets);

    if ( $args->{city} and $args->{state} and not $args->{zip} ) {
	my $city = "$args->{city}, $args->{state}";
	return unless exists $DB{$city};
	@zips = unpack "w*", $DB{$city};

	# city, state might point to the FIPS code of the 
	# place that encompasses it. in which case, get the place
	# name for *that* FIPS code and try again.
	if (@zips == 1 and $zips[0] > 99999) {
	    my $fips = sprintf "%07d", $zips[0];
	    $city = "$DB{$fips}, $args->{state}";
	    return unless exists $DB{$city};
	    @zips = unpack "w*", $DB{$city};
	}

	# finally, format the ZIP codes
	@zips = map { sprintf "%05d", $_ } @zips;
    } else {
	@zips = $args->{zip};
    }

    for my $zip ( @zips ) {
	my $path = "/$zip/$args->{street}/";
	my ($key, $value);
	$DBO->seq( $key = $path, $value, R_CURSOR );
	while ( $key =~ /^$path/i ) {
	    if ($value =~ /^\//o) {
		push @streets, map { "/$zip$_" } split( ",", $value );
	    }  else {
		push @streets, $key;
	    }
	    $DBO->seq( $key, $value, R_NEXT );
	}
    }

    return @streets;
}

sub add_city_and_state {
    my ($class, @results) = @_;

    for my $item (@results) {
	my $fips = sprintf "%07d", $item->{fips};
	my $state = substr($fips, 0, 2);

	# if the FIPS code points to a county subdivision (i.e. not
	# in the database) find the nearest inhabited place by ZIP
	# code instead.
	#
	$fips = sprintf "%07d", unpack( "w", $DB{$item->{zip}} )
	    unless $DB{$fips};

	$item->{city}  = $DB{$fips};
	$item->{state} = $Geo::Coder::US::Codes::State_FIPS{$state};
    }
}

sub find_ranges {
    my ($class, $args) = @_;
    my @streets = $class->find_streets($args);
    my $number = $args->{number};
    my @results;

    $number =~ s/\D//gos; # remove non-numerics, e.g. dashes

    for my $street (@streets) {
	my ($fips, @data) = unpack "w*", $DB{$street};
	my (@from, @to, @range);
	while (@data) {
	    my $matched = 0;
	    @from = splice( @data, 0, 2 ) if $data[0] > 1_000_000;
	    while (@data and $data[0] < 1_000_000) {
		@range = splice( @data, 0, 2 );
		if ($number % 2 == $range[0] % 2 and
		     (($number >= $range[0] and $number <= $range[1]) or
		      ($number <= $range[0] and $number >= $range[1]))) {
		    $matched++;
		    shift @data while @data and $data[0] < 1_000_000;
		}
	    }
	    last unless @data;
	    @to = splice( @data, 0, 2 );
	    if ($matched) {
		my %found = ( fips => $fips );
		@found{qw{ zip street type prefix suffix }} 
		    = split "/", substr($street, 1), 5;
		@found{qw{ toadd fradd }} = @range;
		@found{qw{ frlat frlong tolat tolong }} 
		    = map( $_ / 1_000_000, @from, @to );
		$found{$_} *= -1 for qw/frlong tolong/;
		push @results, \%found;
	    }
	    @from = @to;
	}
    }

    $class->add_city_and_state( @results );
    return $class->filter_ranges( $args, @results );
}

=item Geo::Coder::US->geocode_ranges( $address_spec, @ranges )

Given an address specifier and (optionally) some address ranges from the
database, interpolate the street address into the street segment referred
to by the address range, and return a latitude and longitude for the
given address within each of the given ranges. If @ranges is not given,
geocode_ranges() calls find_ranges() with the given address specifier,
and uses those returned. You probably want to just use geocode() instead,
which also parses an address string and determines whether it's a proper
address or an intersection automatically.

=cut

sub geocode_ranges {
    my ($class, $args, @addrs) = @_;
    my %results;

    @addrs = $class->find_ranges($args) unless @addrs;

    for my $range (@addrs) {
	my %target = %$args;
	my $pct = ($args->{number} - $range->{toadd}) / 
		  ($range->{fradd} - $range->{toadd});

	$target{lat}  = sprintf "%.6f",
	    $range->{frlat}  + ($range->{tolat}  - $range->{frlat} ) * $pct;

	$target{long} = sprintf "%.6f",
	    $range->{frlong} + ($range->{tolong} - $range->{frlong}) * $pct;

	$target{$_} = $range->{$_}
	    for (qw( prefix street type suffix city state zip ));

	$results{"$target{lat}:$target{long}"} = \%target;
    }

    return values %results;
}

=item Geo::Coder::US->find_segments( $intersection_spec )

Given a normalized intersection specifier, find all of the street segments
in the database matching the two given streets in the given locale or
ZIP code.  find_segments() ignores prefix, suffix, and type fields in
the specifier for search purposes, and then filters against them ex
post facto. The intention for find_segments() to find the closest match
possible in preference to returning nothing. You probably want to use
geocode_intersection() instead, which will call find_segments() for you.

=cut

sub find_segments {
    my ($class, $args) = @_; 
    my @streets = $class->find_streets($args);
    my @segments;
    
    for my $street (@streets) {
	my ($fips, @data) = unpack "w*", $DB{$street};
	my (@from, @to);
	while (@data) {
	    @from = splice( @data, 0, 2 ) if $data[0] > 1_000_000;
	    shift @data while @data and $data[0] < 1_000_000;
	    last unless @data;
	    my @to = splice( @data, 0, 2 );

	    my %found = (fips => $fips);
	    @found{qw{ zip street type prefix suffix }} 
		= split "/", substr($street, 1), 5;
	    @found{qw{ city state }}  = @$args{qw{ city state }};
	    @found{qw{ frlat frlong tolat tolong }} 
		= map( $_ / 1_000_000, @from, @to );
	    $found{$_} *= -1 for qw/frlong tolong/;
	    push @segments, \%found;

	    @from = @to;
	}
    }

    $class->add_city_and_state( @segments );
    return $class->filter_ranges( $args, @segments );
}

=item Geo::Coder::US->geocode_intersection( $intersection_spec )

Given an intersection specifier, return all of the intersections in the
database between the two streets specified, plus a latitude and longitude
for each intersection. You probably want to just use geocode() instead,
which also parses an address string and determines whether it's a proper
address or an intersection automatically.

=cut

sub geocode_intersection {
    my ($class, $args) = @_;
    my (@points1, @points2, %results);
    my %subargs = %$args;

    $subargs{$_} = $args->{$_ . 1} for (qw( prefix street suffix type ));
    push @points1,
	[$_->{frlat}, $_->{frlong}, $_], 
	[$_->{tolat}, $_->{tolong}, $_]
	for $class->find_segments(\%subargs);

    $subargs{$_} = $args->{$_ . 2} for (qw( prefix street suffix type ));
    push @points2,
	[$_->{frlat}, $_->{frlong}, $_],
	[$_->{tolat}, $_->{tolong}, $_]
	for $class->find_segments(\%subargs);

    return unless @points1 and @points2;

    %subargs = %$args;

    for my $x (@points1) {
	for my $y (@points2) {
	    if (abs($x->[0] - $y->[0]) < SNAP_DISTANCE and 
		abs($x->[1] - $y->[1]) < SNAP_DISTANCE) {
		my ($st1, $st2, %target) = ($x->[2], $y->[2]);
		$target{lat}  = $x->[0];
		$target{long} = $x->[1];
		$target{$_ . 1}  = $st1->{$_} for (qw( prefix type suffix ));
		$target{street1} = $st1->{street};
		$target{$_ . 2}  = $st2->{$_} for (qw( prefix type suffix ));
		$target{street2} = $st2->{street};
		$target{$_}  = $st1->{$_} || $st2->{$_} for qw/zip city state/;
		$results{"$target{lat}:$target{long}"} = \%target;
	    }
	}
    }

    return values %results;
}


=back 

=head1 CALLING CONVENTIONS

Most Geo::Coder::US methods take a reference to a hash containing
address or intersection information as one of their arguments. This
"address specifier" hash may contain any of the following fields for a
given address:

=head2 ADDRESS SPECIFIER

=over 4

=item number

House or street number.

=item prefix

Directional prefix for the street, such as N, NE, E, etc.  A given prefix
should be one to two characters long.

=item street

Name of the street, without directional or type qualifiers.

=item type

Abbreviated street type, e.g. Rd, St, Ave, etc. See the USPS official
type abbreviations at L<http://www.usps.com/ncsc/lookups/abbr_suffix.txt> 
for a list of abbreviations used.

=item suffix

Directional suffix for the street, as above.

=item city

Name of the city, town, or other locale that the address is situated in.

=item state

The state which the address is situated in, given as its two-letter
postal abbreviation. See L<http://www.usps.com/ncsc/lookups/abbr_state.txt>
for a list of abbreviations used.

=item zip

Five digit ZIP postal code for the address, including leading zero, if needed.

=item lat

The latitude of the address, as returned by geocode() et al. If you
provide this to as part of an argument to a Geo::Coder::US method,
it will be ignored.

=item long

The longitude of the address, as returned by geocode() et al. If you
provide this to as part of an argument to a Geo::Coder::US method,
it will be ignored.

=back

=head2 INTERSECTION SPECIFIER

=over 4

=item prefix1, prefix2

Directional prefixes for the streets in question.

=item street1, street2

Names of the streets in question.

=item type1, type2

Street types for the streets in question.

=item suffix1, suffix2

Directional suffixes for the streets in question.

=item city

City or locale containing the intersection, as above.

=item state

State abbreviation, as above.

=item zip

Five digit ZIP code, as above.

=item lat, long

A single latitude and longitude for the intersection, as specified
above. If you provide these values as part of an argument to a
Geo::Coder::US method, they will be ignored.

=back

=head1 DATA MODEL

Geo::Coder::US instantiates several subclasses for the purposes of
modelling TIGER/Line street data. Although you will probably not need
to use or think about these subclasses, they are described here
for completeness's sake, yadda yadda.

=cut

1;
__END__

=head1 BUGS, CAVEATS, ETC.

The TIGER/Line data is notoriously buggy and inaccurate, but it seems to
work reasonably well for urban areas. Geo::Coder::US uses interpolation
to estimate the position of a particular address within a block, which
means that it will necessarily be slightly inaccurate. Hey, it's only 14
meters off for I<my> house, which is better than the 300 meter error given
by another prominent geocoder, and definitely close enough for navigation.

In rural areas, TIGER/Line doesn't give names for lots of putative roads,
even if the roads have names. Maybe the sign blew down the day before
the Census agents got there, assuming there was ever a sign. What can
you do? Similarly, lots of rural areas have official county subdivision
names that an ordinary user would never think to give. Probably the right
thing to do is map in names from a ZIP code database, but that data's
not in TIGER/Line. What can you do? In general, you should expect the
geocoder to be a lot more accurate in urban versus rural areas.

There may be many kinds of US street addresses which Geo::Coder::US can't
parse. In particular, Geo::Coder::US strips out letters and dashes from
house numbers, which may cause ambiguous results in certain parts of
the country (particularly rural Michigan and Illinois, I think). Mea
culpa. Send patches.

The full TIGER/Line data set is one heck of a lot of data -- about four
gigabytes compressed, and over 24 gigs uncompressed. The BerkeleyDB
database covering the whole US runs to 750+ megabytes uncompressed, or
about 305 megs compressed. I'd really like to host this somewhere so
that people can just download the database itself, rather than having
to download the whole TIGER/Line data set just to build the geocoder
database. Contact us if you can offer a mirror site.

It would be nice to see a version of this for other countries,
e.g. Geo::Coder::CA, Geo::Coder::DK, Geo::Coder::UK, with the same
methods. Contact your local legislator about why the public geographic
data for your country isn't freely available like it is in the US. If
street address data is freely available for your country of choice,
what are you waiting for?

=head1 TODO

A web service and client to access this data, so that people
can reuse other people's geocoders.

A means of importing FIPS-55-3 or perhaps ZIP code data so that
places can be geocoded by alternate place names. For example, this
geocoder won't geocode addresses in Ventura, CA properly because the
official name of the city is San Buenaventura, but no one actually calls
it that.

Reverse geocoding methods, to retrieve the nearest street
address from a given lat/long.

=head1 SEE ALSO

DB_File(3pm), Geo::TigerLine(3pm), Geo::Coder::US::Import(3pm)

TIGER/Line is a registered trademark of the US Census Bureau. Find
out more, and get the latest TIGER/Line files from
L<http://www.census.gov/geo/www/tiger/>. Actually, the best place 
to download the data from is 
L<http://www2.census.gov/geo/tiger/tiger2003/>.

You can find a live demo of this code at
L<http://geocoder.us/>.

=head1 AUTHORS

Schuyler Erle <schuyler@nocat.net>

Jo Walsh <jo@frot.org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 by Schuyler Erle and Jo Walsh

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.3 or,
at your option, any later version of Perl 5 you may have available.

=cut

1;
