use blib;
use Test::More tests => 34;
use strict;
use warnings;

use_ok("Geo::Coder::US");
use_ok("Geo::Coder::US::Import");

my $path = (-r "ORA.RT1" ? "." : "t");

unlink "$path/sample.db"; # in case tests ran previously

Geo::Coder::US->set_db( "$path/sample.db", 1 );
isa_ok( $Geo::Coder::US::DBO, "DB_File", "BDB object" );
is( tied(%Geo::Coder::US::DB), $Geo::Coder::US::DBO, 
    "BDB hash is tied correctly" );
is( keys %Geo::Coder::US::DB, 0, "Database is empty before import" );

Geo::Coder::US::Import->load_tiger_data( "$path/ORA" );

my @expected = (
    "/94931/Gravenstein/Hwy//",
    "/94931/Gravenstein/Hwy//S",
    "/94931/Gravenstein/Way//",
    "/94931/State Highway 116///",
    "/95436/Gravenstein/Hwy//N",
    "/95436/Highway 116///",
    "/95436/State Highway 116///",
    "/95472/Gravenstein/Ave//",
    "/95472/Gravenstein/Hwy//",
    "/95472/Gravenstein/Hwy//N",
    "/95472/Gravenstein/Hwy//S",
    "/95472/Highway 116///",
    "/95472/Mill Station/Rd//",
    "/95472/Old Gravenstein/Hwy//",
    "/95472/State Highway 116///",
    "/95931/Gravenstein/Hwy//",
    "/95931/Gravenstein/Hwy//S",
    "/95931/State Highway 116///",
    "/95472/Railroad/St//",
    "/95444/Railroad/St//",
    "/95472/Railroad/Ave//",
    "0670770",
    "0630812",
    "95472",
    "95444",
    "Sebastopol, CA",
    "Graton, CA",
);

is( scalar keys %Geo::Coder::US::DB, scalar @expected,
    "Database has correct number of entries after import" );
is( exists $Geo::Coder::US::DB{$_}, 1, "Database has key $_"  )
    for @expected;

my @stuff = unpack "w*", $Geo::Coder::US::DB{"/95472/Gravenstein/Hwy//N"};
is( scalar(@stuff), 241, "Gravenstein Hwy N has correct number of items" );
