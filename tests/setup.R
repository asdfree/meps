if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

meps_cat <-
	get_catalog( "meps" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( meps_cat ) ) / ceiling( nrow( meps_cat ) / 2 ) )

meps_cat <- unique( rbind( meps_cat[ record_categories == this_sample_break , ] , meps_cat[ grepl( 2015 , meps_cat$year ) , ] ) )

lodown( "meps" , meps_cat )
