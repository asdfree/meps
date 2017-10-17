if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

meps_cat <-
	get_catalog( "meps" ,
		output_dir = file.path( getwd() ) )

# sample 75% of the records
which_records <- sample( seq( nrow( meps_cat ) ) , round( nrow( meps_cat ) * 0.75 ) )

# always sample year == 2014
meps_cat <- unique( rbind( meps_cat[ which_records , ] , subset( meps_cat , grepl( 2014 , year ) ) ) )

lodown( "meps" , meps_cat )
