if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available MEPS microdata files
meps_cat <-
	get_catalog( "meps" ,
		output_dir = file.path( getwd() ) )

# 2015 only
meps_cat <- subset( meps_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( meps_cat ) > 0 )

options( survey.replicates.mse = TRUE )

library(survey)

meps_cons_df <- 
	readRDS( file.path( getwd() , 
		"2015/full year consolidated.rds" ) )

meps_brr <- 
	readRDS( file.path( getwd() , 
		"meps 1996-2015 replicates for variance estimation.rds" ) )

meps_brr <- 
	meps_brr[ , 
		c( "dupersid" , "panel" , 
			names( meps_brr )[ !( names( meps_brr ) %in% names( meps_cons_df ) ) ] 
		)
	]

meps_df <- merge( meps_cons_df , meps_brr )

stopifnot( nrow( meps_df ) == nrow( meps_cons_df ) )

meps_design <-
	svrepdesign(
		data = meps_df ,
		weights = ~ perwt14f ,
		type = "BRR" ,
		combined.weights = FALSE ,
		repweights = "brr[1-9]+"
	)
meps_design <- 
	update( 
		meps_design , 
		
		one = 1 ,
		
		insured_december_31st = ifelse( ins14x %in% 1:2 , as.numeric( ins14x == 1 ) , NA )
		
	)
sum( weights( meps_design , "sampling" ) != 0 )

svyby( ~ one , ~ region14 , meps_design , unwtd.count )
svytotal( ~ one , meps_design )

svyby( ~ one , ~ region14 , meps_design , svytotal )
svymean( ~ totexp14 , meps_design )

svyby( ~ totexp14 , ~ region14 , meps_design , svymean )
svymean( ~ sex , meps_design )

svyby( ~ sex , ~ region14 , meps_design , svymean )
svytotal( ~ totexp14 , meps_design )

svyby( ~ totexp14 , ~ region14 , meps_design , svytotal )
svytotal( ~ sex , meps_design )

svyby( ~ sex , ~ region14 , meps_design , svytotal )
svyquantile( ~ totexp14 , meps_design , 0.5 )

svyby( 
	~ totexp14 , 
	~ region14 , 
	meps_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ totmcd14 , 
	denominator = ~ totexp14 , 
	meps_design 
)
sub_meps_design <- subset( meps_design , agelast >= 65 )
svymean( ~ totexp14 , sub_meps_design )
this_result <- svymean( ~ totexp14 , meps_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ totexp14 , 
		~ region14 , 
		meps_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( meps_design )
svyvar( ~ totexp14 , meps_design )
# SRS without replacement
svymean( ~ totexp14 , meps_design , deff = TRUE )

# SRS with replacement
svymean( ~ totexp14 , meps_design , deff = "replace" )
svyciprop( ~ insured_december_31st , meps_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( totexp14 ~ insured_december_31st , meps_design )
svychisq( 
	~ insured_december_31st + sex , 
	meps_design 
)
glm_result <- 
	svyglm( 
		totexp14 ~ insured_december_31st + sex , 
		meps_design 
	)

summary( glm_result )
library(srvyr)
meps_srvyr_design <- as_survey( meps_design )
meps_srvyr_design %>%
	summarize( mean = survey_mean( totexp14 ) )

meps_srvyr_design %>%
	group_by( region14 ) %>%
	summarize( mean = survey_mean( totexp14 ) )

