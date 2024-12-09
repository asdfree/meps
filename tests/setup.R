# king dumpty's horsemen
# ahrq stitches payors, bills, claims
# fractured health system
library(haven)

meps_sas_import <-
	function( this_url ){
		
		this_tf <- tempfile()
		
		download.file( this_url , this_tf , mode = 'wb' )
		
		this_tbl <- read_sas( this_tf )

		this_df <- data.frame( this_tbl )
		
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}

meps_cons_df <-
	meps_sas_import( "https://meps.ahrq.gov/mepsweb/data_files/pufs/h243/h243v9.zip" )

meps_brr_df <-
	meps_sas_import( "https://meps.ahrq.gov/mepsweb/data_files/pufs/h036brr/h36brr22v9.zip" )
meps_df <- merge( meps_cons_df , meps_brr_df )

stopifnot( nrow( meps_df ) == nrow( meps_cons_df ) )

meps_df[ , 'one' ] <- 1
# meps_fn <- file.path( path.expand( "~" ) , "MEPS" , "this_file.rds" )
# saveRDS( meps_df , file = meps_fn , compress = FALSE )
# meps_df <- readRDS( meps_fn )
library(survey)

meps_design <-
	svrepdesign(
		data = meps_df ,
		weights = ~ perwt22f ,
		type = "BRR" ,
		combined.weights = FALSE ,
		repweights = "brr[1-9]+" ,
		mse = TRUE
	)
meps_design <- 
	update( 
		meps_design , 
		
		one = 1 ,
		
		insured_december_31st = ifelse( ins22x %in% 1:2 , as.numeric( ins22x == 1 ) , NA )
		
	)
sum( weights( meps_design , "sampling" ) != 0 )

svyby( ~ one , ~ region22 , meps_design , unwtd.count )
svytotal( ~ one , meps_design )

svyby( ~ one , ~ region22 , meps_design , svytotal )
svymean( ~ totexp22 , meps_design )

svyby( ~ totexp22 , ~ region22 , meps_design , svymean )
svymean( ~ sex , meps_design )

svyby( ~ sex , ~ region22 , meps_design , svymean )
svytotal( ~ totexp22 , meps_design )

svyby( ~ totexp22 , ~ region22 , meps_design , svytotal )
svytotal( ~ sex , meps_design )

svyby( ~ sex , ~ region22 , meps_design , svytotal )
svyquantile( ~ totexp22 , meps_design , 0.5 )

svyby( 
	~ totexp22 , 
	~ region22 , 
	meps_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ totmcd22 , 
	denominator = ~ totexp22 , 
	meps_design 
)
sub_meps_design <- subset( meps_design , agelast >= 65 )
svymean( ~ totexp22 , sub_meps_design )
this_result <- svymean( ~ totexp22 , meps_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ totexp22 , 
		~ region22 , 
		meps_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( meps_design )
svyvar( ~ totexp22 , meps_design )
# SRS without replacement
svymean( ~ totexp22 , meps_design , deff = TRUE )

# SRS with replacement
svymean( ~ totexp22 , meps_design , deff = "replace" )
svyciprop( ~ insured_december_31st , meps_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( totexp22 ~ insured_december_31st , meps_design )
svychisq( 
	~ insured_december_31st + sex , 
	meps_design 
)
glm_result <- 
	svyglm( 
		totexp22 ~ insured_december_31st + sex , 
		meps_design 
	)

summary( glm_result )
library(foreign)

xport_2002_tf <- tempfile()

xport_2002_url <- "https://meps.ahrq.gov/data_files/pufs/h70ssp.zip"

download.file( xport_2002_url , xport_2002_tf , mode = 'wb' )

unzipped_2002_xport <- unzip( xport_2002_tf , exdir = tempdir() )

meps_2002_df <- read.xport( unzipped_2002_xport )

names( meps_2002_df ) <- tolower( names( meps_2002_df ) )

meps_2002_design <-
	svydesign(
		~ varpsu ,
		strata = ~ varstr ,
		weights = ~ perwt02f ,
		data = meps_2002_df ,
		nest = TRUE
	)
			
result <- svymean( ~ totexp02 , meps_2002_design )
stopifnot( round( coef( result ) , 2 ) == 2813.24 )
stopifnot( round( SE( result ) , 2 ) == 58.99 )

library(srvyr)
meps_srvyr_design <- as_survey( meps_design )
meps_srvyr_design %>%
	summarize( mean = survey_mean( totexp22 ) )

meps_srvyr_design %>%
	group_by( region22 ) %>%
	summarize( mean = survey_mean( totexp22 ) )
