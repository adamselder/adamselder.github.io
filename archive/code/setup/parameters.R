
#------------------------------------------------------------
# DISPLAY SETTINGS
#------------------------------------------------------------
# PARAMETER: tables_display_nrows
# Vector of options for number of rows to display in default data tables.
# First entry will be the default; other entries available for user selection
tables_display_nrows <- c(5, 20, 100)

#------------------------------------------------------------
# EGOCENTRIC NETWORK DATA
#------------------------------------------------------------
# PARAMETER: network_list
# Load egodata into a named list of all the networks of interest

library(egonet)
data(nsfg.obj_c)
data(nsfg.obj_p)
data(nsfg.obj_i)

network_list <- list(Cohab=nsfg.obj_c, 
		     Persistent=nsfg.obj_p, 
		     OneTime=nsfg.obj_i)

# Egos
egos <- nsfg.obj_c$egos
egos$deg.cohab.c <- as.character(egos$deg.cohab.c)
egos$deg.pers.c2 <- as.character(egos$deg.pers)

# For edge-based stats
alters <- readRDS('input/data/alters.rds')
egos4alters <- readRDS('input/data/egos4alters.rds')

# Specify network ordering, using labels present in the "network" variable of the "alters" data
# Maybe forget the network list above, and just infer it from the network variable?
network_order <- c('Cohab', 'Persistent', 'One-Time')

#------------------------------------------------------------
# EGO CHARACTERISTICS PARAMETERS
#------------------------------------------------------------
# PARAMETER: ego_1way_vars
# Vector of variables by which ego statistics should be stratified
ego_1way_vars <- c('sex', 'sex.ident', 'agecat', 'race', 'sb', 'dsb.cohab', 'dsb.pers')
ego_1way_vars_continuous <- c('age')

# PARAMETER: ego_2way_vars
# Vector of lists of variables by which ego statistics should be stratified (i.e. cross-tabbed)
# Percents of the categories of the 1st variable will sum to 100% within groups of the 2nd variable
ego_2way_vars <- list(c('sex', 'race'),
		   c('sex.ident', 'race'),
		   c('sex', 'agecat'),
		   c('sex.ident', 'agecat'),
		   c('agecat', 'race'),
		   c('deg.cohab.c', 'deg.pers.c2'))

ego_2way_vars_continuous <- list(c('age', 'race'))

#------------------------------------------------------------
# DEGREE PARAMETERS
#------------------------------------------------------------
# PARAMETER: degree_vars
# In the egodata, deg. variables to analyze
# Require these be named
degree_vars <- c(Cohab='deg.cohab', Persistent='deg.pers')

# PARAMETER: degree_1way_vars
# All stats will be 2-way because network is a presumed stratifier
degree_1way_vars <- c('sex', 'sex.ident', 'agecat', 'race', 'sb', 'dsb.cohab', 'ds.pers', 'dsb.pers')

# PARAMETER: degree_2way_vars
# All stats will be 3-way because network is a presumed stratifier
degree_2way_vars <- list(c('sex', 'race'),
		   c('sex.ident', 'race'),
		   c('agecat', 'sex'),
		   c('agecat', 'sex.ident'),
		   c('agecat', 'race'),
		   c('sb', 'race'),
		   c('dsb.cohab', 'race'))

# PARAMETER: network_pairs
# Relevant for networks where relationships have some duration, not one-time/instantaneous networks
# The cross-network degree distribution will be examined for these pairs of degree variables
# Name the degree variables with the desired network label
network_pairs <- list(c(Cohab='deg.cohab', Persistent='deg.pers'))


#------------------------------------------------------------
# MIXING PARAMETERS
#------------------------------------------------------------

# PARAMETER: degree_1way_vars
# All stats will be 2-way because network is a presumed stratifier
mixing_1way_vars <- c('sex', 'agecat', 'race')


#------------------------------------------------------------
# ONE-TIME NETWORK
#------------------------------------------------------------
# RIGHT NOW, the one-time rate is described by 1-way and 2-way tabs defined by ego_1way_vars and ego_2way_vars

# One-time rate variable
ot_rate <- 'otcount'

# Rate variable description
ot_rate_definition <- 'The one-time rate represents the number of one-times per person per year (see egonet/data-prep/all.egodata.Rmd). '

#------------------------------------------------------------
# PARTNERSHIP AGE
#------------------------------------------------------------

# Is there a one-time/instananeous network with no edge duration? If so, specify 
# its value here. Leave NULL if irrelevant
onetime_net <- 'One-Time'

# Edge age variable
edge_age <- 'edge_age_years'

# Time units
edge_age_definition <- 'The age of active partnerships is described below in years. Statistics are stratified by ego characterstics only, unless explicitly specified otherwise.'

# One-way variables. These will automatically be stratified by network.
edgeage_1way_vars <- c('sex', 'sex.ident', 'agecat', 'race', 'deg.cohab', 'deg.pers')

# Two-way but NOT stratified by network
edgeage_2way_vars_nonet <- list(c('deg.cohab', 'deg.pers'))

# NOT IMPLEMENTED: Two-way variables
# All stats will be 3-way because network is a presumed stratifier
edgeage_2way_vars <- list(c('sex', 'race'),
		   c('sex.ident', 'race'),
		   c('agecat', 'sex'),
		   c('agecat', 'sex.ident'),
		   c('agecat', 'race'))


#------------------------------------------------------------
# ELIMINATE FACTORS FOR 1-WAY
# 1/10/19 note - see issue 27. I can't remember why this was necessary. May be best to find a different solution.
#------------------------------------------------------------
# Convert all numeric 1way vars to characters
# Variables meant to be treated as continuous will be analyzed using a 
# different variable list
all_1way <- unique(c(ego_1way_vars, degree_1way_vars, 
		     mixing_1way_vars, edgeage_1way_vars))
for (var in all_1way) {
    if (var %in% colnames(egos4alters)) egos4alters[,var] <- as.character(egos4alters[,var])
    if (var %in% colnames(alters)) alters[,var] <- as.character(alters[,var])
}
