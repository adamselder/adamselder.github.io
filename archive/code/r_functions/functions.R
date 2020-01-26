
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Functions to compute and plot percents
# Unfortunately, I named a lot of things "prop" when the actual output is 
# transformed into percents. This will be very helpful but tedious to fix.

# This note explains how missing values are handled
how_missings_are_handled <- 'Note: the miss_N column counts NA values, which are then excluded when the survey mean or median is computed.'

# Important to realize that percents for each level of prop_var 
# will sum to 100% within the ... vars if binary=FALSE, 
# but if binary=TRUE, then that variable drops out of the 
# returned data frame and percent represents the percent 
# having 1 (or the mean of the 0/1 binary var) for prop_var, for 
# each of the groups defined by the ... vars

#--------------------------------------------------------------------------------
# prop_kvar: compute percents with 0 or more grouping variables
#--------------------------------------------------------------------------------
prop_kvar  <- function(svy, prop_var, char_input=FALSE, binary=FALSE, ...) {

    # group_var will have length=0 if there is no variable passed
    group_vars <- quos(...)

    # If called in a loop, input will be a character 
    # There's probably a better way to do this, but in the interest of time...
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	prop_var <- rlang::sym(prop_var)
	# rlang::syms will throw an error if ... is empty, so check if
	# quos(...) is length 0 first
	if (length(group_vars)>0) {
	    group_vars <- rlang::syms(...)
	} else group_vars <- NULL
    } else {
	prop_var <- enquo(prop_var)
	# defined group_varss using quos above
    }
    prop_var_name <- quo_name(prop_var)

    # When there is 1 or more grouping variables
    if (length(group_vars)>0) {
	if (binary) {
	    svy %>% 
	    dplyr::group_by(!!!group_vars) %>%
		dplyr::summarize(Nobs=unweighted(sum(!!prop_var)),
			  miss_N = unweighted(sum(is.na(!!prop_var))),
			  prop=survey_mean(!!prop_var, 
					   na.rm=TRUE,
						  proportion=TRUE, vartype='ci')) %>%
	    dplyr::mutate_at(vars(matches('prop')), funs(round(100* .,2)))
	} else {
	    # This solution given here:
	    # https://github.com/gergness/srvyr/issues/13#issuecomment-321407979
	    var_levels <- unique(svy$variables[[quo_name(prop_var)]])
	    purrr::map_df(var_levels, function(this_level) {
		# cat(this_level, "\n")
		# Get Nobs - if Nobs=0 for any groups, this tibble will have more rows than the "b" tibble
		a <- svy  %>% 
		  dplyr::group_by(!!!group_vars) %>%
		  dplyr::summarize(Nobs=unweighted(sum(!!prop_var == this_level)),
					  miss_N = unweighted(sum(is.na(!!prop_var == this_level))))

		# Get proportion
		b <- svy  %>% dplyr::group_by(!!!group_vars) %>%
		  dplyr::summarize(prop = survey_mean(!!prop_var == this_level, 
							     na.rm=TRUE,
							 proportion = TRUE, vartype = "ci")) %>%
		  dplyr::mutate_at(vars(matches('prop')), funs(round(100* .,2))) %>% 
		  dplyr::mutate(!!prop_var_name := this_level)
				# See https://www.tidyverse.org/articles/2018/07/ggplot2-tidy-evaluation/ for use of :=

		c <- dplyr::left_join(a, b) 

	    })

	}
    } else {
    # When there is no grouping variable, i.e. "..." is empty

	if (binary) {
	    svy %>% 
	    dplyr::summarize(Nobs=unweighted(sum(!!prop_var)),
			  miss_N = unweighted(sum(is.na(!!prop_var))),
			  prop=survey_mean(!!prop_var, 
					   na.rm=TRUE,
						  proportion=TRUE, vartype='ci')) %>%
	    dplyr::mutate_at(vars(matches('prop')), funs(round(100* .,2)))
	} else {
	    # This solution given here:
	    # https://github.com/gergness/srvyr/issues/13#issuecomment-321407979
	    var_levels <- unique(svy$variables[[quo_name(prop_var)]])
	    purrr::map_df(var_levels, function(this_level) {
		    svy  %>% 
	        dplyr::summarize(Nobs=unweighted(sum(!!prop_var == this_level)),
			      miss_N = unweighted(sum(is.na(!!prop_var == this_level))),
			      prop = survey_mean(!!prop_var == this_level, 
						 na.rm=TRUE,
					     proportion = TRUE, vartype = "ci")) %>%
	        dplyr::mutate_at(vars(matches('prop')), funs(round(100* .,2))) %>% 
	        dplyr::mutate(!!prop_var_name := this_level)

	    })

	}
    }
}
 
#--------------------------------------------------------------------------------
# plot_1var: plot for prop_kvar output when there are 0 grouping vars
#--------------------------------------------------------------------------------
plot_1var <- function(tab, var, char_input=FALSE) {
    
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
    } else {
	var <- enquo(var)
    }
    
    pl  <- ggplot(tab, aes(x=!!var, y=prop, 
		  ymin=prop_low, ymax=prop_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    #ggplotly(pl)
    return(pl)
}

#--------------------------------------------------------------------------------
# plot_kvar: plot for prop_kvar output when there are 1+ grouping vars
#--------------------------------------------------------------------------------
plot_kvar <- function(tab, var, var_group, char_input=FALSE, ...) {
    
    # facet_vars will have length=0 if there is no variable passed
    facet_vars <- quos(...)

    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
	var_group <- rlang::sym(var_group)
	# rlang::syms seems to not work if ... is empty, so I'm using quos 
	# from above to check for empti-ness
	if (length(facet_vars)>0) {
	    facet_vars <- rlang::syms(...)
	} else facet_vars <- NULL
    } else {
	var <- enquo(var)
	var_group <- enquo(var_group)
	# facet_vars were captured above
    }


    pl  <- ggplot(tab, aes(x=!!var_group, y=prop, fill=!!var,
		  ymin=prop_low, ymax=prop_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    #ggplotly(pl)

    # "vars" option for faceting allows for flexible tidy eval
    # https://www.tidyverse.org/articles/2018/07/ggplot2-tidy-evaluation/
    if (length(facet_vars)!=0) pl <- pl + facet_wrap(vars(!!!facet_vars))

    return(pl)
}

if (1==0) {
    # EXAMPLES TO PUT IN HELP FILE

    # prop_kvar(svy, sex, binary=FALSE, char_input=FALSE, raceimm, agecat)
    # prop_kvar(svy, sex, binary=FALSE, char_input=FALSE, raceimm)
    # prop_kvar(svy, 'sex', binary=FALSE, char_input=TRUE, 'raceimm')
    # prop_kvar(svy, sex, binary=FALSE)

    # tab <-  prop_kvar(svy, 'sex', char_input=TRUE, binary=FALSE)
    # plot_1var(tab, sex)

    # Examples - 2-way
    tab <-  prop_kvar(svy, sex, char_input=FALSE, binary=FALSE, raceimm)
    plot_kvar(tab, sex, raceimm, char_input=FALSE)
    plot_kvar(tab, 'sex', 'raceimm', char_input=TRUE)
    # Examples - 3-way
    tab <-  prop_kvar(svy, sex, char_input=FALSE, binary=FALSE, raceimm, agecat)
    plot_kvar(tab, sex, raceimm, char_input=FALSE, agecat)
    plot_kvar(tab, 'sex', 'raceimm', char_input=TRUE, list('agecat'))
    # Examples - 4-way
    # Proportion that deg.pers=1 (deg.pers=0 not represented)
    tab <-  prop_kvar(svy, deg.pers.c, char_input=FALSE, binary=TRUE, raceimm, agecat, sex)
    plot_kvar(tab, sex, raceimm, char_input=FALSE, agecat)
    tab <-  prop_kvar(svy, sex, char_input=FALSE, binary=FALSE, raceimm, agecat, deg.pers)
    plot_kvar(tab, sex, raceimm, char_input=FALSE, agecat, deg.pers)
}

#--------------------------------------------------------------------------------
# tab_continuous: dplyr::summarize continuous variables
#--------------------------------------------------------------------------------
tab_continuous  <- function(svy, cont_var, char_input=FALSE, round_to=1, ...) {

    # group_var will have length=0 if there is no variable passed
    group_vars <- quos(...)

    # If called in a loop, input will be a character 
    # There's probably a better way to do this, but in the interest of time...
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	cont_var <- rlang::sym(cont_var)
	# rlang::syms will throw an error if ... is empty, so check if
	# quos(...) is length 0 first
	if (length(group_vars)>0) {
	    group_vars <- rlang::syms(...)
	} else group_vars <- NULL
    } else {
	cont_var <- enquo(cont_var)
	# defined group_varss using quos above
    }
    cont_var_name <- quo_name(cont_var)

    # When there is 1 or more grouping variables
    if (length(group_vars)>0) {

	    bind_rows(
		svy %>% group_by(!!!group_vars) %>% 
		    summarise(Nobs=unweighted(n()),
			      miss_N = unweighted(sum(is.na(!!cont_var))),
			      est=survey_quantile(!!cont_var, c(0), na.rm=TRUE, vartype='ci'))  %>% 
		    rename_all(funs(stringr::str_replace_all(., 'est_q00', 'est'))) %>% 
		    mutate(stat='min') ,
		svy %>% group_by(!!!group_vars) %>% 
		    summarise(Nobs=unweighted(n()),
			      miss_N = unweighted(sum(is.na(!!cont_var))),
			      est=survey_quantile(!!cont_var, c(0.25), na.rm=TRUE, vartype='ci'))  %>% 
		    rename_all(funs(stringr::str_replace_all(., 'est_q25', 'est'))) %>% 
		    mutate(stat='q25'),
		svy %>% group_by(!!!group_vars) %>% 
		    summarise(Nobs=unweighted(n()),
			      miss_N = unweighted(sum(is.na(!!cont_var))),
			      est=survey_quantile(!!cont_var, c(0.50), na.rm=TRUE, vartype='ci'))  %>% 
		    rename_all(funs(stringr::str_replace_all(., 'est_q50', 'est'))) %>% 
		    mutate(stat='median'),
		svy %>% group_by(!!!group_vars) %>% 
		    summarise(Nobs=unweighted(n()),
			      miss_N = unweighted(sum(is.na(!!cont_var))),
			      est=survey_mean(!!cont_var, na.rm=TRUE, vartype='ci'))  %>% 
		    rename_all(funs(stringr::str_replace_all(., 'est_q00', 'est'))) %>% 
		    mutate(stat='mean'),
		svy %>% group_by(!!!group_vars) %>% 
		    summarise(Nobs=unweighted(n()),
			      miss_N = unweighted(sum(is.na(!!cont_var))),
			      est=survey_quantile(!!cont_var, c(0.75), na.rm=TRUE, vartype='ci'))  %>% 
		    rename_all(funs(stringr::str_replace_all(., 'est_q75', 'est'))) %>% 
		    mutate(stat='q75'),
		svy %>% group_by(!!!group_vars) %>% 
		    summarise(Nobs=unweighted(n()),
			      miss_N = unweighted(sum(is.na(!!cont_var))),
			      est=survey_quantile(!!cont_var, c(1), na.rm=TRUE, vartype='ci'))  %>% 
		    rename_all(funs(stringr::str_replace_all(., 'est_q100', 'est'))) %>% 
		    mutate(stat='max')
	)  %>% 
		mutate_at(vars(matches('est')), funs(round(1* .,round_to)))

    } else {
	# When there is no grouping variable, i.e. "..." is empty
	bind_rows(
	    svy %>% 
		summarise(Nobs=unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!cont_var))),
			  est=survey_quantile(!!cont_var, c(0), na.rm=TRUE, vartype='ci'))  %>% 
		rename_all(funs(stringr::str_replace_all(., 'est_q00', 'est'))) %>% 
		mutate(stat='min'),
	    svy %>% 
		summarise(Nobs=unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!cont_var))),
			  est=survey_quantile(!!cont_var, c(0.25), na.rm=TRUE, vartype='ci'))  %>% 
		rename_all(funs(stringr::str_replace_all(., 'est_q25', 'est'))) %>% 
		mutate(stat='q25'),
	    svy %>% 
		summarise(Nobs=unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!cont_var))),
			  est=survey_quantile(!!cont_var, c(0.50), na.rm=TRUE, vartype='ci'))  %>% 
		rename_all(funs(stringr::str_replace_all(., 'est_q50', 'est'))) %>% 
		mutate(stat='median'),
	    svy %>% 
		summarise(Nobs=unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!cont_var))),
			  est=survey_mean(!!cont_var, na.rm=TRUE, vartype='ci'))  %>% 
		rename_all(funs(stringr::str_replace_all(., 'est_q00', 'est'))) %>% 
		mutate(stat='mean'),
	    svy %>% 
		summarise(Nobs=unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!cont_var))),
			  est=survey_quantile(!!cont_var, c(0.75), na.rm=TRUE, vartype='ci'))  %>% 
		rename_all(funs(stringr::str_replace_all(., 'est_q75', 'est'))) %>% 
		mutate(stat='q75'),
	    svy %>% 
		summarise(Nobs=unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!cont_var))),
			  est=survey_quantile(!!cont_var, c(1), na.rm=TRUE, vartype='ci'))  %>% 
		rename_all(funs(stringr::str_replace_all(., 'est_q100', 'est'))) %>% 
		mutate(stat='max')
    ) %>% 
		mutate_at(vars(matches('est')), funs(round(1* .,round_to)))
    }
} # end tab_continuous

#--------------------------------------------------------------------------------
# plot_1var_continuous: plot weighted density of 1 variable
#--------------------------------------------------------------------------------
plot_1var_continuous  <- function(data, var, char_input=FALSE, title=NULL, type='density') {

    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
    } else {
	var <- enquo(var)
    }
    
    if (type=='density') {
	# Weights must be scaled to 1
	# https://stackoverflow.com/questions/12624771/scaled-weighted-density-plot
	pl <- ggplot(data, aes(x=!!var, weight=weight/sum(weight))) + geom_density() +
	    theme_bw()
    } else if (type=='histogram') {

	# Trying to adjust binwidth well. var%%1 checks for integers; if 90% integers, set 
	# bin width to 1
	int_binwidth <- egos %>% mutate(int=!!var%%1) %>% summarise(sum=sum(int==0)/n())
	if (int_binwidth > 0.9) setwidth = 1 else setwidth = NULL
	
	pl <- ggplot(data, aes(x=!!var, weight=weight)) + geom_histogram(binwidth=setwidth) +
	    theme_bw()
    }
    if (!is.null(title)) pl <- pl + ggtitle(title)
    return(pl)
}


#--------------------------------------------------------------------------------
# plot_1var_mean_degree: plot for output of mean_deg_kvar with 1+ grouping vars
#--------------------------------------------------------------------------------
plot_kvar_continuous <- function(data, var, var_group, char_input=FALSE, type='density', ...) {
    
    # facet_vars will have length=0 if there is no variable passed
    facet_vars <- quos(...)

    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
	var_group <- rlang::sym(var_group)
	# rlang::syms seems to not work if ... is empty, so I'm using quos 
	# from above to check for empti-ness
	if (length(facet_vars)>0) {
	    facet_vars <- rlang::syms(...)
	} else facet_vars <- NULL
    } else {
	var <- enquo(var)
	var_group <- enquo(var_group)
	# facet_vars were captured above
    }
    
    if (type=='density') {
	data2 <- data %>% group_by(!!var_group) %>% 
	    mutate(weight2=weight/sum(weight)) 

	pl <- ggplot(data2, aes(x=!!var, weight=weight2)) +
	    geom_density(aes(color=!!var_group)) + theme_bw()
    } else if (type=='boxplot') {
	pl <- ggplot(data, aes(x=!!var_group, y=!!var, weight=weight)) +
	    geom_boxplot() + theme_bw()
    }

    # "vars" option for faceting allows for flexible tidy eval
    # https://www.tidyverse.org/articles/2018/07/ggplot2-tidy-evaluation/
    if (length(facet_vars)!=0) {
	stop('In plot_kvar_continuous, the function must be updated to 
	     rescale weights by the facet vars as well as the group var')
	pl <- pl + facet_wrap(vars(!!!facet_vars))
    }

    return(pl)
}
#--------------------------------------------------------------------------------
# Functions to compute and plot mean degree

# These functions are super similar to the above, but without the percent 
# transformation and different variable names in the output. More elegant would
# be to have one function and have those few differences coded as options or 
# something

#--------------------------------------------------------------------------------
# mean_deg_kvar
#--------------------------------------------------------------------------------
mean_deg_kvar  <- function(svy, deg_var, char_input=FALSE, round_to=2, ...) {

    # group_var will have length=0 if there is no variable passed
    group_vars <- quos(...)

    # If called in a loop, input will be a character 
    # There's probably a better way to do this, but in the interest of time...
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	deg_var <- rlang::sym(deg_var)
	# rlang::syms will throw an error if ... is empty, so check if
	# quos(...) is length 0 first
	if (length(group_vars)>0) {
	    group_vars <- rlang::syms(...)
	} else group_vars <- NULL
    } else {
	deg_var <- enquo(deg_var)
	# defined group_varss using quos above
    }
    # Abandoning this because it causes trouble if you want to rbind results 
    # from different networks 
    # deg_var_name <- paste('mean', quo_name(deg_var), sep='_')

    # When there is 1 or more grouping variables
    if (length(group_vars)>0) {
	    svy %>% 
		group_by(!!!group_vars) %>%
		dplyr::summarize(
			  Nobs = unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!deg_var))),
			  mean_degree = survey_mean(!!deg_var, proportion=FALSE, 
						    na.rm=TRUE,
					   vartype='ci')
			  ) %>% 
		mutate_at(vars(matches('mean')), funs(round(1* .,round_to)))
    } else {
	    svy %>% 
		dplyr::summarize(
			  Nobs = unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!deg_var))),
			  mean_degree = survey_mean(!!deg_var, proportion=FALSE, 
						    na.rm=TRUE,
					   vartype='ci')
			  ) %>% 
		mutate_at(vars(matches('mean')), funs(round(1* .,round_to)))
    }
}
 

#--------------------------------------------------------------------------------
# combine_mean_deg_nets
#--------------------------------------------------------------------------------
# Rabbit hole figuring out how to succesfully apply dplyr tools
# to this survey object to cleanly run mean_deg_kvar on multiple network degree
# variables. summarize_at worked but then reshaping the resulting data 
# structure long is non-trivial - doesn't appear to be a clean way to 
# reshape multiple variables at the same time, like in Stata? Multiple
# gathers can give the wrong answer, so beware. Ended up with this function
# as a hack.

# https://github.com/tidyverse/dplyr/issues/3101
#svy %>% summarize_at(vars(degree_vars), 
#		     funs(survey_mean), vartype='ci') 
#gather(network, mean, starts_with('deg.')) 

combine_mean_deg_nets <- function(svy, degree_vars, char_input=TRUE, ...) {
                                       # 
    # Argh, couldn't get purrr or summarize_at or anything else cool to work!
    dlist <- vector('list', length=length(degree_vars))
    names(dlist) <- degree_vars
    for (d in degree_vars) {
	dlist[[d]] <-  transform(network=d, 
				 mean_deg_kvar(svy, d, char_input=TRUE, 
					       round_to=3, ...), 
				 stringsAsFactors=FALSE)
    }
    df <- do.call('rbind', dlist)
    rownames(df) <- NULL
    return(df)


    # This did not work with the ..., it just ignored it
    if (1==0) {
	purrr::map_dfr(degree_vars, function(x, ...) {
	    netname <- gsub('deg.', '', x)
	    transform(Network=netname,
		      mean_deg_kvar(svy, x, char_input=TRUE, round_to=3, ...),
		      stringsAsFactors=FALSE)
	})
    }
}

#--------------------------------------------------------------------------------
# plot_1var_mean_degree: plot for output of mean_deg_kvar with 0 grouping vars
#--------------------------------------------------------------------------------
plot_1var_mean_degree <- function(tab, var, char_input=FALSE, title=NULL) {
    
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
    } else {
	var <- enquo(var)
    }
    
    pl  <- ggplot(tab, aes(x=!!var, y=mean_degree, 
		  ymin=mean_degree_low, ymax=mean_degree_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    if (!is.null(title)) pl <- pl + ggtitle(title)
    #ggplotly(pl)
    return(pl)
}

#--------------------------------------------------------------------------------
# plot_1var_mean_degree: plot for output of mean_deg_kvar with 1+ grouping vars
#--------------------------------------------------------------------------------
plot_kvar_mean_degree <- function(tab, var, var_group, char_input=FALSE, ...) {
    
    # facet_vars will have length=0 if there is no variable passed
    facet_vars <- quos(...)

    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
	var_group <- rlang::sym(var_group)
	# rlang::syms seems to not work if ... is empty, so I'm using quos 
	# from above to check for empti-ness
	if (length(facet_vars)>0) {
	    facet_vars <- rlang::syms(...)
	} else facet_vars <- NULL
    } else {
	var <- enquo(var)
	var_group <- enquo(var_group)
	# facet_vars were captured above
    }


    pl  <- ggplot(tab, aes(x=!!var_group, y=mean_degree, fill=!!var,
		  ymin=mean_degree_low, ymax=mean_degree_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    #ggplotly(pl)

    # "vars" option for faceting allows for flexible tidy eval
    # https://www.tidyverse.org/articles/2018/07/ggplot2-tidy-evaluation/
    if (length(facet_vars)!=0) pl <- pl + facet_wrap(vars(!!!facet_vars))

    return(pl)
}

if (1==0) {
    # EXAMPLES TO PUT IN HELP FILE

    # Mean degree by network
    transform( mean_deg_kvar(svy, degree_vars[1], char_input=TRUE, round_to=3),
	      Network=names(degree_vars)[1])
    transform(mean_deg_kvar(svy, degree_vars[2], char_input=TRUE, round_to=3),
	      Network=names(degree_vars[2]))
    mean_deg_kvar(svy, deg.pers, char_input=FALSE, round_to=3, sex)
    mean_deg_kvar(svy, deg.cohab, char_input=FALSE, round_to=3, sex)
    mean_deg_kvar(svy, 'deg.cohab', char_input=TRUE, round_to=3, 'sex')
    mean_deg_kvar(svy, 'deg.cohab', char_input=TRUE, round_to=3, list('sex', 'raceimm'))
    combine_mean_deg_nets(svy, 'deg.cohab', char_input=TRUE, round_to=3, list('sex', 'raceimm'))

    # START HERE - ADD EXAMPLE OF combine_mean_deg_nets WITH ... WORKING
    combine_mean_deg_nets(svy, degree_vars, char_input=TRUE, list('sex', 'raceimm'))
    combine_mean_deg_nets(svy, degree_vars, char_input=TRUE, 'sex')

    # Degree distribution by network
    prop_kvar(svy, 'deg.cohab', char_input=TRUE, binary=FALSE)
    prop_kvar(svy, 'deg.pers', char_input=TRUE, binary=FALSE) # deg.pers is still numeric

    # Add by-variables
    prop_kvar(svy, 'deg.pers', char_input=TRUE, binary=FALSE, 'sex')
    prop_kvar(svy, 'deg.pers', char_input=TRUE, binary=FALSE, list('sex', 'race'))

}

#--------------------------------------------------------------------------------
# plot_1var_rate: plot for output of mean_deg_kvar with 0 grouping vars when 
# mean_deg_kvar was used to compute the mean of the one-time rate variable
#--------------------------------------------------------------------------------
plot_1var_rate <- function(tab, var, char_input=FALSE) {
    
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
    } else {
	var <- enquo(var)
    }
    
    pl  <- ggplot(tab, aes(x=!!var, y=rate, 
		  ymin=rate_low, ymax=rate_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    #ggplotly(pl)
    return(pl)
}

#--------------------------------------------------------------------------------
# plot_kvar_rate: plot for output of mean_deg_kvar with 1+ grouping vars when 
# mean_deg_kvar was used to compute the mean of the one-time rate variable
#--------------------------------------------------------------------------------
plot_kvar_rate <- function(tab, var, var_group, char_input=FALSE, ...) {
    
    # facet_vars will have length=0 if there is no variable passed
    facet_vars <- quos(...)

    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
	var_group <- rlang::sym(var_group)
	# rlang::syms seems to not work if ... is empty, so I'm using quos 
	# from above to check for empti-ness
	if (length(facet_vars)>0) {
	    facet_vars <- rlang::syms(...)
	} else facet_vars <- NULL
    } else {
	var <- enquo(var)
	var_group <- enquo(var_group)
	# facet_vars were captured above
    }


    pl  <- ggplot(tab, aes(x=!!var_group, y=rate, fill=!!var,
		  ymin=rate_low, ymax=rate_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    #ggplotly(pl)

    # "vars" option for faceting allows for flexible tidy eval
    # https://www.tidyverse.org/articles/2018/07/ggplot2-tidy-evaluation/
    if (length(facet_vars)!=0) pl <- pl + facet_wrap(vars(!!!facet_vars))

    return(pl)
}

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Functions to compute and plot edge age

#--------------------------------------------------------------------------------
# mean_median_kvar
#--------------------------------------------------------------------------------
# This function differs more from the others because it computes the survey-weighted
# mean and median, and then converts the median to the implied exponential mean

mean_median_kvar  <- function(svy, deg_var, char_input=FALSE, round_to=2, ...) {

    # group_var will have length=0 if there is no variable passed
    group_vars <- quos(...)

    # If called in a loop, input will be a character 
    # There's probably a better way to do this, but in the interest of time...
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	deg_var <- rlang::sym(deg_var)
	# rlang::syms will throw an error if ... is empty, so check if
	# quos(...) is length 0 first
	if (length(group_vars)>0) {
	    group_vars <- rlang::syms(...)
	} else group_vars <- NULL
    } else {
	deg_var <- enquo(deg_var)
	# defined group_varss using quos above
    }
    # Abandoning this because it causes trouble if you want to rbind results 
    # from different networks 
    # deg_var_name <- paste('mean', quo_name(deg_var), sep='_')

    # When there is 1 or more grouping variables
    if (length(group_vars)>0) {

	# Get Nobs - if Nobs=0 for any groups, this tibble will have more rows than the "b" tibble
	a <- svy  %>% group_by(!!!group_vars) %>%
			dplyr::summarize(Nobs = unweighted(n()), 
				  miss_N = unweighted(sum(is.na(!!deg_var))))

	# Get proportion
	b <- svy  %>% group_by(!!!group_vars) %>%
			dplyr::summarize(mean = survey_mean(!!deg_var, proportion=FALSE, 
				     na.rm=TRUE,
				   vartype='ci'), 
				  median = survey_median(!!deg_var, vartype='ci',
					 na.rm=TRUE)) %>% 
		mutate(exp_mean = 1/(log(2)/median_q50))  %>% 
		mutate_at(vars(matches('median')), funs(round(1* .,round_to))) %>% 
		mutate_at(vars(matches('mean')), funs(round(1* .,round_to)))
				  
				  

	final <- left_join(a, b) 

    } else {
	final <- 
	    svy %>% 
		dplyr::summarize(
			  Nobs = unweighted(n()),
			  miss_N = unweighted(sum(is.na(!!deg_var))),
			  mean = survey_mean(!!deg_var, proportion=FALSE, 
					     na.rm=TRUE,
					   vartype='ci'),
			  median = survey_median(!!deg_var, vartype='ci',
						 na.rm=TRUE)) %>% 
		mutate(exp_mean = 1/(log(2)/median_q50))  %>% 
		mutate_at(vars(matches('median')), funs(round(1* .,round_to))) %>% 
		mutate_at(vars(matches('mean')), funs(round(1* .,round_to)))
    }

    # Would love to do this the tidyverse way, but I'm not familiar with the stringr package
    # and I THINK that's the regular expression type that's used with rename_at, although it's
    # not obvious from the documentation. Variations of the following did not work:
    # rename_at(.vars = vars(ends_with(".q50.")), .funs = funs(sub("[.]q50[.]", "", .)))

    colnames(final) <- gsub("*(_q50)*", "", colnames(final))
    return(final)
}

#--------------------------------------------------------------------------------
# plot_1var_edgeage: plot output of mean_median_kvar for 0 grouping vars
#--------------------------------------------------------------------------------
# I believe this needs to be renamed _median instead of _edgeage

plot_1var_edgeage <- function(tab, var, char_input=FALSE, title=NULL) {
    
    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
    } else {
	var <- enquo(var)
    }
    
    pl  <- ggplot(tab, aes(x=!!var, y=median, 
		  ymin=median_low, ymax=median_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    if (!is.null(title)) pl <- pl + ggtitle(title)
    #ggplotly(pl)
    return(pl)
}

#--------------------------------------------------------------------------------
# plot_kvar_median: plot output of mean_median_kvar for 1+ grouping vars
#--------------------------------------------------------------------------------
plot_kvar_median <- function(tab, var, var_group, char_input=FALSE, ...) {
    
    # facet_vars will have length=0 if there is no variable passed
    facet_vars <- quos(...)

    if (char_input) {
	# From https://edwinth.github.io/blog/dplyr-recipes/
	# See https://edwinth.github.io/blog/nse/ to really understand it
	var <- rlang::sym(var)
	var_group <- rlang::sym(var_group)
	# rlang::syms seems to not work if ... is empty, so I'm using quos 
	# from above to check for empti-ness
	if (length(facet_vars)>0) {
	    facet_vars <- rlang::syms(...)
	} else facet_vars <- NULL
    } else {
	var <- enquo(var)
	var_group <- enquo(var_group)
	# facet_vars were captured above
    }


    pl  <- ggplot(tab, aes(x=!!var_group, y=median, fill=!!var,
		  ymin=median_low, ymax=median_upp)) + 
	geom_bar(stat='identity', position=position_dodge()) +
	geom_errorbar(position=position_dodge(width=0.9), width=0.1)
    #ggplotly(pl)

    # "vars" option for faceting allows for flexible tidy eval
    # https://www.tidyverse.org/articles/2018/07/ggplot2-tidy-evaluation/
    if (length(facet_vars)!=0) pl <- pl + facet_wrap(vars(!!!facet_vars))

    return(pl)
}


