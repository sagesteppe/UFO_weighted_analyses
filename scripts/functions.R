
#' A wrapper function for using spsurveys cont_analysis Prt statistic
#' 
#' @Description For use with strata and/or subpops which may have groups with 
#' several 0 values which do not allow spsurvey to calculate percent standards.
#' This function wraps around cont_analysis and will truncate those groups from 
#' analysis, and append rows indicating that this operation was performed.
#' 
#' @param x a dataframe or sf tibble 
#' @param grp_var1 one level of grouping variables - required
#' @param variables the variables which you are going to compute - see example. 
#' @param ... additional arguments passed onto cont analysis e.g. pctval, cols holding coords
#' 
#' @example This function will applied as a for-loop to understand when are where it fails.
#' 
#' @export
prcnt_meeting <- function(x, grp_var1, variables, ...){
  
  # determine which groups have enough values > 0 for interpolation
  
  all_grps <- distinct(x, !!sym(grp_var1)) %>% pull() 
  
  grps2keep <- x %>% 
    group_by(!!sym(grp_var1)) %>%
    drop_na(!!sym(variables)) %>% 
    filter(!!sym(variables) > 0) %>%  
    mutate(Records = n()) %>% 
    filter(Records > 2) %>% 
    distinct(!!sym(grp_var1)) %>% 
    pull(!!sym(grp_var1)) 
  
  if (length(grps2keep) == 0) {
    
    type <- select(x, !!sym(grp_var1)) %>% colnames() ; type <- type[1]
    indi <- select(x, !!sym(variables)) %>% colnames() ; indi <- indi[1]
    removed_grps <- data.frame(Type = type, Subpopulation = all_grps, Indicator = indi) # which var did we crash on!??
    
    return(removed_grps) ; stop()
    
  } else if(length(grps2keep) > 0) { # remove groups which cannot be calculated by the 
    
    removed_grps <- setdiff(all_grps, grps2keep) %>%  # we will also save
      data.frame(Subpopulation = .) # them out into a single row dataframe to
    x <- filter(x, !!sym(grp_var1) %in% grps2keep) # bind the values back on so
    
  } 
  
  if(nrow(x) > 1){ # calculate our data which we can. 
    return_vals <- cont_analysis(dframe = x, statistics = 'Pct', vars = variables,
                                 subpops = as.character(substitute(grp_var1)), ...)
    return_vals <- return_vals[['Pct']]
  } else {
    return('water')
  }  
  
  
  if(exists('removed_grps')){
    return_vals <- bind_rows(return_vals, removed_grps) %>% 
      fill(Type, .direction = "down") %>% 
      fill(Indicator, .direction = 'down')
    rownames(return_vals) <- NULL
  }
  
  return(return_vals)
  
}