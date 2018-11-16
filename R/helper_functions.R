#' Get factors from a data.frame
#'
#' \code{factors_in_data} helper function to pull out the column names of factors in a data.frame
#'
#'    Takes a data.frame and check the columns to see which is a factor.
#'    Returns a vector of column names
#'
#' @param data data.frame - data to look through
#'
#' @return vector of column names
#'
#' @examples
#' factors_in_data( data_frame )
#'
#' @export
#'
factors_in_data <- function( data_df ){
  factor_col_names <- names(data_df)[ sapply(data_df, class) == 'factor' ]
  if ( length(factor_col_names) == 0 ) {
    return(NULL)
  } else {
    return(factor_col_names)
  }
}

#' Get continuous variables from data.frame
#'
#' \code{continuous_variables_in_data} helper function to pull out the column names of continuous variables in a data.frame
#'
#'    Takes a data.frame and check the columns to see which are continuous.
#'    Returns a vector of column names
#'
#' @param data data.frame - data to look through
#'
#' @return vector of column names
#'
#' @examples
#' continuous_variables_in_data( data_frame )
#'
#' @export
#'
continuous_variables_in_data <- function( data_df ){
  continuous_vars <-
      Reduce('|',
             lapply(c('integer', 'numeric'),
                    function(type){
                      sapply(data_df, class) == type
                    }
             )
      )
  continuous_col_names <- names(data_df)[ continuous_vars ]
  if ( length(continuous_col_names) == 0 ) {
    return(NULL)
  } else {
    return(continuous_col_names)
  }
}

