### Evaluating Duplicates in a column (Indexing or Counting)
#'
# Evaluating Duplicates in a column (Indexing or Counting)
#'
#'
#' @param column the column where ducplicated are beign evaluated 
#'
#' @param index  FALSE is one wants the duplicates to be indixed, and TRUE 
#'               for duplicated to be indicated by number of occurence. 
#'               
#' @param na.rm  TRUE is you want NA or empty responses to be evaluated
#'               
#' @return
#' @export
#'
#' @examples
#' ###Not run
#' dup_tag(column)
#'  
#'  



dup_tag<- function(column,index=FALSE,na.rm=TRUE) {
  ###Condition if one want then of duplicated or index, default is indices
  library(dplyr)
  if (!isTRUE(index)){
    ### This indicates the number of duplication in the duplicates
    dups<-ave(seq_along(column),column, FUN=length)
    ### Remove empty or missing responses 
    dups[which(column %in% c('',NA))]<- NA
  } else {
    ### This create indecies along the duplicates
    dups<-ave(seq_along(column),column, FUN=seq_along)
    ###This remove checking duplicates for empty and missing 
    dups[which(column %in% c('',NA))]<- NA
  }
  return(dups)
}
