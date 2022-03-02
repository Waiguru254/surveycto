#' Tabulates single select variables by group.
#'
#' @param row  Variable to tabulate
#' @param column Group variable.
#' @param data Data collected using the ODK platform
#'
#' @return
#' @export
#'
#' @examples
#' tabx('row','column','data')
tabx <- function(row,column,data) {
  ###Function adding variables that do not exist
  addcols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]
    if(length(add)!=0) data[add] <- NA
    return(data)
  }
  suppressMessages(library(dplyr))
  dof<- addcols(data,{row}) %>%
    dplyr::select({row},{column})
  if (suppressWarnings(max(dof[,1],na.rm = TRUE))=="1" & suppressWarnings(min(dof[,1],na.rm = TRUE))=="1") {
    tabs<-table(dof[,1],dof[,2],useNA="no")
    # print(tabs)
    # print(row.names(tabs))
    # tabs<-tabs[!(rownames(tabs) %in% "0"),]
  }else {
    tabs<-table(dof[,1],dof[,2],useNA="no")
  }
  tab<-cbind(tabs,Overall = rowSums(tabs,na.rm = FALSE))
  cpercent <-tab
  ncol(cpercent)
  cpercent <-tab
  for(i in 1:ncol(tab)) { cpercent[,i] <-paste(tab[,i],"(",ifelse(tab[,i]==0,"0.0",format(round(tab[,i]/colSums(tab)[i]*100, digits=1),trim=TRUE)),"%)", sep="")}
  #colnames
  cnames<-colnames(tab)
  colnames(cpercent) <- cnames
  cpercent<-cbind(rownames(cpercent),cpercent)
  colnames(cpercent) <- c('Labels',cnames)
  rownames(cpercent)<-NULL
  cpercent<-rbind(c(NA),cpercent)
  cpercent[1,1]<- expss::var_lab(dof%>% select(row))
  return(cpercent)

}

