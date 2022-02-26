#' Generates Rscript to add variable labels
#'
#' @param xlsform Latest XLSFORM used to collect data.
#' @param dataName name given to the data.frame collected using the XLSFORM
#' stated
#'
#' @return
#' @export
#'
#' @examples
#' Not run
#' rodkvar(xlsforn,'dataName')
rodkvar<- function(dataName='',language='') {
  suppressMessages(library(dplyr))

  ### Evaluating the language selected
  if (language!="") {
    var_lab_langs=lab_var(language)
  } else {
    var_lab_langs=lab_var('')
  }


  ### If dataname is missing
  if (dataName==""){
    dataName='data'
  }

  ###Extracting the XLSFORM path
  ################################   survey   ################################
  survey <- survey %>%
    dplyr:: select(type,name,c(var_lab_langs))
    colnames(survey)[grepl(c(var_lab_langs),colnames(survey))]<-'label'
    print(names(survey))
    survey<- survey %>% filter(grepl('select_one|select_multiple|integer|text|calculate|decimal|date|geopoint',type))%>%   ###Filtering variables that need value labels
    mutate(type= trimws(gsub("select_one","",
                             gsub("select_multiple","",
                                  gsub("'", "",gsub("\"", "",
                                                    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",type, perl=TRUE)))))),
           label=gsub('<[^>]+>','',gsub("select_one","",
                                        gsub("select_multiple","",
                                             gsub("'", "",gsub("\"", "",
                                                               gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label, perl=TRUE)))))),
           var_labels=paste(gsub('-','',name),"=\"",stringr::str_replace_all(label, "[[:punct:]]", ""),"\")",sep=""))%>%
    mutate(var_labeeds=paste('tryCatch(',dataName," <- expss::apply_labels(",dataName,",",var_labels,')',sep="")) %>%
    dplyr:: select(var_labeeds)
  var_script<- paste(stringr::str_replace_all(form_id, "[[:punct:]]", ""),"Variables Labels.R",sep=" ")
  writeLines(as.character(survey$var_labeeds),var_script)

}




lab_var<- function (language) {
  lab_vars<- names(survey)[min(which(grepl(paste0('label::',language,sep=''),
                                           gsub(' ','',names(survey)))),na.rm=TRUE)]
  return(lab_vars)
}


