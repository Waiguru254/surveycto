#' Compiling Variable type into a data.frame from XLSFORM
#'
#' @param xlsform  Latest XLSFORM used to collect the data.
#'
#' @return
#' @export
#'
#' @examples
#' #Not run
#' rodkvartype(xlsform)
#'
rodkvartype<- function(xlsform) {
  suppressMessages(library(dplyr))
  ###Extracting the XLSFORM path
  file_xls<-dirname(xlsform)
  labfile<- basename(xlsform)
  ################################   survey   ################################
  survey <- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey'))%>%
    dplyr:: select(type,name,label)%>%
    filter(grepl('select_one|select_multiple|integer|text|calculate|decimal|time',type))%>%   ###Filtering variables that need value labels
    mutate(type=trimws(gsub('([A-z]+) .*', '\\1', type)),
           label=gsub('<[^>]+>','',gsub("select_one","",
                                        gsub("select_multiple","",
                                             gsub("'", "",gsub("\"", "",
                                                               gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label, perl=TRUE)))))))%>%
    dplyr:: select(type, name) %>%
    mutate(name=as.character(name))
  return(survey)
}

# xlsform<- surveyImport(Sys.getenv("servername"),'VAVS_CRF_06',Sys.getenv("username"),Sys.getenv("password"))
# survey<-as.data.frame(xlsform[2])
# lab_lang<-"English"
# colnames(survey)[grepl(paste("label..",c(lab_lang),sep=''),colnames(survey))]<-'label_rawd'
# surveys<- survey %>%
# dplyr:: select(type,name,label_rawd)%>%
#   filter(grepl('select_one|select_multiple|integer|text|calculate|decimal|time',type))%>%   ###Filtering variables that need value labels
#   mutate(type=trimws(gsub('([A-z]+) .*', '\\1', type)),
#          label_rawd=gsub('<[^>]+>','',gsub("select_one","",
#                                       gsub("select_multiple","",
#                                            gsub("'", "",gsub("\"", "",
#                                                              gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label_rawd, perl=TRUE)))))))%>%
#   dplyr:: select(type, name) %>%
#   mutate(name=as.character(name))


