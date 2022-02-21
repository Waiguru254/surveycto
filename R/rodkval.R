#' Generates a Script to add single variable value labels.
#'
#'
#' The function uses expss package to label data.
#'
#'
#' @param xlsform Latest XLSFORM used to collect the data.
#' @param dataName Use the name used to save the data, in R memory, collected
#'                 using the XLS-form stated above.
#' stated above.
#' @param language  Enter the language to be used in value labelling.
#'                  If the language is not provided, the most left label
#'                  column will be used to generate the value label script.
#'                  For example, language="English".
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#' NOT run
#' rodkmult(xlsforn,'dataName','English')
  rodkval<- function(xlsform,dataName, language='') {
    suppressMessages(library(dplyr))
    ################################## choices  ###################################
    #For single select variables

    ###Extracting the XLSFORM path
    file_xls<-dirname(xlsform)
    labfile<- basename(xlsform)

    ### Importing choice sheet

    choices <- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='choices'))

    ### Renaming the list or list.name

    colnames(choices)[grepl('list',colnames(choices))] <- 'list.name'

    ### Renaming choices value into name, some XLSform use value in place of name.

    colnames(choices)[min(grepl('value',colnames(choices)),rm.na=TRUE)] <- 'name'
    collnms<-c()
    #### Use the language entry, if not entry it uses the most left label columns
    if (language!="") {
      colnames(choices)[min(grep(paste("^label.*\\.",
                  language,sep=''),colnames(choices)),na.rm=TRUE)] <-'label'
    } else {
      colnames(choices)[min(grep(paste("^label.*\\.",
                  '',sep=''),colnames(choices)),na.rm=TRUE)] <- 'label'
    }


    ###Cleaning the value labels
    ### Removing special characters
    ### We will keep improving this to ensure that special characters do not interfere with
    ### Generating the single select columns.

    choices <- choices %>%
      filter(!is.na(list.name))%>%
      filter(list.name!="")%>%
      filter(!is.na(as.numeric(name))) %>%
      na.omit(name)%>%
      mutate(value_s=paste(paste('\"',str_replace_all(label, "[[:punct:]]", " "),
                        '\"', sep=""),name,sep = " = ")) %>%
      dplyr:: select(list.name, value_s)%>%
      plyr::ddply(.,.(list.name), plyr:: summarize, value_s=paste(value_s, collapse=",")) %>%
      mutate(type=list.name)
    ### Importing survey sheet to merge with choices
    ### only remain with single select variables
    print(choices)
    survey_varname<- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey')) %>%
      dplyr:: select(name,type)%>%
      filter(grepl('select_one',type))%>%
      mutate(type=trimws(gsub("select_one","",type)))%>%
      dplyr:: select(name,type)%>%
      full_join(choices,by = 'type')%>%
      na.omit(value_s)%>%
      filter(!is.na(name))%>%
      mutate(val_labs=paste(dataName," <- expss::apply_labels(",dataName,','," ",
                  name,"=",'c(',value_s,'))',sep=""))%>%
      dplyr:: select(val_labs)
    print(survey_varname)
      vallabels<- paste(paste(file_xls,'/',sep=''),"ODK ",tools::file_path_sans_ext(labfile),"Single Select Value Labels.R",sep=" ")

      ###Generating single select r script

     writeLines(as.character(survey_varname$val_labs),vallabels)
    #   for (i in 1:nrow(survey_varname)){
    #   eval(parse(text=paste(survey_varname$val_labs[i],sep="")), envir=.GlobalEnv)
    # }
  }
