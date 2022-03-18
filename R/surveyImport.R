#' Import the survey Form Definition and Generates the Labels dateframes
#' @description The function stream survey Form defination and generates
#'               a list of datasets; choices survey, variable type,and
#'                variable name against the labels.
#' @param servername  Servername where the data is hosted
#' @param formid      Form ID is the data to be streamed
#' @param username    Server username
#' @param password     Sever password
#' @param language    XLS form language to add the labels
#' @param dataName     Name of the datset to be stored in R memory.
#'
#' @return
#' @export
#'
#' @examples
#' #NOT RUN
#'  dd<-surveyImport(Sys.getenv("servername"),'VAVS_CRF_06',Sys.getenv("username"),Sys.getenv("password"))
surveyImport<- function (servername,formid, username,password, language="",dataName=NULL) {

  ### Check whether you are connected to the internet

  if(isFALSE(curl::has_internet())) {

    stop(paste("You are offline. Connect to the internet."), call. = FALSE)

  }

  ### Confirming that all the details are entered

  ##servername
  if (servername=="") {

    stop(paste("Enter the servername."), call. = FALSE)

  }

  ##formid

  if (formid=="") {

    stop(paste("Enter the formid"), call. = FALSE)

  }

  ###Username

  if (username=="") {
    stop(paste("Enter the username"), call. = FALSE)
  }


  if (password=="") {
    stop(paste("Enter the password"), call. = FALSE)
  }

  ### If dataname is missing
  if (is.null(dataName)){
    dataName='data'
  }
  ###Initialize the data.frame in the event there is data in the memory.

  request_survey<-c() ; data_cto<-c() ## Removing the chances of previous data
  url_survey<- paste("https://",servername, ".surveycto.com/forms/",formid,'/design/',sep='')
  ##print(url_survey)
  request_survey<-httr::GET(url_survey,httr::authenticate(username,password,type="digest"))

  #retrieve the contents of a request as a character vector

  data_survey <- httr:: content(request_survey, "text")

  #convert from JSON data to R object
  data_form <- jsonlite::fromJSON(data_survey, flatten = TRUE)

  ### First row to column names in r ==Choices

  choices.raw<- data_form$choicesRowsAndColumns

  choices<- as.data.frame(choices.raw) %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ]

  ### Organizing the survey sheet

  survey.raw<- data_form$fieldsRowsAndColumns
  ### First row to column names in r ==Survey
  survey<- as.data.frame(survey.raw) %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ]

  form_id<-data_form$settingsRowsAndColumns[2,1]


  ### Evaluating the language selected
  if (language=='') {
    lab_lang= names(survey)[min(which(grepl(paste0('label::','',sep=''),
                                            gsub(' ','',names(survey)))),na.rm=TRUE)]
  } else {
    lab_lang= names(survey)[min(which(grepl(paste0('label::',language,sep=''),
                                            gsub(' ','',names(survey)))),na.rm=TRUE)]
  }

  colnames(survey)[grepl(c(lab_lang),colnames(survey))]<-'label_rawd'
  ### Generating the single select columns.
  colnames(choices)[grepl('list',colnames(choices))] <- 'list.name'
  colnames(choices)[grepl(c(lab_lang),colnames(choices))]<-'label_rawd'

  var_type<- survey %>%
    dplyr:: select(type,name,label_rawd)%>%
    filter(grepl('select_one|select_multiple|integer|text|calculate|decimal|time',type))%>%   ###Filtering variables that need value labels
    mutate(type=trimws(gsub('([A-z]+) .*', '\\1', type)),
           label_rawd=gsub('<[^>]+>','',gsub("select_one","",
                                             gsub("select_multiple","",
                                                  gsub("'", "",gsub("\"", "",
                                                                    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label_rawd, perl=TRUE)))))))%>%
    dplyr:: select(type, name) %>%
    mutate(name=as.character(name))

  #########################################################################################################################################################
   ### Generating multiple variable labels(This inlcudes single variable labels, sometimes multiple choices are single select)
  #######################################################################################################################################################
  keeps_labs <- c("+",'[',']','(',')',"?","_",'/')

  choices_processed <- choices %>%
    filter(grepl("^[0-9]$",name, perl = T))%>%
    na.omit(name)%>%
    dplyr:: select(list.name,name,label_rawd) %>%
    rename(type=list.name)

  ### Importing survey sheet to merge with choices
  ### only remain with single select variables

  var_lab_list<- as.data.frame(survey) %>%
    dplyr:: select(name,type)%>%
    filter(grepl('select_one|select_multiple',type))%>%
    mutate(type=trimws(gsub("select_one|select_multiple","",type)))%>%
    dplyr:: select(name,type)%>%
    full_join(choices_processed,by = 'type')%>%
    dplyr:: select(type,name.x,name.y,label_rawd) %>%
    filter(!is.na(name.y))
    colnames(var_lab_list)<- c('type',"var_name",'name','label')
    ########################################################################################################################
    #For single select variables
    ###Extracting the XLSFORM path
    ###Including multiple variables
    ###Setting the directory to save the scripts and the basename of the electronic survey.
    keep.cols <- names(survey) %in% c("")
    survey <- survey[! keep.cols]
    varlabel <- as.data.frame(survey)  %>%
      filter(grepl('select_multiple|select_one|integer|text|calculate|decimal',type)) %>%
      dplyr:: select(name,label_rawd)%>%
      mutate(labels=paste('[',name,'] ',gsub('<[^>]+>','',gsub("select_one","",
                                                               gsub("select_multiple","",
                                                                    gsub("'", "",gsub("\"", "",
                                                                                      gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label_rawd, perl=TRUE)))))),sep = ''))%>%
      dplyr:: select(labels) %>% filter(labels!=" ")

  ### Retrun th elist of data.frame
  return(list(choices,survey,form_id,var_type,var_lab_list,varlabel))


}
#
#
# dd<-surveyImport(Sys.getenv("servername"),'VAVS_CRF_06',Sys.getenv("username"),Sys.getenv("password"))
# choices<- dd[[1]] ### choices sheet in xlsform
# survey<- dd[[2]] ### Survey shooice sheets in xlsform
# form_id<-dd[[4]] ### type of variable =
# form_id<-dd[[5]] ### select_one select_multiple value labels

#
# # language<- 'English'
# #
# # ### Extracting the label with language. If the language is not especified,
# # ### the most left label will be used
# #
#
# #
# # lang_used<- names(survey)[grepl('label::',names(survey))]
# # lang_used
