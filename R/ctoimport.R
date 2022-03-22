
    ### Importing data function

#' Fetching Data from SurveyCTO server using API.
#'
#'
#' The function fetch data from surveycto server using formid, which is a form
#' identifier. It imports data in .csv format, without repeat group data.
#'
#' @param servername  SurveyCTO servername
#' @param formid      Get this from Setting sheet in the XLSForm workbook or in
#'                     the form definition in the server.
#' @param username    SurveyCTO username, without including  the .survecto.com
#'
#' @param password    SurveyCTO password, it is encouraged not to put the
#'                    password in a plain text in your script. Employ it in
#' @param dataName    Name of the data to be stored in R memory, default is
#'                    data.
#' @param language    This determines that labels language to be used to label
#'                     the data.
#'
#'
#' @return
#' @export
#'
#'@examples
#'  ## NOT RUN
#'  ctoimport(Sys.getenv("servername"),'VAVS_CRF_03',Sys.getenv("username"),Sys.getenv("password"))

    ctoimport<- function (servername,formid, username,password,dataName=NULL,language='') {
      suppressMessages(library(dplyr))
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

      ###Initialize the data.frame in the event there is data in the memory.

      request<-c() ; data_cto<-c() ## Removing the chances of previous data

      ### Construct a URL and fetch the data from surveycto server
      ###For surveycto we are fetching .csv data format, it is compact,
       ### multiple choice columns are not expanded.

      ###Fetching the data

      request <- httr::GET(paste("https://",servername, ".surveycto.com/api/v1/forms/data/csv/",formid,sep=''),
                           config = httr::config(connecttimeout = 600000), progress(), httr::authenticate(username,password))

      ###Reading data using read.csv(), it makes into structured data.

      data <- read.csv (text = httr::content(request, "text"))

      ###Organize the data column names, organizing them by removing special characters
      names<- c()

      names<- sub('.*\\.', '', sub('.*\\:', '', names(data)))

      ### Checking whether logins are valid
        if (grepl("html",data[1])) {

          stop(paste("Invalid servername, formid, Username or password."), call. = FALSE)

        }

      ### Adding variable(s) names to the data

      colnames(data)<-names



      ########################################################################################################################################


       ### Downloading the survey and form definition


      ########################################################################################################################################3


       ###Initialize the data.frame in the event there is data in the memory.

      request_survey<-c() ; data_cto<-c() ## Removing the chances of previous data
      url_survey<- paste("https://",servername, ".surveycto.com/forms/",formid,'/design/',sep='')
      ##print(url_survey)
      request_survey<-httr::GET(url_survey,config = httr::config(connecttimeout = 600000), progress(),httr::authenticate(username,password,type="digest"))

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


      ### If dataname is missing
      if (is.null(dataName)){
        dataName='data'
      }
      ### Characters to keep in columns labels (Variable labels)

      keeps <- c("+", "-",'[',']','(',')',"?","_",'/')

      ## Regex solution

      ######
      ###Extracting the Survey sheets column ansd and column labels

      ################################   survey   ################################

     colnames(survey)[grepl(c(lab_lang),colnames(survey))]<-'label_rawd'

     survey_processd <- survey %>%
     dplyr:: select(type,name,label_rawd)%>%
              filter(grepl('select_one|select_multiple|integer|text|calculate|decimal|date|geopoint',type))%>%   ###Filtering variables that need value labels
               dplyr::mutate(type= trimws(gsub("select_one","",
                  gsub("select_multiple","",
                    gsub("'", "",gsub("\"", "",
                      gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",type, perl=TRUE)))))),
                  label_rawd=gsub('<[^>]+>','',gsub("select_one","",
                          gsub("select_multiple","",
                              gsub("'", "",gsub("\"", "",
                                  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label_rawd, perl=TRUE)))))),
              var_labels=paste(gsub('-','',name),"=\"",gsub(paste0(".*?($|'|", paste(paste0("\\",
                    keeps), collapse = "|"), "|[^[:punct:]]).*?"), "\\1", label_rawd),"\")",sep=""))%>%
              dplyr:: mutate(var_labeeds=paste('try(',dataName," <- expss::apply_labels(",dataName,",",var_labels,', silent=TRUE)',sep="")) %>%
              dplyr:: select(var_labeeds)

 ###################################################################################################################################
                                     ### Adding value labels to the data (both select_one and select_multiple columns)
 ###############################################################################################################################

      ###Extracting the Survey sheets column and and column labels

      ################################   survey   ################################

      ### Generating the single select columns.
      colnames(choices)[grepl('list',colnames(choices))] <- 'list.name'
      colnames(choices)[grepl(c(lab_lang),colnames(choices))]<-'label_rawd'
      ### Symbols to keep in choices labels
      keeps_labs <- c("+",'[',']','(',')',"?","_",'/')

      choices_processed <- choices %>%
        #filter(grepl("^-[0-9]$",name, perl = T))%>%
       # filter(!is.na(as.numeric(name)))%>%
        na.omit(name)%>%
        mutate(value_s=paste(paste('\"',gsub(paste0(".*?($|'|", paste(paste0("\\",
                                                                             keeps_labs), collapse = "|"), "|[^[:punct:]]).*?"), "\\1", label_rawd),
                                   '\"', sep=""),name,sep = " = "))%>%
        dplyr:: select(list.name, value_s) %>%
        group_by_at(vars(list.name)) %>%
        summarize_all(paste, collapse=",") %>%
        rename(type=list.name)

      ### Importing survey sheet to merge with choices
      ### only remain with single select variables

      survey_processed<- survey %>%
        dplyr:: select(name,type)%>%
        filter(grepl('select_one|select_multiple',type))%>%
        mutate(type=trimws(gsub("select_one|select_multiple","",type)))%>%
        dplyr:: select(name,type)%>%
        full_join(choices_processed,by = 'type')%>%
        na.omit(value_s)%>%
        filter(!is.na(name))%>%
        mutate(val_labs=paste('suppressWarnings(try(',dataName," <- expss::apply_labels(",dataName,','," ",
                              gsub(paste0(".*?($|'|", paste(paste0("\\",
                                                                   keeps_labs), collapse = "|"), "|[^[:punct:]]).*?"), "\\1", name),"=",'c(',value_s,')), silent=TRUE))',sep=""))%>%
                               dplyr:: select(val_labs)

      ### Creating an R script folder in the working directory
      ifelse(!dir.exists(file.path('./', 'Scripts')), dir.create(file.path('./','Scripts')), FALSE)
      ###Writing the value label script
      var_script<- paste('./Scripts/',stringr::str_replace_all(form_id, "[[:punct:]]", "")," Variables Labels.R",sep="")

      val_script<- paste('./Scripts/',stringr::str_replace_all(form_id, "[[:punct:]]", "")," Value Labels.R",sep="")

      writeLines(as.character(survey_processed$val_labs),val_script)

      ###Writing the columns label
      writeLines(as.character(survey_processd$var_labeeds),var_script)

      ###Adding the column value labels from the script
      try(suppressWarnings(source(val_script,local = TRUE)), silent = T)

      ###Adding the column labels from the script
      try(suppressWarnings(source(var_script,local = TRUE)), silent = T)

      ###Remove the var label script if stated
      #if (var.script.rm==TRUE) {
      # unlink(var_script, recursive = TRUE, force = TRUE)
      # file.remove(var_script)
      #}
      ###Returning the data
      return(data)
      ### Printing the number of submission(s) download form the server

      cat(crayon::green(paste(nrow(data),"submissions of",formid, " form have been downloaded.",sep=' ')))


      }

#
# # #
#      data<- ctoimport(Sys.getenv("servername"),'VAVS_CRF_06',Sys.getenv("username"),Sys.getenv("password"))
# #
# #
# #      expss::write_labelled_xlsx(data,"labelled_data.xlsx" )
#
#




     ### Fetching the survey form server
#     xlsform<- surveyImport(Sys.getenv("servername"),'VAVS_CRF_03',Sys.getenv("username"),Sys.getenv("password"))
#     choices<- xlsform[[1]]
#     survey<- xlsform[[2]]
#     form_id<- xlsform[[3]]
#     ###Including the language, this is assuming that data is stored as data and langauge ois English
#     rodkvar('','')
#     source(paste(form_id,"Variables Labels.R",sep=""), local = TRUE)
#     ### Deleting the data after labelling
#     invisible(file.remove(paste(form_id,"Variables Labels.R",sep="")))
