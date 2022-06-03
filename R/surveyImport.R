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
surveyImport<- function (servername, formid, username, password, language = "",
                         dataName = NULL) {
  ###Loading the require packages
  library(dplyr)
  if (isFALSE(curl::has_internet())) {
    stop(paste("You are offline. Connect to the internet."),
         call. = FALSE)
  }
  if (servername == "") {
    stop(paste("Enter the servername."), call. = FALSE)
  }
  if (formid == "") {
    stop(paste("Enter the formid"), call. = FALSE)
  }
  if (username == "") {
    stop(paste("Enter the username"), call. = FALSE)
  }
  if (password == "") {
    stop(paste("Enter the password"), call. = FALSE)
  }
  if (is.null(dataName)) {
    dataName = "data"
  }
  request_survey <- c()
  data_cto <- c()
  url_survey <- paste("https://", servername, ".surveycto.com/forms/",
                      formid, "/design/", sep = "")
  request_survey <- httr::GET(url_survey, httr::authenticate(username,
                                                             password, type = "digest"))
  data_survey <- httr::content(request_survey, "text")
  data_form <- jsonlite::fromJSON(data_survey, flatten = TRUE)
  choices.raw <- data_form$choicesRowsAndColumns
  choices <- as.data.frame(choices.raw) %>% `colnames<-`(.[1,
  ]) %>% .[-1, ]
  survey.raw <- data_form$fieldsRowsAndColumns
  survey <- as.data.frame(survey.raw) %>% `colnames<-`(.[1,
  ]) %>% .[-1, ]
  form_id <- data_form$settingsRowsAndColumns[2,1]
  if (language == "") {

    lab_lang = names(survey)[min(which(names(survey)=='label'), na.rm = TRUE)]


  }
  else {
    lab_lang = names(survey)[min(which(grepl(paste0("label::",
                                                    language, sep = ""), gsub(" ", "",
                                                                              names(survey)))), na.rm = TRUE)]
  }
  colnames(survey)[grepl(c(lab_lang), colnames(survey))] <- "label_rawd"
  colnames(choices)[grepl("list", colnames(choices))] <- "list.name"
  colnames(choices)[grepl(c(lab_lang), colnames(choices))] <- "label_rawd"
  var_type <- survey %>% dplyr::select(type, name, label_rawd) %>%
    filter(grepl("select_one|select_multiple|integer|text|calculate|decimal|time",
                 type)) %>% mutate(type = trimws(gsub("([A-z]+) .*",
                                                      "\\1", type)), label_rawd = gsub("<[^>]+>",
                                                                                       "", gsub("select_one", "", gsub("select_multiple",
                                                                                                                       "", gsub("'", "", gsub("\"",
                                                                                                                                              "", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$",
                                                                                                                                                       "", label_rawd, perl = TRUE))))))) %>%
    dplyr::select(type, name) %>% mutate(name = as.character(name))
  keeps_labs <- c("+", "[", "]", "(",
                  ")", "?", "_", "/")
  choices_processed <- choices %>% filter(grepl("^[0-9]$",
                                                name, perl = T)) %>% na.omit(name) %>% dplyr::select(list.name,
                                                                                                     name, label_rawd) %>% rename(type = list.name)
  var_lab_list <- as.data.frame(survey) %>% dplyr::select(name,
                                                          type) %>% filter(grepl("select_one|select_multiple",
                                                                                 type)) %>% mutate(type = trimws(gsub("select_one|select_multiple",
                                                                                                                      "", type))) %>% dplyr::select(name, type) %>% full_join(choices_processed,
                                                                                                                                                                              by = "type") %>% dplyr::select(type, name.x, name.y,
                                                                                                                                                                                                             label_rawd) %>% filter(!is.na(name.y))
  colnames(var_lab_list) <- c("type", "var_name",
                              "name", "label")
  keep.cols <- names(survey) %in% c("")
  survey <- survey[!keep.cols]
  varlabel <- as.data.frame(survey) %>% filter(grepl("select_multiple|select_one|integer|text|calculate|decimal",
                                                     type)) %>% dplyr::select(name, label_rawd) %>% mutate(labels = paste("[",
                                                                                                                          name, "] ", gsub("<[^>]+>", "", gsub("select_one",
                                                                                                                                                               "", gsub("select_multiple", "",
                                                                                                                                                                        gsub("'", "", gsub("\"", "",
                                                                                                                                                                                           gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
                                                                                                                                                                                                label_rawd, perl = TRUE)))))), sep = "")) %>%
    dplyr::select(labels) %>% filter(labels != " ")
  return(list(choices,survey, form_id, var_type, var_lab_list,
              varlabel))
}
#
library(surveycto)
##dd<-surveyImport(Sys.getenv("servername"),'VAVS_CRF_06',Sys.getenv("username"),Sys.getenv("password"))
# choices<- dd[[1]] ### choices sheet in xlsform
# survey<- dd[[2]] ### Survey shooice sheets in xlsform
#form_id<-dd[[4]] ### type of variable =
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
