#' Fetching data from Project's RedCap server using API
#'
#' @param token This is the unique identifier of projects in RedCap.
#'              This function uses the token to fetch data using API.
#' @param removecols   This to indcate whether expanded checkbox columns 
#'                     should be included in the dataset
#'
#'
#' @return
#' @export
#'
#' @examples
#' ###NOT RUN
#' redimport('token','url')
#'
#  redimport<- function (token,url) {

### Check whether you are connected to the internet

#    if(isFALSE(curl::has_internet())) {

#      stop(paste("You are offline. Connect to the internet."), call. = FALSE)

#    }

### Checking whether the token is valid, it should have 32 characters.

#   if (nchar(token)!=32) {
#
#     stop(paste("Kindly enter a correct project token."), call. = FALSE)

#  }

### This make sure the columns are not collapsed and all the variables are imported
redimport<- function (token, removecols = FALSE) {
  library(stringr)
  library(dplyr)
  library(redcapAPI)
  library(httr)
  library(jsonlite)
  library(crayon)
  
  ### Universal url
  url <- "https://redcap.vetmed.wsu.edu/api/"
  
  ### This include the project info, name and project id.
  project_details <- function(token) {
    url <- "https://redcap.vetmed.wsu.edu/api/"
    formData <- list("token" = token,
                     content = 'project',
                     format = 'json',
                     returnFormat = 'json'
    )
    response <- httr::POST(url, body = formData, encode = "form")
    result <- httr::content(response)
    project <- paste( result$project_id,   
                      result$project_title, sep = '-')
    
    return(project)
    
  }
  ### this returns the project id and project name
  
  form_name <- paste('PID',project_details(token))
  
  ### Suppressing the  warning from split function
  withr::local_options(.new = list(warn = -1))
  if (isFALSE(curl::has_internet())) {
    stop(paste("You are offline. Connect to the internet."), 
         call. = FALSE)
  }
  if (nchar(token) != 32) {
    stop(paste("Kindly enter a correct project token."), 
         call. = FALSE)
  }
  
  ####Initializing the field to be collapsed 
  records_collapsed <- NULL
  fields_collapsed <- NULL
  post_body <- list(token = token, content = "record", 
                    format = "csv", type = "flat", records = records_collapsed, 
                    fields = fields_collapsed
  )
  ### Downloading the data into R envrinment 
  raw_text <- httr::POST(url = url, body = post_body, config = httr::config(connecttimeout = 6e+05),progress())
  data<- read.csv(text = httr::content(raw_text, "text"))
  
  #### Adding variable and value labels 
  # check_box<- data %>% select(contains('__'))
  # View(check_box)
  ### Downloading the project disctionary
  redcap_dic<-   try(suppressWarnings(REDCapR::redcap_metadata_read(redcap_uri=url, token=token,verbose=FALSE)$data), silent = T)
  ### Generating Value label from codebook || Inlcude cleaning the codebook
  data_dictionary <-redcap_dic %>%
    janitor:: clean_names() %>%  ### Using janitor package to clean the column names 
    ### Removing rows without valeu labels 
    filter(select_choices_or_calculations != '')%>% ## filteriing for columns or without choices, not needed in adding value labels
    dplyr:: rename(choices = select_choices_or_calculations)%>%  ### Renaming the choices
    dplyr:: rename(variable_label = field_label, variable_name = field_name) %>% ### Renaming the variable name
    dplyr:: select(choices, variable_name,variable_label) %>% ### Selecting choices and variable names
    splitstackshape:: cSplit(.,"choices","|") %>% ### Splitting the choices and value labels
    #dplyr:: filter(!is.na(choices_01)) %>% ### Remove rows without any choices
    reshape:: melt(id.vars = c("variable_name"))%>% ### Reshaping the choices into long
    dplyr:: select(-c(variable))%>% ### Only have the reshape
    dplyr:: arrange(variable_name) %>%
    filter(!is.na(value) & !grepl("if",value)) %>% ### arranging the choices by variable name
    dplyr:: rename(value_choices=value) %>% ### Renaiming the variable for proper debugging
    dplyr:: filter(!is.na(value_choices)) %>% ### Remove if missing the first value/ choices
    dplyr:: mutate(value_label = trimws(sub("^[^,]*,", "",value_choices)), ### Cleaning and removing spaces
                   values = sub(',.*$','',value_choices)) %>% ### Extracting value label from the value _choices columns
    dplyr:: select(-c(value_choices)) %>% ### Removing the coluns after cleaning
    dplyr:: mutate(value_choice = paste("\"",value_label,"\"","=",values,sep = ""))%>% ### Extracting the value from value_choices
    dplyr:: select(c(variable_name,value_choice)) %>%
    group_by(variable_name) %>%
    mutate(id = 1:n()) %>%
    filter(id != 1) %>%
    ungroup()%>% ### Only remaining with  variable name and value choices
    dplyr:: group_by_at(vars(variable_name)) %>%  ### Group by variable name
    dplyr:: summarize_all(paste,collapse = ",") %>%  ### Collapse and separate by a comma
    dplyr:: mutate(vlabels=paste("suppressWarnings(try(data <- expss::apply_labels(data,",variable_name,"=c(",
                                 value_choice,")),silent=TRUE))")) #%>% ### Create a column to export, with both the commands and values needed for value labelling.
  #dplyr:: select(vlabels) #### Only remain with one column with both the commands and value labels.
  
  ### This check whether the Scrpit folder exist in the current working directory,
  ### if it does not exist it will be created.
  ifelse(!dir.exists(file.path("./", "Scripts")),
         dir.create(file.path("./", "Scripts")), FALSE)
  
  ### Creating the script path, creating a folder and adding the scripts
  val_script <- paste("./Scripts/",form_name, " Value Labels.R",sep = "")
  ### Exporting the scripts
  writeLines(as.character(data_dictionary$vlabels),val_script)
  
  ### Generating Variable labels from the codebook
  data_col <-  redcap_dic %>%
    janitor:: clean_names() %>%
    filter(!grepl('descriptive',field_type))%>%
    dplyr::rename(variable_label=field_label)%>%
    dplyr::rename(variable_name=field_name)%>%
    dplyr:: select(variable_name,variable_label) %>%
    dplyr:: mutate(variable_labels= trimws(gsub("<.*?>", "", variable_label))) %>%
    dplyr:: select(variable_name,variable_labels) %>%
    mutate(variable_labels=sub("'", '', variable_labels))%>%
    dplyr:: mutate(variableLabels= paste(
      "try(data <- expss::apply_labels(data,",variable_name,"=\"",gsub('"','',variable_labels),"\"), silent=TRUE)",sep="")) %>%
    dplyr:: select(variableLabels)
  
  var_script <- paste("./Scripts/",form_name," Variable Labels.R",sep = "")
  
  ### Creating the script in the working directory within the script sub-folder
  writeLines(as.character(data_col$variableLabels),var_script)
  
  
  #### Generating label for multiple choice question \
  data_mul <-redcap_dic %>%
    janitor:: clean_names() %>%
    ### Removing rows without valeu labels 
    filter(select_choices_or_calculations!='')%>%
    dplyr:: rename(choices=select_choices_or_calculations)%>%  ### Renaming the choices
    dplyr:: rename(variable_label=field_label, variable_name=field_name) %>% ### Renaming the variable name
    filter(grepl('checkbox',field_type)) %>%
    dplyr:: select(choices, variable_name,variable_label) %>% ### Selecting choices and variable names
    splitstackshape:: cSplit(.,"choices","|") %>% ### Splitting the choices and value labels
    #dplyr:: filter(!is.na(choices)) %>% ### Remove rows without any choices
    reshape:: melt(id.vars = c("variable_name"))%>% ### Reshaping the choices into long
    dplyr:: select(-c(variable))%>% ### Only have the reshape
    dplyr:: arrange(variable_name) %>% 
    filter(!is.na(value)) %>%
    group_by(variable_name) %>%
    mutate(id=1:n()) %>%
    filter(id!=1) %>%
    select(-id) %>%
    ungroup() %>%
    mutate(values=gsub("[[:punct:]]", "_",sub(',.*$','',value)),
           variable_name_1=paste(variable_name,'___',values,sep=''),
           variable_label=gsub("^[^,]*,", "", value)) %>%
    select(variable_name_1,variable_label)%>%
    dplyr:: mutate(variableLabels= paste(
      "try(data <- expss::apply_labels(data,",variable_name_1,"=\"",variable_label,"\"), silent=TRUE)",sep="")) %>%
    dplyr:: select(variableLabels)
  var_mul_script <- paste("./Scripts/",form_name, " CheckBox Variable Labels.R",
                          sep = "")
  writeLines(as.character(data_mul$variableLabels),var_mul_script)
  
  ### Combing the checkbox columns in From Redcap 
  
  replace_col_suffix <- function(x) {
    #### Extracting the name 
    var_R<- sub("_","-",stringr::str_split_fixed(x, "___", 2)[,2])
    return(var_R)
  }
  
  data<- data %>% 
    mutate_at(.vars = vars(contains("___")), .funs=funs(ifelse(.==1, deparse(substitute(.)), NA))) %>%
    mutate(across(contains('___'),replace_col_suffix))# %>%
  #select(contains('hh_like_hlth_info'))
  
  #### Extracting the checkbox columns 
  checkbox_cols <- redcap_dic %>%
    janitor:: clean_names() %>%
    ### Removing rows without valeu labels 
    filter(select_choices_or_calculations!='')%>%
    dplyr:: rename(choices=select_choices_or_calculations)%>%  ### Renaming the choices
    dplyr:: rename(variable_label=field_label, variable_name=field_name) %>% ### Renaming the variable name
    filter(grepl('checkbox',field_type)) %>%
    select(variable_name)
  checkbox_col<- c(levels(as.factor(checkbox_cols$variable_name)))

  #### This unites, shrink multiple choice columns into one 
  odkshrink<- function(ColName, data, remove = removecols) {
    ### Making sure that is ti only evaluating Mulitple choice colums 
    mul_colms<-paste(ColName,'___',sep='')
    col1<- paste(ColName,"1_vxzs",sep='')
    data_unite<- data %>%
      tidyr::unite(col1,starts_with(mul_colms),sep=' ',na.rm = TRUE, remove = remove) %>%
      mutate({{ColName}}:= paste(" ",stringr::str_squish(paste(" ",col1," ",sep='')),' ',sep='') )
    return(data_unite)
  }
  
  if (isTRUE(removecols)) {
    xv<-paste("data<-odkshrink('",checkbox_col,"',data, remove = TRUE)",sep="")
  } else {
    xv<-paste("data<-odkshrink('",checkbox_col,"',data, remove = FALSE)",sep="")
  }
  
  var_check_script <- paste("./Scripts/",form_name, " Organizing CheckBox Variable.R",
                            sep = "")
  writeLines(as.character(xv),var_check_script)
  ### Ordering the columns as they are in the disctionary  
  try(suppressWarnings(source(var_check_script, local = TRUE)), silent = T)
  order_col<- c(redcap_dic$field_name)
  #print(order_col)
  cols_on <- order_col[which(order_col %in% colnames(data) )]
  
  # data <- data[,cols_on]
  #   as.data.frame()
  
  ### Recode checkbox variable
  for ( cols in c(checkbox_col)) {
    col <- paste(cols, "___", sep = '')
    data <- data %>%
      mutate(across(starts_with(col),
                    .fns  = ~ if_else(.x != "", '1' , 
                                      if_else(get(sub("___.*", "", col)) !=  "  ",'0',
                                              NA))
      ))
    
  }
  
  data <- data %>% 
    select(
           map(cols_on, ~ c(.x,names(data)[grep(paste0("^.x|^",.x), names(data))]) [1:length(names(data)[grep(paste0("^.x|^",.x), names(data))])]
               
           ) %>% unlist, 
           everything())
  
  
  data <- data %>% as.data.frame()
  try(suppressWarnings(source(val_script, local = TRUE)), silent = T)
  try(suppressWarnings(source(var_script, local = TRUE)), silent = T)
  try(suppressWarnings(source(var_mul_script, local = TRUE)), silent = T)
  #try(suppressWarnings(source(var_check_script, local = TRUE)), silent = T)
  #print(expss::val_lab(data_human$record_id))
  ###Number of submission downlaoded 
  cat(crayon::green(paste(nrow(data), ":submissions of '", 
                          # gsub(str_sub(token, 3, 30), "********", token),
                          form_name,
                          "' project have been downloaded.", sep = "")))
  return(data)
  #return(value= list(data=data,checkbox_col =checkbox_col))
  
  
}

