### Importing data function
#' Importing data from Kobo tool box (even Humanitarian Account)
#' KoBoToolBox provides a suites of tools to collect data in challenging
#' environment. It is free and opne source and work both online and offline.
#'
#' KoboTool Box also provides advanced feature and make it useful in
#' advanced scenarios.It provide data access through RESTAPI, which
#' automte the workflow of data colelection, processing and visualization.
#' This is possible even the project is no public, using username and
#' password authentication.
#'
#' @param servername either 'kobo.humanitarianresponse.info/' or
#'                  'kc.kobotoolbox.org' However, the current function is
#'                  build majorly to stream data from kobotoolbox.org.
#'
#' @param formid  To download data, a unique identifier of the form is needed
#'                   to construct a URL used to fetch data. The fetch data
#'                   from a given form login to the server and click the form
#'                   and navigate the the form tab. From the link, the text
#'                   between forward slash(/)(after "forms" and before "landing").
#'                   For example,
#'                  https://kf.kobotoolbox.org/#/forms/aTxwr9Fg4ouTYnRN5tHq2z/landing
#'                  formid='aTxwr9Fg4ouTYnRN5tHq2z'
#' @param username your username
#' @param password you password
#'
#' @return
#' @export
#'
#' @examples
#' ###Not run
#' data<-koboimport(servername,form_id,username,password)
#'
  koboimport<- function (servername,formid, username,password) {
    suppressMessages(library(dplyr))
    request<-c() ; data_cto<-c() ## Removing the chances of previous data
    urls<-paste("https://kf.",servername,"/assets/",formid,"/submissions/?format=json",sep='')
    print(urls)
    rawdata<-httr::GET(urls,httr::authenticate(username,password))
    d_content <- rawToChar(rawdata$content)
    d_content <- jsonlite::fromJSON(d_content)
    data_cto <-suppressWarnings(suppressMessages(as.data.frame(d_content)))
    print(paste(nrow(data_cto),"of",formid, "have been downloaded.",sep=' '))
    df_new <- data_cto %>% dplyr:: select(-contains("]/")) # Dropping repeat group data.
    df_names<-gsub('/_','_',gsub("(\\d+)$", "_\\1",gsub('-','',names(df_new)))) ### Removing group names
    df_namesd<- sub('.*/','', df_names) ### Further cleaning of column names
    colnames(df_new)<-df_namesd
    ###Formatting the data
    df_new[] <- lapply(df_new, gsub, pattern='n/a', replacement='NA')
    df_new[] <- lapply(df_new, gsub, pattern='True', replacement='1')
    df_new[] <- lapply(df_new, gsub, pattern='False', replacement='0')
    return(df_new)
  }


  #data<- koboimport('kobotoolbox.org','a595eJvpxXHATXpJqR8GdM','lytons_analytica','Nyakingei#52#')
