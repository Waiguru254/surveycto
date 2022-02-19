### Importing data function
#' Importing data from Kobo tool box (even Humanitarian Account)
#'       KoBoToolBox provides a suites of tools to collect data in challenging
#'       environment. It is free and opne source and work both online and offline.
#'
#' KoboTool Box also provides advanced feature and make it useful in
#' advanced scenarios.It provide data access through RESTAPI, which
#' automte the workflow of data colelection, processing and visualization.
#' This is possible even the project is no public, using username and
#' password authentication.
#'
#' @param servername either 'kobo.humanitarianresponse.info/' or
#'                  'kc.kobotoolbox.org' However, the current function is
#'                  largely build to stream data from kobotoolbox.org.
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

    ### Loading dplyr, which is used in this function.
    suppressMessages(library(dplyr))

    ### Check whether you are connected to the internet

    if(isFALSE(curl::has_internet())) {

        stop(paste("You are offline. Connect to the internet."), call. = FALSE)

      }

    ### Checking the validity of servername
    if (nchar(servername)!=15 & grepl("kobotoolbox",servername)) {

        stop(paste("Kindly use \"kobotoolbox.org\" as your servername"), call. = FALSE)

      }
    ###initializing the data before fetching
    request<-c() ; data_cto<-c() ## Removing the chances of previous data

    ### Fetching data from the server
    ###Creating the url from the details provided to the function (Servername and formID)

    urls<-paste("https://kf.",servername,"/assets/",formid,"/submissions/?format=json",sep='')

    ### Authenticate the access to the server, and fetching the data in form of json

    rawdata<-httr::GET(urls,httr::authenticate(username,password))

    #### Converting json data into characters

    d_content <- rawToChar(rawdata$content)
    d_content <- jsonlite::fromJSON(d_content)

    ### Checking whether logins are valid
    if (grepl("username/password",d_content[1])) {

      stop(paste("Invalid Username or password."), call. = FALSE)

    }

    ### Converting the data into dataframe for processing

    data_cto <-suppressWarnings(suppressMessages(as.data.frame(d_content)))

    ### Printing the number of submission(s) download form the server

    cat(crayon::green(paste(nrow(data_cto),"submissions of",formid, " form have been downloaded.",sep=' ')))

    #### Dropping the repeat group variables(This will be include in updated version of the funtion, going forward)

    df_new <- data_cto %>% dplyr:: select(-contains("]/")) # Dropping repeat group data.

    ### Removing the repeat group column names for re-labelling of the data.frame

    df_name<-gsub('/_','_',gsub("(\\d+)$", "_\\1",gsub('-','',names(df_new)))) ### Removing group names

    df_names<- sub('.*/','', df_name) ### Further cleaning of column names

    ###Adding column names to the processed data

    colnames(df_new)<-df_names

    ###Formatting the data for exporting
    df_new[] <- lapply(df_new, gsub, pattern='n/a', replacement='NA')

    ### Binary variables are fetched as true/ false. Converting them to binary code(0,1)
    ### to ease the process of processing and analysis.

    df_new[] <- lapply(df_new, gsub, pattern='True', replacement='1') ## for true
    df_new[] <- lapply(df_new, gsub, pattern='False', replacement='0') ## for false

    ### List of items to returned by the function.
    return(df_new)
    return(urls)
    return(df_names)

  }

