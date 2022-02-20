#' Fetching data from Project's RedCap server using API
#'
#' @param token This is the unique identifier of projects in RedCap.
#'              This function uses the token to fetch data using API.
#' @param url   This is the link to the Redcap API. An example is
#'              'https://redcap.xxxxxxxx/api/'
#'
#'
#' @return
#' @export
#'
#' @examples
#' ###NOT RUN
#' redimport('token','url')
#'
  redimport<- function (token,url) {

    ### Check whether you are connected to the internet

    if(isFALSE(curl::has_internet())) {

      stop(paste("You are offline. Connect to the internet."), call. = FALSE)

    }

    ### Checking whether the token is valid, it should have 32 characters.

    if (nchar(token)!=32) {

      stop(paste("Kindly enter a correct project token."), call. = FALSE)

    }

    ### This make sure the columns are not collapsed and all the variables are imported

    records_collapsed           <- NULL
    fields_collapsed            <- NULL

    ### Creating the components for fetching

    post_body <- list(
                      token           = token,
                      content         = 'record',
                      format          = 'csv',
                      type            = 'flat',
                      records         = records_collapsed,
                      fields          = fields_collapsed
    )

    ### Fetching the data

    raw_text <- httr::POST( url = url,body= post_body)

    ### Converting the data from json to structured data.frame.

    data_cto <- read.csv (text = httr::content(raw_text, "text"))

    ### Printing the number of submission(s) download form the server

    cat(crayon::green(paste(nrow(data_cto),"submissions of",gsub(str_sub(token,8,24),"********",token), " proect have been downloaded.",sep=' ')))


    ###Returning the data to r
    return(data_cto)

  }
