
    ### Importing data function

#' Fetching Data from SurveyCTO server usin API.
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
#'                     .Renviron files to hide your passwords.

#'
#' @return
#' @export
#'
#' @examples
#' ctoimport('servername','formid','username','password')
    ctoimport<- function (servername,formid, username,password) {

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
                           httr::authenticate(username,password))

      ###Reading data using read.csv(), it makes into structured data.

      data_cto <- read.csv (text = httr::content(request, "text"))

      ###Organize the data column names, organizing them by removing special characters

      names<- sub('.*\\.', '', sub('.*\\:', '', names(data_cto)))

      ### Checking whether logins are valid
        if (grepl("html",data_cto[1])) {

          stop(paste("Invalid servername, formid, Username or password."), call. = FALSE)

        }

      ### Adding variable(s) names to the data

      colnames(data_cto)<-names

      ### Printing the number of submission(s) download form the server

      cat(crayon::green(paste(nrow(data_cto),"submissions of",formid, " form have been downloaded.",sep=' ')))

      ###Returning the data
      return(data_cto)

    }


