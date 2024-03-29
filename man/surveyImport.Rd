% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveyImport.R
\name{surveyImport}
\alias{surveyImport}
\title{Import the survey Form Definition and Generates the Labels dateframes}
\usage{
surveyImport(
  servername,
  formid,
  username,
  password,
  language = "",
  dataName = NULL
)
}
\arguments{
\item{servername}{Servername where the data is hosted}

\item{formid}{Form ID is the data to be streamed}

\item{username}{Server username}

\item{password}{Sever password}

\item{language}{XLS form language to add the labels}

\item{dataName}{Name of the datset to be stored in R memory.}
}
\value{

}
\description{
The function stream survey Form defination and generates
              a list of datasets; choices survey, variable type,and
               variable name against the labels.
}
\examples{
#NOT RUN
 dd<-surveyImport(Sys.getenv("servername"),'VAVS_CRF_06',Sys.getenv("username"),Sys.getenv("password"))
}
