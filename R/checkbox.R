### Checkbox or Multiple choice Tabulation
#'
# Tabulation of checkbox or multiple choice columns 
#'
#'
#' @param column A multiple choice or checkbox column 
#'
#' @param decimal  Number of decimal point for the percentages
#'
#' @return
#' @export
#'
#' @examples
#' ###Not run
#' checkbox(column,decimal)
#'  
#'  

checkbox<- function(column,decimal=1) {
suppressWarnings(suppressPackageStartupMessages({
  library(knitr)
  library(dplyr)
}))
### Make R to evaluate the column name with dollar  sign

col_name <- deparse(substitute(column))
### Extract the value label as attched with expss package
labs<- as.data.frame(expss::val_lab(column))
### Convert the rownames into column one 

vals_b<- cbind(row.names(labs),labs)
### Remove the rownames 
row.names(vals_b)<- NULL

### Add the column labels
colnames(vals_b)<- c(col_name, 'Values_choice')
### Get the number of responses that had responses on the selected question
NN<- sum(ifelse(column!="  ",1,0))
### Loop through the value labels are indcated in the value label box

for (v in c(1:nrow(vals_b))) {
  ### Adding the number of times a valeu appear ina column
  vals_b[v,'N']<- sum(ifelse(grepl(paste(' ',vals_b[v,'Values_choice'],' ',sep=''),column),1,0),na.rm=TRUE)
  ### Divides the frequency with the N to get percentage
  vals_b[v,'`%`']<- paste(round(vals_b[v,'Freq']/sum(ifelse(column!="  ",1,0))*100,decimal),"%", sep='')
}

### Remove the value label column
tables<- vals_b %>% dplyr:: select(-c('Values_choice'))
### remove the rownames

rownames(tables)<- NULL

### Gnerate a knitr table , just to look nice
table<- knitr::kable(tables, "rst")
### Return the table

return(table)
}
