#' The function generate proportion from a multiple-select variable
#' @param row Multiple-select variable
#' @param column Group by variable
#' @param data  data with multiple select variable
#'
#' @return
#' @export
#'
#' @examples
#' tabmmult('row','column','data','valuelabel')
#'
tabmult<- function(row, column,data) {
  suppressMessages(library(dplyr))
    dof<- data %>%
      filter(!is.na(as.character({row}))) %>%
      filter(!is.na(as.character({column})))%>%
      dplyr::select({row},{column})
    rr<-sort(as.numeric(unique(scan(text=dof[,1], what="", sep=" ",quiet =TRUE))))
    mat<- matrix(NA,length(rr),length(unique(na.omit(dof[,2])))+1)
    rownames(mat) <- rr
    dd<-c(unique(na.omit(dof[,2])),"Overall")
    row2col<-rownames(table(data[{column}]))
    colnames(mat) <- dd
    if (is.null(attr(dof[,1],"label"))){g=row} else {g=attr(dof[,1],"label")}
    for (i in dd) {
      for (j in rr) {
        mat[paste(j),paste(i)]<- paste(sum(grepl(paste(" ",j," ",sep=""),
                                                 paste(" ",dof[dof[,2]==i,][[row]]," ",sep=""))),"(",
                                       round((sum(grepl(paste(" ",j," ",sep=""),paste(" ",dof[dof[,2]==i,][[row]]," ",sep="")))/nrow(dof[dof[,2]==i & !is.na(dof[,2]),]))*100,1), "%)",sep = "")

        mat[paste(j),length(unique(na.omit(dof[,2])))+1]<-paste(sum(grepl(paste(" ",j," ",sep=""),
                                                                          paste(" ",dof[,1]," ",sep=""))),"(",round((sum(grepl(paste(" ",j," ",sep=""),
                                                                                                                               paste(" ",dof[,1]," ",sep="")))/length(is.na(dof[,1])))*100,1), "%)",sep = "")
      }
    }
   ### Using the label in the data to label the multiple select tabulation
   ### table
    xx<-expss::val_lab(dof[,1])
    cc <- rbind(names(xx), xx) %>%t()
    rownames(cc)<-NULL
    rownames(cc)<- cc[,2]
    cc<- cc[,-2]
    tabs <- merge(cc,mat, by=0, all=TRUE)
    tabs<- tabs %>%select(-c("Row.names"))%>% na.omit(Overall) %>% rename(Labels=x)
    rownames(tabs)<-NULL
    ### Ordering on asceind order
    tabs<- tabs %>% mutate(arrnge=sub("\\(.*", "", Overall))%>%arrange(desc(arrnge))
    print(tabs)
    tabs<-rbind(c(NA),tabs)
    tabs[1,1]<- expss::var_lab(data%>% select(row))

      return(as.data.frame(tabs))
    }



