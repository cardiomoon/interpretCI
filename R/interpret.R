#'Whether the arg is provided in function call
#'@param x An object of class "meanCI" or function call or character string
#'@param seek character. Default="mu"
#'@return logical
#'@export
#'@examples
#'x=meanCI(mtcars,am,mpg)
#'isProvided(x)
isProvided=function(x,seek='mu'){
        if(is.language(x)){
                temp=deparse(x)
        } else if(is.character(x)){
                temp=x
        } else{
                temp=deparse(x$call)
        }
        temp=gsub(" ","",temp)
        temp=gsub(".*\\(|\\)","",temp)
        temp=unlist(strsplit(temp,","))
        seek=paste0(seek,"=")
        ifelse(length(grep(seek,temp))==0,FALSE,TRUE)
}

#' Interpret an object of meanCI
#'
#' Interpret an object of meanCI. Render appropriate rmarkdown file to html file and show RStudio viewer
#' or browser.
#' @param x An object of class "meanCI"
#' @param viewer Character One of c("rstudio","browser")
#' @importFrom rmarkdown render
#' @return No return value, called for side effect
#' @export
#' @examples
#' x=meanCI(mtcars$mpg)
#' x=meanCI(mtcars,mpg,mu=23)
#' x=meanCI(n=150,m=115,s=10,alpha=0.01)
#' x=meanCI(n=50,m=295,s=20,mu=300)
#' x= meanCI(n=20,m=108,s=10,mu=110,alpha=0.01,alternative="less")
#' x=meanCI(n1=500,n2=1000,m1=20,s1=3,m2=15,s2=2,alpha=0.01)
#' x=meanCI(n1=15,n2=20,m1=1000,s1=100,m2=950,s2=90,alpha=0.1)
#' x=meanCI(n1=30,n2=25,m1=78,s1=10,m2=85,s2=15,mu=0,alpha=0.10)
#' x=meanCI(n1=100,n2=100,m1=200,s1=40,m2=190,s2=20,mu=7,alpha=0.05,alternative="greater")
#' x1=c(95,89,76,92,91,53,67,88,75,85,90,85,87,85,85,68,81,84,71,46,75,80)
#' y1=c(90,85,73,90,90,53,68,90,78,89,95,83,83,83,82,65,79,83,60,47,77,83)
#' x=meanCI(x=x1,y=y1,paired=TRUE,alpha=0.1,mu=0)
#' x=propCI(n=1600,p=0.4,alpha=0.01)
#' x=propCI(n=100,p=0.73,P=0.8,alpha=0.01)
#' x=propCI(n=100,p=0.73,P=0.8,alpha=0.05,alternative="greater")
#' x=propCI(n1=100,n2=200,p1=0.38,p2=0.51,alpha=0.01)
#' x=propCI(n1=150,n2=100,p1=0.71,p2=0.63,P=0,alternative="greater")
#' \dontrun{
#' interpret(x)
#' interpret(x,"browser")
#' }
interpret=function(x, viewer="rstudio"){
     fname=attr(x,"measure")
     if(fname %in% c("prop","propdiff")){
            if(isProvided(x,"P")) fname=paste0(fname,"2")
     } else if(isProvided(x)) fname=paste0(fname,"2")

     rmdfile=paste0(path.package("interpretCI", quiet = FALSE),paste0("/",fname,".Rmd"))
     rmarkdown::render(rmdfile,output_dir=".",params=list(result=x))
     # if(viewer=="rstudio") {
     rstudio_viewer(paste0(fname,".html"),viewer=viewer)
     # } else{
     #      utils::browseURL(paste0(fname,".html"))
     # }
     file.remove(paste0(fname,".html"))

}

#' Show html file in RStudio viewer or browser
#' @param file_name character file name
#' @param file_path  character file path
#' @param viewer Character One of c("rstudio","browser")
#' @importFrom rstudioapi viewer
#' @return No return value, called for side effect
#' @export
rstudio_viewer <- function(file_name, file_path = NULL,viewer="rstudio") {
     temporary_file <- tempfile()
     dir.create(temporary_file)
     html_file <- file.path(temporary_file, file_name)
     current_path <- ifelse(is.null(file_path),
                            getwd(),
                            path.expand(file_path))
     file.copy(file.path(current_path, file_name), html_file)
     if(viewer=="rstudio") {
     rstudioapi::viewer(html_file)
     } else{
     utils::browseURL(html_file)
     }
}
