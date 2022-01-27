#' Convert numeric to string with uppercase first letter
#' @param x A numeric
#' @param digits integer indicating the number of decimal places
#' @return A string
#' @export
#' @examples
#' English(40)
#' English(13.1)
English=function(x,digits=2){
     temp=english2(x,digits=digits)
     first=substring(temp,1,1)
     last=substring(temp,2)
     paste0(toupper(first),last)
}


#' Convert numeric to string
#'
#' @param x A numeric
#' @param digits integer indicating the number of decimal places
#' @return A character string
#' @importFrom english english
#' @export
#' @examples
#' english2(45)
#' english2(12.34)
english2=function(x,digits=2){
     x=as.character(round(x,digits))
     if(length(grep(".",x,fixed=TRUE))==0){
          result=english(as.integer(x))
     } else{
          temp=unlist(strsplit(x,".",fixed=TRUE))
          temp1=paste(english(as.integer(temp[1])),"point")
          temp2=paste(english(as.integer(unlist(strsplit(temp[2],"")))),collapse=" ")
          result=paste(temp1,temp2)
     }
     result
}
