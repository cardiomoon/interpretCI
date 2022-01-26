#' Convert numeric to string with uppercase first letter
#'
#' Convert numeric to string with uppercase first letter
#' @param x A numeric
#' @importFrom english english
#' @return A string
#' @export
English=function(x){
     temp=english(x)
     first=substring(temp,1,1)
     last=substring(temp,2)
     paste0(toupper(first),last)
}


#' @importFrom english english
#' @export
english::english
