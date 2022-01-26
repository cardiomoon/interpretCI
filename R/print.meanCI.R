#' S3 method "print" for class "meanCI"
#' @param x An object of class "meanCI"
#' @param ... Further arguments
#' @importFrom purrr map2_dfc
#' @importFrom dplyr all_of
#' @export
print.meanCI=function(x,...){
     digits = getOption("digits")
     cat("\n")
     cat("call:",deparse(x$call),"\n")
     cat("method:",x$result$method[1],"\n")
     temp=attr(x,"measure")
     null.value=paste(temp,if(temp!="mean") "differences in means")
     cat("alternative hypothesis:\n   true",null.value,"is ")
     cat(switch(x$result$alternative[1], two.sided = "not equal to",
                less = "less than", greater = "greater than"))
     cat(" ",x$result$mu[1],"\n")
     cat("\nResults\n")
     if(temp=="mean") {
          cols=c("m","se","DF","lower","upper","t","p")
     } else{
          cols=c("control","test","DF","CI","t","p")
     }
     df=x$result %>% select(all_of(cols))
     res=map2_dfc(df,names(df),function(x,y){
          fmt=paste0("%.",digits,"f")
          if(y %in% c("m1","m2","m","md","lower","upper")){
               format(x, digits = digits)
          } else if(y %in% c("se","DF","t")){
               format(x, digits = max(1L, digits - 2L))
          } else if(y=="p"){
               fp <- format.pval(x, digits = max(1L, digits - 3L))
          } else{
               x
          }
     })
     print(res)
}
