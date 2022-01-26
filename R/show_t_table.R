#' Show t-value table
#'
#' Show t-value table
#' @param DF Numeric degree of freedom
#' @param p Numeric probability
#' @param alternative Character One of c("two.sided","greater","less")
#' @importFrom flextable flextable highlight autofit
#' @importFrom purrr map_dfc
#' @return An object of class "flextable"
#' @export
#' @examples
#' show_t_table()
show_t_table=function(DF=20,p=0.05,alternative="two.sided"){
     DF=round(DF)
     if(alternative=="two.sided") p=p/2
     pp=c(0.4,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
     pp=unique(c(pp,p))
     pp=sort(pp,decreasing=TRUE)
     df=max(1,DF-2):(max(1,DF-2)+3)
     suppressMessages(res<-map_dfr(df,function(x){
          map_dfc(pp,~sprintf("%.3f",qt(.,x)))
     }))
     colnames(res)=pp
     res$df=df
     myrow=which(res$df==DF)
     res<-res %>% select(.data$df,everything())
     flextable(res)%>%
          align(i=1,align="center",part="header") %>%
          align(align="center",part="body") %>%
          highlight(i=myrow,j=grep(p,colnames(res))) %>%
          autofit()
}

#' Show z-value table
#'
#' Show z-value table
#' @param p Numeric probability
#' @param alternative Character One of c("two.sided","greater","less")
#' @importFrom flextable flextable highlight autofit
#' @return An object of class "flextable"
#' @export
#' @importFrom flextable flextable align highlight autofit
#' @examples
#' show_z_table()
#' show_z_table(p=0.01)
show_z_table=function(p=0.05,alternative="two.sided"){
     if(alternative=="two.sided") p=p/2
     pp=c(0.4,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
     pp=unique(c(pp,p))
     pp=sort(pp,decreasing=TRUE)
     suppressMessages(res<-map_dfc(pp,~sprintf("%.3f",qnorm(.))))
     colnames(res)=pp
     res$alpha="z"
     res<-res %>% select(.data$alpha,everything())
     flextable(res)%>%
          align(i=1,align="center",part="header") %>%
          align(align="center",part="body") %>%
          highlight(j=grep(p,colnames(res))) %>%
          autofit()
}
