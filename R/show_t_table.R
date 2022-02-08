#' Show t-value table
#'
#' Show t-value table
#' @param DF Numeric degree of freedom
#' @param t Numeric vector of quantile
#' @param p Numeric probability
#' @param alternative Character One of c("two.sided","greater","less")
#' @importFrom flextable flextable highlight autofit
#' @importFrom purrr map_dfc
#' @return An object of class "flextable"
#' @export
#' @examples
#' show_t_table()
#' show_t_table(t=1.4)
#' show_t_table(DF=10)
show_t_table=function(DF=20,t=NULL,p=0.05,alternative="two.sided"){
      # DF=20;t=1.5;alternative="two.sided"
     DF=round(DF)
     if(!is.null(t)){
          if(alternative=="two.sided"){
               p=pt(-abs(t),df=DF)*2
          } else if(alternative=="less"){
               p=pt(t,df=DF)
          } else{
               p=pt(t,df=DF,lower.tail=FALSE)
          }
     }
     if(alternative=="two.sided") p=p/2
     if(is.null(t)){
     pp=c(0.4,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
     pp=unique(c(pp,p))
     pp=sort(pp,decreasing=TRUE)
     df=max(1,DF-2):(max(1,DF-2)+3)
     suppressMessages(res<-map_dfr(df,function(x){
          map_dfc(pp,~sprintf("%.3f",qt(.,x)))
     }))
     colnames(res)=sprintf("%.03f",pp)
     res$df=df
     } else{
          pp=c(0.4,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
          tt=map_dbl(pp,~qt(.,df=DF))
          tt=sort(unique(c(tt,-abs(t))),decreasing=TRUE)
          pp=map_dbl(tt,~pt(.,df=DF))
          df=max(1,DF-2):(max(1,DF-2)+3)
          suppressMessages(res<-map_dfr(df,function(x){
               map_dfc(pp,~sprintf("%.3f",qt(.,x)))
          }))
          # pp=unique(c(pp,p))
          # pp=sort(pp,decreasing=TRUE)
          res
          colnames(res)=sprintf("%.03f",pp)
          res$df=df
     }
     myrow=which(res$df==DF)
     res<-res %>% select(.data$df,everything())
     ft<-flextable(res)%>%
          align(i=1,align="center",part="header") %>%
          align(align="center",part="body") %>% autofit()
      if(is.null(t)) {
           ft=highlight(ft,i=myrow,j=grep(p,colnames(res)))
      } else{
           ft=highlight(ft,i=myrow,j=grep(t,res))
      }
      ft

}

#' Show z-value table
#'
#' Show z-value table
#' @param z Numeric vector of quantile
#' @param p Numeric probability
#' @param alternative Character One of c("two.sided","greater","less")
#' @importFrom flextable flextable highlight autofit
#' @return An object of class "flextable"
#' @export
#' @importFrom flextable flextable align highlight autofit
#' @examples
#' show_z_table()
#' show_z_table(z=1.4)
#' show_z_table(z=-1.39234)
#' show_z_table(p=0.160)
show_z_table=function(z=NULL,p=0.05,alternative="two.sided"){

       # z=NULL;p=0.05;alternative="two.sided"
     if(!is.null(z)) {
          if(alternative=="two.sided") {
               p=pnorm(q=-abs(z))*2
          } else if(alternative!="less"){
               p=pnorm(z,lower.tail=FALSE)
          } else{
               p=pnorm(z)
          }
     }
     if(alternative=="two.sided") p=p/2
     if(is.null(z)){
     pp=c(0.4,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
     pp=unique(c(pp,p))
     pp=sort(pp,decreasing=TRUE)
     suppressMessages(res<-map_dfc(pp,~sprintf("%.3f",qnorm(.))))
     colnames(res)=sprintf("%.03f",pp*ifelse(alternative=="two.sided",2,1))
     res$p="z"
     res<-res %>% select(.data$p,everything())

     } else{
          pp=c(0.4,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
          suppressMessages(res<-map_dbl(pp,~qnorm(.)))
          res
          zz=sort(unique(c(res,-abs(z))))
          zz
          suppressMessages(res<-map_dfc(zz,~sprintf("%.3f",pnorm(.)*ifelse(alternative=="two.sided",2,1))))
          res
          colnames(res)=sprintf("%.03f",zz)
          res$z="p"

          res<-res %>% select(.data$z,everything())

     }

     ft=flextable(res)%>%
          align(i=1,align="center",part="header") %>%
          align(align="center",part="body") %>%
          autofit()

     if(!is.null(z)) ft=highlight(ft,j=grep(sprintf("%.3f",z),colnames(res)),part="all")
     else {
          if(alternative=="two.sided") p=p*2
          ft=highlight(ft,j=grep(sprintf("%.3f",p),colnames(res)),part="all")
     }
     ft
}

#' Show chisquare table
#'
#' Show chisquare table
#' @param DF Numeric degree of freedom
#' @param x2 Numeric vector of chi-square value
#' @param p Numeric probability
#' @importFrom flextable flextable highlight autofit
#' @importFrom purrr map_dfc map_dbl
#' @return An object of class "flextable"
#' @export
#' @examples
#' show_x2_table(DF=2,x2=1.5)
#' show_x2_table(p=0.05)
show_x2_table=function(DF=1,x2=NULL,p=0.05){
        # DF=1;x2=1.5
        DF=round(DF)
        if(!is.null(x2)) {
             pp=c(0.995,0.990,0.975,0.950,0.90,0.750,0.5,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
             xx<-map_dbl(pp,~qchisq(df=DF,.,lower.tail = FALSE))
             xx=sort(unique(c(xx,x2)))
             pp=map_dbl(xx,~pchisq(df=DF,.,lower.tail = FALSE))
             df=max(1,DF-2):(max(1,DF-2)+4)
             suppressMessages(res<-map_dfr(df,function(x){
                  map_dfc(pp,~sprintf("%.03f",qchisq(.,x,lower.tail = FALSE)))
             }))
             colnames(res)=sprintf("%.03f",pp)
             res$df=df

        } else{


        pp=c(0.995,0.990,0.975,0.950,0.90,0.750,0.5,0.25,0.10,0.05,0.025,0.01,0.005,0.001)
        pp=unique(c(pp,p))
        pp=sort(pp,decreasing=TRUE)
        df=max(1,DF-2):(max(1,DF-2)+4)
        suppressMessages(res<-map_dfr(df,function(x){
                map_dfc(pp,~sprintf("%.03f",qchisq(.,x,lower.tail = FALSE)))
        }))
        colnames(res)=sprintf("%.03f",pp)
        res$df=df
        }
        myrow=which(res$df==DF)
        res<-res %>% select(.data$df,everything())
        ft=flextable(res)%>%
                align(i=1,align="center",part="header") %>%
                align(align="center",part="body") %>% autofit()

        if(is.null(x2)){
           ft<- highlight(ft,i=myrow,j=grep(p,colnames(res)))
        } else{
             ft<- highlight(ft,i=myrow,j=grep(x2,res[myrow,]))
        }
        ft
}
