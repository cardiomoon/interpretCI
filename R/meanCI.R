#' Calculate confidence intervals of mean or difference between means
#'@param x An object of class data.frame or vector
#'@param ... Further arguments
#' @return An object of class "meanCI" which is a list containing at least the following components:
#'\describe{
#'   \item{data}{A tibble containing raw data or a list of numeric vector}
#'   \item{result}{A data.frame consist of summary statistics}
#'   \item{call}{the matched call}
#'   \item{attr(*,"measure")}{character. One of c("mean","unpaired","paired")}
#'}
#' @examples
#' meanCI(mtcars$mpg)
#' meanCI(n=150,m=115,s=10,alpha=0.01)
#' meanCI(n=50,m=295,s=20,mu=300)
#' meanCI(n=20,m=108,s=10,mu=110,alpha=0.01,alternative="less")
#' meanCI(mtcars,am,mpg)
#' meanCI(n1=15,n2=20,m1=1000,s1=100,m2=950,s2=90,alpha=0.1)
#' meanCI(n1=500,n2=1000,m1=20,s1=3,m2=15,s2=2,alpha=0.01)
#' meanCI(n1=30,n2=25,m1=78,s1=10,m2=85,s2=15,alpha=0.10)
#' meanCI(n1=100,n2=100,m1=200,s1=40,m2=190,s2=20,mu=7,alpha=0.05,alternative="greater")
#' x=c(3.04,2.92,2.86,1.71,3.60,3.49,3.30,2.28,3.11,2.88,2.82,2.13,2.11,3.03,3.27,2.60,3.13)
#' y=c(2.56,3.47,2.65,2.77,3.26,3.00,2.70,3.20,3.39,3.00,3.19,2.58,2.98)
#' meanCI(x=x,y=y)
#' x=c(95,89,76,92,91,53,67,88,75,85,90,85,87,85,85,68,81,84,71,46,75,80)
#' y=c(90,85,73,90,90,53,68,90,78,89,95,83,83,83,82,65,79,83,60,47,77,83)
#' meanCI(x=x,y=y,paired=TRUE,alpha=0.1)
#' meanCI(10:30,1:15)
#' meanCI(acs,sex,age)
#' meanCI(iris$Sepal.Width,iris$Sepal.Length)
#' meanCI(iris$Sepal.Width,iris$Sepal.Length,paired=TRUE)
#'@export
meanCI=function(x,...)  UseMethod("meanCI")


#'@describeIn meanCI2 Default S3 method
#'@param x Numeric
#'@param ... Further arguments to be passed to meanCI2
#'@export
meanCI.default=function(x,...){
       res= meanCI2(x,...)
       res$call=match.call()
       res
}


#' Calculate confidence intervals of mean or difference between means
#' @param x A vector
#' @param y A vector
#' @param m,m1,m2 Numeric mean value of sample(s)
#' @param n,n1,n2 integer sample(s) size
#' @param s,s1,s2 Numeric standard deviation of sample(s)
#' @param mu numeric hypothesized true value of mean or mean difference
#' @param paired logical If true, difference between paired sample calculated
#' @param var.equal logical If true, pooled standard deviation is used
#' @param alpha Numeric Confidence level
#' @param digits integer indicating the number of decimal places
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @importFrom stats qt pt na.omit
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @export
#' @return An object of class "meanCI" which is a list containing at least the following components:
#'\describe{
#'   \item{data}{A tibble containing raw data or a list of numeric vector}
#'   \item{result}{A data.frame containing of summary statistics}
#'   \item{call}{the matched call}
#'   \item{attr(*,"measure")}{character. One of c("mean","unpaired","paired")}
#'}
meanCI2=function(x,y,n,m,s,n1,n2,m1,m2,s1,s2,mu=0,paired=FALSE,var.equal=FALSE,alpha=0.05,digits=2,
                alternative="two.sided"){

     if(missing(y) & missing(m2)){

          if(!missing(x)){
               x[is.na(x)]<-NULL
               n=length(x)
               m=mean(x)
               s=sd(x)
          } else{
               x=NA
          }
          se=s/sqrt(n)
          se
          DF=n-1

          if(alternative=="two.sided"){
               critical=qt(1-alpha/2,DF)
               ME=critical*se
               lower=m-ME
               upper=m+ME
          } else if(alternative=="greater"){
               critical=-qt(1-alpha,DF,lower.tail=FALSE)
               ME=critical*se
               lower=m-ME
               upper=Inf
          } else{
               critical=qt(1-alpha,DF)
               ME=critical*se
               lower=-Inf
               upper=m+ME
          }
          fmt=paste0("%.",digits,"f")
          CI=paste0(sprintf(fmt,m)," [",(1-alpha)*100,"CI ",sprintf(fmt,lower),"; ",sprintf(fmt,upper),"]")
          method="One sample t-test"
          result=data.frame(n=n,m=m,s=s,alpha=alpha,se=se,DF=DF,critical=critical,
                            ME=ME,lower=lower,upper=upper,CI=CI,alternative=alternative,method=method)

          t=(m-mu)/se
          if(alternative=="two.sided"){
                    p=pt(-abs(t),df=DF)*2
          } else if(alternative!="less"){
                   p=pt(t,df=DF,lower.tail=FALSE)
          } else{
               p=pt(t,df=DF)
          }
          result$mu=mu
          result$t=t
          result$p=p


          res=list(data=as_tibble(x=x),result=result)
          attr(res,"measure")="mean"


     } else{

          if(!paired){

               if(!missing(x)){

                    x=x[!is.na(x)]
                    m1=mean(x)
                    s1=sd(x)
                    n1=length(x)
               } else{

                    x<-NA
               }
               if(!missing(y)){

                    y=y[!is.na(y)]
                    m2=mean(y)
                    s2=sd(y)
                    n2=length(y)
               } else{

                    y<-NA
               }
               # n1=100;n2=100;m1=190;m2=200;s1=20;s2=40;alpha=0.05;d=7
               md=m1-m2
               method="Two Sample t-test"
               if(var.equal){

                         se=sqrt((((n1-1)*s1^2)+(n2-1)*s2^2)/(n1+n2-2))*sqrt(1/n1+1/n2)
                         DF=n1+n2-2
               } else{
                    method=paste("Welch",method)
                    se=sqrt(s1^2/n1+s2^2/n2)
                    DF=(s1^2/n1+s2^2/n2)^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))

               }
               if(alternative=="two.sided"){
                    critical=qt(1-alpha/2,DF)
                    ME=critical*se
                    lower=md-ME
                    upper=md+ME
               } else if(alternative=="greater"){
                    critical=qt(1-alpha,DF)
                    ME=critical*se
                    lower=md-ME
                    upper=Inf
               } else{
                    critical=qt(1-alpha,DF)
                    ME=critical*se
                    lower=-Inf
                    upper=md+ME
               }
               fmt=paste0("%.",digits,"f")
               CI=paste0(sprintf(fmt,md)," [",(1-alpha)*100,"CI ",sprintf(fmt,lower),"; ",sprintf(fmt,upper),"]")
               result=data.frame(control="x",test="y",n1=n1,n2=n2,m1=m1,m2=m2,md=md,s1=s1,s2=s2,alpha=alpha,se=se,DF=DF,
                                 critical=critical,ME=ME,lower=lower,upper=upper,CI=CI,alternative=alternative,method=method)

               t=(md-mu)/se
               if(alternative=="two.sided"){
                    p=pt(-abs(t),df=DF)*2
               } else if(alternative=="less"){
                         p=pt(t,df=DF)
               } else{
                         p=pt(t,df=DF,lower.tail=FALSE)
               }

               result$mu=mu
               result$t=t
               result$p=p

               data=list(x=x,y=y)

               res=list(data=data,result=result)
               attr(res,"measure")="unpaired"
          } else{  ## paired sample
               method="Paired t-test"
               df=data.frame(x,y)
               df=na.omit(df)
               n=nrow(df)
               m1=mean(df$x)
               m2=mean(df$y)
               md=m1-m2
               df %>% mutate(d=x-y,sum=(.data$d-mean(.data$d))^2) ->df
               sd=sqrt(sum(df$sum)/(n-1))

               se=sd/sqrt(n)
               DF=n-1
               if(alternative=="two.sided"){
                    critical=qt(1-alpha/2,DF)
                    ME=critical*se
                    lower=md-ME
                    upper=md+ME
               } else if(alternative=="greater"){
                    critical=qt(1-alpha,DF)
                    ME=critical*se
                    lower=md-ME
                    upper=Inf
               } else{
                    critical=qt(1-alpha,DF)
                    ME=critical*se
                    lower=-Inf
                    upper=md+ME
               }
               digits=2
               fmt=paste0("%.",digits,"f")
               CI=paste0(sprintf(fmt,md)," [",(1-alpha)*100,"CI ",sprintf(fmt,lower),"; ",sprintf(fmt,upper),"]")


               result=data.frame(control="x",test="y",n=n,m1=m1,m2=m2,md=md,sd=sd,se=se,alpha=alpha,DF=n-1,
                                 critical=critical,ME=ME,lower=lower,upper=upper,CI=CI,alternative=alternative,method=method)

               t=(md-mu)/se
               if(alternative=="two.sided"){
                    p=pt(-abs(t),df=DF)*2
               } else if(alternative=="less"){
                    p=pt(t,df=DF)
               } else{
                    p=pt(t,df=DF,lower.tail=FALSE)
               }

               result$mu=mu
               result$t=t
               result$p=p

               res=list(data=as_tibble(df),result=result)
               attr(res,"measure")="paired"
          }
     }
        class(res)="meanCI"
        invisible(res)

}




