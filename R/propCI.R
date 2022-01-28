#' Calculate confidence intervals of proportion or difference between proportions
#'@param x A vector
#'@param y A vector
#'@param n,n1,n2 integer sample size
#'@param p,p1,p2,P Numeric proportion
#'@param alpha numeric confidence level
#'@param digits integer indicating the number of decimal places
#'@param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#'@importFrom stats qt pnorm
#'@importFrom dplyr as_tibble
#'@export
#'@return A list containing at least the following components:
#'\describe{
#'   \item{data}{A tibble containing raw data or a list of numeric vector}
#'   \item{result}{A data.frame consists of summary statistics}
#'   \item{call}{the matched call}
#'   \item{attr(*,"measure")}{character. One of c("prop","propdiff")}
#'}
#'#'@examples
#'propCI(acs$sex)
#'propCI(acs$sex,acs$DM)
#'propCI(n=1600,p=0.4,alpha=0.01)
#'propCI(n=100,p=0.73,P=0.8,alpha=0.01)
#'propCI(n1=400,n2=300,p1=0.4,p2=0.3,alpha=0.1)
#'propCI(n1=100,n2=200,p1=0.38,p2=0.51,alpha=0.01)
#'propCI(n1=100,n2=200,p1=0.38,p2=0.51,alpha=0.01,alternative="less")
propCI=function(x,y,n,p,n1,n2,p1,p2,P=0,alpha=0.05,digits=2,alternative="two.sided"){
     ref=2
     if(missing(y)& missing(n2)){
          # x=mtcars$am
          # alpha=0.05
          # digits=2
          # p=0.4;alpha=0.01;n=1600
             # n=100;p=0.73;P=0.8;alpha=0.01
             # digits=2;print=TRUE;alternative="two.sided"

          if(!missing(x)){

               t1=table(x)
               n=length(x)
               p=unname(prop.table(table(x))[ref])

          } else{

               x=NA
               t1=NA
          }
          if(isProvided(match.call(),"P")){
                  se=sqrt(P*(1-P)/n)
          } else{
              se=sqrt(p*(1-p)/n)
          }
          DF=n-1
          #critical=qnorm(1-alpha/2)
          critical=ifelse(alternative=="two.sided",qnorm(1-alpha/2),qnorm(1-alpha))
          ME=critical*se
          fmt=paste0("%.",digits,"f")
          lower=p-ME
          upper=p+ME
          CI=paste0(sprintf(fmt,p)," [",(1-alpha)*100,"CI ",sprintf(fmt,lower),"; ",sprintf(fmt,upper),"]")
          z=(p-P)/se
          if(alternative=="two.sided"){
                    pvalue=pnorm(-abs(z))*2
          } else if(alternative!="less"){
               pvalue=pnorm(z,lower.tail=FALSE)
          } else{
               pvalue=pnorm(z)
          }
          data=as_tibble(x=x)
          result=data.frame(alpha=alpha,n=n,df=n-1,p=p,P=P,se=se,critical=critical,ME=ME,lower=lower,upper=upper,CI=CI,z=z,pvalue=pvalue,alternative=alternative)
          res=list(data=data,result=result)
          attr(res,"measure")="prop"


     } else{

          if(!missing(x)){

               t1=table(x,y)
               n1=length(x)
               n2=length(y)
               p1=unname(t1[1,ref]/rowSums(t1)[1])
               p2=unname(t1[2,ref]/rowSums(t1)[2])
          } else{

               x=NA
               y=NA
               t1=NA
          }
          pd=p1-p2-P
          se=sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
          DF=n1+n2-2
          critical=ifelse(alternative=="two.sided",qnorm(1-alpha/2),qnorm(1-alpha))
          ME=critical*se
          fmt=paste0("%.",digits,"f")
          lower=pd-ME
          upper=pd+ME
          CI=paste0(sprintf(fmt,pd)," [",(1-alpha)*100,"CI ",sprintf(fmt,lower),"; ",sprintf(fmt,upper),"]")
          pp=(p1*n1+p2*n2)/(n1+n2)
          sep=sqrt(pp*(1-pp)*((1/n1)+(1/n2)))
          z=pd/sep
          if(alternative=="two.sided"){
               pvalue=pnorm(-abs(z))*2
          } else if(alternative!="less"){
               pvalue=pnorm(z,lower.tail=FALSE)
          } else{
               pvalue=pnorm(z)
          }
          data=as_tibble(data.frame(x=x,y=y))
          result=data.frame(alpha=alpha,p1=p1,p2=p2,n1=n1,n2=n2,DF=DF,pd=pd,se=se,critical=critical,ME=ME,lower=lower,upper=upper,CI=CI,
                            ppooled=pp,sepooled=sep,z=z,pvalue=pvalue,alternative=alternative)
          res=list(data=data,result=result)
          attr(res,"measure")="propdiff"


     }
     res$call=match.call()
     res
}

#' Calculate confidence intervals of proportion or difference between proportions in a data.frame
#'@param data A data.frame
#'@param x Character Name of a categorical column
#'@param y Character Optional. Name of another categorical column
#'@export
#'@return A list containing at least the following components:
#'\describe{
#'   \item{data}{A tibble containing raw data or a list of numeric vector}
#'   \item{result}{A data.frame consists of summary statistics}
#'   \item{call}{the matched call}
#'   \item{attr(*,"measure")}{character. One of c("prop","propdiff")}
#'}
#'@examples
#'propCI_sub(acs,"sex")
#'propCI_sub(acs,"sex","HBP")
propCI_sub=function(data,x,y=NULL){
     if(is.null(y)){
          propCI(data[[x]])
     } else{
          propCI(data[[x]],data[[y]])
     }
}
