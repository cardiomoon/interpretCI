#' Prepare data to plot from an object of class meanCI
#' @param x An object of class meanCI
#' @return A data.frame
#' @export
#' @examples
#' x=meanCI(acs,sex,age)
#' meanCI2df(x)
meanCI2df=function(x){
     result=x
     df=result$data
     xname=names(df)[1]
     yname=names(df)[2]
     paired=FALSE
     if(attr(result,"measure")=="paired") paired=TRUE
     longform=FALSE
     if(!is.null(attr(result,"form"))){
          longform=TRUE
          if(ncol(result$data)>2){
               if(names(df)[1]=="id"){
                    xname=names(df)[2]
                    yname=names(df)[3]
               }
          }
     }
     simuldata=FALSE
     if("list" %in% class(result$data)){
          res=result$data
          if(length(res[[1]])==1 & is.na(res[[1]][1])){
               res[[1]]=rnorm(n=result$result$n1,mean=result$result$m1,sd=result$result$s1)
               simuldata=TRUE
          }
          df1=data.frame(value=res[[1]])
          df1$name=names(res)[1]
          if(length(res[[2]])==1 & is.na(res[[2]][1])){
               res[[2]]=rnorm(n=result$result$n2,mean=result$result$m2,sd=result$result$s2)
               simuldata=TRUE
          }
          df2=data.frame(value=res[[2]])
          df2$name=names(res)[2]
          longdf=rbind(df1,df2)
          longdf$name=factor(longdf$name,levels=names(res))
          longdf$x=as.numeric(longdf$name)
          df=longdf
          mode=1
     } else{
          mode=2
     }

     if(mode==2){
          if(is.numeric(df[[xname]])) df[[xname]]=factor(df[[xname]])
          df$x=as.numeric(factor(df[[xname]]))
          df$name=df[[xname]]
          df$value=df[[yname]]
     }
     df
}
