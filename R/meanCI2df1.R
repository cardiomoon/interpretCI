#' Extract data from an object of class meanCI
#' @param x An object of class meanCI
#' @return A data.frame summarizing mean and confidence interval
#' @export
#' @examples
#' x=meanCI(acs,sex,age)
#' meanCI2df1(x)
meanCI2df1=function(x){
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
          # df1=data.frame(value=res[[1]])
          # df1$name=names(res)[1]
          if(length(res[[2]])==1 & is.na(res[[2]][1])){
               res[[2]]=rnorm(n=result$result$n2,mean=result$result$m2,sd=result$result$s2)
               simuldata=TRUE
          }
          mode=1
     } else if(ncol(result$data)>2){
          res=split(result$data$value,result$data$name)
          mode=2
     } else{
          res=split(result$data[[2]],result$data[[1]])
          mode=2
     }
     if(simuldata){
          name=c(x$result$control,x$result$test)
          m=c(x$result$m1,x$result$m2)
          lower=c(x$result$m1-x$result$s1,x$result$m2-x$result$s1)
          upper=c(x$result$m1+x$result$s1,x$result$m2+x$result$s1)
          no=c(x$result$n1,x$result$n2)
          df1=data.frame(name=name,m=m,lower=lower,upper=upper,no=no)
     } else{
          df1=map2_dfr(names(res),res,function(x,y){
               y=y[!is.na(y)]
               data.frame(x,m=mean(y),lower=mean(y)-sd(y),upper=mean(y)+sd(y),no=length(y))

          })
     }
     df1<-df1 %>% dplyr::filter(.data$no!=0)

     if(mode==1) {
          names(df1)[1]="name"
          df1$label=paste0(df1[["name"]],"\nN=",df1$no)
     } else{
          names(df1)[1]=xname
          df1$label=paste0(df1[[xname]],"\nN=",df1$no)
     }
     df1$x=1:nrow(df1)+0.4
     if(mode==2) df1$name=df1[[xname]]
     df1
}
