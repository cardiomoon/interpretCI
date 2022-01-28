#'@describeIn meanCI_sub S3 method for data.frame
#'@export
meanCI.data.frame=function(x,...){
     res=meanCI_sub(data=x,...)
     res$call=match.call()
     res
}


utils::globalVariables("where")

#' Calculate confidence intervals of mean or difference between means in a data.frame
#' @param data A data.frame
#' @param x Name of a categorical or numeric column. If !missing(y), name of continuous variable
#' @param y Name of a numeric column
#' @param group Name of categorical column
#' @param paired logical
#' @param idx A vector containing factors or strings in the x columns. These must be quoted (ie. surrounded by quotation marks).
#' The first element will be the control group, so all differences will be computed for every other group and this first group.
#' @param ... Further arguments to be passed to meanCI
#' @importFrom purrr map_dfr map2_dfr
#' @importFrom dplyr select `%>%` everything as_tibble rename arrange
#' @importFrom rlang .data enexpr
#' @import utils
#' @return An object of class "meanCI" which is a list containing at least the following components:
#'\describe{
#'   \item{data}{A tibble containing raw data or a list of numeric vector}
#'   \item{result}{A data.frame consists of summary statistics}
#'   \item{call}{the matched call}
#'   \item{attr(*,"measure")}{character. One of c("mean","unpaired","paired")}
#'}
#' @export
#' @examples
#' meanCI(acs,age)
#' meanCI(acs,sex,age)
#' meanCI(acs,Dx,age)
#' acs %>% select(age) %>% meanCI()
#' acs %>% select(sex,age) %>% meanCI()
#' meanCI(iris,Species,Sepal.Length)
#' meanCI(iris,Sepal.Width,Sepal.Length,paired=TRUE)
#' meanCI(iris,Sepal.Length,Sepal.Width)
#' iris %>% select(starts_with("Petal")) %>% meanCI(paired=TRUE)
#' iris %>% meanCI(paired=TRUE)
#' meanCI(acs,sex,age,Dx,mu=10)
#' acs %>% select(sex,TC,TG,HDLC) %>% meanCI(group=sex)
#' acs %>% select(sex,TC,TG,HDLC) %>% meanCI(sex)
#' iris %>% select(Species,starts_with("Sepal")) %>% meanCI(Species)
#' iris %>% select(Species,starts_with("Sepal")) %>% meanCI(group=Species)
meanCI_sub=function(data=data,x,y,group,paired=FALSE,idx=NULL,...){

        if(!missing(group)){

             if(missing(x)){
                  groupname=as.character(enexpr(group))
                  dflist=split(data,data[[groupname]])
                  result=map2_dfr(dflist,names(dflist),function(i,j){
                       temp=meanCI(i,paired=paired,idx=idx,...)$result
                       temp$control=paste0(j,".",temp$control)
                       temp$test=paste0(j,".",temp$test)
                       temp
                  })

                  df1 <-data %>% select(where(is.mynumeric),{{group}}) %>%
                       pivot_longer(cols=-{{group}}) %>%
                       mutate(name=paste0(.data[[groupname]],".",.data$name)) %>%
                       select(.data$name,.data$value)

                  res<-list(data=df1,
                            result=result)
                  res$call=match.call()
                  class(res)="meanCI"
                  attr(res, "measure")=ifelse(paired,"paired","unpaired")
                  attr(res,"form")="long"
                  return(res)

             } else{

                xname=as.character(enexpr(x))
                yname=as.character(enexpr(y))
                groupname=as.character(enexpr(group))

                dflist=split(data,data[[groupname]])
                result=map2_dfr(dflist,names(dflist),function(i,j){
                        temp=meanCI(i,!!{{xname}},!!{{yname}},paired=paired,idx=idx,...)$result
                        temp$control=paste0(j,".",temp$control)
                        temp$test=paste0(j,".",temp$test)
                        temp
                })

                data %>% select({{x}},{{y}},{{group}}) %>%
                        mutate(name=paste0({{group}},".",{{x}})) %>%
                        rename(value={{y}}) %>%
                        select(.data$name,.data$value) ->df1

                x=list(data=df1,
                       result=result)
                x$call=match.call()
                class(x)="meanCI"
                attr(x, "measure")="unpaired"
                attr(x,"form")="long"
                return(x)
             }
        }

     if(!is.null(idx)){
             if(missing(x)){
                     data=data[unlist(idx)]
                     data$id=1:nrow(data)
                     longdf=data %>% pivot_longer(cols=-.data$id)
                     longdf=longdf %>% select(.data$name,.data$value,.data$id)
                     longdf$name=factor(longdf$name,levels=unlist(idx))

                     result=map_dfr(idx,function(x){
                             meanCI(data[x],paired=paired,...)$result
                     })
                     result

                     x=list(data=longdf,result=result)
                     x$call=match.call()
                     class(x)="meanCI"
                     attr(x, "measure")= ifelse(paired,"paired","unpaired")
                     attr(x,"form")="long"
                     return(x)
             } else{
                     xname=as.character(enexpr(x))
                     yname=as.character(enexpr(y))

                     result=map_dfr(idx,function(y){
                             df=data %>%
                                     dplyr::filter({{x}} %in% y)
                             df[[xname]]=factor(df[[xname]],levels=y)
                             meanCI(df,!!{{xname}},!!{{yname}})$result
                     })
                     result
                     data=data %>% select({{x}},{{y}}) %>%
                             filter({{x}} %in% unlist(idx))

                     no=length(unlist(idx))
                     data[[xname]]=factor(data[[xname]],levels=unlist(idx))
                     if(paired) data$id=rep(1:(nrow(data)/no),no)
                     x=list(data=data,result=result)
                     class(x)="meanCI"
                     attr(x, "measure")= ifelse(paired,"paired","unpaired")
                     attr(x,"form")="long"
                     x$call=match.call()
                     return(x)

             }
     }
     if(missing(x)){

          if((ncol(data)==1)&(is.numeric(data[[1]]))){
               result=meanCI(data[[1]],paired=paired,...)
               df=result$result
               df$name=names(data)[1]
               df %>% select(.data$name,everything()) -> df
               res=list(data=as_tibble(data[x]),result=df)
               attr(res,"measure")="mean"
          } else if(ncol(data)==2){
               mysum=sum(sapply(data,is.mynumeric))
               if(mysum==2){
                    res=meanCI(data[[1]],data[[2]],paired=paired,...)
                    names(res$data)[1:2]=names(data)
                    res$result$control=names(data)[1]
                    res$result$test=names(data)[2]
                    attr(res,"measure")=ifelse(paired,"paired","unpaired")
               } else if(mysum==1){
                    if(is.mynumeric(data[[1]])){
                         x=names(data)[2]
                         y=names(data)[1]
                    } else{
                         x=names(data)[1]
                         y=names(data)[2]
                    }

                    dflist=split(data[[y]],data[[x]])
                    no=length(dflist)
                    result=list()
                    for(i in 2:length(dflist)){
                         result[[i-1]]=meanCI(dflist[[1]],dflist[[i]],paired=paired,...)
                    }
                    df=map_dfr(result,~.$result)

                    names(dflist)[2:no]
                    df$control=names(dflist)[1]
                    df$test=names(dflist)[2:no]
                    df %>% select(.data$control,.data$test,everything()) -> df
                    df
                    res=list(data=as_tibble(data[c(x,y)]),result=df)
                    attr(res,"measure")=ifelse(paired,"paired","unpaired")
               } else{
                    cat("There is no numeric data!")
                    return()
               }
          } else{
               cols=which(sapply(data,is.mynumeric))
               data=data[cols]
               data$id=1:nrow(data)
               longdf=pivot_longer(data=data,cols=-.data$id)
               longdf
               x="name"
               y="value"
               longdf$name=factor(longdf$name,levels=names(data))
               longdf
               dflist=split(longdf$value,longdf$name)
               dflist[["id"]]<-NULL
               dflist
               no=length(dflist)
               result=list()
               for(i in 2:length(dflist)){
                    result[[i-1]]=meanCI(dflist[[1]],dflist[[i]],paired=paired,...)
               }
               df=map_dfr(result,~.$result)

               names(dflist)[2:no]
               df$control=names(dflist)[1]
               df$test=names(dflist)[2:no]
               df %>% select(.data$control,.data$test,everything()) -> df
               df
               res=list(data=as_tibble(longdf),result=df)
               attr(res,"form")="long"
               attr(res,"measure")=ifelse(paired,"paired","unpaired")

          }

     } else if(missing(y)){
          xname=as.character(enexpr(x))

          if(!is.numeric(data[[xname]])){

               xname=as.character(enexpr(x))

               temp=names(which(unlist(lapply(data,is.numeric))))

               dflist=split(data,data[[xname]])

               result=map_dfr(temp,function(y){
                    temp=meanCI(data,!!{{xname}},!!{{y}})$result
                    temp$control=paste0(y,".",temp$control)
                    temp$test=paste0(y,".",temp$test)
                    temp

               })


               df1 <-data %>% select(where(is.numeric),{{x}}) %>%
                    pivot_longer(cols=-{{x}}) %>%
                    mutate(name1=paste0(.data$name,".",.data[[xname]])) %>%
                    arrange(.data$name) %>%
                    select(.data$name1,.data$value) %>%
                    rename(name=.data$name1)


               res<-list(data=df1,
                       result=result)
               res$call=match.call()
               class(res)="meanCI"
               attr(res, "measure")=ifelse(paired,"paired","unpaired")
               attr(res,"form")="long"


          } else{

          result=meanCI(data[[xname]],paired=paired,...)

          df=result$result
          df$name=xname
          df %>% select(.data$name,everything()) -> df
          res=list(data=as_tibble(data[xname]),result=df)
          attr(res,"measure")="mean"
          }

     } else{
             x=as.character(enexpr(x))
             y=as.character(enexpr(y))
          # if(paired){
          #      res=meanCI(data[[x]],data[[y]],paired=paired,...)
          #      names(res$data)[1:2]=c(x,y)
          # } else
          if(is.mynumeric(data[[x]]) & is.mynumeric(data[[y]])){
               res=meanCI(data[[x]],data[[y]],paired=paired,...)
               names(res$data)[1:2]=c(x,y)
               res$result$control=x
               res$result$test=y
          } else {
               dflist=split(data[[y]],data[[x]])
               no=length(dflist)
               result=list()
               for(i in 2:length(dflist)){
                    result[[i-1]]=meanCI(dflist[[1]],dflist[[i]],paired=paired,...)
               }
               df=map_dfr(result,~.$result)

               names(dflist)[2:no]
               df$control=names(dflist)[1]
               df$test=names(dflist)[2:no]
               df %>% select(.data$control,.data$test,everything()) -> df
               df
               data=as_tibble(data[c(x,y)])
               if(paired) data$id=rep(1:(nrow(data)/no),no)
               res=list(data=data,result=df)
               attr(res,"form")="long"
          }

          attr(res,"measure")=ifelse(paired,"paired","unpaired")
     }
     res[["call"]]=match.call()
     class(res)="meanCI"
     res
}

