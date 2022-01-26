#' Draw t distribution curve
#' @param DF numeric degree of freedom
#' @param t numeric t value
#' @param p numeric p value
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less".
#' @importFrom dplyr near
#' @return A ggplot
#' @export
#' @examples
#' draw_t(DF=30)
#' draw_t(DF=20,t=2.5)
#' draw_t(DF=49,t=1.77)
#' draw_t(DF=49,p=0.005)
#' draw_t(DF=19,t=-0.894,alternative="less")
#' draw_t(DF=146,t=0.67,alternative="greater")
draw_t=function(DF=50,t=NULL,p=0.05,alternative="two.sided"){
       # DF=19;t=2;alternative="greater"
      if(!is.null(t)) {
          if(alternative=="two.sided") {
               p=pt(-abs(t),DF)
               p=p*2
          } else if(alternative=="less"){
               p=pt(t,DF)
          } else{
               p=pt(t,DF,lower.tail=FALSE)
               p
          }

          showp=TRUE
     } else{
          showp=FALSE
     }
     x=seq(qt(p=0.0001,df=DF),qt(p=0.9999,df=DF),length=100)
     if(near(p,1)){
          x[1]=-Inf
     } else if(near(p,0)){
          x[100]=Inf
     }

     y=dt(x,df=DF)
     data=data.frame(x=x,y=y)
     data=rbind(data,data[100,],data[100,])
     data$y[101:102]=0
     data$x[102]=data$x[1]


     if(alternative=="two.sided"){

          if(near(p,0)){
             x1=-Inf
             x2=Inf
          } else if(near(p,1)){
               x1=-Inf
               x2=Inf
          } else{
               x1=seq(qt(p=0.0001,df=DF),qt(p=p/2,df=DF),length=50)
               x2=seq(qt(p=1-p/2,df=DF),qt(p=0.9999,df=DF),length=50)
          }
          y1=dt(x1,df=DF)
          y2=dt(x2,df=DF)
          data2=data.frame(x=x1,y=y1)
          if(nrow(data2)==50){
               data2=rbind(data2,data2[50,],data2[50,])
               data2$y[51:52]=0
               data2$x[52]=data2$x[1]
          }
          data3=data.frame(x=x2,y=y2)
          if(nrow(data3)==50){
               data3=rbind(data3,data3[50,],data3[50,])
               data3$y[51:52]=0
               data3$x[52]=data3$x[1]
          }

     } else if(alternative=="less"){
             if(near(p,1)){
                 x1=seq(qt(p=0.0001,df=DF),qt(p=0.9999,df=DF),length=50)
                 x1[50]=-Inf
             } else if(near(p,0)){
                  x1=Inf
             }else{
                 x1=seq(min(qt(p=0.0001,df=DF),qt(p=p,df=DF)),qt(p=p,df=DF),length=50)
             }
             y1=dt(x1,df=DF)
             data2=data.frame(x=x1,y=y1)
             if(nrow(data2)==50){
                    data2=rbind(data2,data2[50,],data2[50,])
                    data2$y[51:52]=0
                    data2$x[52]=data2$x[1]
             }
     } else{

             if(near(p,1)){
                  x2=seq(qt(p=0.0001,df=DF),qt(p=0.9999,df=DF),length=50)
                  x2[1]=-Inf
             } else if(near(p,0)){
                  x2=Inf
             } else{
                 x2=seq(qt(p=1-p,df=DF),max(qt(p=1-p,df=DF),qt(p=0.9999,df=DF)),length=50)
             }
             y2=dt(x2,df=DF)
             data3=data.frame(x=x2,y=y2)
             if(nrow(data3)==50){
                data3=rbind(data3,data3[50,],data3[50,])
                data3$y[51:52]=0
                data3$x[52]=data3$x[1]
             }

     }

     if(alternative=="two.sided"){
     if(showp){
          labels=paste0("p=",rep(round(p/2,3),3))
     } else{
          labels=paste0("t=",c(round(qt(p=p/2,DF),3),round(qt(p=1-p/2,DF),3)))
     }
     } else{
             if(showp){
                     labels=paste0("p=",rep(round(p,3),3))
             } else{
                     labels=paste0("t=",c(round(qt(p=p,DF),3),round(qt(p=1-p,DF),3)))
             }
     }
     p1=ggplot(data,aes(x=x,y=y))+
          geom_line()

     if(alternative!="greater"){
       no=ifelse(nrow(data2)>50,50,nrow(data2))
       hjust=ifelse(is.infinite(data2$x[no]),-1,1.3)
     p1=p1+
          geom_polygon(data=data2,aes(x=x,y=y),fill="red",alpha=0.5)+
          annotate(geom="text",x=data2$x[no],y=data2$y[no],label=labels[1],hjust=hjust,vjust=-0.5)

     }
     if(alternative!="less"){
       hjust=ifelse(is.infinite(data3$x[1]),1.3,-0.3)
      p1=p1+ geom_polygon(data=data3,aes(x=x,y=y),fill="red",alpha=0.5)+
          annotate(geom="text",x=data3$x[1],y=data3$y[1],label=labels[2],hjust=hjust,vjust=-0.5)
     }

     p1<-p1+ annotate(geom="text",x=0,y=0.1,label=paste0("t(df=",DF,")"))+
          annotate(geom="text",x=0,y=0.08,label=paste0("p=",round(p,3)))+
          annotate(geom="text",x=0,y=0.06,label=paste0("alternative=",alternative))+
          labs(x="t-value",y="Probability density")+
          theme_classic()
     p1

}


