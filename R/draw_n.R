#' Draw normal distribution curve
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param z vector of quantiles
#' @param p vector of probabilities
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less".
#' @importFrom stats dnorm qnorm
#' @return A ggplot
#' @export
#' @examples
#' draw_n()
#' draw_n(alternative="less")
#' draw_n(alternative="greater")
#' draw_n(z=-1.75)
#' draw_n(z=-1.75,alternative="greater")
#' draw_n(z=-1.75,alternative="less")
draw_n=function(mean=0,sd=1,z=NULL,p=0.05,alternative="two.sided"){
           # mean=0;sd=1;z=100;alternative="greater"
     if(!is.null(z)) {
             if(alternative=="two.sided"){
                     p=pnorm(-abs(z))*2
             } else if(alternative!="less"){
                     p=pnorm(z,lower.tail=FALSE)
             } else{
                     p=pnorm(z)
             }
          showp=TRUE
     } else{
          showp=FALSE
     }
     x=c(seq(qnorm(p=0.0001,mean=mean,sd=sd),qnorm(p=0.9999,mean=mean,sd=sd),length=100))
     y=dnorm(x,mean=mean,sd=sd)
     data=data.frame(x=x,y=y)
     data=rbind(data,data[100,],data[100,])
     data$y[101:102]=0
     data$x[102]=data$x[1]
     data

     if(alternative=="two.sided"){
             if(near(p,0)){
                     x1=-Inf
                     x2=Inf
             } else if(near(p,1)){
                     x1=-Inf
                     x2=Inf
             } else{
          x1=seq(qnorm(p=0.0001,mean=mean,sd=sd),qnorm(p=p/2,mean=mean,sd=sd),length=50)
          x2=seq(qnorm(p=1-p/2,mean=mean,sd=sd),qnorm(p=0.9999,mean=mean,sd=sd),length=50)
             }
          y1=dnorm(x1,mean=mean,sd=sd)
          y2=dnorm(x2,mean=mean,sd=sd)
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
                     x1=seq(qnorm(p=0.0001,mean=mean,sd=sd),qnorm(p=0.9999,mean=mean,sd=sd),length=50)
                     x1[50]=-Inf
             } else if(near(p,0)){
                     x1=Inf
             }else{
          x1=seq(qnorm(p=0.0001,mean=mean,sd=sd),qnorm(p=p,mean=mean,sd=sd),length=50)
             }
          y1=dnorm(x1,mean=mean,sd=sd)
          data2=data.frame(x=x1,y=y1)
          if(nrow(data2)==50){
          data2=rbind(data2,data2[50,],data2[50,])
          data2$y[51:52]=0
          data2$x[52]=data2$x[1]
          }
     } else{
             if(near(p,1)){
                     x2=seq(qnorm(p=0.0001,mean=mean,sd=sd),qnorm(p=0.9999,mean=mean,sd=sd),length=50)
                     x2[1]=-Inf
             } else if(near(p,0)){
                     x2=Inf
             } else{
          x2=seq(qnorm(p=1-p,mean=mean,sd=sd),qnorm(p=0.9999,mean=mean,sd=sd),length=50)
             }
          y2=dnorm(x2,mean=mean,sd=sd)
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
               labels=paste0("z=",c(round(qnorm(p=p/2,mean=mean,sd=sd),3),round(qnorm(p=1-p/2,mean=mean,sd=sd),3)))
          }
     } else{
          if(showp){
               labels=paste0("p=",rep(round(p,3),3))
          } else{
               labels=paste0("z=",c(round(qnorm(p=p,mean=mean,sd=sd),3),round(qnorm(p=1-p,mean=mean,sd=sd),3)))
          }
     }
     p1=ggplot(data,aes(x=x,y=y))+
          geom_line()

     if(alternative!="greater"){
          p1=p1+
               geom_polygon(data=data2,aes(x=x,y=y),fill="red",alpha=0.5)+
               annotate(geom="text",x=data2$x[50],y=data2$y[50],label=labels[1],hjust=ifelse(p>0.5,-0.3,1.3),vjust=-0.5)
     }
     if(alternative!="less"){
          p1=p1+ geom_polygon(data=data3,aes(x=x,y=y),fill="red",alpha=0.5)+
               annotate(geom="text",x=data3$x[1],y=data3$y[1],label=labels[2],hjust=ifelse(p>0.5,1.3,-0.3),vjust=-0.5)
     }

     p1+ annotate(geom="text",x=0,y=0.1,label=paste0("N(",mean,",",sd,")"))+
          annotate(geom="text",x=0,y=0.08,label=paste0("p=",round(p,3)))+
          annotate(geom="text",x=0,y=0.06,label=paste0("alternative=",alternative))+
          labs(x="z-score",y="Probability density")+
          theme_classic()

}
