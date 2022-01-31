#' Draw chi-squared distribution curve
#' @param q vector of quantiles
#' @param p vector of probabilities
#' @param df degrees of freedom (non-negative, but can be non-integer).
#' @importFrom stats dchisq qchisq pchisq
#' @return A ggplot
#' @export
#' @examples
#' draw_x2(df=1)
#' draw_x2(q=0.78)
draw_x2=function(q=NULL,p=0.05,df=2){

              # q=100;p=0.05;df=1
      if(!is.null(q)) {
          p=pchisq(q,df=df,lower.tail=FALSE)
     }

     x=seq(qchisq(p=0.001,df=df),qchisq(p=0.999,df=df),length=100)
     y=dchisq(x,df=df)
     data=data.frame(x=x,y=y)
     data=data[data$y<10,]


          if(near(p,1)){
               x2=seq(qchisq(p=0.001,df=df),qchisq(p=0.999,df=df),length=50)
               x2[1]=-Inf
          } else if(near(p,0)){
               x2=Inf
          } else{
               x2=seq(qchisq(p=1-p,df=df),qchisq(p=0.999,df=df),length=50)
          }
          y2=dchisq(x2,df=df)
          data3=data.frame(x=x2,y=y2)
          if(nrow(data3)==50){
               data3=rbind(data3,data3[50,],data3[50,])
               data3$y[51:52]=0
               data3$x[52]=data3$x[1]
          }



          labels=paste0("chi^2==",round(qchisq(p=1-p,df=df),3))


     p1=ggplot(data,aes(x=x,y=y))+
          geom_line()


          if(near(p,0)){
               p1=p1+ annotate(geom="point",x=data3$x[1],y=data3$y[1],color="red")+
                    annotate(geom="text",x=data3$x[1],y=data3$y[1],label=labels,
                             hjust=1.3,vjust=-1)
          } else{
          p1=p1+ geom_polygon(data=data3,aes(x=x,y=y),fill="red",alpha=0.5)+
               annotate(geom="text",x=data3$x[1],y=data3$y[1],label=labels,
                        hjust=0,vjust=-0.5,parse=TRUE)
          }

     plabel=ifelse(p<0.001,"p<.0001",paste0("p=",round(p,3)))
     label2=paste0(paste0("df=",df),"\n",plabel)
     centerx=mean(aplot::xrange(p1))
     maxy=aplot::yrange(p1)[2]
     p1+ annotate(geom="text",x=centerx,y=maxy,label=label2,vjust=2)+
          labs(x=expression(chi^2),y="Probability density")+
          theme_classic()

}

