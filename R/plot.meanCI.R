#' S3 method for an object of class "meanCI"
#' @param x an object of class "meanCI"
#' @param print logical If true, print plot
#' @param side logical or NULL If true draw side by side plot
#' @param ... Further arguments to be passed
#' @param ref string One of c("test","control").
#' @param palette The name of color palette from RColorBrewer package or NULL
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_point geom_segment geom_polygon
#' annotate theme_classic labs theme element_blank scale_y_continuous sec_axis guides
#' scale_x_discrete aes geom_line element_text scale_x_continuous ggplot_build geom_area
#' expand_limits
#' @importFrom aplot ylim2 yrange xrange
#' @importFrom ggbeeswarm geom_quasirandom
#' @importFrom aplot ylim2 yrange xrange
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map2_dfr
#' @importFrom stats sd qt dt rnorm
#' @importFrom dplyr filter
#' @importFrom grid arrow unit
#' @importFrom scales hue_pal
#' @examples
#' meanCI(mtcars,mpg) %>% plot()
#' meanCI(mtcars,am,mpg) %>% plot()
#' meanCI(iris,Sepal.Width) %>% plot()
#' meanCI(iris,Sepal.Width,Sepal.Length) %>% plot()
#' \dontrun{
#' meanCI(iris,Sepal.Width,Sepal.Length,paired=TRUE) %>% plot(palette="Dark2")
#' meanCI(iris,Sepal.Width,Sepal.Length) %>% plot()
#' meanCI(iris,Species,Sepal.Width) %>% plot(side=TRUE)
#' meanCI(iris,Species,Sepal.Width,mu=0.5,alternative="less") %>% plot(ref="test")
#' meanCI(acs,age) %>% plot()
#' meanCI(acs,sex,age) %>% plot()
#' meanCI(acs,smoking,age) %>% plot(palette="Set1")
#' meanCI(acs,Dx,age) %>% plot()
#' meanCI(acs,Dx,age,sex,mu=0) %>% plot(palette="Dark2")
#' x=c(95,89,76,92,91,53,67,88,75,85,90,85,87,85,85,68,81,84,71,46,75,80)
#' y=c(90,85,73,90,90,53,68,90,78,89,95,83,83,83,82,65,79,83,60,47,77,83)
#' meanCI(x=x,y=y,paired=TRUE,alpha=0.1) %>% plot()
#' meanCI(10:30,1:15) %>% plot()
#' iris %>% meanCI() %>% plot(side=TRUE)
#' meanCI(n=150,m=115,s=10,alpha=0.01) %>% plot()
#' meanCI(n1=30,n2=25,m1=78,s1=10,m2=85,s2=15,alpha=0.10) %>% plot()
#' data(anscombe2,package="PairedData")
#' meanCI(anscombe2,idx=list(c("X1","Y1"),c("X4","Y4"),c("X3","Y3"),c("X2","Y2")),
#' paired=TRUE,mu=0) %>% plot()
#' x=meanCI(anscombe2,idx=list(c("X1","X2","X3","X4"),c("Y1","Y2","Y3","Y4")),paired=TRUE,mu=0)
#' plot(x)
#' longdf=tidyr::pivot_longer(anscombe2,cols=X1:Y4)
#' x=meanCI(longdf,name,value,idx=list(c("X1","X2","X3","X4"),c("Y1","Y2","Y3","Y4")),paired=TRUE,mu=0)
#' plot(x)
#' acs %>% select(sex,TC,TG,HDLC) %>% meanCI(group=sex) %>% plot()
#' acs %>% select(sex,TC,TG,HDLC) %>% meanCI(sex) %>% plot()
#' }
plot.meanCI=function(x,ref="control",print=TRUE,side=NULL,palette=NULL,...){
        # x= meanCI(n1=30,n2=25,m1=78,s1=10,m2=85,s2=15,alpha=0.10)
               # ref="control";print=TRUE;side=FALSE;palette=NULL
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

        if(attr(result,"measure")=="mean"){
                p=meanCIplot1(x)
                if(print) print(p)
                return(invisible(p))

        }

        if(paired){
                xname="name"
                mode=2
                ref1=1
                if(nrow(result$result)>1){
                        for(i in 2:nrow(result$result)){
                                if(result$result$control[i]!=result$result$control[i-1]){
                                        ref1=c(ref1,i+length(ref1))
                                }
                        }
                }

                ref1
                if(longform==FALSE){
                        df=df[1:2]
                        df=na.omit(df)
                        df1=map2_dfr(names(df),df,function(x,y){
                                data.frame(x,m=mean(y),lower=mean(y)-sd(y),upper=mean(y)+sd(y),no=length(y))

                        })
                        names(df1)[1]="name"
                        df1$x=c(0.8,2.2)
                        df1$label=paste0(df1$name,"\nN=",df1$no)
                        df1
                        df1$name=factor(df1$name,levels=names(df)[1:2])

                        df$id=1:nrow(df)
                        longdf=pivot_longer(df,cols=1:2)
                        longdf$name=factor(longdf$name,levels=names(df)[1:2])

                        p1=pairPlot(longdf,ref=ref1,palette=palette)

                        p1=p1+
                                geom_segment(data=df1,aes_string(x="x",xend="x",y="lower",yend="upper",color="name"),
                                             size=1.5)+
                                geom_point(data=df1,aes_string(x="x",y="m",color="name"),pch=21,fill="white",size=2)

                } else{
                        p1=pairPlot(df,ref=ref1,palette=palette)
                        p1
                        res=split(df[[2]],df[[1]])
                        df1=map2_dfr(names(res),res,function(x,y){
                                        y[is.na(y)]<-NULL
                                        data.frame(x,m=mean(y),lower=mean(y)-sd(y),upper=mean(y)+sd(y),no=length(y))

                        })
                        df1<-df1 %>% dplyr::filter(.data$no!=0)

                }


        } else{
                simuldata=FALSE
                if("list" %in% class(result$data)){
                        mode=1
                } else{
                     mode=2
                }

                df1=meanCI2df1(x)
                df=meanCI2df(x)

                p1= estimationPlot1(x,palette=palette)
        }

        if(is.null(side)) {
                side=ifelse(nrow(result$result)==1,TRUE,FALSE)
        }

        result2df=function(result){
                map_dfr(1:nrow(result),function(i){
                        temp=result[i,]
                        x=c(seq(qt(p=0.001,df=temp$DF),qt(p=0.999,df=temp$DF),length=100))
                        y=dt(x,df=temp$DF)
                        x=x*temp$se

                        x=x+temp$md

                        data=data.frame(x=x,y=y)
                        data=rbind(data,data[100,],data[100,])
                        data$y[101:102]=0
                        data$x[102]=data$x[1]
                        data$group=i+1

                        data$y=data$y+data$group-ifelse(side,2,0)

                        data
                })
        }
        df2=result2df(result$result)

        result2md=function(result){
                map_dfr(1:nrow(result),function(i){
                        temp=result[i,]
                        x=i+ifelse(side,-1,1)
                        y=temp$md
                        lower=temp$lower
                        upper=temp$upper
                        ME=temp$ME
                        if(ref=="control"){
                                label=paste0(temp$test,"\nminus\n",temp$control)
                        } else{
                                label=paste0(temp$control,"\nminus\n",temp$test)
                        }
                        res=data.frame(x=x,y=y,lower=lower,upper=upper,label=label,ME=ME)
                        if(is.infinite(lower[1])) res$arrow="first"
                        if(is.infinite(upper[1])) res$arrow="last"
                        res
                })
        }
        df3=result2md(result$result)


        if((!side) & nrow(result$result)>1){
                ref1=1
                for(i in 2:nrow(result$result)){
                        if(result$result$control[i]!=result$result$control[i-1]){
                                ref1=c(ref1,i)
                        }
                }
                ref1
                ref2=ref1
                ref2
                if(length(ref2)>1){
                        for(i in 2:length(ref2)){

                                df3$x[df3$x>ref2[i]]=df3$x[df3$x>ref2[i]]+1
                                df2$y[df2$group>ref2[i]]=df2$y[df2$group>ref2[i]]+1
                                df2$group[df2$group>ref2[i]]=df2$group[df2$group>ref2[i]]+1
                                ref2=ref2+1
                        }
                }
        }

        if(ref=="control"){
                df2$x=-df2$x
                df3$y=-df3$y
                df3$upper=-df3$upper
                df3$lower=-df3$lower
        }
        if(!is.null(palette)){
             fillcolor=palette2colors(palette)
        } else{
            fillcolor=scales::hue_pal()(max(df2$group))
        }
        df2$group1=fillcolor[df2$group]


        if(!side){

                #xrange=aplot::xrange(p1)
                xrange1=aplot::xrange(p1)

                p2<-ggplot(df2,aes_string(x="y",y="x"))+
                        geom_polygon(aes_string(group="group"),fill=df2$group1,alpha=0.5)+
                        geom_point(data=df3,aes_string(x="x",y="y"),size=2)

                if(is.null(df3$arrow)){
                        p2<-p2+geom_segment(data=df3,aes_string(x="x",xend="x",y="lower",yend="upper"))
                } else{
                        p2<-p2+geom_segment(data=df3,aes_string(x="x",xend="x",y="lower",yend="upper"),
                                            arrow=arrow(length = unit(0.1, "inches"),ends=df3$arrow))
                }
                if(isProvided(x)){
                        if(ref=="control"){
                                p2=p2+geom_hline(yintercept=-x$result$mu,color="red")
                        } else{
                                p2=p2+geom_hline(yintercept=x$result$mu,color="red")
                        }
                }

                p2<-p2+
                        theme(axis.text=element_text(size=11),axis.title=element_text(size=14))+
                        scale_x_continuous(breaks=df3$x,labels=df3$label)+
                        theme_classic()+
                        labs(x="",y=paste0(ifelse(paired,"Paired","Unpaired"),"\nmean difference"))+
                        theme(axis.text=element_text(size=11),axis.title =element_text(size=14))+
                        expand_limits(x=c(0.5))

                df1

                myrange=c(xrange1[1]+0.1,xrange1[2]-0.1)
                p1=p1+expand_limits(x=myrange)
                p2=p2+expand_limits(x=myrange)
                reslist=list(p1=p1,p2=p2,
                             side=FALSE,
                             heights=c(3,2))
                class(reslist)="plotCI"
               # if(print) print(p1/p2+plot_layout(heights=c(3,2)))

                return(reslist)

        } else{

                if(ref=="control"){
                        df2$x=df2$x + result$result$m1[1]
                        df3$y=df3$y + result$result$m1[1]
                        df3$upper=df3$upper + result$result$m1[1]
                        df3$lower=df3$lower + result$result$m1[1]
                } else{
                        df2$x=df2$x + min(result$result$m2)
                        df3$y=df3$y + min(result$result$m2)
                        df3$upper=df3$upper + min(result$result$m2)
                        df3$lower=df3$lower + min(result$result$m2)
                }

                ylims=aplot::yrange(p1)
                ylims

                df2

                if(mode==1) xname="name"
                if(!("name" %in% names(df1))) xname="x"
                df1[[xname]]=factor(df1[[xname]],levels=df1[[xname]])
                df1$m1=(df1$m[1]-df1$m)+min(df1$m)


                p2=ggplot(data=df2,aes_string(x="y",y="x"))+theme_classic()+
                        labs(x="",y="")+
                        theme(axis.line.y.left = element_blank(),
                              axis.text=element_text(size=11),axis.title=element_text(size=14))+
                        guides(color="none")+
                        scale_x_continuous(breaks=0:(nrow(df3)-1),labels=df3$label,limits=c(-0.5,nrow(df3)-1+0.5))+
                        geom_polygon(aes_string(group="group"),fill=df2$group1,alpha=0.8)+
                        annotate(geom="point",x=df3$x,y=df3$y,color="black",size=2)
                if(ref=="control") {
                        p2=p2+geom_segment(data=df1,aes_string(y="m",yend="m",color=xname),x=-Inf,xend=Inf)
                } else{
                        p2=p2+geom_segment(data=df1,aes_string(y="m1",yend="m1",color=xname),x=-Inf,xend=Inf)
                }

                if(is.null(df3$arrow)){
                        p2<-p2+annotate(geom="segment",x=df3$x,xend=df3$x,y=df3$lower,yend=df3$upper)
                } else{
                        p2<-p2+annotate(geom="segment",x=df3$x,xend=df3$x,y=df3$lower,yend=df3$upper,
                                        arrow=arrow(length = unit(0.1, "inches"),ends=df3$arrow))
                }
                if(isProvided(x)){
                        if(ref=="test"){
                                p2=p2+annotate(geom="point",x=df3$x,y=min(x$result$m2)+x$result$mu,color="red")
                                #annotate(geom="text",x=df3$x,y=min(x$result$m2)+x$result$mu,label=paste0("mu=",x$result$mu),hjust=1.2)
                        } else{
                                p2=p2+annotate(geom="point",x=df3$x,y=x$result$m1[1]-x$result$mu,color="red")
                                #annotate(geom="text",x=df3$x,y=x$result$m1[1]-x$result$mu,label=paste0("mu=",x$result$mu),hjust=1.2)
                        }

                }
                p2<-p2+scale_y_continuous(labels=NULL,breaks=NULL,
                                          sec.axis=sec_axis(~.-ifelse(ref=="control",result$result$m1[1],min(result$result$m2)),
                                                            name=paste0(ifelse(paired,"Paired","Unpaired")," mean difference")))+
                     theme(axis.text=element_text(size=11),axis.title =element_text(size=14))+
                     expand_limits(y=ylims)
                #scale_color_brewer(palette = "Set1")+

                p1
                p2


                p1<-p1+aplot::ylim2(p2)
                reslist=list(p1=p1,p2=p2,
                             side=TRUE,
                             widths=c(2,1))
                class(reslist)="plotCI"
                return(reslist)
        }
}


#'S3 method for class plotCI
#'@param x An object of class plotCI
#'@param ... Further arguments
#'@importFrom patchwork plot_layout
#'@export
print.plotCI=function(x,...){
        if(x$side){
                print(x$p1+x$p2+plot_layout(widths=x$widths))

        } else{
                print(x$p1/x$p2+plot_layout(heights=x$heights))
        }
}


#' Draw meanCI plot for data with single vector
#' @param x An object of class "meanCI" with attr(x,"measure")=="mean"
#' @return A ggplot
#' @export
#' @examples
#' x=meanCI(mtcars,mpg)
#' meanCIplot1(x)
meanCIplot1=function(x){
        temp=x$result
        x1=c(seq(qt(p=0.001,df=temp$DF),qt(p=0.999,df=temp$DF),length=100))
        y1=dt(x1,df=temp$DF)
        x1=x1*temp$se
        x1=x1+temp$m

        data=data.frame(x=x1,y=y1)
        data=rbind(data,data[100,],data[100,])
        data$y[101:102]=0
        data$x[102]=data$x[1]
        data$y=data$y*0.3+0.5

        if(is.na(x$data[1,1])){
                value=rnorm(n=x$result$n,mean=x$result$m,sd=x$result$s)
                x$data=data.frame(value=value)
        }


        p<-ggplot(x$data,aes_string(x="0",y=names(x$data)[1]))+
                geom_quasirandom(width=0.3)+
                geom_hline(yintercept=x$result$m,color="grey80")+
                geom_segment(data=x$result,aes_string(x="0.4",xend="0.4",y="m-s",yend="m+s"))+
                geom_point(data=x$result,aes_string(x="0.4",y="m"),pch=21,fill="white",size=2)+
                geom_polygon(data=data,aes_string(x="y",y="x"),fill="grey80",alpha=0.8)
        if(is.infinite(x$result$upper)){
                p=p+geom_segment(data=x$result,aes_string(x="0.5",xend="0.5",y="lower",yend="upper"),
                                 size=1,arrow=arrow(length = unit(0.1, "inches")))
        } else if(is.infinite(x$result$lower)){
                p=p+geom_segment(data=x$result,aes_string(x="0.5",xend="0.5",y="lower",yend="upper"),
                                 size=1,arrow=arrow(length = unit(0.1, "inches"),ends="first"))
        } else{
                p=p+geom_segment(data=x$result,aes_string(x="0.5",xend="0.5",y="lower",yend="upper"),
                                 size=1)
        }
        p<-p+
                geom_point(data=x$result,aes_string(x="0.5",y="m"),pch=21,fill="white",size=2)+
                annotate(geom="text",x=0.55,y=x$result$upper,label=paste0("Mean\n",(1-x$result$alpha)*100,"%CI"),vjust=-1.5)+
                annotate(geom="text",x=0.4,y=x$result$m+x$result$s,label="Mean+SD",vjust=-1.5)+
                annotate(geom="text",x=0.4,y=x$result$m-x$result$s,label="Mean-SD",vjust=2)+
                theme_classic()+
                labs(x="")+
                scale_y_continuous(sec.axis=sec_axis(~.-x$result$m,name="Mean difference"))+
                theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
        if(isProvided(x)){
                p=p+geom_hline(yintercept=x$result$mu,color="red")
        }
        p
}

#' Draw a pair plot
#' @param data a data.frame
#' @param ref Numeric or NULL
#' @param palette The name of color palette from RColorBrewer package or NULL
#' @return a ggplot
#' @importFrom dplyr count
#' @importFrom ggplot2 expand_limits scale_color_brewer
#' @export
#' @examples
#' x=meanCI(mtcars,paired=TRUE)
#' pairPlot(x$data)
#' pairPlot(x$data,ref=c(1,4,6))
#' pairPlot(x$data,ref=c(1,3))
pairPlot=function(data,ref=NULL,palette=NULL){

        df=data
        temp=setdiff(names(df),"id")
        if(is.mynumeric(df[[temp[1]]])){
                xname=temp[2]
                yname=temp[1]
        } else{
                xname=temp[1]
                yname=temp[2]
        }
        groupname="id"
        if(!is.null(ref)){
                refnames=list()
                for(i in seq_along(ref)){
                        start=ref[i]
                        stop=ifelse(i==length(ref),length(unique(df[[xname]])),ref[i+1]-1)
                        refnames[[i]]=levels(df[[xname]])[start:stop]
                }

                names2ref=function(names){
                        result=c()
                        for(i in seq_along(names)){
                                for(j in seq_along(refnames)){
                                        if(names[i] %in% refnames[[j]]) result=c(result,j)
                                }
                        }
                        result
                }
                df$refno=names2ref(df[[xname]])
                df$idref=paste0(df$id,df$refno)
                groupname="idref"
        }
        df
        df$x=as.numeric(df$name)

        df1<-df %>% count(.data[[xname]]) %>% mutate(label=paste0(.data[[xname]],"\nN=",.data$n))
        df1
        p=ggplot(df,aes_string(x="x",y=yname))+
                geom_line(aes_string(group=groupname),alpha=0.5)+
                geom_point(aes_string(color=xname))+
                theme_classic()+
                scale_x_continuous(breaks=1:length(df1$label),labels=df1$label)+
                theme(axis.text=element_text(size=11),axis.title =element_text(size=14))+
                labs(y="Measurement",x="")+
                guides(color="none")+
                expand_limits(x=c(0.5,round(max(df$x))+0.5))
        if(!is.null(palette)){
             p=p+scale_color_brewer(palette=palette)
        }
        p
}


#' Draw estimation plot1
#' @param x An object of class meanCI
#' @param palette The name of color palette from RColorBrewer package or NULL
#' @return A ggplot
#' @importFrom ggplot2 scale_color_brewer
#' @export
#' @examples
#' x=meanCI(iris,Species,Sepal.Length)
#' estimationPlot1(x)
estimationPlot1=function(x,palette=NULL){
     data=meanCI2df(x)
     df1=meanCI2df1(x)

     ylabel=ifelse(names(data)[2]=="name","Mesurement",names(data)[2])

     p=ggplot(data,aes_string(x="x",y="value",color="name"))+
          geom_quasirandom(width=0.3,alpha=0.5)+
          geom_segment(data=df1,aes_string(x="x",xend="x",y="lower",yend="upper",color="name"),
                       size=1.5)+
          geom_point(data=df1,aes_string(x="x",y="m",color="name"),pch=21,fill="white",size=2)+
          scale_x_continuous(breaks=1:length(df1$label),labels=df1$label)+
          theme_classic()+
          labs(x="",y=ylabel)+
          theme(axis.text=element_text(size=11),axis.title =element_text(size=14))+
          guides(color="none")+
          expand_limits(x=0.5)
     if(!is.null(palette)){
          p=p+scale_color_brewer(palette=palette)
     }
     p
}
