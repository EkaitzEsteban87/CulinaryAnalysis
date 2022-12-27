
# This file is a generated template, your changes will not be overwritten
# jmvtools::check(home="C:/Program Files/jamovi 2.3.21.0")
# options(jamovi_home="C:/Program Files/jamovi 2.3.21.0")
# setwd("C:/Users/eesteban/Desktop/JAR")
# jmvtools::install()
# contact@jamovi.org

JARScalesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "JARScalesClass",
    inherit = JARScalesBase,
    private = list(
        .run = function() {
            require(jmvcore)

            # Get data from UI pane
            atr_names=self$options$attr
            atr=self$data[atr_names] # atributos datos
            N=length(atr) # n de atributos

            b=self$options$lik # liking
            like=self$data[b] # liking

            # Analisis de los atributos
            sc=self$options$attrscale
            jjar= switch(sc,"Three"= 2,"Five"= 3,"Seven"= 4,"Nine"= 5)

            # self$results$text$setContent(N)

            if (N){ # Consumer Reseach
                names_low=data.frame(rep(0,N)) # zero buffer
                names_high=data.frame(rep(0,N))
                n_low=data.frame(rep(0,N))
                n_high=data.frame(rep(0,N))
                n_jar=data.frame(rep(0,N))
                for (k in 1:N){
                    n_high[k,1]=mean((atr[,k]>jjar))*100
                    n_low[k,1]=mean((atr[,k]<jjar))*100
                    n_jar[k,1]=mean((atr[,k]==jjar))*100

                    names_low[k,1]=paste(atr_names[k],"low",sep="_")
                    names_high[k,1]=paste(atr_names[k],"high",sep="_")
                }
            }else{
                names_low=data.frame(rep(NA,N))
                names_high=data.frame(rep(NA,N))
                n_low=data.frame(rep(NA,N))
                n_high=data.frame(rep(NA,N))
                n_jar=data.frame(rep(NA,N))
            }

            # Mean drop - Analisis del liking
            if (length(like) & N){
                mdroph=data.frame(rep(0,N))
                mdropl=data.frame(rep(0,N))
                for (k in 1:N){
                    nivel_jar=mean(like[(atr[,k]==jjar),1]) # promedio liking para jar
                    nivel_alto=mean(like[(atr[,k]>jjar),1])
                    nivel_bajo=mean(like[(atr[,k]<jjar),1])

                    mdroph[k,1]=nivel_jar-nivel_alto # mean drop alto
                    mdropl[k,1]=nivel_jar-nivel_bajo # mean drop bajo
                }
            }else{
                mdroph=data.frame(rep(NA,N))
                mdropl=data.frame(rep(NA,N))
            }

            # Aplicar el test de Tukey para determinar si se quiere cribar o no
            if (self$options$posthoc & length(like)){
                p_tukeyh=data.frame(rep(0,N))
                p_tukeyl=data.frame(rep(0,N))
                y1=like[,1]
                x1=as.character(like)

                for (k in 1:N){
                    x1[atr[,k]==jjar]="jar"
                    x1[atr[,k]>jjar]="high"
                    x1[atr[,k]<jjar]="low"
                    model=data.frame(y1,x1)
                    fm1=aov(y1~x1,data=model)
                    a=TukeyHSD(fm1)
                    pval0=a$x1[,4]
                    pvalues=pval0[grepl("jar",names(pval0))]
                    pvallow=pvalues[grepl("low",names(pvalues))]
                    pvalhigh=pvalues[grepl("high",names(pvalues))]

                    p_tukeyh[k,1]=pvalhigh
                    p_tukeyl[k,1]=pvallow
                }
            }else {
                p_tukeyh=data.frame(rep(NA,N))
                p_tukeyl=data.frame(rep(NA,N))
            }

            # Get data for plotting
            if (self$options$showternary & N){
                plotData=cbind(n_jar,n_high,n_low)
                image <- self$results$ternaplot
                image$setState(plotData) # se guarda en image los datos
                image$setVisible(TRUE)
            }

            if (self$options$showbarras & N){
                plotData3=t(cbind(n_low,n_jar,n_high))
                image3 <- self$results$barraplot
                image3$setState(plotData3) # se guarda en image los datos
                image3$setVisible(TRUE)
            }

            if (self$options$showdiagnose & N & length(like)){
                plotData2=cbind(mdroph,mdropl,n_high,n_low,names_high,names_low)
                image2 <- self$results$diagplot
                image2$setState(plotData2) # se guarda en image los datos
                image2$setVisible(TRUE)
            }

            # Tabla de resultados - consumidores
            if (N){
                table <- self$results$Consumidores
                for (xk in 1:N){
                    table$setRow(rowNo=xk, values=list(
                    var=atr_names[xk],
                    consulow=n_low[xk,1],
                    consujar=n_jar[xk,1],
                    consuhigh=n_high[xk,1]
                    ))} # Cierre de tabla dinamica
                }

            #self$results$text$setContent(table2$visible)

            # Tabla de resultados 2 - Liking
            if (length(like) & N){
                table2 <- self$results$MeanDrop
                for (xk in 1:N){
                    table2$setRow(rowNo=xk, values=list(
                        var=atr_names[xk],
                        droplow=mdropl[xk,1],
                        tukeylow=p_tukeyl[xk,1],
                        drophigh=mdroph[xk,1],
                        tukeyhigh=p_tukeyh[xk,1]
                    ))}
                table2$setVisible(TRUE)
                }
        },
        .plot=function(image,...) {  # <-- TERNARY PLOT
            if (self$options$showternary){
                plotData <- image$state
                TernaryPlot(point="up",atip="JAR",btip="Too High",ctip="Too Low",alab="JAR % \u2192",blab="High % \u2192",clab="Low % \u2190")
                TernaryPoints(plotData, pch = 16)
                plotLabels = plotData
                plotLabels[,1]=plotLabels[,1]*0.95
                plotLabels[,2]=plotLabels[,2]*1.05
                TernaryText(plotLabels,labels=self$options$attr,col="blue")
                TRUE}
        },
        .plot2=function(image2,...) {  # <-- DIAGNOSIS PLOT
            if (self$options$showdiagnose){
                plotData <- image2$state
                freq=c(plotData[,3],plotData[,4])
                mdrop=c(plotData[,1],plotData[,2])
                plotlabels=c(plotData[,5],plotData[,6])

                xmax=max(freq)
                xl=c(0,ceiling(xmax))
                if(floor(min(mdrop))>0){
                    yl=c(0,ceiling(max(mdrop)))
                } else {
                    yl=c(floor(min(mdrop)),ceiling(max(mdrop)))
                }

                # Diagrama de atributos
                plot(plotData[,3],plotData[,1],pch="+",col="red",ylim=yl,xlim=xl,xlab="Consumer (%)",ylab="Mean-Drop")
                points(plotData[,4],plotData[,2],pch="-",col="blue",ylim=yl,xlim=xl,xlab="Consumer (%)",ylab="Mean-Drop")
                text(freq,mdrop,labels=plotlabels,cex=0.8,font=1,pos=2)
                # Add the legend to the chart
                legend("bottomleft",pch=c("+","-"),c("High","Low"),col=c("red","blue"),horiz = TRUE,bty="n")
                abline(h=self$options$mdropthreshold,lwd=2,lty=3)
                abline(v=self$options$threshold,lwd=2,lty=3)
                TRUE}
        },
        .plot3=function(image3,...) {  # <-- BARPLOT
            if (self$options$showbarras){
                plotData3 <- image3$state
                atr_names=self$options$attr
                # Diagrama de Barras
                barplot(plotData3,names.arg=atr_names,xlab ="Customer %",ylab="Attributes",axes=TRUE,col=c("cyan","green","red"),horiz=TRUE)
                # Add the legend to the chart
                legend("bottomleft",c("Low","JAR","High"),cex = 0.9,fill=c("cyan","green","red"),horiz = TRUE,bty="n")
                TRUE}
            }) # Cierre - Lista
) # Cierre - R6::R6Class
