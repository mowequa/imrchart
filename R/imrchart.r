imrchart<-function(x,xname="Response",xLabName="Index",bMed=F,bSD=F,bTests=rep(T,8),pTs=c(2,4,6,9,8,14,15)){

  #' A function to create individuals and moving range charts
  #'
  #' This function allows you to create an individuals and moving range chart from a single, numeric data vector
  #' @param x a numeric data vector, no default
  #' @param  xname a character string for the name of the data vector, defaults to 'Response'
  #' @param xLabName a character string for the name of the x-axis, defaults to 'Index'
  #' @param bMed a logical statement, T = control limits based on median moving range, F = control limits based on mean moving range, defaults to FALSE
  #' @param bSD a logical parameter, T = show standard deviation lines, defaults to F
  #' @param bTests a logical parameter vector of length 8, T = show test number n, defautls to rep(T,8)
  #' @param pTs a vector of integers for the test parameter, defaults to c(2,4,6,9,8,14,15)
  #' @keywords control chart, imr, xmr
  #' @export
  #' @examples
  #' imrchart(EuStockMarkets[1:30,2],"FTSE","Day")


  library(calibrate)
  library(ggplot2)
  library(reshape2)

  # CHECKS

  # Input vector must be numeric
  if(!is.numeric(x))
  {return("x must be numeric!")}

  # Must have 7 test values
  if(length(pTs)!=7)
  {return(paste0("You must enter exactly 7 test values! Entered",length(pTs), " values."))}

  # Test values must be numeric
  bpTsNumeric<-T
  for(i in seq_along(pTs))
  {if(!is.numeric(pTs[i])){bpTsNumeric<-F}}
  if(!bpTsNumeric)
  {return("Test values must be numeric!")}

  # Mean/median switch and Std Dev lines switch must be logical
  if(!is.logical(bMed))
  {return(paste0("bMed must be logical, TRUE or FALSE! Entered: ",bMed))}

  if(!is.logical(bSD))
  {return(paste0("bSD must be logical, TRUE or FALSE! Entered: ",bSD))}


  # DATA INTO DATA FRAME
  x<-unlist(x)
  sframe<-data.frame(xname=x)
  names(sframe)<-c(xname)

  # OUTPUT DATA FRAME
  dfout<-data.frame(matrix(vector(),0,2,dimnames=list(c(),c("Parameter","Value"))))

  # TEST VALUES
  t2s<-as.numeric(pTs[1])
  t3s<-as.numeric(pTs[2])
  t4s<-as.numeric(pTs[3])
  t5s<-as.numeric(pTs[4])
  t6s<-as.numeric(pTs[5])
  t7s<-as.numeric(pTs[6])
  t8s<-as.numeric(pTs[7])


  # PARAMETERS
  x.dim<-length(x)

  #X DATA
  x.ave <- ave(x)
  sframe$xave<-x.ave



  #MOVING RANGE
  x.mr<-vector()
  x.mr[1]<-NA
  for(i in 2:x.dim)
  {
    x.mr[i]<-abs(x[i]-x[i-1])
  }
  x.avemr<-ave(x.mr[2:x.dim])
  x.avemr<-c(x.avemr,x.avemr[1])

  x.medmr<-rep(median(x.mr[2:x.dim]),x.dim)
  # x.medmr<-c(x.medmr,x.medmr[1])

  sframe$mr<-x.mr
  sframe$mrave<-x.avemr
  sframe$mrmed<-x.medmr

  #CONTROL LIMITS

  #MEAN / MEDIAN SWITCH
  if(bMed)
  {
    sframe$UCLx<-x.ave+3.145*x.medmr[1]
    sframe$LCLx<-x.ave-3.145*x.medmr[1]
    sframe$UCLr<-x.medmr*3.865
  }
  else
  {
    sframe$UCLx<-x.ave+2.66*x.avemr[1]
    sframe$LCLx<-x.ave-2.66*x.avemr[1]
    sframe$UCLr<-x.avemr*3.268
  }


  sframe$LCLr<-rep(0,x.dim)

  # SD LINES
  sframe$SDP2<-x.ave+2*(sframe$UCLx-x.ave)/3
  sframe$SDP1<-x.ave+(sframe$UCLx-x.ave)/3
  sframe$SDN1<-x.ave-(x.ave-sframe$LCLx)/3
  sframe$SDN2<-x.ave-2*(x.ave-sframe$LCLx)/3

  #LABELS
  labelsx<-rep("",x.dim)
  labelsr<-rep("",x.dim)


  #OUTPUT
  dfout[1,1]<-"Data Average"
  dfout[1,2]<-sframe$xave[1]

  dfout[2,1]<-"Data Upper Control Limt"
  dfout[2,2]<-sframe$UCLx[1]

  dfout[3,1]<-"Data Lower Control Limt"
  dfout[3,2]<-sframe$LCLx[1]

  if(bMed)
  {
    dfout[4,1]<-"Median Moving Range"
    dfout[4,2]<-sframe$mrmed[1]
  }
  else
  {
    dfout[4,1]<-"Average Moving Range"
    dfout[4,2]<-sframe$mrave[1]

  }

  dfout[5,1]<-"Moving Range Upper Control Limt"
  dfout[5,2]<-sframe$UCLr[1]

  dfout[6,1]<-"Moving Range Lower Control Limt"
  dfout[6,2]<-0

  ###########  X DATA ###########

  #TEST 8 15 PTS IN A ROW WITHIN 1 SD
  if(bTests[8] && x.dim > t8s)
  {
    for(i in t8s:length(x))
    {
      if(all(x[(i-(t8s-1)):i] < (sframe$SDP1[1])) && all(x[(i-(t8s-1)):i] > (sframe$SDN1[1])))
      {
        labelsx[i] <- "8"
      }
    }
  }

  #TEST 7 14 PTS IN A ROW ALT UP/DOWN
  if(bTests[7]&& x.dim > t7s)
  {
    pattern = rep(1,(t7s-1))
    pattern[2*(1:((t7s-2)/2))] = -1



    #pattern<-c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)

    for(i in t7s:length(x))
    {
      bPlot7 = TRUE
      bHasNAN = TRUE



      test7<-(diff(x[(i-(t7s-1)):i]))/(abs(diff(x[(i-(t7s-1)):i])))
      bHasNAN<- TRUE %in% is.nan(test7)


      if(!bHasNAN && bPlot7)
      {
        if(all(test7 == pattern) || all(test7 == -pattern))
        {
          labelsx[i] <- "7"
        }
      }
    }
  }

  #TEST 6 8 PTS IN A ROW BEYOND 1 SD
  if(bTests[6]&& x.dim > t6s)
  {
    for(i in t6s:length(x))
    {
      if(all(x[(i-(t6s-1)):i] > (sframe$SDP1[1])) || all(x[(i-(t6s-1)):i] < (sframe$SDN1[1])))
      {
        labelsx[i] <- "6"
      }
    }
  }

  # TEST 5 9 PTS IN A ROW, EITHER SIDE OF CENTER LINE
  if(bTests[5]&& x.dim > t5s)
  {
    for(i in t5s:x.dim)
    {
      if(all(x[(i-(t5s-1)):i] > x.ave[1]) || all(x[(i-(t5s-1)):i] < x.ave[1]))
      {
        labelsx[i] <- "5"
      }

    }
  }

  # TEST 4 6 PTS IN A ROW, DECREASING OR INCREASING
  if(bTests[4]&& x.dim > t4s)
  {
    for(i in t4s:x.dim)
    {
      if(all(x[(i-(t4s-1)):i] == sort(x[(i-(t4s-1)):i])) || all(x[(i-(t4s-1)):i] == sort(x[(i-(t4s-1)):i], decreasing = T)))
      {
        labelsx[i] <- "4"
      }
    }
  }

  #Test 3 - 4 OUT OF 5 PTS OUTSIDE +/- 1 SD
  if(bTests[3]&& x.dim > t3s + 1)
  {
    for(i in (t3s+1):length(x))
    {
      varL = (x[(i-t3s):i] < (sframe$SDN1[1]))
      varG = (x[(i-t3s):i] > (sframe$SDP1[1]))

      if(length(varL[varL==TRUE]) > t3s-1 || length(varG[varG==TRUE]) > t3s-1)
      {
        labelsx[i]<-"3"
      }
    }
  }

  #Test 2 - 2 OUT OF 3 PTS OUTSIDE +/- 2 SD
  if(bTests[2]&& x.dim > t2s+1)
  {
    for(i in (t2s+1):length(x))
    {
      varL = (x[(i-t2s):i] < (sframe$SDN2[1]))
      varG = (x[(i-t2s):i] > (sframe$SDP2[1]))

      if(length(varL[varL==TRUE]) > t2s-1 || length(varG[varG==TRUE]) > t2s-1)
      {
        labelsx[i]<-"2"
      }
    }
  }


  # TEST 1 OUTSIDE CONTROL LIMITS 'X'
  if(bTests[1])
  {
    for(i in 1:x.dim)
    {

      if(x[i] > sframe$UCLx[1])
      {
        labelsx[i]<-"1"
      }

      if(x[i] < sframe$LCLx[1])
      {
        labelsx[i]<-"1"
      }
    }
  }
  ######### MOVING RANGE #########

  # TEST 1 OUTSIDE CONTROL LIMITS 'R'
  if(bTests[1])
  {
    for(i in 2:x.dim)
    {
      if(x.mr[i] > sframe$UCLr[1])
      {
        labelsr[i]<-"1"
      }

    }
  }

  #LABELS INTO THE RESULT

  sframe$labelsx<-labelsx
  sframe$labelsr<-labelsr


  xend<-length(sframe[,1])

  #Labels
  chartTitle = "Individuals Chart"
  yaxisTitle = xname

  sXlab = xLabName

  # PLOT CANVAS, 2 HIGH x 1 WIDE
  #par(mfrow=c(2,1))

  # AXIS LIMITS FOR I-CHART
  xmax<-ceiling(max(sframe[,6],sframe[,1]))
  xmin<-floor(min(sframe[,7],sframe[,1]))


  # SHOW STANDARD DEVIATION LINES IF SELECTED
  if(bSD)
  {
    xsd1<-geom_line(linetype="dotted",aes(y=SDP2,colour="Std Dev")) #  lines(seq(1:xend),sframe$SDP2,type="l",col="gray")
    xsd2<-geom_line(linetype="dotted",aes(y=SDP1,colour="Std Dev"))#  lines(seq(1:xend),sframe$SDP1,type="l",col="gray")
    xsd3<-geom_line(linetype="dotted",aes(y=SDN1,colour="Std Dev")) #  lines(seq(1:xend),sframe$SDN1,type="l",col="gray")
    xsd4<-geom_line(linetype="dotted",aes(y=SDN2,colour="Std Dev")) #  lines(seq(1:xend),sframe$SDN2,type="l",col="gray")

  }

  #X AXIS VARIABLE
  sframe$plotx<-seq(1:xend)

  colsx<-c("Data"="black","Average"="blue","Control Limit"="red","Std Dev"="gray")
  names(colsx)[1]<-paste0("Data (",xname,")")

  colsr<-c("Moving Range"="black","Control Limit"="red")
  names(colsr)[1]<-paste0("Moving Range (",xname,")")

  #INDIVIDUALS PLOT
  sframe$ydata<-sframe[[1]]
  xplot<-ggplot(sframe,aes(x=plotx,y=ydata),na.rm=TRUE)+ggtitle(chartTitle)+scale_x_continuous(name=xLabName)+scale_y_continuous(name=names(sframe)[1])
  xline<-geom_line(aes(x=plotx,y=ydata,colour=paste0("Data (",xname,")")),na.rm=TRUE)
  aveline<-geom_line(aes(y=xave,colour="Average"),na.rm=TRUE)
  xuclline<-geom_line(aes(y=UCLx,colour="Control Limit"),na.rm=TRUE)
  xlclline<-geom_line(aes(y=LCLx,colour="Control Limit"),na.rm=TRUE)
  xplot<-xplot+xline+aveline+xuclline+xlclline+theme_bw()+geom_text(hjust=1,colour="red",aes(label=labelsx),size=4,na.rm=TRUE)+scale_color_manual(name="",values=colsx)
  xplot<-xplot+theme(legend.position="bottom")

  if(bSD)
  {xplot<-xplot+xsd1+xsd2+xsd3+xsd4
  }

  #MOVING RANGE PLOT
  rplot<-ggplot(sframe[-1,],aes(x=plotx,y=mr,colour="Moving Range"),na.rm=TRUE)+ggtitle("Moving Range Chart")+scale_y_continuous(name="Moving Range")+scale_x_continuous(name=xLabName)
  rline<-geom_line(aes(y=mr,colour=paste0("Moving Range (",xname,")")),na.rm=TRUE)
  ruclline<-geom_line(aes(y=UCLr,colour="Control Limit"),na.rm=TRUE)
  rlclline<-geom_line(aes(y=LCLr,colour="Control Limit"),na.rm=TRUE)
  rplot<-rplot+rline+ruclline+rlclline+theme_bw()+geom_text(hjust=1,colour="red",aes(label=labelsr),size=4,na.rm=TRUE)+scale_color_manual(name="",values=colsr)
  rplot<-rplot+theme(legend.position="bottom")

  # > 1 PLOT ON A PAGE FROM THE R GRAPHICS COOKBOOK
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
      print(plots[[1]])

    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }

  #PLOT OUTPUT
  multiplot(xplot,rplot,cols=1)

  #TABLE OUTPUT
  return(dfout)


}

