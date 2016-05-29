imrchart<-function(data,xcol=NULL,ycol=NULL,bMed=F,bSD=F,bTests=rep(T,8),pTs=c(2,4,6,9,8,14,15)){

  #' A function to create individuals and moving range charts
  #'
  #' This function allows you to create an individuals and moving range chart from a single, numeric data vector
  #' @param data data frame or time series containing the data to be plotted
  #' @param ycol process output data
  #' @param xcol sequence data, default is a sequence called "Index"
  #' @param bMed a logical statement, T = control limits based on median moving range, F = control limits based on mean moving range, defaults to FALSE
  #' @param bSD a logical parameter, T = show standard deviation lines, defaults to F
  #' @param bTests a logical parameter vector of length 8, T = show test number n, defautls to rep(T,8)
  #' @param pTs a vector of integers for the test parameter, defaults to c(2,4,6,9,8,14,15)
  #' @keywords control chart, imr, ymr
  #' @export
  #' @examples
  #' imrchart(discoveries,bTests = c(T,T,rep(F,6)))


  require(grid)
  require(MASS)
  require(calibrate)
  require(ggplot2)
  require(reshape2)
  require(scales)
  require(timeDate)
  require(timeSeries)
  require(lubridate)




  # CHECK CLASS OF DATA
  bHasTimeData<-FALSE
  if(is.ts(data)){

    df_mtrx<-as.data.frame(data)

    timeData<-as.numeric(getTime(data))
    xdata<-format(date_decimal(timeData))

    if(is.null(ycol))
    {
      yname<-deparse(substitute(data))
      df_new<-data.frame(Date.Time=xdata,Ydata=df_mtrx[,1])
      ycol<-yname

    }
    else
    {
      yname<-deparse(substitute(ycol))
      df_new<-data.frame(Date.Time=xdata,Ydata=df_mtrx[,yname])

    }

    names(df_new)[2]<-yname

    data<-df_new

    bHasTimeData<-TRUE
  }


  if(class(data)!="data.frame")
  {return(paste0("Class of 'data' must be 'data frame' or 'time series'! Class of data entered: '",class(data),"'. Try casting."))}

  # Is Date function
  is.Date <- function(x) inherits(x, 'Date')

  # Y COLOUMN
  if(!bHasTimeData){ycol <- deparse(substitute(ycol))}

  # REMOVE FROM DATA FRAME ROWS WHERE Y IS NA

  data<-data[complete.cases(data[,ycol]),]

  # GET Y
  y<-as.vector(data[[ycol]])


  # X DATA
  if(deparse(substitute(xcol))!="NULL")
  {
  xcol <- deparse(substitute(xcol))
  x<-data[[xcol]]
  }else{

    if(bHasTimeData)
    {
      x<-as.Date(data[,1])
      xcol<-"Date.Time"

    }
    else
    {
    x<-seq(1:nrow(data))
    xcol<-"Index"
    }

  }


  # CHECKS

  # Input vector must be numeric
  if(!is.numeric(y))
  {return("y must be numeric!")}

  # Must have 7 test values
  if(length(pTs)!=7)
  {return(paste0("You must enter eyactly 7 test values! Entered",length(pTs), " values."))}

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
  y<-unlist(y)
  sframe<-data.frame(ycol=y)
  names(sframe)<-c(ycol)

  # OUTPUT DATA FRAME
  dataout<-data.frame(matrix(vector(),0,2,dimnames=list(c(),c("Parameter","Value"))))

  # TEST VALUES
  t2s<-as.numeric(pTs[1])
  t3s<-as.numeric(pTs[2])
  t4s<-as.numeric(pTs[3])
  t5s<-as.numeric(pTs[4])
  t6s<-as.numeric(pTs[5])
  t7s<-as.numeric(pTs[6])
  t8s<-as.numeric(pTs[7])


  # PARAMETERS
  y.dim<-length(y)

  #y DATA
  y.ave <- ave(y)
  sframe$yave<-y.ave

  #MOVING RANGE
  y.mr<-vector()
  y.mr[1]<-NA
  for(i in 2:y.dim)
  {
    y.mr[i]<-abs(y[i]-y[i-1])
  }
  y.avemr<-ave(y.mr[2:y.dim])
  y.avemr<-c(y.avemr,y.avemr[1])

  y.medmr<-rep(median(y.mr[2:y.dim]),y.dim)
  # y.medmr<-c(y.medmr,y.medmr[1])

  sframe$mr<-y.mr
  sframe$mrave<-y.avemr
  sframe$mrmed<-y.medmr

  #CONTROL LIMITS

  #MEAN / MEDIAN SWITCH
  if(bMed)
  {
    sframe$UCLy<-y.ave+3.145*y.medmr[1]
    sframe$LCLy<-y.ave-3.145*y.medmr[1]
    sframe$UCLr<-y.medmr*3.865
  }
  else
  {
    sframe$UCLy<-y.ave+2.66*y.avemr[1]
    sframe$LCLy<-y.ave-2.66*y.avemr[1]
    sframe$UCLr<-y.avemr*3.268
  }


  sframe$LCLr<-rep(0,y.dim)

  # SD LINES
  sframe$SDP2<-y.ave+2*(sframe$UCLy-y.ave)/3
  sframe$SDP1<-y.ave+(sframe$UCLy-y.ave)/3
  sframe$SDN1<-y.ave-(y.ave-sframe$LCLy)/3
  sframe$SDN2<-y.ave-2*(y.ave-sframe$LCLy)/3

  #LABELS
  labelsy<-rep("",y.dim)
  labelsr<-rep("",y.dim)


  #OUTPUT
  dataout[1,1]<-"Data Average"
  dataout[1,2]<-sframe$yave[1]

  dataout[2,1]<-"Data Upper Control Limt"
  dataout[2,2]<-sframe$UCLy[1]

  dataout[3,1]<-"Data Lower Control Limt"
  dataout[3,2]<-sframe$LCLy[1]

  if(bMed)
  {
    dataout[4,1]<-"Median Moving Range"
    dataout[4,2]<-sframe$mrmed[1]
  }
  else
  {
    dataout[4,1]<-"Average Moving Range"
    dataout[4,2]<-sframe$mrave[1]

  }

  dataout[5,1]<-"Moving Range Upper Control Limt"
  dataout[5,2]<-sframe$UCLr[1]

  dataout[6,1]<-"Moving Range Lower Control Limt"
  dataout[6,2]<-0

  ###########  y DATA ###########

  #TEST 8 15 PTS IN A ROW WITHIN 1 SD
  if(bTests[8] && y.dim > t8s)
  {
    for(i in t8s:length(y))
    {
      if(all(y[(i-(t8s-1)):i] < (sframe$SDP1[1])) && all(y[(i-(t8s-1)):i] > (sframe$SDN1[1])))
      {
        labelsy[i] <- "8"
      }
    }
  }

  #TEST 7 14 PTS IN A ROW ALT UP/DOWN
  if(bTests[7]&& y.dim > t7s)
  {
    pattern = rep(1,(t7s-1))
    pattern[2*(1:((t7s-2)/2))] = -1



    #pattern<-c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)

    for(i in t7s:length(y))
    {
      bPlot7 = TRUE
      bHasNAN = TRUE



      test7<-(diff(y[(i-(t7s-1)):i]))/(abs(diff(y[(i-(t7s-1)):i])))
      bHasNAN<- TRUE %in% is.nan(test7)


      if(!bHasNAN && bPlot7)
      {
        if(all(test7 == pattern) || all(test7 == -pattern))
        {
          labelsy[i] <- "7"
        }
      }
    }
  }

  #TEST 6 8 PTS IN A ROW BEYOND 1 SD
  if(bTests[6]&& y.dim > t6s)
  {
    for(i in t6s:length(y))
    {
      if(all(y[(i-(t6s-1)):i] > (sframe$SDP1[1])) || all(y[(i-(t6s-1)):i] < (sframe$SDN1[1])))
      {
        labelsy[i] <- "6"
      }
    }
  }

  # TEST 5 9 PTS IN A ROW, EITHER SIDE OF CENTER LINE
  if(bTests[5]&& y.dim > t5s)
  {
    for(i in t5s:y.dim)
    {
      if(all(y[(i-(t5s-1)):i] > y.ave[1]) || all(y[(i-(t5s-1)):i] < y.ave[1]))
      {
        labelsy[i] <- "5"
      }

    }
  }

  # TEST 4 6 PTS IN A ROW, DECREASING OR INCREASING
  if(bTests[4]&& y.dim > t4s)
  {
    for(i in t4s:y.dim)
    {
      if(all(y[(i-(t4s-1)):i] == sort(y[(i-(t4s-1)):i])) || all(y[(i-(t4s-1)):i] == sort(y[(i-(t4s-1)):i], decreasing = T)))
      {
        labelsy[i] <- "4"
      }
    }
  }

  #Test 3 - 4 OUT OF 5 PTS OUTSIDE +/- 1 SD
  if(bTests[3]&& y.dim > t3s + 1)
  {
    for(i in (t3s+1):length(y))
    {
      varL = (y[(i-t3s):i] < (sframe$SDN1[1]))
      varG = (y[(i-t3s):i] > (sframe$SDP1[1]))

      if(length(varL[varL==TRUE]) > t3s-1 || length(varG[varG==TRUE]) > t3s-1)
      {
        labelsy[i]<-"3"
      }
    }
  }

  #Test 2 - 2 OUT OF 3 PTS OUTSIDE +/- 2 SD
  if(bTests[2]&& y.dim > t2s+1)
  {
    for(i in (t2s+1):length(y))
    {
      varL = (y[(i-t2s):i] < (sframe$SDN2[1]))
      varG = (y[(i-t2s):i] > (sframe$SDP2[1]))

      if(length(varL[varL==TRUE]) > t2s-1 || length(varG[varG==TRUE]) > t2s-1)
      {
        labelsy[i]<-"2"
      }
    }
  }


  # TEST 1 OUTSIDE CONTROL LIMITS 'y'
  if(bTests[1])
  {
    for(i in 1:y.dim)
    {

      if(y[i] > sframe$UCLy[1])
      {
        labelsy[i]<-"1"
      }

      if(y[i] < sframe$LCLy[1])
      {
        labelsy[i]<-"1"
      }
    }
  }
  ######### MOVING RANGE #########

  # TEST 1 OUTSIDE CONTROL LIMITS 'R'
  if(bTests[1])
  {
    for(i in 2:y.dim)
    {
      if(y.mr[i] > sframe$UCLr[1])
      {
        labelsr[i]<-"1"
      }

    }
  }

  #LABELS INTO THE RESULT

  sframe$labelsy<-labelsy
  sframe$labelsr<-labelsr


  yend<-length(sframe[,1])

  #Labels
  chartTitle = "Individuals Chart"
  yayisTitle = ycol

  # PLOT CANVAS, 2 HIGH y 1 WIDE
  #par(mfrow=c(2,1))

  # AXIS LIMITS FOR I-CHART
  ymax<-ceiling(max(sframe[,6],sframe[,1]))
  ymin<-floor(min(sframe[,7],sframe[,1]))


  # SHOW STANDARD DEVIATION LINES IF SELECTED
  if(bSD)
  {
    ysd1<-geom_line(linetype="dotted",aes(y=SDP2,colour="Std Dev")) #  lines(seq(1:yend),sframe$SDP2,type="l",col="gray")
    ysd2<-geom_line(linetype="dotted",aes(y=SDP1,colour="Std Dev"))#  lines(seq(1:yend),sframe$SDP1,type="l",col="gray")
    ysd3<-geom_line(linetype="dotted",aes(y=SDN1,colour="Std Dev")) #  lines(seq(1:yend),sframe$SDN1,type="l",col="gray")
    ysd4<-geom_line(linetype="dotted",aes(y=SDN2,colour="Std Dev")) #  lines(seq(1:yend),sframe$SDN2,type="l",col="gray")

  }

  #X AXIS VARIABLE
  sframe$plotx<-x#seq(1:yend)

  #X AXIS SCALE
  if(is.Date(x))
  {gg_scale_x<-scale_x_date(name = xcol,breaks=pretty_breaks())}
  else
  {gg_scale_x<-scale_x_continuous(name=xcol)}

  colsy<-c("Data"="black","Average"="blue","Control Limit"="red","Std Dev"="gray")
  names(colsy)[1]<-paste0("Data (",ycol,")")

  colsr<-c("Moving Range"="black","Control Limit"="red")
  names(colsr)[1]<-paste0("Moving Range (",ycol,")")

  #INDIVIDUALS PLOT
  sframe$ydata<-sframe[[1]]
  yplot<-ggplot(sframe,aes(x=plotx,y=ydata),na.rm=TRUE)+ggtitle(chartTitle)+gg_scale_x+scale_y_continuous(name=names(sframe)[1])
  yline<-geom_line(aes(x=plotx,y=ydata,colour=paste0("Data (",ycol,")")),na.rm=TRUE)
  aveline<-geom_line(aes(y=yave,colour="Average"),na.rm=TRUE)
  yuclline<-geom_line(aes(y=UCLy,colour="Control Limit"),na.rm=TRUE)
  ylclline<-geom_line(aes(y=LCLy,colour="Control Limit"),na.rm=TRUE)
  yplot<-yplot+yline+aveline+yuclline+ylclline+theme_bw()+geom_text(hjust=1,colour="red",aes(label=labelsy),size=4,na.rm=TRUE)+scale_color_manual(name="",values=colsy)
  yplot<-yplot+theme(legend.position="bottom")

  if(bSD)
  {yplot<-yplot+ysd1+ysd2+ysd3+ysd4
  }

  #MOVING RANGE PLOT
  rplot<-ggplot(sframe,aes(x=plotx,y=mr,colour="Moving Range"),na.rm=TRUE)+ggtitle("Moving Range Chart")+scale_y_continuous(name="Moving Range")+gg_scale_x
  rline<-geom_line(aes(y=mr,colour=paste0("Moving Range (",ycol,")")),na.rm=TRUE)
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
        matchidy <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidy$row,
                                        layout.pos.col = matchidy$col))
      }
    }
  }

  #PLOT OUTPUT
  multiplot(yplot,rplot,cols=1)

  #TABLE OUTPUT
  return(dataout)


}

