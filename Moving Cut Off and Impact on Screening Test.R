# Moving Cut Off point and Impact on Screening Test


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(ggtext)
library(stringr)
library(plotly)
library(tidyr)
library(kableExtra)
library(shinyjs)
library(shinyalert)
library(shinyBS)
library(tibble)

#################################################################################
#style function for Action button default 50px and width 180px; you can change
#################################################################################
styleButtonBlue<- function(xheight="50px",xwidth="180px",xcolor='#4682B4'){
  paste("white-space: normal;
  text-align:center;
  color: #ffffff;
        background-color:",xcolor,";",
        "border-color: #ffffff;
        border-width:2px;
        height:",xheight,";
        width:",xwidth,";
        font-size: 13px;")
}


fnpercentZeroDigit <- function(x, digits = 0, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}


fnpercent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}


jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'


mStatBoxHeight <- "75px"

######################################################################################
## function to change color for TP,TN,FP,FN using Polygon
## Thanks to Christopher DeSante for this polygon coloring function posted in 
## https://gist.github.com/cdesante/3750663. I have modified to suit plotly
######################################################################################

fnaddpolygonplotly <- function(MU,SD, Lower, Upper,dataLength,fill.color,Fig){
  cord.x <- c(Lower,seq(from = Lower,to = Upper,length.out = dataLength),Upper)
  cord.y <- c(0,dnorm(seq(from = Lower,to = Upper,length.out = dataLength ), mean = MU, sd = SD),0)
  add_polygons(p = Fig,x = cord.x, y = cord.y, fillcolor=fill.color,
               line=list(width=1,color="black"))
}



######################################################################################
## function Percentage of overlapping regions of two normal distributions
## Thanks to the contribution in stackexchange.com
##https://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions
######################################################################################
get_overlap_coef <- function(mu1, mu2, sd1, sd2){
  xs  <- seq(min(mu1 - 4*sd1, mu2 - 4*sd2),
             max(mu1 + 4*sd1, mu2 + 4*sd2),
             length.out = 1000)
  f1  <- dnorm(xs, mean=mu1, sd=sd1)
  f2  <- dnorm(xs, mean=mu2, sd=sd2)
  int <- xs[which.max(pmin(f1, f2))]
  l   <- pnorm(int, mu1, sd1, lower.tail = mu1>mu2)
  r   <- pnorm(int, mu2, sd2, lower.tail = mu1<mu2)
  l+r
}


ui <- dashboardPage(
  dashboardHeader(title ="Changes in Cut Off Point & Impact on Screening Test",
                  titleWidth = '100%' ),
  
  dashboardSidebar(
    # Remove the sidebar toggle element
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"))
  ),
  
  dashboardBody(
    #code to change the title font of the dashboard header  # where you got: https://rstudio.github.io/shinydashboard/appearance.html#css
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }')
                         )),

    useShinyalert(),
    shinyjs::useShinyjs(),
    useShinyjs(),
    extendShinyjs(text = jsCode,functions = c('backgroundCol')),
    
    fluidRow(
        align="center",
        column(
          width = 12,
          box(
            width = 4,
            height = '325px',
            # title = 'Dual Chart-cut-off movement',
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            align ='center',
            HTML(paste('<h5><u><b>',"Dual Chart-cut-off movement",'</b></u><h5>')),
            plotlyOutput(outputId = "mcutoffplot",width = "100%",height = '100px')
          ),
          column(
            width = 4,
            tags$head(
              tags$style(
                paste0("#mstatcolumn{color:white; font-size:12px; font-style:bold;overflow:none;text-align: center;
                                            width: '100%';height: 170px;max-height: 170px; background: #000000;}"),
                paste0("#mstatcolumn2{color:white; font-size:12px; font-style:bold;overflow:none;text-align: center;
                                            width: '100%';height: 170px;max-height: 170px; background: #000000;}")
              )
            ),
            column(id='mstatcolumn',
                   width = 6,
                   uiOutput("xmTNR"),
                   '----------------------------------',
                   uiOutput("xmPPV")
            ),
            column(id='mstatcolumn2',
                   width = 6,
                   uiOutput("xmTPR"),
                   '----------------------------------',
                   uiOutput("xmNPV")
            ),
            column(
              id='mslidercolumn',
              width = 12,
              tags$head(
                tags$style(
                  paste0("#mslidercolumn{color:white; font-size:11px; font-style:bold;overflow:none;text-align: center;
                                            width: '100%';height: 155px;max-height: 155px; background: #808080;}")
                )
              ),
              tags$br(),
              noUiSliderInput(
                inputId = "mCutOffLine", 
                label = NULL,
                min = -2, max = 2,
                value = 0,
                tooltips = TRUE, step = .01,
                direction = 'ltr',
                orientation = "horizontal",
                width = "100%",
                height = "13px",
                format = wNumbFormat(decimals = 2 )
              ),
              HTML(paste('<h5><u>', "Move the slider to see cutoff A and B.<br> It's impact on Key Statistics",'</u><h5>'))
              )
          ),
          box(
            width = 4,
            height = '325px',
            title = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            HTML(paste('<h5><u><b>',"Impact on Key Statistics of Screening Test",'</b></u><h5>')),
            plotOutput('mshowmatrixplot', width = "100%",height = '275px'),
          )#box closure
        ), #column closure
        column(
          width = 12,
          tags$head(
            tags$style(
              paste0("#moverviewBox1{color:black; font-size:10px; font-style:bold;overflow:none;text-align: justify;margin: 1px 1px 1px 1px;
                                            width: '100%';height: 160px;max-height: 160px; background: #ffffcd;}"),
              paste0("#moverviewBox2{color:black; font-size:10px; font-style:bold;overflow:none;text-align: justify;margin: 1px 1px 1px 1px;
                                            width: '100%';height: 160px;max-height: 160px; background: #ffffcd;}")
              
            )
          ),
          box(
            id = 'moverviewBox1',
            width = 3,
            status = 'warning',
            solidHeader = T,
            uiOutput(outputId = 'moverview1')
          ),
          box(
            id = 'moverviewBox2',
            width = 3,
            status = 'warning',
            solidHeader = T,
            uiOutput(outputId = 'moverview2')
          ),
          tags$head(
            tags$style(
              paste0("#mkbltablecolumn1{color:black; font-size:12px; font-style:bold;overflow:none;text-align: center;margin: 1px 1px 1px 1px;
                                            width: '100%';height: 160px;max-height: 160px; background: #ffffcd;}"),
              paste0("#mkbltablecolumn2{color:black; font-size:12px; font-style:bold;overflow:none;text-align: center;margin: 1px 1px 1px 1px;
                                            width: '100%';height: 160px;max-height: 160px; background: #ffffcd;}")
            )
          ),
        box(
          id='mkbltablecolumn1',
          width = 3,
          status = 'warning',
          solidHeader = T,
          HTML(paste('<h5><b><u>','2 x 2 Table - Cutoff-A','</u></b><h5>')),
          htmlOutput(outputId = 'mCutoffATable')
        ),
          box(
            id='mkbltablecolumn2',
            width = 3,
            status = 'warning',
            solidHeader = T,
            HTML(paste('<h5><b><u>','2 x 2 Table - Cutoff-B','</u></b><h5>')),
            htmlOutput(outputId = 'mCutoffBTable')
          )
        )#column closure
    ), #fluidrow closure
    
    bsPopover("justbsPopover", "Just a dummy bs, without which you cannot create addpopover",
              placement = 'left', options = list(container = "body"))
    
  )#dashboardBody closure
)


server <- function(input, output,session) {
  #this is to hide right side bar
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  
 

  
  ######################################################################################
  #here is the reactive variable and ObserveEvent update values for the plot
  ######################################################################################
  vc <- reactiveValues(x=NULL,f.x=NULL,y=NULL,f.y=NULL,tpval=NULL,fpval=NULL,tnval=NULL,fnval=NULL,data=NULL,rr=NULL,tt=NULL,zz=NULL,mm=NULL,pp=NULL)
  
  vc$rr  <- data.frame(TP1=0,FP1=0,TN1=0,FN1=0,TP2=0,FP2=0,TN2=0,FN2=0,Cutoff1=0,Cutoff2=0)
  
  
  t<-list(
    family = "Tahoma",
    size = 15,
    color = toRGB("black"))
  f <-list(
    family = "Tahoma",
    size = 12,
    color = "black"
  )
  
  
  Cutoff_A <- c(0,0,0,0)
  Cutoff_B <- c(0,0,0,0)
  Statistics_  <- c("PPV","NPV","Sensitivity","Specificity")
  vc$tt     <- data.frame(Statistics_,Cutoff_A,Cutoff_B)
  
  observeEvent(input$mCutOffLine,{
   
    xTPlabel <-  1.50
    xFPlabel <-  0.65
    xTNlabel <- -1.50
    xFNlabel <- -0.30
    
    yTPlabel <- 0.20
    yFPlabel <- 0.04
    yTNlabel <- 0.20
    yFNlabel <- 0.04
    
    
    if (input$mCutOffLine>=1 || input$mCutOffLine<=-1 ){
      xmTPfactor   <- ifelse(input$mCutOffLine>=1,0.40,2.40)
      xmTNfactor   <- ifelse(input$mCutOffLine>=1,2.40,0.40)
      ymTPfactor   <- 0.11
      ymTNfactor   <- 0.11
      xmFPfactor   <- 0.25
      xmFNfactor   <- 0.25
      ymFPfactor   <- 0.01
      ymFNfactor   <- 0.01
    }
    else {
      xmTPfactor   <- ifelse(input$mCutOffLine>=0 & input$mCutOffLine<=1,0.75,1.30)
      xmTNfactor   <- ifelse(input$mCutOffLine>=0 & input$mCutOffLine<=1,1.30,0.75)
      ymTPfactor   <- 0.15
      ymTNfactor   <- 0.15
      xmFPfactor   <- 0.25
      xmFNfactor   <- 0.25
      ymFPfactor   <- 0.04
      ymFNfactor   <- 0.04
    }
    xTPlabel <-  round(input$mCutOffLine+1.5*xmTPfactor,2)
    xTNlabel <-  round(input$mCutOffLine-1.5*xmTNfactor,2)
    yTPlabel <-  ymTPfactor
    yTNlabel <-  ymTNfactor
    xFPlabel <-  round(input$mCutOffLine+1.5*xmFPfactor,2)
    xFNlabel <-  round(input$mCutOffLine-1.5*xmFNfactor,2)
    yFPlabel <-  ymFPfactor
    yFNlabel <-  ymFNfactor
    
    vc$x      <- seq(from= -6,to= 7,length.out = 1000)
    vc$f.x    <- dnorm(vc$x,mean = -1,sd = 1.5)
    vc$y      <- seq(from=-6,to=7,length.out = 1000)
    vc$f.y    <-  dnorm(vc$y,mean =1,sd = 1.5)
    vc$data   <- data.frame(x=vc$x,trace_0=vc$f.x , trace_1=vc$f.y)
    mydata <<- vc$data
    
    vc$fnval  <-pnorm(q = input$mCutOffLine,mean = 1,sd = 1.5)
    vc$fpval  <- 1- pnorm(q = input$mCutOffLine,mean = -1,sd = 1.5)
    if (input$mCutOffLine>=0){
      vc$tnval  <- 1- get_overlap_coef(mu1=-1, mu2=1, sd1=1.5, sd2=1.5)
      vc$tpval <- 1-(vc$fnval+vc$fpval)
    }
    else{
      vc$tnval  <- 1-(vc$fnval+vc$fpval)
      vc$tpval  <- 1- get_overlap_coef(mu1=-1, mu2=1, sd1=1.5, sd2=1.5)
    }
    xxgtotal  <-(vc$fnval+vc$fpval+vc$tnval+vc$tpval)
    vc$fnval  <- fnpercentZeroDigit(vc$fnval/xxgtotal)
    vc$fpval  <- fnpercentZeroDigit(vc$fpval/xxgtotal)
    vc$tnval  <- fnpercentZeroDigit(vc$tnval/xxgtotal)
    vc$tpval  <- fnpercentZeroDigit(vc$tpval/xxgtotal)
    
    
    if (sum(abs(vc$rr[1:4]))==0){
      vc$rr[1,1]    <- c(as.numeric(str_sub(vc$tpval, end=-2)))
      vc$rr[1,2]    <- c(as.numeric(str_sub(vc$fpval, end=-2)))
      vc$rr[1,3]    <- c(as.numeric(str_sub(vc$tnval, end=-2)))
      vc$rr[1,4]    <- c(as.numeric(str_sub(vc$fnval, end=-2)))
      vc$rr[1,9]    <- c(as.numeric(input$mCutOffLine))
      vc$rr[1,10]    <- c(as.numeric(input$mCutOffLine))
      
    }
    
    ##############################################
    
    if (sum(abs(vc$rr[5:8]))==0){
      vc$rr[1,5]    <- c(as.numeric(str_sub(vc$tpval, end=-2)))
      vc$rr[1,6]    <- c(as.numeric(str_sub(vc$fpval, end=-2)))
      vc$rr[1,7]    <- c(as.numeric(str_sub(vc$tnval, end=-2)))
      vc$rr[1,8]    <- (100-(vc$rr[1,5]+vc$rr[1,6]+vc$rr[1,7]))
      # vc$rr[1,8]    <- c(as.numeric(str_sub(vc$fnval, end=-2)))
      vc$rr[1,9]    <- c(as.numeric(input$mCutOffLine))
      vc$rr[1,10]    <- c(as.numeric(input$mCutOffLine))
    }
    else if (sum(abs(vc$rr[5:8]))!=0){
      vc$rr[1,1]    <<- vc$rr[1,5]
      vc$rr[1,2]    <<- vc$rr[1,6]
      vc$rr[1,3]    <<- vc$rr[1,7]
      vc$rr[1,4]    <<- vc$rr[1,8]
      vc$rr[1,9]    <<- vc$rr[1,10]
      
      vc$rr[1,5]    <- c(as.numeric(str_sub(vc$tpval, end=-2)))
      vc$rr[1,6]    <- c(as.numeric(str_sub(vc$fpval, end=-2)))
      vc$rr[1,7]    <- c(as.numeric(str_sub(vc$tnval, end=-2)))
      vc$rr[1,8]    <- (100-(vc$rr[1,5]+vc$rr[1,6]+vc$rr[1,7]))
      #vc$rr[1,8]    <- c(as.numeric(str_sub(vc$fnval, end=-2)))
      vc$rr[1,10]    <- c(as.numeric(input$mCutOffLine))
      
    }
    
    Cutoff_A <- c(0,0,0,0)
    Cutoff_B <- c(0,0,0,0)
    Statistics_  <- c("PPV","NPV","Sensitivity","Specificity")
    vc$tt     <- data.frame(Statistics_,Cutoff_A,Cutoff_B)
    
    vc$tt[1,2]  <- round(vc$rr$TP1 / (vc$rr$TP1 + vc$rr$FP1),4)  # PPV1
    vc$tt[2,2]  <- round(vc$rr$TN1 / (vc$rr$FN1 + vc$rr$TN1),4)  #mNPV1
    vc$tt[3,2]  <- round(vc$rr$TP1 / (vc$rr$TP1 + vc$rr$FN1),4)  #mSensitivity1
    vc$tt[4,2]  <- round(vc$rr$TN1 / (vc$rr$FP1 + vc$rr$TN1),4)  #mSpecificity1
    
    vc$tt[1,3]  <- round(vc$rr$TP2 / (vc$rr$TP2 + vc$rr$FP2),4) # PPV2
    vc$tt[2,3]  <- round(vc$rr$TN2 / (vc$rr$FN2 + vc$rr$TN2),4) #mNPV2
    vc$tt[3,3]  <- round(vc$rr$TP2 / (vc$rr$TP2 + vc$rr$FN2),4) #mSensitivity2
    vc$tt[4,3]  <- round(vc$rr$TN2 / (vc$rr$FP2 + vc$rr$TN2),4) #mSpecificity2
    
    
    ##############################################
    
    
    js$backgroundCol("xTPlabel","#ffd3d9")
    js$backgroundCol("xFPlabel","#ffd3d9")
    js$backgroundCol("xTNlabel","#ffd3d9")
    js$backgroundCol("xFNlabel","#ffd3d9")
    
    js$backgroundCol("yTPlabel","lightblue")
    js$backgroundCol("yFPlabel","lightblue")
    js$backgroundCol("yTNlabel","lightblue")
    js$backgroundCol("yFNlabel","lightblue")
    
    ######################################################################################
    #fixing margin for the plot
    ######################################################################################
    par(mar = c(2, 2, 1, 1))
    mXaxisname <- paste0("Cut-off A:",
                    "TP:",fnpercentZeroDigit(vc$rr$TP1/100),";  ",
                    "FP:",fnpercentZeroDigit(vc$rr$FP1/100),";  ",
                    "FN:",fnpercentZeroDigit(vc$rr$FN1/100),";  ",
                    "TN:",fnpercentZeroDigit(vc$rr$TN1/100) ,'<br>',
                    "Cut-off B:",
                    "TP:", fnpercentZeroDigit(vc$rr$TP2/100),";  ",
                    "FP:",fnpercentZeroDigit(vc$rr$FP2/100),";  ",
                    "FN:",fnpercentZeroDigit(vc$rr$FN2/100),";  ",
                    "TN:",fnpercentZeroDigit(vc$rr$TN2/100)
                    )
    
    output$mcutoffplot <- renderPlotly({
      m <- list(l = 0.1,r = 0.1,b = 0.1,t = 0.1,pad = 0.05)
      fig <- plot_ly(vc$data, x = ~x, y = ~trace_0, name = 'trace 0', 
                     type = 'scatter', mode = 'lines',showlegend = FALSE) 
      fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
      fig <- fig %>% layout(
        xaxis = list(
          title = "",
          titlefont = f,
          showticklabels = TRUE,
          exponentformat = "E",
          showgrid = FALSE,
          showline = T,plot_bgcolor = "#000000",
          tickformat = ".1f"
        ),
        yaxis = list(
          title = "",
          titlefont = f,
          showticklabels = FALSE,
          exponentformat = "E",
          showgrid = ifelse(input$mxygrid==TRUE,TRUE,FALSE),
          showline = T,
          tickformat = ".2f"
        )
      )
      
      #True Negative
      fig <- fnaddpolygonplotly(MU = -1,SD = 1.5,Lower = -6,Upper = input$mCutOffLine,dataLength = 1000,fill.color = '#cbf3f3',Fig = fig)
      #False Negative
      fig <- fnaddpolygonplotly(MU = 1,SD = 1.5,Lower = -6,Upper = input$mCutOffLine,dataLength = 1000,fill.color = "#e5a3ad",Fig = fig)
      #True Positive
      fig <- fnaddpolygonplotly(MU = 1,SD = 1.5,Lower = input$mCutOffLine,Upper = 7,dataLength = 1000,fill.color = "#ffd3d9",Fig = fig)
      #False Positive
      fig <- fnaddpolygonplotly(MU = -1,SD = 1.5,Lower = input$mCutOffLine,Upper = 7,dataLength = 1000,fill.color = "#98c0c0",Fig = fig)
      
      
      fig <-  add_lines(p = fig,x =vc$rr[1,9],y = NULL,z = NULL, color = I("black"))
      fig <-  add_text(p = fig,x =  vc$rr[1,9], y =max(vc$data$trace_0)*1.02, text = "A", textfont = t,color = I("black"))
      
      fig <-  add_lines(p = fig,x =vc$rr[1,10],y = NULL,z = NULL, color = I("red"))
      fig <-  add_text(p = fig,x =  vc$rr[1,10], y =max(vc$data$trace_0)*1.02, text = "B", textfont = t,color = I("red"))
      
      fig <-  add_text(p = fig,x =  xTPlabel, y = yTPlabel, text = "TP", textfont = t)
      fig <-  add_text(p = fig,x =  xTNlabel, y = yTNlabel, text = "TN", textfont = t)
      fig <-  add_text(p = fig,x =  xFPlabel, y = yFPlabel, text = "FP", textfont = t)
      fig <-  add_text(p = fig,x =  xFNlabel, y = yFNlabel, text = "FN", textfont = t)
      
      fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
      fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 1', mode = 'lines')
      
      fig <- fig %>% layout(autosize = F, width = 350, height = 275, margin = m)
      
      fig <- fig %>% add_text(
          x=3.5,
         y=.30,
        color=I("#ff0c0c"),
        text='-----Diseased (Pos Test)-----'
        )

      
      fig <- fig %>% add_text(
         x=-3,
         y=.30,
        color=I("#006400"),
        text='----Healthy (Neg Test)----'
        )
      fig <- fig %>% config(displayModeBar=F)
      fig
    })
    
  })
  
  library(ggtext)
  output$mshowmatrixplot <- renderPlot({
    vvrr  <- vc$tt[c(2,3,1,4),] 
    vv <- gather(vvrr, key="cutoff", value="value", 2:3)
    vv$value[vv$cutoff=="Cutoff_A"] <- vv$value*-1

    ggplot(vv, aes(fill=cutoff, y=value, x= Statistics_)) + 
      geom_rect(xmin = -Inf, xmax = 6,   ymin = -Inf, ymax = 0,   fill = "#e2edf7") +   #where you got : https://stackoverflow.com/questions/15903868/draw-multiple-squares-with-ggplot
      geom_rect(xmin = -Inf, xmax = 6,   ymin = 0, ymax = Inf,   fill = "#d9ead3") +
      geom_bar(position='stack', stat='identity', color = "black", width = 0.45) +
      scale_y_continuous(labels = scales::percent)+
      geom_text(aes(label = fnpercentZeroDigit(ifelse(value<0,-value,value)), x=Statistics_, y=value),
                position = position_stack(vjust = .5,reverse = F), size = 5, angle = 360, color = 'white',fontface='bold')+
      theme_bw(base_size = 14)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      labs(fill="")+
      coord_flip()+  
      annotate("text", x = 0.6, y = .55, label = "------------Cut-off B--------------",
               col = I("#000000"), size = 4.0,fontface='bold')+
      annotate("text", x = 0.6, y = -.55, label = "--------------Cut-off A------------",
               col = I("#000000"), size = 4.0,fontface='bold')+
      # xlim(-1, 1)+
      ylim(-1, 1)+
      annotate("text", x = 4.0+.4, y = 0, label = "Specificity",
               col = I("#000000"), size = 4,fontface='bold')+
      annotate("text", x = 3.0+.4, y = 0, label = "PPV",
               col = I("#000000"), size = 4,fontface='bold')+
      annotate("text", x = 2.0+.4, y = 0, label = "Sensitivity",
               col = I("#000000"), size = 4,fontface='bold')+
      annotate("text", x = 1.0+.4, y = 0, label = "NPV",
               col = I("#000000"), size = 4,fontface='bold')+
      theme(legend.position= 'none',axis.title=element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())+
      scale_x_discrete(limits=c(unique(vv$Statistics_)))+
      #Specify colours
      scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
      theme(plot.margin = unit(c(.1, .1, .1, .1), "cm"))
  })

  output$mCutoffBTable <- renderUI({
    x <- data.frame(
      Disease=c(vc$rr$TP2,vc$rr$FN2,(vc$rr$TP2+vc$rr$FN2)),
      'No Disease'=c(vc$rr$FP2,vc$rr$TN2,(vc$rr$FP2+vc$rr$TN2)),
      Total = c((vc$rr$TP2+vc$rr$FP2),(vc$rr$FN2+vc$rr$TN2),(vc$rr$TP2+vc$rr$FP2+vc$rr$FN2+vc$rr$TN2))
      
      )
    rownames(x) <- c("Positive","Negative","Total")
    HTML(kbl(x,  escape = F, align=c('cccc'),
      table.attr = 'class="table" style="color: red font-style: bold; font-family:Cambria; font size="6";"')%>%
        row_spec(0:nrow(x), angle = 360,bold=FALSE, color = "black",background = '#ffffcd',font_size = 12)%>%
        row_spec(nrow(x), angle = 360,bold=TRUE, color = "black",font_size = 12)%>%  #for formatting table header
        column_spec(4,bold=TRUE, color = "black")%>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    )
  })
 
  
 
  
  output$mCutoffATable <- renderPrint({
    x <- data.frame(
      Disease=c(vc$rr$TP1,vc$rr$FN1,(vc$rr$TP1+vc$rr$FN1)),
      'No Disease'=c(vc$rr$FP1,vc$rr$TN1,(vc$rr$FP1+vc$rr$TN1)),
      Total = c((vc$rr$TP1+vc$rr$FP1),(vc$rr$FN1+vc$rr$TN1),(vc$rr$TP1+vc$rr$FP1+vc$rr$FN1+vc$rr$TN1))
      
    )
    rownames(x) <- c("Positive","Negative","Total")
    HTML(kbl(x,  escape = F, align=c('cccc'),
             table.attr = 'class="table" style="color: red font-style: bold; font-family:Cambria; font size="6";"')%>%
           row_spec(0:nrow(x), angle = 360,bold=FALSE, color = "black",background = '#ffffcd',font_size = 12)%>%
           row_spec(nrow(x), angle = 360,bold=TRUE, color = "black",font_size = 12)%>%  #for formatting table header
           column_spec(4,bold=TRUE, color = "black")%>%
           kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    )
  })
  


  output$m1stplotly <- renderPlotly({
    df <- data.frame(
      x = c(vc$rr$TP1,vc$rr$FN1,vc$rr$FP1,vc$rr$TN1),
      y = c(vc$rr$TP2,vc$rr$FN2,vc$rr$FP2,vc$rr$TN2),
      labels = c('TP','FN','FP','TN')
    )
    m <- list(l = 0.1,r = 0.1,b = 0.1,t = 0.1,pad = 0.05)
    fig <- plot_ly(textinfo='label+percent')
    fig <- fig %>% add_pie(data = df, labels = ~labels, values = ~x,showlegend = FALSE,
                           title='Area Share::Cut-off A', hoverinfo = 'none',
                           marker = list(colors = c("#ffd3d9","#e5a3ad","#98c0c0","#cbf3f3")),
                           name = "Cut")
    fig <- fig %>% config(displayModeBar=F)
    fig <- fig %>% layout(autosize = F, width = 175, height = 175, margin = m)
    fig
  })
  
  output$m2ndplotly <- renderPlotly({
    df = data.frame(
      x = c(vc$rr$TP1,vc$rr$FN1,vc$rr$FP1,vc$rr$TN1),
      y = c(vc$rr$TP2,vc$rr$FN2,vc$rr$FP2,vc$rr$TN2),
      labels = c('TP','FN','FP','TN')
    )
    
    m <- list(l = 0.1,r = 0.1,b = 0.1,t = 0.1,pad = 0.05)
    fig <- plot_ly(textinfo='label+percent')
    fig <- fig %>% add_pie(data = df, labels = ~labels, values = ~y,showlegend = FALSE,
                           title='Area Share::Cut-off B', hoverinfo = 'none',
                           marker = list(colors = c("#ffd3d9","#e5a3ad","#98c0c0","#cbf3f3")),
                           name = "Cut")
    fig <- fig %>% config(displayModeBar=F)
    
    fig <- fig %>% layout(autosize = F, width = 175, height = 175, margin = m)
    fig
  })
  

observe({
    popupdf <- data.frame(
      stat_code =as.character(), 
      Intro = as.character(),
      Spl_comment =as.character()
    )
    
    popupdf <- add_row(popupdf,
                         stat_code="xmPPV",
                         Intro =paste("The positive predictive value (PPV/Precision) describes the probability of having the disease if the test result is positive.",
                                      "Higher PPV means that a positive test result is more likely to be true. PPV is related to both sensitivity and specificity"
                         ),
                         Spl_comment=paste( "A change in a test cutoff point causing a decrease in false positives and true positive will increase PPV.",
                                            "PPV also depends on disease prevalence as it is calculated using both diseased patients (TPs) and healthy patients (FPs).",
                                            "If disease prevalence is high (eg, 20% prevalence), an individual in that population will be more likely to have the disease.", 
                                            "The greater the prevalence, the greater the PPV.",
                                            "One thing you may notice is that in a rare condition;",
                                            "even a diagnostic test with a very high sensitivity may result in a low PPV")
    )
    popupdf <- add_row(popupdf,
                         stat_code="xmNPV",
                         Intro = paste("The negative predictive value (NPV) is defined as the probability of being free of a disease if the test result is negative.",
                                       "One very important thing to remember is that the NPV will vary with the pretest probability of a disease.",
                                       "A patient with a high probability of having a disease will have a low NPV",
                                       "and a patient with a low probability of having a disease will have a high NPV.",
                                       " A change in a test cutoff point that causes an increase in the number of false negative and true negative will decrease NPV"
                         ),
                         Spl_comment=paste("NPV decreases as prevalence increases and increases as prevalence decreases.",
                                           "Note that the prevalence of a disease is directly related to the pre-test probability of having the disease  and also affects the NPV.")
    )
    popupdf <- add_row(popupdf,
                         stat_code="xmTPR",
                         Intro =paste("Sensitivity (recall / True Positive Rate (TPR) is the proportion of subjects with disease in whom the test result is positive.  It indicates how well a test can screen for a disease. A higher sensitivity makes it less likely that there are false negatives, meaning that a negative test better rules out the disease",'<b>',"(snout).",'</b>'),
                         Spl_comment=paste( "The sensitivity and specificity of a test are fixed values which do not vary with the pretest probability of a disease. Most researchers agree that the ideal diagnostic test should have high sensitivity and specificity.")
    )
    popupdf <- add_row(popupdf,
                         stat_code="xmTNR",
                         Intro =paste("Specificity (True Negative Rate (TNR)) is the poportion of subjects without disese in whom the test result is negative. It indicates how well a test can confirm the diagnosis. The higher the specificity, the less likely there are false positives, meaning that a positive test result better rules in the condition",'<b>',"(spin).",'</b>'),
                          Spl_comment=paste("The sensitivity and specificity of a test are fixed values which do not vary with the pretest probability of a disease. Most researchers agree that the ideal diagnostic test should have high sensitivity and specificity."))
    
    for (r in 1:nrow(popupdf)){
      assign(paste0(popupdf[r,1],"TXT"),eval(HTML(paste("<h5>",'<b>',"Intro:",'</b>',popupdf[r,2],'<br>','<b>',"Special Note:",'</b>', popupdf[r,3]))))
    }
    
    addPopover(session,id = "xmTNR",content = xmTNRTXT, title = NULL,
               placement = 'left', trigger = 'hover' , options = list(container = 'body'))
    addPopover(session,id = "xmPPV",content = xmPPVTXT, title = NULL,
               placement = 'left', trigger = 'hover' , options = list(container = 'body'))
    addPopover(session,id = "xmTPR",content = xmTPRTXT, title = NULL,
               placement = 'right', trigger = 'hover' , options = list(container = 'body'))
    addPopover(session,id = "xmNPV",content = xmNPVTXT, title = NULL,
               placement = 'right', trigger = 'hover' , options = list(container = 'body'))  
})
  
  
  
  output$xmPPV <- renderText({
    HTML(paste('<h6><b><i>','PPV','</i></b><br>',
               "TP / (TP + FP)","<br>",
               "For A: ",vc$rr$TP1,"/(",vc$rr$TP1,"+",vc$rr$FP1,")",' = ',
               fnpercentZeroDigit(vc$rr$TP1 /(vc$rr$TP1+vc$rr$FP1)),'<br>',
               "For B: ",vc$rr$TP2,"/(",vc$rr$TP2,"+",vc$rr$FP2,")",' = ',
               fnpercentZeroDigit(vc$rr$TP2 /(vc$rr$TP2+vc$rr$FP2))
               ))
  })
  output$xmNPV <- renderText({
    HTML(paste('<h6><b><i>','NPV','</i></b><br>',
                "TN / (FN + TN)","<br>",
               "For A: ",vc$rr$TN1,"/(",vc$rr$TN1,"+",vc$rr$FN1,")",' = ',
               fnpercentZeroDigit(vc$rr$TN1 /(vc$rr$TN1+vc$rr$FN1)),'<br>',
               "For B: ",vc$rr$TN2,"/(",vc$rr$TN2,"+",vc$rr$FN2,")",' = ',
               fnpercentZeroDigit(vc$rr$TN2 /(vc$rr$TN2+vc$rr$FN2))
               ))
  })
  output$xmTPR <- renderText({
    HTML(paste('<h6><b><i>','Sensitivity','</i></b><br>',
               "TP / (TP + FN)","<br>",
               "For A: ",vc$rr$TP1,"/(",vc$rr$TP1,"+",vc$rr$FN1,")",' = ',
               fnpercentZeroDigit(vc$rr$TP1 /(vc$rr$TP1+vc$rr$FN1)),'<br>',
               "For B: ",vc$rr$TP2,"/(",vc$rr$TP2,"+",vc$rr$FN2,")",' = ',
               fnpercentZeroDigit(vc$rr$TP2 /(vc$rr$TP2+vc$rr$FN2))
               ))
  })
  output$xmTNR <- renderText({
    HTML(paste('<h6><b><i>','Specificity','</i></b><br>',
               "TN / (FP + TN)","<br>",
               "For A: ",vc$rr$TN1,"/(",vc$rr$TN1,"+",vc$rr$FP1,")",' = ',
               fnpercentZeroDigit(vc$rr$TN1 /(vc$rr$TN1+vc$rr$FP1)),'<br>',
               "For B: ",vc$rr$TN2,"/(",vc$rr$TN2,"+",vc$rr$FP2,")",' = ',
               fnpercentZeroDigit(vc$rr$TN2 /(vc$rr$TN2+vc$rr$FP2))
               ))
  })
  
  urlScreening  <- a("Screening tests: a review with examples", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4389712/")

  output$moverview1 <- renderUI({
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h5><b><CENTER>',"Screening Test",'</CENTER></b>')),
        HTML(paste('<h6><b>',"Overview:",'</b><br>',
                   "Screening tests are widely used in medicine to assess the likelihood that members of a defined population have a particular disease.",'<br>','<br>',
                   "With few exceptions, screening tests do not diagnose the illness."))
        
      )
    )
  })
  output$moverview2 <- renderUI({
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h6><b>',"Overview: Cont....",'</b><h6>',
                   "A major objective of most screening tests is early detection of disease.",'<br>','<br>',
                   "Key Statistics of Screening Test includes sensitivity, specificity, positive predictive values (PPV) and, negative predictive value (NPV)",
                   HTML(paste('<h6><b><CENTER>',"Reference PMCID: PMC4389712:",urlScreening,'</CENTER></b><h6>'))
                   ))
        
      )
    )
  })
  

} #server closure


shinyApp(ui,server)