library(rgdal)
library(viridis)
library(sf)

fillmap2<-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,leg.cex=1){
  # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
  y.uq=sort(unique(c(y,y.scl)))
  cols<-viridis(length(y.uq),direction=-1)
  shading=y
  for (i in 1:length(y)){shading[i]<-cols[which(y.uq==y[i])]}
  par(mar=c(0,0,2,0))
  if (leg.loc=="beside"){layout(matrix(1:2,ncol=2),width=c(.8,.2))}
  else
    if (leg.loc=="below"){layout(matrix(1:2,nrow=2),height=c(.6,.4))} 
  else (print("leg.loc options are below or beside"))
  plot(map,col=shading,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line)
  par(mar=c(5, 4, 4, 2) + 0.1)
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  cols.5=cols[seq(1,length(y.uq),length.out=5)]
  lab.5=cols.5
  for (i in 1:5){lab.5[i]=y.uq[which(cols==cols.5[i])[1]]}
  lab.5=round(as.numeric(lab.5),leg.rnd)
  par(mar=c(0,0,0,0))
  if (leg.loc=="beside"){
    legend_image <- as.raster(matrix(cols, ncol=1))
    text(x=1.6,
         y = seq(0,length(y.uq),length.out=5)/length(y.uq),
         labels = rev(lab.5), cex=leg.cex)
    rasterImage(legend_image, 0, 0, 1,1)} 
  else{
    legend_image <- as.raster(matrix(cols, nrow=1))
    text(y=-0.25,
         x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),
         labels = lab.5, cex=leg.cex)
    rasterImage(legend_image, 0, 0, 2,1)}
}

library(shiny)

#data and shapefile setup

data = read.csv("data/final_data.csv")
effects = read.csv("data/shiny effects.csv")

NCtracts = readOGR("data/tl_2016_37_tract.shp")
NHtracts = NCtracts[which(NCtracts$COUNTYFP == 129),]
tracts = NHtracts[order(NHtracts$TRACTCE),]
tracts = tracts[which(tracts$TRACTCE != 990100),]

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("2010-2018 Wilmington Police Department Arrest Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  label="Year", 
                  min=2010,
                  max=2018,
                  value=2010,
                  sep="",
                  animate=animationOptions(interval=500,loop=TRUE)),      
      radioButtons("data",
                   label="Data:",
                   c("Total Overall Arrests",
                     "White Only Arrests",
                     "Black Only Arrests")),
      radioButtons("adj",
                   label="Data Adjustment:",
                   c("No Adjustments",
                     "As a Percent of the County Population",
                     "As a Percent of Totals Overall Arrests",
                     "Standardized Incidence Ratio",
                     "Poisson Regression"))),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      tableOutput("table"))
  )))

# Define server logic required to draw a histogram
server <- (function(input, output) {
  output$text <- renderText({
    if (input$adj=="None"){
      "No adjustments specified. These are the counts of arrests during each year for the selected data."
    } else
      if (input$adj=="Standardized Incidence Ratio"){
        "The standardized incidence ratio (SIR) adjustment was applied here. SIR is a method of adjusting for tract population by calculating a ratio of the observed count of arrests to the expected counts of arrests. The expected count of arrests is calculated as a rate of arrests over all tracts and years times the tract population for a given year. The population used reflects the data being displayed (e.g. Black population only when 'Black Only Arrests' is selected). Values greater than 1 suggest more observed arrests than expected."
      } else
        if (input$adj=="Poisson Regression"){
          "The Poisson regression option for adjustment was applied here. In this method of adjustment, a Poisson regression model with spatio-temporal covariate adjustment was applied (see table output). The mapped values display the residual spatial variation in arrests after adjustment where higher (darker) values indicate areas of increased risk. All estimates are transformed so that they can be interpreted as a multiplicative change in the relative risk of arrests. Tract population is indirectly adjusted for."
        } else 
          if (input$adj=="As a Percent of the Population"){
            "The 'As a Percent of the Population' adjustment displays the selected arrests counts divided by the appropriate population (e.g. Black population only when 'Black Only Arrests' is selected) times 100%."
          } else{#% tot arrests
            "The 'As a Percent of Total Arrests' adjustment displays the selected arrests counts divided by the total arrest counts times 100%."
          }})
  
  output$map <- renderPlot({
    if (input$adj == "None"){
      if (input$data == "Total Arrests"){
        fillmap2(tracts,paste(input$year,input$data),data$arrests_total[which(data$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$arrests_total,leg.cex=1,leg.rnd=3)
      }
      if (input$data == "White Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data),data$arrests_W[which(data$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$arrests_W,leg.cex=1,leg.rnd=3)
      }
      if (input$data == "Black Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data),data$arrests_B[which(d.inla$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$arrests_B,leg.cex=1,leg.rnd=3)
      }}
    if (input$adj == "As a Percent of the Population"){
      if (input$data == "Total Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$tot_arr_pct_co_pop[which(data$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$tot_arr_pct_co_pop,leg.cex=1,leg.rnd=3)
      }
      if (input$data == "White Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$w_arr_pct_co_pop[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$data == "Black Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$b_arr_pct_co_pop[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }}
    if (input$adj == "As a Percent of Total Arrests"){
      if (input$data == "Total Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$tot_arr_pct_total[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$data == "White Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$w_arr_pct_total[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$data == "Black Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$b_arr_pct_total[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }}
    if (input$adj == "Standardized Incidence Ratio"){
      if (input$data == "Total Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$sir_total[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$data == "White Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$sir_white[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$data == "Black Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),data$sir_black[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }}
    if (input$adj == "Poisson Regression"){
      if (input$data == "Total Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),exp(data$total_effects[which(data$year==input$year)]),map.lty=0,leg.loc='below',y.scl=exp(data$total_effects),leg.cex=1,leg.rnd=3)
      }
      if (input$data == "White Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),exp(data$white_effects[which(data$year==input$year)]),map.lty=0,leg.loc='below',y.scl=exp(data$white_effects),leg.cex=1,leg.rnd=3)
      }
      if (input$data == "Black Only Arrests"){
        fillmap2(tracts,paste(input$year,input$data,input$adj),exp(data$black_effects[which(data$year==input$year)]),map.lty=0,leg.loc='below',y.scl=exp(data$black_effects),leg.cex=1,leg.rnd=3)
      }}})
  
  
  output$table <- renderTable({
    if (input$adj=="Poisson Regression"){
      if (input$data=="Total Arrests"){
        mat=effects[1:7,]
        colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
        rownames(mat)<-c("% Black",
                         "% Living in Poverty",
                         "% Bachelors degree or more",
                         "% Male",
                         "% Secondary Homes",
                         "% Aged 18-24",
                         "Government Entity")
        mat
      } else
        if (input$data=="White Only Arrests"){
          mat=effects[8:14,]
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24",
                           "Government Entity")
          mat
        } else {
          mat=effects[15:21,]
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24",
                           "Government Entity")
          mat
        }
    }},rownames=T,colnames=T,digits=3,width="100%")
})

# Run the application 
shinyApp(ui = ui, server = server)

