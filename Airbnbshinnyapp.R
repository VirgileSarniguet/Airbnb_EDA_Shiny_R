#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(viridis)
library(plotly)
library(ggmap)
library(GGally)
library(zoo)


load("AirBnB.RData")

L2<-L%>%
  select(c('id','host_id','host_name','host_is_superhost','latitude','longitude','zipcode','price','bedrooms','property_type','room_type','bed_type','accommodates','bedrooms','bathrooms','number_of_reviews'))%>%
  filter(zipcode %in% c(seq(75001,75020,1),75116),!is.na(bedrooms),!is.na(bathrooms))%>%
  mutate(arrondissement=as.factor(substring(zipcode,5-1,5)),pricepernight=(as.numeric(gsub("[\\$,]","",price))))

#remove the price and zipcode column that have no more utility
L2<-L2%>%
  select(-c('price','zipcode'))%>%
  mutate(pricesizeratio=pricepernight/accommodates)

L2<-L2%>%
  filter(!(id %in% c(5180527,3493067,12484402,6897854,4984405,8259190,8874063,9269657,11872027,1324779,957071,9173969,2217395,11640000,7474143,7284676,5046286,6755931)))

Lvar<-L2%>%
  mutate_each(funs(factor),c('bedrooms','accommodates','bathrooms'))

L4<-
  inner_join(R,L2,by=c('listing_id'='id'))%>%
  mutate(date2=as.Date(date),month=as.factor(months(date2)),year=year(date2),monthyear=as.yearmon(date))

Lhost<-Lvar%>%
  group_by(host_id,host_name)%>%
  summarise(num_of_accomodation=n(),average_rental_price=mean(pricepernight))

Lhost<-inner_join(Lvar,Lhost,"host_id")
Lhost<-Lhost%>%
  select(c("host_id","latitude","longitude","num_of_accomodation"))

paris_map5<-get_stamenmap(
  bbox=c(left=2.24,right=2.43,top=48.91,bottom=48.81),
  maptype = "toner-lite",
  zoom=12
)

# User interface

ui2 <- navbarPage("AirBnb Paris dataset EDA",
                  #First panel
                  
                  
                  tabPanel("Variables",
                           titlePanel("Univariate distribution"),         
                           sidebarLayout(
                             
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                               
                               # Input: Slider for the number of bins ----
                               selectInput(inputId = "variabledis",
                                           label = "Select a variable:",
                                           choices = c('n° of bedrooms'='bedrooms','n° of bathrooms'='bathrooms','Host is superhost'='host_is_superhost','Type of room'='room_type','Type of accomodation'='property_type','arrondissement'='arrondissement','n° of people accomodated'='accommodates','type of bed'='bed_type',"Number of reviews"="number_of_reviews","Price per night (continous)"="pricepernight","Price/accomodates ratio (continuous)"="pricesizeratio"),
                                           selected='arrondissement'),
                               checkboxInput(inputId = "zoomin",
                                             label = "Zoom in (only for continuous)",
                                             value = FALSE),
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Plot",plotOutput(outputId ="vardisPlot")),
                                 tabPanel("Summary",verbatimTextOutput(outputId = "summary"))
                               )
                             )),
                           hr(),
                           titlePanel("Bivariate distribution"),
                           sidebarLayout(
                             sidebarPanel(h4("Select 2 categorical variables to cross"),
                                          selectInput(inputId = "xvardis",
                                                      label = "Select x variable:",
                                                      choices = c('n° of bedrooms'='bedrooms','n° of bathrooms'='bathrooms','Type of room'='room_type','Type of accomodation'='property_type','arrondissement'='arrondissement','n° of people accomodated'='accommodates','type of bed'='bed_type'),
                                                      selected='arrondissement'),
                                          selectInput(inputId = "yvardis",
                                                      label = "Select y variable:",
                                                      choices = c('n° of bedrooms'='bedrooms','n° of bathrooms'='bathrooms','Type of room'='room_type','Type of accomodation'='property_type','arrondissement'='arrondissement','n° of people accomodated'='accommodates','type of bed'='bed_type'),
                                                      selected='property_type'),
                                          checkboxInput(inputId = "histo",
                                                        label = "Display histogram",
                                                        value = FALSE)
                                          
                             ),
                             mainPanel(
                               plotOutput(outputId="crossplotvar")
                             ))),
                  
                  #Price vs features panel
                  tabPanel("Prices" ,
                           titlePanel("Accommodations prices distribution for each feature"), 
                           sidebarLayout(
                             
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                               
                               # Input: Slider for the number of bins ----
                               selectInput(inputId = "variable",
                                           label = "Select a variable:",
                                           choices = c('n° of bedrooms'='bedrooms','n° of bathrooms'='bathrooms','Type of room'='room_type','Type of accomodation'='property_type','arrondissement'='arrondissement','n° of people accomodated'='accommodates','type of bed'='bed_type'),
                                           selected='arrondissement'),
                               sliderInput(inputId = "price",
                                           label = "Select a price range to zoom in or out",
                                           min=0,
                                           max=3650,
                                           value = c(0,2000)
                               )
                             ),
                             
                             mainPanel(
                               
                               plotOutput(outputId = "varPlot")
                               
                             )),
                           hr(),
                           titlePanel("Bivariate analysis of price"),
                           sidebarLayout(
                             sidebarPanel(h5("Cross 2 variables"),
                                          selectInput(inputId = "xvar",
                                                      label = "Select x variable:",
                                                      choices = c('n° of bedrooms'='bedrooms','n° of bathrooms'='bathrooms','Type of room'='room_type','Type of accomodation'='property_type','arrondissement'='arrondissement','n° of people accomodated'='accommodates','type of bed'='bed_type'),
                                                      selected='arrondissement'),
                                          selectInput(inputId = "yvar",
                                                      label = "Select y variable:",
                                                      choices = c('n° of bedrooms'='bedrooms','n° of bathrooms'='bathrooms','Type of room'='room_type','Type of accomodation'='property_type','arrondissement'='arrondissement','n° of people accomodated'='accommodates','type of bed'='bed_type'),
                                                      selected='property_type'),
                                          checkboxInput(inputId = "median",
                                                        label = "Display median price",
                                                        value=FALSE)
                                          
                             ),
                             mainPanel(
                               plotOutput(outputId="crossplot")
                             ))
                  ),
                  
                  
                  
                  tabPanel("Prices mapping",
                           titlePanel("Listings mapping according to price range"),    
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput(inputId = "price2",
                                           label = "Select a price range",
                                           min=0,
                                           max=3650,
                                           value = c(0,3650)
                               ),
                               checkboxInput(inputId = "pricesizeratio",
                                             label = "Display price per person accomodated (from 0 to 751) ",
                                             value = FALSE),
                               actionButton("display", "Update map")
                             ),
                             mainPanel(
                               plotOutput(outputId = "mappy"), 
                               plotOutput(outputId= "priceplot")
                             )    
                           ) 
                  ),
                  
                  
                  tabPanel("Hosts",
                           titlePanel("Hosts analysis"), 
                           tabsetPanel(
                             tabPanel("host variables",plotOutput(outputId ="hostplot")),
                             tabPanel("Map",plotOutput(outputId ="hostmap")),
                             tabPanel("Table",dataTableOutput(outputId = "hosttab"))),
                           hr(),
                           fluidRow(
                             column(6,
                                    selectInput(inputId = "variablehost",
                                                label = "Select a variable to see its distribution :",
                                                choices = c('Number of listing per owner'='num_of_accomodation','Average rental price per owner'='average_rental_price')),
                                    checkboxInput(inputId = "zoomo",
                                                  label = "Zoom in",
                                                  value = FALSE),
                                    checkboxInput(inputId = "density",
                                                  label = "Display density",
                                                  value = FALSE)
                             ),
                             column(6,
                                    sliderInput(inputId = "price3",
                                                label = "Select a num of listing range to display on the map",
                                                min=0,
                                                max=153,
                                                value = c(0,153)
                                    ),
                                    actionButton("display2", "Update map")
                             ))
                  ),
                  tabPanel("Visit frequency",
                           fluidPage(
                             # App title ----
                             titlePanel("Airbnb rental time serie analysis"),
                             
                             tabsetPanel(
                               tabPanel("Global timeline plot",plotlyOutput(outputId = "globtime")),
                               tabPanel("Year and quarter comparison plot", plotOutput(outputId = "distPlot")),
                               tabPanel("Small multiple plots",plotOutput(outputId = "smulPlot")),
                               tabPanel("Table", dataTableOutput(outputId = "distTab"))
                             ),
                             hr(),
                             fluidRow(
                               column(3,
                                      h3("Airbnb rental in paris 2010/2016"),
                                      h5("This plots allows comparison between years and arrondissement")
                               ),
                               
                               column(4,selectInput(inputId = "years",
                                                    label = "Select the years:",
                                                    choices = c('2010'='2010','2011'='2011','2012'='2012','2013'='2013','2014'='2014','2015'='2015','2016'='2016'),
                                                    selected='2015',
                                                    multiple = TRUE),
                                      checkboxInput(inputId = "allyears",
                                                    label = "Select all the years",
                                                    value = FALSE)
                               )
                               ,
                               column(4,
                                      selectInput(inputId = "arrondissement",
                                                  label = "Select the arrondissements:",
                                                  choices = c('1'='01','2'='02','3'='03','4'='04','5'='05','6'='06','7'='07','8'='08','9'='09','10'='10','11'=                   '11','12'='12','13'='13','14'='14','15'='15','16'='16','17'='17','18'='18','19'='19','20'='20'),
                                                  selected='08',
                                                  multiple = TRUE),
                                      checkboxInput(inputId = "allarrond",
                                                    label = "Select all the arrondissements",
                                                    value = FALSE),
                                      checkboxInput(inputId = "cumarrond",
                                                    label = "Cumulate all the arrondissements",
                                                    value = FALSE),
                                      checkboxInput(inputId = "byarrondissement",
                                                    label = "Show facet by arrondissement",
                                                    value = FALSE)
                                      
                               ))))
)

server2 <- function(input, output) {
# Panel n°1 Variable Distribution  
  #Variable distribution plot
  output$vardisPlot<-renderPlot({
    
    if (input$variabledis %in% c("pricepernight","pricesizeratio")){
      if(input$zoomin){
        mccp<-500
        mccr<-150}
      else{
        mccp<-3650
        mccr<-670
      }
      if (input$variabledis %in% c("pricepernight")){ 
        Lvar%>%
          ggplot()+
          geom_histogram(aes(x=.data[[input$variabledis]],y=..density..,fill=..count..),bins=120)+
          geom_rug(aes(x=.data[[input$variabledis]]))+geom_density(aes(x=.data[[input$variabledis]]))+geom_vline(aes(xintercept = mean(.data[[input$variabledis]]),color="mean"),linetype = "dashed", size = 0.6)+
          geom_vline(aes(xintercept = median(.data[[input$variabledis]]),color="median"),linetype = "dashed", size = 0.6,show.legend = TRUE)+
          coord_cartesian(c(0,mccp))+
          theme_classic()+
          scale_color_manual(name = "averages", values = c(median = "blue", mean = "red"))+
          theme(legend.justification = c(1, 1), legend.position = c(1,1),legend.background = element_rect(fill = "white", color = "black"))+
          scale_fill_gradient(low = "blue", high = "orange") 
      }
      else {
        Lvar%>%
          ggplot()+
          geom_histogram(aes(x=.data[[input$variabledis]],y=..density..,fill=..count..),bins=120)+
          geom_rug(aes(x=.data[[input$variabledis]]))+
          geom_density(aes(x=.data[[input$variabledis]]))+
          geom_vline(aes(xintercept = mean(.data[[input$variabledis]]),color="mean"),linetype = "dashed", size = 0.6)+
          geom_vline(aes(xintercept = median(.data[[input$variabledis]]),color="median"),linetype = "dashed", size = 0.6,show.legend = TRUE)+coord_cartesian(c(0,mccr))+
          theme_classic()+
          theme(legend.justification = c(1, 1), legend.position = c(1,1),legend.background = element_rect(fill = "white", color = "black"))+
          scale_color_manual(name = "averages", values = c(median = "blue", mean = "red"))+
          scale_fill_gradient(low = "blue", high = "orange")
      }
    }
    else{  
      Lvar%>%
        ggplot(aes(x=.data[[input$variabledis]]))+
        geom_bar(aes(fill=..count..))+
        theme_classic()+
        theme(legend.position = 'none')+
        scale_fill_gradient(low = "blue", high = "orange")
    }   
  })
  #Variable distribution cross data
  output$crossplotvar<-renderPlot({
    
    if (input$histo){
      Lvar%>%
        ggplot(aes(x=.data[[input$xvardis]],fill=.data[[input$yvardis]]))+
        geom_bar(position = "fill")+
        theme_classic() 
    }  
    else{  
      Lvar%>%
        count(.data[[input$xvardis]],.data[[input$yvardis]])%>%  
        ggplot(aes(.data[[input$xvardis]],.data[[input$yvardis]]))+
        geom_point(aes(size=n,color=n))+
        scale_color_continuous(low = "blue", high = "red", breaks = c(0,1000,2000,3000,5000,10000,16000) )+
        scale_size(breaks=c(0,1000,2000,3000,5000,10000,16000),range = c(1, 10))+
        guides(color= guide_legend(), size=guide_legend())
    }
  })
  
  #Summary tabset
  output$summary <- renderPrint({
    summary(L2[[input$variabledis]])
  })
  
#Periodicity study panel
  #Global timeline plot
  output$globtime <- renderPlotly({
    ggplotly(L4%>%
               group_by(monthyear)%>%
               summarise(numofrental=n())%>%
               ggplot(aes(x=monthyear,y=numofrental))+
               geom_point(aes(y=numofrental))+
               geom_line()+theme_classic()+
               geom_smooth(se = FALSE))
  })
  
  #Line and point plot
  output$distPlot <- renderPlot({
    
    (if(input$allyears){
      targeted_year<-c(2010,2011,2012,2013,2014,2015,2016)  
    }
    else
      targeted_year<-c(input$years))
    
    (if(input$allarrond){
      targeted_arrondissement<-c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20')  
    }
      else
        targeted_arrondissement<-c(input$arrondissement))
    
    if(input$cumarrond){
      L4%>%
        filter(year %in% targeted_year)%>%
        group_by(year,month)%>%
        summarize(nummonth=n())%>%
        ggplot(aes(x=factor(month,levels=c('janvier','février','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre')),y=nummonth))+
        geom_line(aes(group=(year),linetype=factor(year)),color='#FF5A5F')+
        geom_point(aes(shape=factor(year)),color='#FF5A5F',show.legend = TRUE)+    
        theme_classic()+
        xlab('Month of the year')+ylab('n° of rental')+
        theme(axis.title.y = element_text(angle = 0),
              axis.text.x = element_text(angle = -45),
              plot.title = element_text(color="black", size=14, face="bold"),
              legend.title = element_text(face = "bold"),
              legend.background = element_rect(size=0.5, linetype="solid",colour ="black"),
        )+
        ggtitle('Airbnb reservation per arrondissement in time',subtitle = 'shows a clear pattern over the years')+
        labs(color="Arrondissement",linetype="Year",shape="Year")
    }
    else
      L4%>%
      filter(year %in% targeted_year,arrondissement %in% targeted_arrondissement)%>%
      group_by(year,month,arrondissement)%>%
      summarize(nummonth=n())%>%
      ggplot(aes(x=factor(month,levels=c('janvier','février','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre')),y=nummonth,color=arrondissement))+
      geom_line(aes(group=interaction(year,arrondissement),linetype=factor(year)))+
      geom_point(aes(shape=factor(year)),show.legend = TRUE)+    
      theme_classic()+xlab('Month of the year')+ylab('n° of rental')+
      theme(axis.title.y = element_text(angle = 0),
            axis.text.x = element_text(angle = -45),
            plot.title = element_text(color="black", size=14, face="bold"),
            legend.title = element_text(face = "bold"),
            legend.background = element_rect(size=0.5, linetype="solid",colour ="black"),
      )+
      ggtitle('Airbnb reservation per arrondissement in time',subtitle = 'shows a clear pattern over the years')+
      labs(color="Arrondissement",linetype="Year",shape="Year") 
    
    
  })
  
  #Small multiples plots
  output$smulPlot<-renderPlot({
    if (input$byarrondissement){
      if(input$allyears){
        targeted_year<-c(2010,2011,2012,2013,2014,2015,2016)  
      }
      else
      {targeted_year<-c(input$years)}  
      L4%>%
        filter(year %in% targeted_year)%>%
        group_by(year,month,arrondissement)%>%
        summarize(nummonth=n())%>%
        ggplot(aes(x=factor(month,levels=c('janvier','février','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre')),y=nummonth,color=arrondissement))+
        geom_line(aes(group=interaction(year,arrondissement),linetype=factor(year)))+
        geom_point()+facet_wrap(~arrondissement,scales = "free_y")+    
        theme_classic()+xlab('Month of the year')+ylab('n° of rental')+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1),legend.position="none")
    }
    else{
      L4%>%
        group_by(year,month)%>%
        summarize(nummonth=n())%>%
        ggplot(aes(x=factor(month,levels=c('janvier','février','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre')),y=nummonth))+
        geom_line(aes(group=(year),linetype=factor(year),color=factor(year)))+
        geom_point(aes(color=factor(year)))+
        facet_wrap(~year,scale="free_y")+    
        theme_classic()+xlab('Month of the year')+ylab('n° of rental')+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1))  
    }
  })
  
  output$distPlot2<-renderPlot({
    
    L4%>%
      ggplot(aes(x=as.factor(arrondissement)))+geom_bar()
    
  })
  
  #Time analysis plot
  output$distTab<- renderDataTable({
    targeted_year<-c(input$years)
    targeted_arrondissement<-c(input$arrondissement)
    L4%>%
      filter(year %in% targeted_year,arrondissement %in% targeted_arrondissement)%>%
      group_by(year,month,arrondissement)%>%
      summarize(rentalpermonth=n())
  }  
  )
  
  
#Price panel  
  #Variable vs pricepernight boxplot
  output$varPlot <- renderPlot({
    targeted_min<-c(input$price[1])
    targeted_max<-c(input$price[2])
    Lvar%>%
      group_by(.data[[input$variable]])%>%
      mutate(median=median(pricepernight))%>%
      ggplot(aes_string(x=input$variable))+
      geom_boxplot(aes(y=pricepernight,fill=median),alpha=0.7)+
      stat_summary(aes(y=pricepernight,color="mean"),fun = "mean", size = 1, geom = "point",shape=23)+
      theme_classic()+theme()+coord_flip(ylim=c(targeted_min,targeted_max))+
      scale_fill_gradient(low="blue", high="orange")+
      labs(x="bedrooms",colour="")
    
  },bg="transparent")
  
  #cross variables plot
  output$crossplot<- renderPlot({
    
    if(input$median){
      Lvar%>%
        group_by(.data[[input$xvar]],.data[[input$yvar]])%>%
        summarise(median_price=median(pricepernight))%>%
        ggplot(aes(.data[[input$xvar]],.data[[input$yvar]]))+
        geom_point(aes(size=median_price,color=median_price))+
        scale_color_continuous(low = "blue", high = "red", breaks = c(50,100,250,500,1000,2000,5000) )+
        scale_size(breaks=c(50,100,250,500,1000,2000,5000),range = c(1, 11))+
        guides(color= guide_legend(), size=guide_legend())
      
    }
    else{
      Lvar%>%
        group_by(.data[[input$xvar]],.data[[input$yvar]])%>%
        summarise(average_price=mean(pricepernight))%>%
        ggplot(aes(.data[[input$xvar]],.data[[input$yvar]]))+
        geom_point(aes(size=average_price,color=average_price))+
        scale_color_continuous(low = "blue", high = "red", breaks = c(50,100,250,500,1000,2000,5000) )+
        scale_size(breaks=c(50,100,250,500,1000,2000,5000),range = c(1, 11))+
        guides(color= guide_legend(), size=guide_legend())
    }
  })
  
#Price mapping tabset  
  #Prices map of Paris
  slidertype <- reactiveValues(minval=0,maxval=3650)
  
  observeEvent(input$display,{slidertype$minval<-input$price2[1]})
  observeEvent(input$display,{slidertype$maxval<-input$price2[2]})
  
  output$mappy <- renderPlot({
    if(input$pricesizeratio){
      Linter<-Lvar%>%
        filter(pricesizeratio< slidertype$maxval & pricesizeratio> slidertype$minval)  
    }
    else{
      Linter<-Lvar%>%
        filter(pricepernight< slidertype$maxval & pricepernight> slidertype$minval)
    }
    ggmap(paris_map5)+
      stat_density2d(
        aes(x = longitude, y = latitude, fill = ..level..,alpha=..level..),
        bins = 10, data = Linter,
        geom = "polygon"
      )+scale_fill_viridis(option="magma")
  })
  
  #barplot number of accommodations per arrondissement in a given price range
  output$priceplot <- renderPlot({
    targeted_min<-c(input$price2[1])
    targeted_max<-c(input$price2[2])
    
    if(input$pricesizeratio){
      Lvar%>%  
        filter(pricesizeratio< targeted_max & pricesizeratio>targeted_min)%>%
        ggplot(aes(x=arrondissement))+
        geom_bar(aes(fill=..count..))+theme_classic()+
        theme(legend.position = 'none')+
        scale_fill_gradient(low = "blue", high = "orange")
    }
    
    else{
      Lvar%>%
        filter(pricepernight< targeted_max & pricepernight>targeted_min)%>%
        ggplot(aes(x=arrondissement))+
        geom_bar(aes(fill=..count..))+
        theme_classic()+
        theme(legend.position = 'none')+
        scale_fill_gradient(low = "blue", high = "orange")
    }
  })
  
#Host sutdy tabset
  # Variable plot for host
  output$hostplot<-renderPlot({
    if (input$zoomo){
      if(input$variablehost %in% c("num_of_accomodation")){ xmax<-10}
      else{xmax=150}
    }
    else{
      if(input$variablehost %in% c("num_of_accomodation")){ xmax<-153}
      else{xmax=3700}
    }
    
    if (input$density){
      Lvar%>%
        group_by(host_id,host_name)%>%
        summarise(num_of_accomodation=n(),average_rental_price=mean(pricepernight))%>%
        ggplot()+
        geom_histogram(aes(x=.data[[input$variablehost]],y=..density..,fill=..count..),bins=153)+
        geom_rug(aes(x=.data[[input$variablehost]]))+
        geom_density(aes(x=.data[[input$variablehost]]))+
        geom_vline(aes(xintercept = mean(.data[[input$variablehost]])),linetype = "dashed", size = 0.6,color="red")+
        geom_vline(aes(xintercept = median(.data[[input$variablehost]])),linetype = "dashed", size = 0.6,color="blue")+
        theme_classic()+
        coord_cartesian(xlim=c(0,xmax))+
        scale_fill_gradient(low = "blue", high = "orange") 
    }
    else{
      Lvar%>%
        group_by(host_id,host_name)%>%
        summarise(num_of_accomodation=n(),average_rental_price=mean(pricepernight))%>%
        ggplot()+
        geom_histogram(aes(x=.data[[input$variablehost]],fill=..count..),bins=153)+
        geom_rug(aes(x=.data[[input$variablehost]]))+
        theme_classic()+coord_cartesian(xlim=c(0,xmax))+
        scale_fill_gradient(low = "blue", high = "orange")   
    }
  })
  
  #Host map plot  
  slidertype2 <- reactiveValues(minval=0,maxval=153)
  
  observeEvent(input$display2,{slidertype2$minval<-input$price3[1]})
  observeEvent(input$display2,{slidertype2$maxval<-input$price3[2]})
  
  output$hostmap <- renderPlot({
    
    Linter2<-Lhost%>%
      filter(num_of_accomodation<= slidertype2$maxval & num_of_accomodation>= slidertype2$minval)
    
    ggmap(paris_map5)+
      stat_density2d(
        aes(x = longitude, y = latitude, fill = ..level..,alpha=..level..),
        bins = 10, data = Linter2,
        geom = "polygon"
      )+scale_fill_viridis(option="magma")
  })
  
  #Tab output for Host
  output$hosttab<- renderDataTable({
    Lvar%>%
      group_by(host_id,host_name)%>%
      summarise(num_of_accomodation=n(),average_rental_price=mean(pricepernight))
  }) 
}

shinyApp(ui = ui2, server = server2)
