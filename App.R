#### THIS IS THE ONEBENTHIC SHINY DASHBOARD ####

#### LOAD LIBRARIES ####


library(shinydashboard)
library(shiny)
library(scales)
library(ggplot2)
library (RPostgres)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(DBI)
library(sf)
library(sp)
library(scales)
#__________________________________________________________________________________________
#### MAKE CONNECTION TO THE INEBENTHIC DB ####


pw <- {
  "postgres1234"
}
logged= FALSE;

## Loads PostgreSQL driver
drv <- dbDriver("Postgres")

## Creates connection to the Postgres database. Note that "con" will be used later in each connection to the database
con =  dbConnect(drv, dbname = "wgext",
                 host = "localhost",
                 port = 5433,
                 user = "postgres",
                 password = pw)
rm(pw) # remove the password


#### ####
agg <-  st_read(con, query = "SELECT * FROM areas.emodnet_ha_aggregates_areas_20190621;")

#View(agg)## Check class of objects
class(agg)#[1] "sf"         "data.frame"

## Set CRS where necessary
st_crs(agg) <- 4326

UK <- agg[which(agg$country=="United Kingdom"),]
France <- agg[which(agg$country=="France"),]
Belgium <- agg[which(agg$country=="Belgium"),]
Netherlands <- agg[which(agg$country=="The Netherlands"),]
Denmark <- agg[which(agg$country=="Denmark"),]
Germany <- agg[which(agg$country=="Germany"),]
Poland <- agg[which(agg$country=="Poland"),]
Finland <- agg[which(agg$country=="Finland"),]
Italy <- agg[which(agg$country=="Italy"),]
Russia <- agg[which(agg$country=="Russia"),]
Lithuania <- agg[which(agg$country=="Lithuania "),]
#__________________________________________________________________________________________
#### bring in some ems footprint data ####
footuk2015 <- st_read("DATA/2015footprintlatlong.shp")
footuk2014 <- st_read("DATA/2014footprintlatlong.shp")
footuk2013 <- st_read("DATA/2013footprintlatlong.shp")
footuk2012 <- st_read("DATA/2012footprintlatlong.shp")
footuk2011 <- st_read("DATA/2011footprintlatlong.shp")
footuk2010 <- st_read("DATA/2010footprintlatlong.shp")
#View(footuk2015)
#__________________________________________________________________________________________
#### SELECT DATA ####
#rm(pw) # remove the password
data = dbGetQuery(con,
                  "SELECT * FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
aggcategory_type ='Construction fill/land reclamation';")




#__________________________________________________________________________________________
#### NUMBER OF RECORDS ####

numberofsamples = dbGetQuery(con,"SELECT COUNT(*) FROM amounts.amount;")
numberofsamples <- as.numeric(as.character(numberofsamples$count))
#__________________________________________________________________________________________
#### LARGEST EXTRACTORS CUMULATIVE####

datatotext = dbGetQuery(con,
                        "SELECT * FROM amounts.amount where aggcategory_type = 'Total extracted';")

datatotext2 <- datatotext%>%group_by(country_countryname)%>%summarize(x=sum(volume))%>%arrange(desc(x))

#__________________________________________________________________________________________


#### total extracted by year (all countries) fill by agg category####

datatotextyr = dbGetQuery(con,
                          "SELECT * FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
aggcategory_type ='Construction fill/land reclamation';")
## remove na
datatotextyr <- datatotextyr[complete.cases(datatotextyr),]

#datatotextyr2 <- datatotextyr%>%group_by(year,aggcategory_type)%>%summarize(x=sum(volume))%>%arrange(desc(x))
datatotextyr2 <- datatotextyr%>%group_by(year,aggcategory_type,conventionarea_areaname)%>%summarize(x=sum(volume))%>%arrange(desc(x))

#__________________________________________________________________________________________
#### cum plot#### 
datacumplot = dbGetQuery(con,
                         "SELECT * FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
                         aggcategory_type ='Construction fill/land reclamation';")
## remove 2013 data
#datacumplot2 <- datacumplot[which(datacumplot$year!= 2013),]

## remove na
datacumplot2 <- datacumplot[complete.cases(datacumplot),]

datacumplot3 <- datacumplot2[,c(2,4,5,6),]
datacumplot4 <- datacumplot3%>%group_by(year,aggcategory_type,conventionarea_areaname)%>%summarize(x=sum(volume))
datacumplot5 <- datacumplot4%>%group_by(aggcategory_type,conventionarea_areaname)%>% mutate(cv=cumsum(x))
#View(datacumplot5)


#__________________________________________________________________________________________


#### total extracted by year (all countries) fill by jurisdiction####

datatotextjur = dbGetQuery(con,
                           "SELECT * FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
aggcategory_type ='Construction fill/land reclamation';")
## remove na
datatotextjur2 <- datatotextjur[complete.cases(datatotextjur),]

datatotextjur3 <- datatotextjur2%>%group_by(year,conventionarea_areaname)%>%summarize(x=sum(volume))%>%arrange(desc(x))
#__________________________________________________________________________________________
#### licensed area (all) #####
#licareaall = dbGetQuery(con,
#                          "SELECT * FROM areas.licence;")
#licareaall2 <- licareaall%>%group_by(year)%>%summarize(x=sum(totalarealicensed))%>%arrange(desc(x))
#__________________________________________________________________________________________
#### NATIONAL AREAS DREDGED ####

naddata = dbGetQuery(con,"select * from areas.licence;")
head(naddata)
## add col for UndredgedArea
naddata$totalareaundredged <- naddata$totalarealicensed-naddata$totalareadredged

## Get data into long format
naddata2 <- naddata[,c(2,3,5,6,8)]
head(naddata2)
library(tidyr)

naddata_long <- gather(naddata2, category, area, totalarealicensed:totalareaundredged, factor_key=FALSE)
naddata_long


## remove rows with na
naddata_long2 <-naddata_long[complete.cases(naddata_long), ]
head(naddata_long2)

library(ggplot2)
## Create seperate objects for totalarealicensed and totalareadredged
naddata_long3 <- naddata_long2[which(naddata_long2$category=="totalarealicensed"),]
naddata_long4 <- naddata_long2[which(naddata_long2$category=="totalareadredged"),]
#__________________________________________________________________________________________
#### quantities data for download####
dredgestatquantity = dbGetQuery(con,"select * from amounts.amount;")

## Drop id col
dredgestatquantity2 <- dredgestatquantity[,2:6]
#View(dredgestatquantity2)
## convert to wide format
dredgestatquantity3 <- spread(dredgestatquantity2, aggcategory_type, volume)#fill = NA,
#View(dredgestatquantity3)
dredgestatquantity4 <- dredgestatquantity3[,c(2,1,3,7,5,6,8,4,9)]
colnames(dredgestatquantity4) <- c("Country","Year","Convention","Construction","Beach","Fill","Non-agg","Exp","Total")
#__________________________________________________________________________________________
#### UI ####

ui <- dashboardPage(
  #__________________________________________________________________________________________
  #### HEADER ####  
  dashboardHeader(title=tags$b("WGEXT Dredging Stats Dashboard "),titleWidth = 400),#title = "OneBenthic dashboard"
  
  #__________________________________________________________________________________________
  #### SIDEBAR ####
  dashboardSidebar(selectInput(inputId="variableInput", multiple = T,h4("Select country",style="color:white"),choices = c("",as.character(unique(data$country_countryname)))),
                   #################
                   selectInput(inputId="fillInput", multiple = F,h4("Select fill",style="color:white"),choices = c("aggcategory_type","conventionarea_areaname"),selected ="aggcategory_type"),
                   ############
                   #h4("Map options:"),
                   #    selectInput(inputId="variableInput", multiple = F,h4("Select variable",style="color:white"),choices = c("",as.character(unique(long$variable)))),
                   #                  selectInput(inputId="valueInput", multiple = F,h4("Select value",style="color:white"),choices =NULL)
                   
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   #__________________________________________________________________________________________
                   #### OB LOGO ####
                   #   HTML('&emsp;'), HTML('&emsp;'),img(src="OBLogo.png",height = 85, width =160),
                   
                   #__________________________________________________________________________________________
                   #### OTHER LOGOS ####
                   HTML('&emsp;'),img(src="iceslogov3.png", width="77%"),#,height = 50, width =80
                   #HTML('&emsp;'),img(src="postgreslogo.png",height = 50, width =50),
                   #HTML('&emsp;'),img(src="rstudiologo.png",height = 50, width =50),
                   #HTML('&emsp;'),img(src="rshinylogo.png",height = 50, width =50),
                   br()),
  
  
  #__________________________________________________________________________________________
  #### DASHBOARD BODY ####
  
  #img(src="logos_long.png",height = 300, width = 50)
  dashboardBody(
    # Boxes need to be put in a row (or column)
    #__________________________________________________________________________________________
    #### INFO BOXES ####   
    #fluidRow(infoBoxOutput("purpose",width=12)),
    #fluidRow(
    #infoBoxOutput("samples",width=3)#,
    # infoBoxOutput("surveys",width=3),
    # infoBoxOutput("records",width=3),
    # infoBoxOutput("taxa",width=3),
    # infoBoxOutput("providers",width=3),
    # infoBoxOutput("value",width=3),
    # infoBoxOutput("papers",width=3),
    # infoBoxOutput("apps",width=3)
    #),
    #__________________________________________________________________________________________
    #### MAP ####
    fluidRow(
      
      leafletOutput("map",width="100%", height="400")#
      
    ),
    
    ######################
    fluidRow( 
      #column(width = 5,
      #box(background="black",
      tabBox(title = tags$b("QUANTITY"),#width="100%",
             side = "right", height = "490px",id="tabset1",
             selected = "Total",
             tabPanel("Stats",div(DT::dataTableOutput("dredgestatquantity"),style = 'font-size:85%'),downloadButton("downloadData", "Download data")),
             tabPanel("Extracted",div(DT::dataTableOutput("totalcumext"),style = 'font-size:85%')),
             tabPanel("Cum",width = NULL,plotOutput("totalcum")),
             tabPanel("By country",width = NULL,plotOutput("amountcountryselect")),
             
             tabPanel("All",width = NULL,plotOutput("yearplot")),
             
             tabPanel("Total",width = NULL,plotOutput("totextyrall"))
             #)
      ),
      
      
      #column(width = 5,
      #box( background="black", 
      tabBox(title = tags$b("AREA"),id="tabset2",#width="100%",
             side = "left", height = "490px",
             selected = "All",
             tabPanel("All",width = NULL,plotOutput("licareaall")),
             tabPanel("Country",width = NULL,plotOutput("AreaCountry")),
             tabPanel("Detail",width = NULL,plotOutput("areacountryselect")),
             tabPanel("AREA LICENSED (ALL2)",width = NULL,plotOutput("extbyyearjur"))
             
             
             #)
      )
    )))

######################



#__________________________________________________________________________________________
#### SERVER ####

server <- function(input, output, session) {
  
  #__________________________________________________________________________________________
  #### MAP ###
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery)%>%
      
      addPolygons(data=agg,color = "white", weight = 1, smoothFactor = 0.5,group = "agg",popup = paste0("<b>Name: </b>", agg$name, "<br>","<b>Country: </b>", agg$country))%>%
      addPolygons(data=UK,color = "yellow", weight = 1, smoothFactor = 0.5,group = "UK",popup = paste0("<b>Name: </b>", UK$name))%>%
      addPolygons(data=France,color = "yellow", weight = 1, smoothFactor = 0.5,group = "France",popup = paste0("<b>Name: </b>", France$name))%>%
      addPolygons(data=Belgium,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Belgium",popup = paste0("<b>Name: </b>", Belgium$name))%>%
      addPolygons(data=Netherlands,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Netherlands",popup = paste0("<b>Name: </b>", Netherlands$name))%>%
      addPolygons(data=Denmark,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Denmark",popup = paste0("<b>Name: </b>", Denmark$name))%>%
      addPolygons(data=Germany,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Germany",popup = paste0("<b>Name: </b>", Germany$name))%>%
      addPolygons(data=Poland,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Poland",popup = paste0("<b>Name: </b>", Poland$name))%>%
      addPolygons(data=Finland,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Finland",popup = paste0("<b>Name: </b>", Finland$name))%>%
      addPolygons(data=Italy,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Italy",popup = paste0("<b>Name: </b>", Italy$name))%>%
      addPolygons(data=Russia,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Russia",popup = paste0("<b>Name: </b>", Russia$name))%>%
      addPolygons(data=Lithuania,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Lithuania",popup = paste0("<b>Name: </b>", Lithuania$name))%>%
      addPolygons(data=footuk2015,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2015)")%>%
      addPolygons(data=footuk2014,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2014)")%>%
      addPolygons(data=footuk2013,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2013)")%>%
      addPolygons(data=footuk2012,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2012)")%>%
      addPolygons(data=footuk2011,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2011)")%>%
      addPolygons(data=footuk2010,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2010)")%>%
      addLayersControl(overlayGroups = c("Belgium","Denmark","Finland","France","Germany","Italy","Lithuania","Netherlands", "Poland","Russia","UK","EMS (2010)","EMS (2011)","EMS (2012)","EMS (2013)","EMS (2014)","EMS (2015)"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup(c("Belgium","Denmark","Finland","France","Germany","Italy","Lithuania","Netherlands", "Poland","Russia","UK","EMS (2010)","EMS (2011)","EMS (2012)","EMS (2013)","EMS (2014)","EMS (2015)"))%>%
      setView(-20, 55.4, zoom = 3.4)
  })
  #__________________________________________________________________________________________
  
  #__________________________________________________________________________________________
  #### ONEBENTHIC MISSION STATEMENT ####
  output$purpose <- renderInfoBox({
    infoBox(
      #      "onebenthic purpose:",value = tags$p(paste0("A Postgres database for publicly available benthic data: Turning data into information, promoting more sustainable use of the seabed."), style = "font-size: 100%;"), downloadButton("downloadpdf", "Read More"),width = 12, icon = icon("info-circle", lib = "font-awesome"),
      "onebenthic mission:",value = tags$p(paste0("To communicate dredging stats"),HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;')  , style = "font-size: 150%;"),width = 12, icon = icon("info-circle", lib = "font-awesome"),     
      #      "onebenthic purpose:",value = tags$p(paste0("Turning publicly available data into new information, promoting more sustainable use of the seabed."), style = "font-size: 150%;"), downloadButton("downloadpdf", "Read More"),width = 12, icon = icon("info-circle", lib = "font-awesome"),
      color = "black",fill = T
    )
  })
  
  #__________________________________________________________________________________________
  #### INFOBOX: SAMPLES ####
  output$samples <- renderInfoBox({
    infoBox(
      "samples",value = tags$p(paste0(comma(numberofsamples)), style = "font-size: 190%;"), width = 2, icon = icon("record", lib = "glyphicon"),
      color = "yellow",fill = TRUE
    )
  })
  #__________________________________________________________________________________________
  #### TAB PANEL 'LATEST': APPS ####
  
  #output$latestapps <- renderPrint({
  output$totalcumext <- renderDataTable({
    DT::datatable(datatotext2,rownames=FALSE,options = list(dom = 'B<"clear">rtip',pageLength = 10,
                                                            headerCallback = JS(
                                                              "function(thead, data, start, end, display){",
                                                              "  $(thead).remove();",
                                                              "}")),escape=FALSE)%>% formatRound("x", 0)%>% formatCurrency("x", currency = '',interval = 3, mark = ',', before = FALSE)
    
    #DT::datatable(listapps2, options = list(pageLength = 14),escape=FALSE)
    
  })
  #__________________________________________________________________________________________
  
  #__________________________________________________________________________________________
  #### PLOTS: YEAR ####
  
  output$yearplot <- renderPlot({
    
    
    #################
    ## remove countries with no data
    data <- data[which(data$volume>0),]
    ## Facel of dredged quantities by country
    #ggplot(data=data, aes(x=year,y=volume,fill=aggcategory_type))+
    #geom_bar(position="stack",stat="identity")+  
    ggplot(data=data, aes(x=year,y=volume))+ 
      
      geom_bar(aes_string(fill= input$fillInput),position="stack",stat="identity")+
      theme(legend.position="bottom")+
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      #ylab("Volume (m3)")+
      labs(y = expression ("Volume "~(m^3))) + 
      theme(legend.title = element_blank())+
      #coord_flip()+
      #facet_grid(year~.)
      facet_wrap(~country_countryname,ncol = 4)+
      scale_x_continuous(breaks= pretty_breaks())
    #################
    
  })
  #__________________________________________________________________________________________
  #### QUANTITY PLOTS: TOTAL ####
  
  output$totextyrall <- renderPlot({
    
    
    #################
    
    ## Facel of dredged quantities by country
    ggplot(data=datatotextyr2, aes(x=year,y=x))+
      
      geom_bar(aes_string(fill= input$fillInput),position="stack",stat="identity")+
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      #ylab("Volume (m3)")+
      labs(y = expression ("Volume "~(m^3))) + 
      theme(legend.title = element_blank())+
      #theme(legend.position="bottom")#+
      #coord_flip()+
      #facet_grid(year~.)
      # facet_wrap(~country_countryname,ncol = 4)
      scale_x_continuous(breaks= pretty_breaks())
    #################
    #__________________________________________________________________________________________  
  })
  #__________________________________________________________________________________________
  #### QUANTITY PLOTS: CUM ####
  
  output$totalcum <- renderPlot({
    
    
    #################
    
    ## Facel of dredged quantities by country
    ggplot(data=  datacumplot5, aes(x=year,y=cv))+#,fill=aggcategory_type
      
      geom_bar(aes_string(fill= input$fillInput),stat="identity")+#position="fill",
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      labs(y = expression ("Volume "~(m^3))) +
      scale_x_continuous(breaks= pretty_breaks())
    #################
    
  })
  #__________________________________________________________________________________________
  #### PLOTS: TOTAL EXTRACTED BY YEAR (ALL COUNTRIES) ####
  
  output$extbyyearjur <- renderPlot({
    
    
    #################
    
    ## Facel of dredged quantities by country
    ggplot(data=  datatotextjur3, aes(x=year,y=x,fill=conventionarea_areaname))+
      
      geom_bar(position="fill",stat="identity")#+
    #theme(legend.position="bottom")#+
    #coord_flip()+
    #facet_grid(year~.)
    # facet_wrap(~country_countryname,ncol = 4)
    #################
    
  })
  #__________________________________________________________________________________________ 
  #### licensed area (all) #####
  output$licareaall <- renderPlot({
    
    
    #################
    
    ggplot()+
      
      geom_bar(data=naddata_long3 , aes(x=year,y=area,fill=category),stat="identity")+
      geom_bar(data=naddata_long4 , aes(x=year,y=area,fill=category),stat="identity")+
      theme(legend.position="bottom")+
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      labs(y = expression ("Area"~(km^2))) +
      #ylab("Area (km2)")+
      theme(legend.title = element_blank())
    #coord_flip()+
    #facet_grid(year~.)
    #facet_wrap(~country_countryname,ncol = 4)
    
  })
  
  
  ##############################################################
  #### licensed area (Country) #####
  output$AreaCountry <- renderPlot({
    
    ggplot()+
      
      geom_bar(data=naddata_long3 , aes(x=year,y=area,fill=category),stat="identity")+
      
      geom_bar(data=naddata_long4 , aes(x=year,y=area,fill=category),stat="identity")+
      theme(legend.position="bottom")+
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      #ylab("Area (km2)")+
      labs(y = expression ("Area"~(km^2))) +
      theme(legend.title = element_blank())+
      #coord_flip()+
      #facet_grid(year~.)
      facet_wrap(~country_countryname,ncol = 4)
  })
  #__________________________________________________________________________________________ 
  
  #__________________________________________________________________________________________
  #### QUANTITY PLOTS: BY COUNTRY ####
  
  output$amountcountryselect <- renderPlot({
    
    validate(
      need(input$variableInput != "", "Please select one or more countries froom the dropdown list")
    )
    
    #data2 <- data[which(data$country_countryname==input$variableInput)]
    data2 <- subset(data, country_countryname %in%input$variableInput)
    #################
    ## remove countries with no data
    data3 <- data2[which(data2$volume>0),]
    ## Facel of dredged quantities by country
    ggplot(data=data3, aes(x=year,y=volume))+#,fill=aggcategory_type
      
      geom_bar(aes_string(fill= input$fillInput),position="stack",stat="identity")+
      theme(legend.position="bottom")+
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      labs(y = expression ("Volume"~m^3)) + 
      #ylab("Volume (m3)")+
      theme(legend.title = element_blank())+
      #coord_flip()+
      #facet_grid(year~.)
      facet_wrap(~country_countryname,ncol = 4)+
      scale_x_continuous(breaks= pretty_breaks())
    #################
    #observeEvent(input$variableInput,{
    #updateSelectInput(session,'valueInput',
    #               choices=sort(unique(long$value[long$variable==input$variableInput])))  
  })
  #__________________________________________________________________________________________
  ##############################################################
  #### Area:  select country #####
  output$areacountryselect <- renderPlot({
    
    naddata_long3v2 <- subset(naddata_long3, country_countryname %in%input$variableInput)
    naddata_long4v2 <- subset(naddata_long4, country_countryname %in%input$variableInput)
    ggplot()+
      
      geom_bar(data=naddata_long3v2 , aes(x=year,y=area,fill=category),stat="identity")+
      
      geom_bar(data=naddata_long4v2 , aes(x=year,y=area,fill=category),stat="identity")+
      theme(legend.position="bottom")+
      scale_y_continuous(labels = scales::comma)+
      xlab("Year")+
      ylab("Area (km2)")+
      theme(legend.title = element_blank())+
      #coord_flip()+
      #facet_grid(year~.)
      facet_wrap(~country_countryname,ncol = 4)
  })
  #__________________________________________________________________________________________  
  
  output$dredgestatquantity <- DT::renderDataTable({
    #coord() # Reactive metadata object
    
    
    DT::datatable(subset(dredgestatquantity4, Country %in%input$variableInput), options = list(pageLength = 8), rownames= FALSE)%>% formatRound(c("Total","Construction","Beach","Fill","Non-agg","Exp"), 0)%>% formatCurrency(c("Total","Construction","Beach","Fill","Non-agg","Exp"), currency = '',interval = 3, mark = ',', before = FALSE) 
    
    
    
  })
  #"Total","Construction","Beach","Fill","Non-agg","Exp",
  #__________________________________________________________
  #________________________________  
  #### DOWNLOAD FAUNAL DATA FOR SELECTED SURVEY(S) ####  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")#data2-",Sys.Date(),
    },
    content = function(file) {
      #dredgestatquantity4selected <- dredgestatquantity4[which(dredgestatquantity4$Country==input$variableInput),]
      dredgestatquantity4selected <-subset(dredgestatquantity4, Country %in%input$variableInput)
      write.csv(dredgestatquantity4selected,file,row.names = F)
    })
  
  #__________________________________________________________________________________________  
  
  
  
}

shinyApp(ui, server)
