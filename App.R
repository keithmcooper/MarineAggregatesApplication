#### THIS IS THE ONEBENTHIC SHINY DASHBOARD ####
#test
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
library(rgdal)
library(zip)
library(pool)
library(formattable)
library(tidyr)
library(janitor)

#__________________________________________________________________________________________
#### CREATE A CONNECTION TO DB wgext ####
Sys.setenv(R_CONFIG_ACTIVE = "one_benthic")

dw <- config::get()

pool <- dbPool(drv = dbDriver(dw$driver),
               dbname = dw$database,
               host = dw$server,
               port =  dw$port,
               user = dw$uid,
               password = dw$pwd)

#__________________________________________________________________________________________


#agg <-  st_read(con, query = "SELECT * FROM areas.emodnet_ha_aggregates_areas_20190621;")
agg <-  st_read(pool, query = "SELECT * FROM areas.licence_polygon;")
#View(agg)## Check class of objects
class(agg)#[1] "sf"         "data.frame"

## Set CRS where necessary
st_crs(agg) <- 4326

UK <- agg[which(agg$country=="UK"),]
France <- agg[which(agg$country=="France"),]
Belgium <- agg[which(agg$country=="Belgium"),]
Netherlands <- agg[which(agg$country=="Netherlands"),]
Denmark <- agg[which(agg$country=="Denmark"),]
Germany <- agg[which(agg$country=="Germany"),]
Poland <- agg[which(agg$country=="Poland"),]
Finland <- agg[which(agg$country=="Finland"),]
Sweden <- agg[which(agg$country=="Sweden"),]
#Italy <- agg[which(agg$country=="Italy"),]
#Russia <- agg[which(agg$country=="Russia"),]
#Lithuania <- agg[which(agg$country=="Lithuania "),]
Azores <- agg[which(agg$country=="Portugal (Azores)"),]
#__________________________________________________________________________________________
#### bring in some ems footprint data ####
#footuk2015 <- st_read("DATA/2015footprintlatlong.shp")
#footuk2014 <- st_read("DATA/2014footprintlatlong.shp")
#footuk2013 <- st_read("DATA/2013footprintlatlong.shp")
#footuk2012 <- st_read("DATA/2012footprintlatlong.shp")
#footuk2011 <- st_read("DATA/2011footprintlatlong.shp")
#footuk2010 <- st_read("DATA/2010footprintlatlong.shp")
#View(footuk2015)
dredged_footprint <- st_read(pool, query = "SELECT * FROM areas.dredged_polygon;")

## Test to create a shapefile from 
#st_write(dredged_footprint, dsn = "nc1.shp", layer = "nc.shp", driver = "ESRI Shapefile")
#__________________________________________________________________________________________
#### SELECT DATA ####
#rm(pw) # remove the password
data = dbGetQuery(pool,
                  "SELECT * FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
aggcategory_type ='Construction fill/land reclamation';")
#__________________________________________________________________________________________
#### Get EEZs ###

eez <-  st_read(pool, query = "SELECT * FROM categories.eez
                where country = 'Belgium' 
                or country = 'Netherlands'
                or country = 'United Kingdom'
                or country = 'Sweden'
                or country = 'France'
                or country = 'Portugal'
                or country = 'Ireland'
                or country = 'United States'
                or country = 'Denmark'
                or country = 'Latvia'
                or country = 'Poland'
                or country = 'Norway'
                or country = 'Iceland'
                or country = 'Spain'
                or country = 'Canada'
                or country = 'Germany'
                or country = 'Finland'
                or country = 'Estonia'
                ;")



#__________________________________________________________________________________________
#### NUMBER OF RECORDS ####

numberofsamples = dbGetQuery(pool,"SELECT COUNT(*) FROM amounts.amount;")
numberofsamples <- as.numeric(as.character(numberofsamples$count))
#__________________________________________________________________________________________
#### LARGEST EXTRACTORS CUMULATIVE####

datatotext = dbGetQuery(pool,
                        "SELECT * FROM amounts.amount where aggcategory_type = 'Total extracted';")

datatotext2 <- datatotext%>%group_by(country_countryname)%>%summarize(x=sum(volume))%>%arrange(desc(x))

#__________________________________________________________________________________________


#### total extracted by year (all countries) fill by agg category####

datatotextyr = dbGetQuery(pool,
                          "SELECT id, year, country_countryname, conventionarea_areaname, aggcategory_type, volume FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
aggcategory_type ='Construction fill/land reclamation';")
## remove na
datatotextyr <- datatotextyr[complete.cases(datatotextyr),]

#datatotextyr2 <- datatotextyr%>%group_by(year,aggcategory_type)%>%summarize(x=sum(volume))%>%arrange(desc(x))
datatotextyr2 <- datatotextyr%>%group_by(year,aggcategory_type,conventionarea_areaname)%>%summarize(x=sum(volume))%>%arrange(desc(x))

#__________________________________________________________________________________________
#### cum plot#### 
datacumplot = dbGetQuery(pool,
                         "SELECT * FROM amounts.amount where aggcategory_type = 'Construction/industrial' or
 aggcategory_type ='Beach replenishment' or 
                         aggcategory_type ='Construction fill/land reclamation';")
## remove 2013 data
#datacumplot2 <- datacumplot[which(datacumplot$year!= 2013),]


## Take only cols 1-6 (otherwise the notes col leads to data being removed in next step)
datacumplot1 <- datacumplot[,1:6]
## remove na
datacumplot2 <- datacumplot1[complete.cases(datacumplot1),]

datacumplot3 <- datacumplot2[,c(2,4,5,6),]
datacumplot4 <- datacumplot3%>%group_by(year,aggcategory_type,conventionarea_areaname)%>%summarize(x=sum(volume))
datacumplot5 <- datacumplot4%>%group_by(aggcategory_type,conventionarea_areaname)%>% mutate(cv=cumsum(x))
#View(datacumplot5)


#__________________________________________________________________________________________
#### total extracted by year (all countries) fill by jurisdiction####

datatotextjur = dbGetQuery(pool,
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

naddata = dbGetQuery(pool,"select * from areas.licence;")
head(naddata)

############
naddata <- naddata[,1:ncol(naddata)-1]

###############
## add col for UndredgedArea
naddata$totalareaundredged <- naddata$totalarealicensed-naddata$totalareadredged

## Get data into long format
#naddata2 <- naddata[,c(2,3,5,6,8)] error noted on 17/01/2023
naddata2 <- naddata[,c(2,3,5,6,7)]
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
dredgestatquantity = dbGetQuery(pool,"select * from amounts.amount;")

## Drop id col
dredgestatquantity2 <- dredgestatquantity[,2:6]
#View(dredgestatquantity2)
## convert to wide format
dredgestatquantity3 <- spread(dredgestatquantity2, aggcategory_type, volume)#fill = NA,
#View(dredgestatquantity3)
dredgestatquantity4 <- dredgestatquantity3[,c(2,1,3,7,5,6,8,4,9)]
colnames(dredgestatquantity4) <- c("Country","Year","Convention","Construction","Beach","Fill","Non-agg","Exp","Total")
#__________________________________________________________________________________________
#### Spatial data (licensed) ####

spatial_licensed = dbGetQuery(pool,"select distinct
country,
year
from areas.licence_polygon
order by country DESC, year ASC;")

colnames(spatial_licensed) <- c("Country","Year")

#__________________________________________________________________________________________
#### Spatial data (dredged) ####

spatial_dredged = dbGetQuery(pool,"select distinct
country_countryname as country,
year
from areas.dredged_polygon
order by country DESC, year ASC;")

colnames(spatial_dredged) <- c("Country","Year")
#__________________________________________________________________________________________
#### Amounts data availability ####

amounts_data_avail = dbGetQuery(pool,
                  "SELECT
DISTINCT 
country_countryname as country,
year
FROM
amounts.amount
ORDER BY
country asc,
year asc;")

colnames(amounts_data_avail) <- c("Country","Year")
#__________________________________________________________________________________________
#### Unique countries in Amounts table ####
countries = dbGetQuery(pool,
                                "SELECT
DISTINCT 
country_countryname as country
FROM
amounts.amount
ORDER BY
country asc;")

vector_countries <- c(t(countries))
#__________________________________________________________________________________________
#### total area licensed (countries/years reported) ####
tot_area_lic = dbGetQuery(pool,
                       "SELECT DISTINCT 
country_countryname as country,
year
FROM areas.licence
WHERE totalarealicensed IS NOT NULL;")

colnames(tot_area_lic) <- c("Country","Year")
#__________________________________________________________________________________________
#### total area footprint (countries/years reported) ####
tot_area_dredged = dbGetQuery(pool,
                          "SELECT DISTINCT 
country_countryname as country,
year
FROM areas.licence
WHERE totalareadredged IS NOT NULL;")

colnames(tot_area_dredged) <- c("Country","Year")
#__________________________________________________________________________________________
#### UI ####

ui <- dashboardPage(
  #__________________________________________________________________________________________
  #### HEADER ####  
  dashboardHeader(title=tags$b("WGEXT Dredging Stats Dashboard "),titleWidth = 400),#title = "OneBenthic dashboard"
  
  #__________________________________________________________________________________________
  #### SIDEBAR ####
  
  dashboardSidebar(

    selectInput(inputId="variableInput", multiple = T,h4("Select country",style="color:white"),choices = c("",as.character(unique(data$country_countryname)))),
                   #################
                   selectInput(inputId="yearInput", multiple = T,h4("Select year",style="color:white"),choices = c(2022:1993)),#,selected =2020
                   #selectInput(inputId="fillInput", multiple = F,h4("Select fill",style="color:white"),choices = c("aggcategory_type","conventionarea_areaname"),selected ="aggcategory_type"),
                   br(),#br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                   tags$style(
                     ".main-sidebar {padding-left:10px;}"),
                   h4("Map downloads (.shp)",style="color:white"),
                   downloadButton('AreaLicensedExport', 'Licencesed Polygons'),br(),br(),
                   downloadButton('AreaDredgedExport', 'Dredged Polygons'),
                   br(),br(),br(),
                   selectInput(inputId="fillInput", multiple = F,h4("Barplot fill (QUANTITY)",style="color:white"),choices = c("aggcategory_type","conventionarea_areaname"),selected ="aggcategory_type"),
                   ############
                   #h4("Map options:"),
                   #    selectInput(inputId="variableInput", multiple = F,h4("Select variable",style="color:white"),choices = c("",as.character(unique(long$variable)))),
                   #                  selectInput(inputId="valueInput", multiple = F,h4("Select value",style="color:white"),choices =NULL)
                   
                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                  # h4("***ATTENTION***TEST VERSION USE DATA WITH CAUTION"),
                   #__________________________________________________________________________________________
                   #### OB LOGO ####
                   #   HTML('&emsp;'), HTML('&emsp;'),img(src="OBLogo.png",height = 85, width =160),
                   
                   #__________________________________________________________________________________________
                   #### OTHER LOGOS ####
                   HTML('&emsp;'),img(src="iceslogov3.png", width="77%"),#,height = 50, width =80
                   ##HTML('&emsp;'),img(src="postgreslogo.png",height = 50, width =50),
                   ##HTML('&emsp;'),img(src="rstudiologo.png",height = 50, width =50),
                   ##HTML('&emsp;'),img(src="rshinylogo.png",height = 50, width =50),
                   br()),
  
  
  #__________________________________________________________________________________________
  #### DASHBOARD BODY ####
  
  #img(src="logos_long.png",height = 300, width = 50)
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    
    ##############
    modalDialog(
      h4("This app is being developed by the",tags$a(href="https://www.ices.dk/community/groups/pages/wgext.aspx","ICES WGEXT"),
      " and is made available for use on an as is and as available basis. No warranty is given in relation to the accuracy of the
      content or in respect of its use for any particular purpose. Note the app is still in development and it's contents are
      incomplete (see",tags$a(href="https://sway.office.com/orIuJoHSruYfhy09?ref=Link","here")," for further information).
Your access to and use of the content available on this app is entirely at your own risk. This work is licensed under",
      tags$a(href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1","CC BY 4.0"),"."),
      title = h4("Disclaimer"),
      size = "l",
      easyClose = FALSE
    ),
    
    ###############
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
             side = "right", height = "520px",id="tabset1",
             selected = "Total",
             tabPanel("Stats",div(DT::dataTableOutput("dredgestatquantity"),style = 'font-size:85%'),downloadButton("downloadData", "Download data")),
             tabPanel("Extracted",div(DT::dataTableOutput("totalcumext"),style = 'font-size:85%')),
             tabPanel("Cum",width = NULL,plotOutput("totalcum")),
             tabPanel("By country",width = NULL,plotOutput("amountcountryselect")),
             ####
             
             tabPanel("Data Availbility",
                      div(style = 'overflow-y:scroll;height:420px;',
                      width = NULL,div(formattableOutput("amounts_data_avail"),style = 'font-size:58%'))),
             #amounts_data_avail 
             ###########
             tabPanel("All",width = NULL,plotOutput("yearplot")),
             
             tabPanel("Total",width = NULL,plotOutput("totextyrall"))
             #)
      ),
      
      
      #column(width = 5,
      #box( background="black", 
      tabBox(title = tags$b("AREA"),id="tabset2",#width="100%",
             side = "left", height = "520px",
             selected = "All",
             tabPanel("All",width = NULL,plotOutput("licareaall")),
             tabPanel("Country",width = NULL,plotOutput("AreaCountry")),
             tabPanel("Detail",width = NULL,plotOutput("areacountryselect")),
             tabPanel("AREA LICENSED (ALL2)",width = NULL,plotOutput("extbyyearjur")),
            #tabPanel("Spatial data (licensed)", div(DT::dataTableOutput("spat_lic"),style = 'font-size:85%')),
            tabPanel("Spatial Data Availability",
            tabsetPanel(
            tabPanel("Licensed Areas (.shp)",
                     div(style = 'overflow-y:scroll;height:380px;',# add vertical scrollbar
                     div(formattableOutput("spat_lic"),style = 'font-size:58%'))),
            
            #tabPanel("Dredged Footprints", div(DT::dataTableOutput("spat_dredged"),style = 'font-size:85%'))
            tabPanel("Dredged Footprints (.shp)",
                     div(style = 'overflow-y:scroll;height:380px;',# add vertical scrollbar
                         div(formattableOutput("spat_dredged"),style = 'font-size:58%'))),
            tabPanel("Total Area Licensed (km2)",
                     div(style = 'overflow-y:scroll;height:380px;',# add vertical scrollbar
                     div(formattableOutput("tot_area_lic"),style = 'font-size:58%') )),
            tabPanel("Total Area Footprint (km2)",
                     div(style = 'overflow-y:scroll;height:380px;',# add vertical scrollbar
                         div(formattableOutput("tot_area_dredged"),style = 'font-size:58%') ))
            )
            )
             #tabPanel("Spatial data (dredged)", width = NULL, plotOutput("extbyyearjur"))
             
             #)
      )
    )))

######################
box(formattableOutput("table"))


#__________________________________________________________________________________________
#### SERVER ####

server <- function(input, output, session) {
  
  #__________________________________________________________________________________________
  
  #__________________________________________________________________________________________ 
  #### BASELINE DATA SUBSET ####
  eez2 <- reactive({
    # 1year,
    #2 stationcode,
    #3 surveyname,
    #4 samplecode,
    #5 samplelong,
    #6 samplelat,
    #7 wwacr,
    #8 SUM(sv.percentage) 
    #a <- subset( coord()[,c(4,5,6,7,11,12)],surveyname %in% input$baselineInput)
    eez1 <- subset( eez,country %in% input$variableInput)
    
    return(eez1)
  })
  #__________________________________________________________________________________________
  #__________________________________________________________________________________________ 
  #### BASELINE DATA SUBSET ####
  agg2 <- reactive({
    # 1year,
    #2 stationcode,
    #3 surveyname,
    #4 samplecode,
    #5 samplelong,
    #6 samplelat,
    #7 wwacr,
    #8 SUM(sv.percentage) 
    #a <- subset( coord()[,c(4,5,6,7,11,12)],surveyname %in% input$baselineInput)
    agg1 <- subset( agg,country %in% input$variableInput)
    agg1.5 <- subset(agg1,year %in% input$yearInput)#
    return(agg1.5)
  })
  #__________________________________________________________________________________________
  ## DOWNLOAD SELECTED LICENCE POLYGONS AS SHAPEFILE
  output$AreaLicensedExport <- downloadHandler(
    filename <- function() {
      "AreaLicensedExport.zip"
      
    },
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, "AreaLicensed")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(agg2(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }  
  )
  # st_write(dredged_footprint, dsn = "nc1.shp", layer = "nc.shp", driver = "ESRI Shapefile")
  #__________________________________________________________________________________________  
  #### BASELINE DATA SUBSET ####
  dredged_footprint2 <- reactive({
    # 1year,
    #2 stationcode,
    #3 surveyname,
    #4 samplecode,
    #5 samplelong,
    #6 samplelat,
    #7 wwacr,
    #8 SUM(sv.percentage) 
    #a <- subset( coord()[,c(4,5,6,7,11,12)],surveyname %in% input$baselineInput)
    dredged_footprint1 <- subset( dredged_footprint,country_countryname %in% input$variableInput)
    dredged_footprint1.5 <- subset(dredged_footprint1,year %in% input$yearInput)#
    return(dredged_footprint1.5)
  })
  #__________________________________________________________________________________________
  #__________________________________________________________________________________________
  ## DOWNLOAD SELECTED DREDGED POLYGONS AS SHAPEFILE
  output$AreaDredgedExport <- downloadHandler(
    filename <- function() {
      "AreaDredgedExport.zip"
      
    },
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, "AreaDredged")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(dredged_footprint2(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }  
  )
  # st_write(dredged_footprint, dsn = "nc1.shp", layer = "nc.shp", driver = "ESRI Shapefile")
  #__________________________________________________________________________________________ 
  #### MAP ###
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery)%>%
      
      addPolygons(data=agg,color = "white", weight = 1, smoothFactor = 0.5,group = "All licensed areas",popup = paste0("<b>Country: </b>", agg$country,"<br>","<b>Name: </b>", agg$site_name,  "<br>","<b>Number: </b>", agg$site_numbe))%>%
      addPolygons(data=agg2(),color = "yellow",fillColor = "yellow", fillOpacity = 0.5,weight = 1, smoothFactor = 0.5,opacity = 1,group = "Area Licensed",popup = paste0("<b>Country: </b>", agg2()$country,"<br>","<b>Name: </b>", agg2()$site_name,  "<br>","<b>Number: </b>", agg2()$site_numbe))%>%
      addPolygons(data=dredged_footprint2(),color = "#4CBB17",fillColor = "#4CBB17", fillOpacity = 1,weight = 1, smoothFactor = 0.5,opacity = 1,group = "Area Dredged",popup = paste0("<b>Country: </b>", dredged_footprint2()$country_countryname,"<br>","<b>year: </b>", dredged_footprint2()$year))%>%
      addPolygons(data=eez2(),color = "orange", weight = 0, smoothFactor = 0.5,fillOpacity = 0.3,group = "eez",popup = paste0("<b>EEZ: </b>", eez2()$country))%>%
      #addPolygons(data=France,color = "yellow", weight = 1, smoothFactor = 0.5,group = "France",popup = paste0("<b>Name: </b>", France$name))%>%
      #addPolygons(data=Belgium,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Belgium",popup = paste0("<b>Name: </b>", Belgium$name))%>%
      #addPolygons(data=Netherlands,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Netherlands",popup = paste0("<b>Name: </b>", Netherlands$name))%>%
      #addPolygons(data=Denmark,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Denmark",popup = paste0("<b>Name: </b>", Denmark$name))%>%
      #addPolygons(data=Germany,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Germany",popup = paste0("<b>Name: </b>", Germany$name))%>%
      #addPolygons(data=Poland,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Poland",popup = paste0("<b>Name: </b>", Poland$name))%>%
      #addPolygons(data=Finland,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Finland",popup = paste0("<b>Name: </b>", Finland$name))%>%
      #addPolygons(data=Azores,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Azores",popup = paste0("<b>Name: </b>", Azores$name))%>%
      #addPolygons(data=Sweden,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Sweden",popup = paste0("<b>Name: </b>", Sweden$name))%>%      
      # addPolygons(data=Italy,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Italy",popup = paste0("<b>Name: </b>", Italy$name))%>%
      #addPolygons(data=Russia,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Russia",popup = paste0("<b>Name: </b>", Russia$name))%>%
    #addPolygons(data=Lithuania,color = "yellow", weight = 1, smoothFactor = 0.5,group = "Lithuania",popup = paste0("<b>Name: </b>", Lithuania$name))%>%
    #addPolygons(data=footuk2015,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2015)")%>%
    #addPolygons(data=footuk2014,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2014)")%>%
    #addPolygons(data=footuk2013,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2013)")%>%
    #addPolygons(data=footuk2012,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2012)")%>%
    #addPolygons(data=footuk2011,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2011)")%>%
    #addPolygons(data=footuk2010,color = "green", weight = 1, smoothFactor = 0.5,group = "EMS (2010)")%>%
    #addLayersControl(overlayGroups = c("UK","France","Belgium","Netherlands","Denmark","Germany","Poland","Finland", "Finland","Azores","Sweden","EMS (2010)","EMS (2011)","EMS (2012)","EMS (2013)","EMS (2014)","EMS (2015)"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup(c("Belgium","Denmark","Finland","France","Germany","Italy","Lithuania","Netherlands", "Poland","Russia","UK","EMS (2010)","EMS (2011)","EMS (2012)","EMS (2013)","EMS (2014)","EMS (2015)"))%>%
    addLayersControl(overlayGroups = c("Area Dredged","eez"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup(c("Area Dredged","eez"))%>%
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
  #### Spatial Data Availability: Licensed Areas (.shp) ####
 # output$spat_lic <- renderDataTable({
  #  DT::datatable(subset(spatial_licensed, Country %in%input$variableInput),options = list(pageLength = 8), rownames= FALSE)
  #   })
    
  spatial_licensed$value <- 1# Create a data value of 1 for each instance where licence data exist
  spatial_licensed$Country <- as.factor(spatial_licensed$Country) ## Ensure data in correct format
  spatial_licensed2 <- spatial_licensed[order(spatial_licensed$Year, decreasing = TRUE),] # Order data by year
  spatial_licensed_piv <- pivot_wider(spatial_licensed2, names_from = Country, values_from = value)# Convert data from long to wide format
  
  all_years = as.integer(c(lubridate::year(Sys.Date())-1):1993)# vector of all years to present
  spatial_licensed_piv <-tidyr::complete(spatial_licensed_piv, Year = all_years, fill = list(Count = 0))# Add in missing years and add zeros
  spatial_licensed_piv <-  spatial_licensed_piv[order(spatial_licensed_piv$Year, decreasing = TRUE),] # Order data by year
  
  new_order = sort(colnames(spatial_licensed_piv[2:ncol(spatial_licensed_piv)]))# Sort columns (i.e. countries) in ascending alphabetical order
  spatial_licensed_piv2 <- spatial_licensed_piv[, new_order]# Sort columns (i.e. countries) in ascending alphabetical order
  spatial_licensed_piv3 <- cbind(spatial_licensed_piv$Year,spatial_licensed_piv2)# Sort columns (i.e. countries) in ascending alphabetical order
  colnames(spatial_licensed_piv3)[1] <- 'Year'
  
  missing <- setdiff(vector_countries, names(spatial_licensed_piv3))  # Find names of missing columns (i.e. countries that have submitted amounts)
  spatial_licensed_piv3[missing] <- 0# Add them, filled with '0's
  spatial_licensed_piv3 <- spatial_licensed_piv3[vector_countries]## Put columns in desired order
  spatial_licensed_piv3 <- cbind(spatial_licensed_piv[,1],spatial_licensed_piv3)# bring in years column
  spatial_licensed_piv3[spatial_licensed_piv3 == 0] <- NA #Replace on all columns
  
  spatial_licensed_piv4 <- spatial_licensed_piv3 %>%janitor::adorn_totals("row")%>%janitor::adorn_totals("col")### Add row and col sums
  format <- formatter("span",style = x ~ style(color = ifelse(x, "green", "red")),x ~ icontext(ifelse(x, "ok", "remove")))# Specify format for tick symbols
  output$spat_lic <- renderFormattable({formattable(spatial_licensed_piv4,align ="c",list(Normal = format,area(row = 1:nrow(spatial_licensed_piv4)-1, col = c(-1,-20)) ~ format))# Create table
  })

  #__________________________________________________________________________________________
  #### Spatial Data Availability: Dredged Footprints (km2) ####
 # output$spat_dredged <- renderDataTable({
 #   DT::datatable(subset(spatial_dredged, Country %in%input$variableInput),options = list(pageLength = 8), rownames= FALSE)
  # })
  spatial_dredged$value <- 1# Create a data value of 1 for each instance where licence data exist
  spatial_dredged$Country <- as.factor(spatial_dredged$Country) ## Ensure data in correct format
  spatial_dredged2 <- spatial_dredged[order(spatial_dredged$Year, decreasing = TRUE),] # Order data by year
  spatial_dredged_piv <- pivot_wider(spatial_dredged2, names_from = Country, values_from = value)# Convert data from long to wide format
  
  all_years = as.integer(c(lubridate::year(Sys.Date())-1):1993)# vector of all years to present
  spatial_dredged_piv <-tidyr::complete(spatial_dredged_piv, Year = all_years, fill = list(Count = 0))# Add in missing years and add zeros
  spatial_dredged_piv <-  spatial_dredged_piv[order(spatial_dredged_piv$Year, decreasing = TRUE),] # Order data by year
  
  new_order = sort(colnames(spatial_dredged_piv[2:ncol(spatial_dredged_piv)]))# Sort columns (i.e. countries) in ascending alphabetical order
  spatial_dredged_piv2 <- spatial_dredged_piv[, new_order]# Sort columns (i.e. countries) in ascending alphabetical order
  spatial_dredged_piv3 <- cbind(spatial_dredged_piv$Year,spatial_dredged_piv2)# Sort columns (i.e. countries) in ascending alphabetical order
  colnames(spatial_dredged_piv3)[1] <- 'Year'

  missing <- setdiff(vector_countries, names(spatial_dredged_piv3))  # Find names of missing columns (i.e. countries that have submitted amounts)
  spatial_dredged_piv3[missing] <- 0# Add them, filled with '0's
  spatial_dredged_piv3 <- spatial_dredged_piv3[vector_countries]## Put columns in desired order
  spatial_dredged_piv3 <- cbind(spatial_dredged_piv[,1],spatial_dredged_piv3)# bring in years column
  spatial_dredged_piv3[spatial_dredged_piv3 == 0] <- NA #Replace on all columns
  
  spatial_dredged_piv4 <- spatial_dredged_piv3 %>%janitor::adorn_totals("row")%>%janitor::adorn_totals("col")### Add row and col sums
  format <- formatter("span",style = x ~ style(color = ifelse(x, "green", "red")),x ~ icontext(ifelse(x, "ok", "remove")))# Specify format for tick symbols
  output$spat_dredged <- renderFormattable({formattable(spatial_dredged_piv4,align ="c",list(Normal = format,area(row = 1:nrow(spatial_dredged_piv4)-1, col = c(-1,-20)) ~ format))# Create table
  })
  #__________________________________________________________________________________________
  #### Data Availability: Amounts ####
  amounts_data_avail$value <- 1# Create a data value of 1 for each instance where licence data exist
  amounts_data_avail$Country <- as.factor(amounts_data_avail$Country) ## Ensure data in correct format
  amounts_data_avail2 <- amounts_data_avail[order(amounts_data_avail$Year, decreasing = TRUE),] # Order data by year
  amounts_data_avail_piv <- pivot_wider(amounts_data_avail2, names_from = Country, values_from = value)# Convert data from long to wide format
  
  all_years = as.integer(c(lubridate::year(Sys.Date())-1):1993)# vector of all years to present
  amounts_data_avail_piv <-tidyr::complete(amounts_data_avail_piv, Year = all_years, fill = list(Count = 0))# Add in missing years and add zeros
  amounts_data_avail_piv <-  amounts_data_avail_piv[order( amounts_data_avail_piv$Year, decreasing = TRUE),] # Order data by year
 
  new_order = sort(colnames(amounts_data_avail_piv[2:ncol(amounts_data_avail_piv)]))# Sort columns (i.e. countries) in ascending alphabetical order
  amounts_data_avail_piv2 <- amounts_data_avail_piv[, new_order]# Sort columns (i.e. countries) in ascending alphabetical order
  amounts_data_avail_piv3 <- cbind(amounts_data_avail_piv$Year,amounts_data_avail_piv2)# Sort columns (i.e. countries) in ascending alphabetical order
  colnames(amounts_data_avail_piv3)[1] <- 'Year'
  amounts_data_avail_piv4 <- amounts_data_avail_piv3 %>%janitor::adorn_totals("row")%>%janitor::adorn_totals("col")### Add row and col sums
  format <- formatter("span",style = x ~ style(color = ifelse(x, "green", "red")),x ~ icontext(ifelse(x, "ok", "remove")))# Specify format for tick symbols
  output$amounts_data_avail <- renderFormattable({formattable(amounts_data_avail_piv4,align ="c",list(Normal = format,area(row = 1:nrow(amounts_data_avail_piv4)-1, col = c(-1,-20)) ~ format))# Create table
  })
  #__________________________________________________________________________________________
  #### Spatial Data Availability:Total Area Licensed (km2) ####
  tot_area_lic$value <- 1# Create a data value of 1 for each instance where licence data exist
  tot_area_lic$Country <- as.factor(tot_area_lic$Country) ## Ensure data in correct format
  tot_area_lic2 <- tot_area_lic[order(tot_area_lic$Year, decreasing = TRUE),] # Order data by year
  tot_area_lic_piv <- pivot_wider(tot_area_lic2, names_from = Country, values_from = value)# Convert data from long to wide format
  
  all_years = as.integer(c(lubridate::year(Sys.Date())-1):1993)# vector of all years to present
  tot_area_lic_piv <-tidyr::complete(tot_area_lic_piv, Year = all_years, fill = list(Count = 0))# Add in missing years and add zeros
  tot_area_lic_piv <-  tot_area_lic_piv[order(tot_area_lic_piv$Year, decreasing = TRUE),] # Order data by year
  
  new_order = sort(colnames(tot_area_lic_piv[2:ncol(tot_area_lic_piv)]))# Sort columns (i.e. countries) in ascending alphabetical order
  tot_area_lic_piv2 <- tot_area_lic_piv[, new_order]# Sort columns (i.e. countries) in ascending alphabetical order
  tot_area_lic_piv3 <- cbind(tot_area_lic_piv$Year,tot_area_lic_piv2)# Sort columns (i.e. countries) in ascending alphabetical order
  colnames(tot_area_lic_piv3)[1] <- 'Year'
  
  missing <- setdiff(vector_countries, names(tot_area_lic_piv3))  # Find names of missing columns (i.e. countries that have submitted amounts)
  tot_area_lic_piv3[missing] <- 0# Add them, filled with '0's
  tot_area_lic_piv3 <- tot_area_lic_piv3[vector_countries]## Put columns in desired order
  tot_area_lic_piv3 <- cbind(tot_area_lic_piv[,1],tot_area_lic_piv3)# bring in years column
  tot_area_lic_piv3[tot_area_lic_piv3 == 0] <- NA #Replace on all columns
  
  tot_area_lic_piv4 <- tot_area_lic_piv3 %>%janitor::adorn_totals("row")%>%janitor::adorn_totals("col")### Add row and col sums
  format <- formatter("span",style = x ~ style(color = ifelse(x, "green", "red")),x ~ icontext(ifelse(x, "ok", "remove")))# Specify format for tick symbols
  output$tot_area_lic <- renderFormattable({formattable(tot_area_lic_piv4,align ="c",list(Normal = format,area(row = 1:nrow(tot_area_lic_piv4)-1, col = c(-1,-20)) ~ format))# Create table
  })
  #__________________________________________________________________________________________
  #### Spatial Data Availability:Total Area Footprint (km2) ####
  tot_area_dredged$value <- 1# Create a data value of 1 for each instance where licence data exist
  tot_area_dredged$Country <- as.factor(tot_area_dredged$Country) ## Ensure data in correct format
  tot_area_dredged2 <- tot_area_dredged[order(tot_area_dredged$Year, decreasing = TRUE),] # Order data by year
  tot_area_dredged_piv <- pivot_wider(tot_area_dredged2, names_from = Country, values_from = value)# Convert data from long to wide format
  
  all_years = as.integer(c(lubridate::year(Sys.Date())-1):1993)# vector of all years to present
  tot_area_dredged_piv <-tidyr::complete(tot_area_dredged_piv, Year = all_years, fill = list(Count = 0))# Add in missing years and add zeros
  tot_area_dredged_piv <-  tot_area_dredged_piv[order(tot_area_dredged_piv$Year, decreasing = TRUE),] # Order data by year
  
  new_order = sort(colnames(tot_area_dredged_piv[2:ncol(tot_area_dredged_piv)]))# Sort columns (i.e. countries) in ascending alphabetical order
  tot_area_dredged_piv2 <- tot_area_dredged_piv[, new_order]# Sort columns (i.e. countries) in ascending alphabetical order
  tot_area_dredged_piv3 <- cbind(tot_area_dredged_piv$Year,tot_area_dredged_piv2)# Sort columns (i.e. countries) in ascending alphabetical order
  colnames(tot_area_dredged_piv3)[1] <- 'Year'
  
  missing <- setdiff(vector_countries, names(tot_area_dredged_piv3))  # Find names of missing columns (i.e. countries that have submitted amounts)
  tot_area_dredged_piv3[missing] <- 0# Add them, filled with '0's
  tot_area_dredged_piv3 <- tot_area_dredged_piv3[vector_countries]## Put columns in desired order
  tot_area_dredged_piv3 <- cbind(tot_area_dredged_piv[,1],tot_area_dredged_piv3)# bring in years column
  tot_area_dredged_piv3[tot_area_dredged_piv3 == 0] <- NA #Replace on all columns
  
  tot_area_dredged_piv4 <- tot_area_dredged_piv3 %>%janitor::adorn_totals("row")%>%janitor::adorn_totals("col")### Add row and col sums
  format <- formatter("span",style = x ~ style(color = ifelse(x, "green", "red")),x ~ icontext(ifelse(x, "ok", "remove")))# Specify format for tick symbols
  output$tot_area_dredged <- renderFormattable({formattable(tot_area_dredged_piv4,align ="c",list(Normal = format,area(row = 1:nrow(tot_area_lic_piv4)-1, col = c(-1,-20)) ~ format))# Create table
  })
  #__________________________________________________________________________________________
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