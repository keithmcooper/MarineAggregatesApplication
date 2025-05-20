#### THIS IS THE ONEBENTHIC SHINY DASHBOARD ####
#setwd("C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/DredgingDashboard/dev scripts")
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
#library(rgdal)
library(zip)
library(pool)
library(formattable)
library(tidyr)
library(janitor)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(kableExtra)
library(knitr)
library(dplyr)
#_______________________________________________________________________________
## Load the blog content in your main app script:
source("blog_content.R")

## Create a function to render the blog posts:
renderBlogPosts <- function(posts) {
  lapply(posts, function(post) {
    box(
      title = post$title,
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      p(strong("Date: "), post$date),
      #p(post$content),
      HTML(post$content)
    )
  })
}
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
## Publications
publications = dbGetQuery(pool,
                          "select 
                              p.title,
                              p.ref,
                              p.url,
                              p.year,
                              p.country_countryname,
                              p.pub_type,
                              p.authors,
                              pt.tag_name
                              
                          FROM
                          publications.publication as p
                          inner join publications.publication_tag as pt on p.ref = pt.publication_ref
                          ;")

#publications <- publications[,2:5]
colnames(publications)[1] <- 'Title'
colnames(publications)[2] <- 'Reference'
colnames(publications)[3] <- 'url'
colnames(publications)[4] <- 'Year'
colnames(publications)[5] <- 'Country'
#__________________________________________________________________________________________
## Management
management = dbGetQuery(pool,
                        "select * from management.management order by country_countryname asc;")
management <- management[,2:5]
colnames(management)[1] <- 'Country'
management$question[management$question == "Have you mapped aggregate resources in your country?"] <- "Aggregate resources mapped?"
management$question[management$question == "Does your country have a strategy for sustainable use of aggregate resources?"] <- "Strategy for sustainable use of aggregate resources?"
management$question[management$question == "What systems are used for tracking dredging activity?"] <- "System used for tracking dredging activity?"
management$question[management$question == "Is an EIA (environmental impact report) required in your country?"] <- "Requirement for an Environmental Impact Assessment (EIA)?"

#View(management)
colnames(management)[4] <- 'Response'
#__________________________________________________________________________________________
## Guidelines
guidelines = dbGetQuery(pool,
                        "select * from management.guidelines;")
guidelines <- guidelines[,2:3]
guidelines$question[guidelines$question == "Have you mapped aggregate resources in your country?"] <- "Aggregate resources mapped?"
guidelines$question[guidelines$question == "Does your country have a strategy for sustainable use of aggregate resources?"] <- "Strategy for sustainable use of aggregate resources?"
guidelines$question[guidelines$question == "What systems are used for tracking dredging activity?"] <- "System used for tracking dredging activity?"
guidelines$question[guidelines$question == "Is an EIA (environmental impact report) required in your country?"] <- "Requirement for an Environmental Impact Assessment (EIA)?"
#__________________________________________________________________________________________
#__________________________________________________________________________________________
#### NUMBER OF PAPERS ####

numberofpapers = dbGetQuery(pool,"SELECT COUNT(pub_type) FROM publications.publication where pub_type = 'paper';")
numberofpapers <- as.numeric(as.character(numberofpapers$count))
#__________________________________________________________________________________________
#### Number of REPORTS ####
numberofreports = dbGetQuery(pool,"SELECT COUNT(pub_type) FROM publications.publication where pub_type = 'report';")
numberofreports <- as.numeric(as.character(numberofreports$count))
#__________________________________________________________________________________________
#### Number of PHDs ####
numberofphds = dbGetQuery(pool,"SELECT COUNT(pub_type) FROM publications.publication where pub_type = 'phd';")
numberofphds <- as.numeric(as.character(numberofphds$count))
#__________________________________________________________________________________________
#### Number of BOOKS ####
numberofbooks = dbGetQuery(pool,"SELECT COUNT(pub_type) FROM publications.publication where pub_type = 'book';")
numberofbooks <- as.numeric(as.character(numberofbooks$count))
#__________________________________________________________________________________________
#### Number of Websites ####
numberofwebsites = dbGetQuery(pool,"SELECT COUNT(pub_type) FROM publications.publication where pub_type = 'website';")
numberofwebsites <- as.numeric(as.character(numberofwebsites$count))
#__________________________________________________________________________________________

## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  #####
  ######

  
  #####
  #####
  
  #__________________________________________________________________________________________
  #### HEADER ####  
  #dashboardHeader(title=tags$b("WGEXT Dredging Stats Dashboard "),titleWidth = 400),#title = "OneBenthic dashboard"
  dashboardHeader(title=tags$b("North Atlantic Marine Aggregates Application (NAMAAP)"),titleWidth = 600),#title = "OneBenthic dashboard"
  #__________________________________________________________________________________________
  #### SIDEBAR ####
  dashboardSidebar(
    #____________________________________________
    tags$head(
      tags$style(HTML("
        .sidebar {
          position: relative;
          height: 100vh;
        }
        .sidebar img {
          position: absolute;
          bottom: 75px;
          left: 40%;
          transform: translateX(-50%);
        }
      "))
    ),
    #____________________________________________
    
    sidebarMenu(
      id = "tabs",
      # adding sidebar tools to each menu item https://stackoverflow.com/questions/43003817/shiny-dashboard-inputs-inside-menuitems-problems
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")#,
               
               
               
               
      ),#menuItem 'dahboard' close
      
      conditionalPanel(
        "input.tabs == 'dashboard'",
        #____________________________________________
        
        selectInput(inputId="variableInput", multiple = T,h4("Select country",style="color:white"),choices = c("",as.character(sort(unique(data$country_countryname))))),
        #################
        selectInput(inputId="yearInput", multiple = T,h4("Select year",style="color:white"),choices = c(2024:1993)),#,selected =2020
        #selectInput(inputId="fillInput", multiple = F,h4("Select fill",style="color:white"),choices = c("aggcategory_type","conventionarea_areaname"),selected ="aggcategory_type"),
        br(),#br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
        tags$style(
          ".main-sidebar {padding-left:10px;}"),
        h4("Map downloads (.shp)",style="color:white"),
        downloadButton('AreaLicensedExport', 'Licencesed Polygons'),br(),br(),
        downloadButton('AreaDredgedExport', 'Dredged Polygons'),
        br(),br(),br(),
        selectInput(inputId="fillInput", multiple = F,h4("Barplot fill (QUANTITY)",style="color:white"),choices = c("aggcategory_type","conventionarea_areaname"),selected ="aggcategory_type")
        
        #____________________________________________
      ),
      
      
      menuItem("Publications", tabName = "publications", icon = icon("binoculars")),
      # icon options: https://fontawesome.com/v4/icons/

      conditionalPanel(
        "input.tabs == 'publications'",
        #____________________________________________
        ## Keywords
        selectInput(inputId="subjectInput", multiple = T,label = h4("Select keywords",style="color:white"),choices = c("",as.character(sort(unique(publications$tag_name))))),
        ## Authors
       selectizeInput(inputId="authorInput", multiple = T,label = h4("Select Author(s)",style="color:white"),choices = c("",as.character(sort(unique(publications$authors)))),
                      options = list(
                         persist = TRUE,
                         closeAfterSelect = FALSE,
                         openOnFocus = TRUE,
                         selectOnTab = TRUE,
                         create = FALSE,
                        plugins = list('remove_button')
                      ))
        
        #____________________________________________
      ),
      
      
      
      
      
      #menuItem("Publications", tabName = "publications", icon = icon("th")),
      #menuItem("Publications", tabName = "publications", icon = icon("book")),
      
      
  
      #____________________________________________
      menuItem("Guidelines", tabName = "guidelines", icon = icon("book")),
      #______________________________________________
      # menuItem("Management", tabName = "management", icon = icon("list-check")),
      
          #_______________________________________________
      menuItem("Management", tabName = "management", icon = icon("list-check")),
      conditionalPanel(
        "input.tabs == 'management'",
        #____________________________________________
        
        #selectInput(inputId="country2Input", multiple = T,h4("Select Country",style="color:white"),choices = c("",as.character(unique(management$Country)))),#country_countryname
        selectInput(inputId="questionInput", multiple = F,h4("Select question",style="color:white"),choices = c("",as.character(unique(management$question))),selected = as.character(unique(management$question))[1] ) # Default to the first question
        
        #____________________________________________
      ),
  
      #____________________________________________
      menuItem("Blog", tabName = "blog", icon = icon("fas fa-newspaper")),
      
      
      ############
      #h4("Map options:"),
      #    selectInput(inputId="variableInput", multiple = F,h4("Select variable",style="color:white"),choices = c("",as.character(unique(long$variable)))),
      #                  selectInput(inputId="valueInput", multiple = F,h4("Select value",style="color:white"),choices =NULL)
      
      #br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      # h4("***ATTENTION***TEST VERSION USE DATA WITH CAUTION"),
      #__________________________________________________________________________________________
      #### OB LOGO ####
      #   HTML('&emsp;'), HTML('&emsp;'),img(src="OBLogo.png",height = 85, width =160),
      
      #__________________________________________________________________________________________
      #### OTHER LOGOS ####
      #HTML('&emsp;'),
 img(src="iceslogov3.png", width="77%")#,#,height = 50, width =80
      ##HTML('&emsp;'),img(src="postgreslogo.png",height = 50, width =50),
      ##HTML('&emsp;'),img(src="rstudiologo.png",height = 50, width =50),
      ##HTML('&emsp;'),img(src="rshinylogo.png",height = 50, width =50),
      #br()
      
    )#sidebarMenu close
    #____________________________________________
    
    
  ),
  dashboardBody(
    
    modalDialog(
      
      title = div(h4("Welcome to the ",tags$b("Marine Aggregates Application (MAAP)")," from ICES WGEXT"),style = "text-align: center;"),
      h4("What is ICES WGEXT?"),
      p("The Working Group on the Effects of Extraction of Marine Sediments on the Marine Ecosystem (",tags$a(href="https://www.ices.dk/community/groups/pages/wgext.aspx","WGEXT"),"),
        under the International Council for the Exploration of the Sea (",tags$a(href="https://www.ices.dk/Pages/default.aspx","ICES"),"), plays a crucial role in ensuring the sustainable management of marine sand and gravel extraction. 
        WGEXT develops understanding and provides guidance on the environmental impacts of sediment extraction, including data collection, habitat mapping, and research on mitigation measures. 
        The group also reviews national extraction activities, seabed resource mapping programs, and environmental impact assessments to support informed decision-making and policy development. 
        Their work helps balance resource extraction with environmental protection, ensuring long-term sustainability."),
      br(),
      h4("App purpose"),
      p("The Marine Aggregates Application is designed to promote the work of ICES WGEXT through the following pages:"),
      p(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),icon("dashboard"),tags$b("Dashboard"), "- examine the location of aggregate dredging and quanitites involved."),
      p(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),icon("binoculars"),tags$b("Publications"), "- explore the extensive database of research literature concerning marine aggregates."),
      p(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),icon("book"),tags$b("Guidelines"), "- ICES guidelines for Management of Marine Sediment Extraction."),
      p(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),icon("list-check"),tags$b("Management"), "- compare management practices in different ICES member states."),
      p(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),icon("fas fa-newspaper"),tags$b("Blog"), "- posts concerning topics of interest from annual WGEXT meetings."),
      br(),
      h4("Disclaimer"),
      p("This app is made available for use on an 'as is' and 'as available' basis. No warranty is given in relation to the accuracy of the
      content or in respect of its use for any particular purpose. Note the app is still in development and it's contents are
      incomplete (see",tags$a(href="https://sway.office.com/orIuJoHSruYfhy09?ref=Link","here")," for further information).
Your access to and use of the content available on this app is entirely at your own risk. This work is licensed under",
         tags$a(href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1","CC BY 4.0"),"."),
      
      size = "l",
      easyClose = FALSE
    ),
    #____________________________________________
    tabItems(
      #____________________________________________
      # First tab content
      tabItem(tabName = "dashboard",
              #____________________________________________
              # Boxes need to be put in a row (or column)
              fluidRow(
                
                leafletOutput("map",width="100%", height="400")#
                
              ),    
              
              fluidRow(
                
                
                #____________________________________________
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
                                               div(formattableOutput("tot_area_lic"),style = 'font-size:58%'))),
                                  tabPanel("Total Area Footprint (km2)",
                                           div(style = 'overflow-y:scroll;height:380px;',# add vertical scrollbar
                                               div(formattableOutput("tot_area_dredged"),style = 'font-size:58%'))),
                                  
                                )#tabsetPanel close
                       )# tabPanel close 'Spatial Data Availability'
                       
                )# tabBox close
                #____________________________________________
              )#fluidrow close
              
              #____________________________________________
      ),##tabitem 'dashboard' close
      #____________________________________________
      # Second tab content
      
      tabItem(tabName = "publications",
              fluidRow(
                
                valueBox(numberofpapers, "Papers", color = "light-blue",icon = icon("file-alt")),
                
                valueBox(numberofreports, "Reports",color = "light-blue", icon = icon("newspaper")),
               
                valueBox(numberofphds, "PhDs", color = "light-blue",icon = icon("graduation-cap")),
                
                valueBox(numberofbooks, "Books", color = "light-blue",icon = icon("book")),
                valueBox(numberofwebsites, "Websites", color = "light-blue",icon = icon("globe"))
                
              ),
              box(
                width = 12,
                
                tabBox(
                  width = 12,
                  tabPanel("Papers",
                           
                           DT::dataTableOutput("mytable")),
              tabPanel("Reports",
                       DT::dataTableOutput("mytable_report")),
              tabPanel("PhD",
                       DT::dataTableOutput("mytable_phd")),
              tabPanel("Book",
                       DT::dataTableOutput("mytable_book")),
              tabPanel("Website",
                       DT::dataTableOutput("mytable_website"))
                )
              )
      ),#tabitem 'publications' end
      #____________________________________________
      
      # Second tab content
      tabItem(tabName = "management",
              #h2("Management: ", textOutput("selected_element")),
              #h2(),
             # verbatimTextOutput("selected_question"),
              h2(textOutput("selected_question")),
              #h2("Guidelines"),
             br(),
             textOutput("guidelinestable"),
              #DT::dataTableOutput("guidelinestable"),
             br(),
             br(),
             
              # h3("National Approaches "),
              box(title = "National picture",width = 12,DT::dataTableOutput("managementtable"))
              
      ),#tabitem 'publications' end
      #____________________________________________
      # Second tab content
      tabItem(tabName = "guidelines",
              #h2("Management: ", textOutput("selected_element")),
             
             # h2(" ICES GUIDELINES FOR THE MANAGEMENT OF MARINE SEDIMENT EXTRACTION"),
              #DT::dataTableOutput("managementtable"),
              
              
 ##############################################################################             
 #dashboardSidebar(),
 #dashboardBody(
 #  fluidRow(
     box(
       title = "ICES Guidelines for the Management of Marine Sediment Extraction",
       width = 12,
       tabBox(

         id = "tabset1",
         # Title can include an icon
         #title = tagList(shiny::icon("gear"), "Version: 1.0"),
         title = tagList(icon("file-alt", lib = "font-awesome"),"Guidelines version: 1.0"),
         width = 12,
         height = "850px",#250
         tabPanel("Text", 
                  bsCollapse(
                    id = "collapseExample",
                    
                    #_______________________________________________________________________________
                    ## INTRODUCTION ##
                    bsCollapsePanel(

                      ## Title                      
                      "Introduction", 
                      ## Text
                      "In many countries sand and gravel dredged from the seabed makes an important contribution to the national demand for aggregates, directly replacing materials extracted from land-based sources. This reduces the pressure to work land of agricultural importance or environmental and hydrological value, and where materials can be landed close to the point of use, there can be additional benefits of avoiding long distance over land transport. Marine dredged sand and gravel is also increasingly used in flood and coastal defence, fill and land reclamation schemes. For beach replenishment, marine materials are usually preferred from an amenity point of view, and are generally considered to be the most appropriate economically, technically and environmentally.",
                      br(),br(),
                      "However, these benefits need to be balanced against the potential negative impacts of aggregate dredging. Aggregate dredging activity, if not carefully controlled, can cause significant damage to the seabed and its associated biota, to commercial fisheries and to the adjacent coastlines, as well as creating conflict with other users of the sea. In addition, current knowledge of the resource indicates that while there are extensive supplies of some types of marine sand, there appear to be more limited resources of gravel suitable, for example, to meet current concrete specifications and for beach nourishment." ,
                      br(),br(),
                      "Against the background of utilising a finite resource, with the associated environmental impacts, it is recommended that regulators develop and work within a strategic framework which provides a system for examining and reconciling the conflicting claims on land and at sea. Decisions on individual applications can then be made within the context of the strategic framework.",
                      br(),br(),
                      "General principles for the sustainable management of all mineral resources overall include:",
                      br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("conserving minerals as far as possible, whilst ensuring that there are adequate supplies to meet the demands of society;"),
                        tags$li("encouraging their efficient use (and where appropriate re-use), minimising wastage and avoiding the use of higher quality materials where lower grade materials would suffice; "),
                        tags$li("ensuring that methods of extraction minimise the adverse effects on the environment, and preserve the overall quality of the environment once extraction has ceased;"),
                        tags$li("the encouragement of an ecosystem approach to the management of extraction activities and identification of areas suitable for extraction;"),
                        tags$li("protecting sensitive areas and important habitats (such as marine conservation areas) and industries (including fisheries) and the interests of other legitimate uses of the sea;"),
                        tags$li("preventing unnecessary sterilisation of mineral resources by other forms of development.")),
                      ## Text
                      br(),br(),
                      "The implementation of these principles requires a knowledge of the resource, and an understanding of the potential impacts of its extraction and of the extent to which rehabilitation of the seabed is likely to take place. The production of an Environmental Statement, developed along the lines suggested below, should provide a basis for determining the potential effects and identifying possible mitigating measures. There will be cases where the environment is too sensitive to disturbance to justify the extraction of aggregate, and unless the environmental and coastal issues can be satisfactorily resolved, extraction should not normally be allowed.",
                      br(),br(),
                      "It should also be recognised that improvements in technology may enable exploitation of marine sediments from areas of the seabed which are not currently commercially viable, while development of technical specifications for concrete, etc., may in the future enable lower quality materials to be used for a wider range of applications. In the shorter term, continuation of programmes of resource mapping may also identify additional sources of coarser aggregates.",
                      style = "info"),
                    #_______________________________________________________________________________
                    ## SCOPE ##
                    bsCollapsePanel(
                      ## Title
                      "Scope",
                      ## Text
                      p("It is recognised that sand and gravel extraction, if undertaken in an inappropriate way, may cause significant harm to the marine and coastal environment. There are a number of international and regional initiatives that should be taken into account when developing national frameworks and guidelines. These include the Convention on Biological Diversity (CBD), EU Directives (particularly those on birds, habitats, Environmental Impact Assessment (EIA), and Strategic Environmental Assessment (SEA)—once implemented) and other regional conventions/agreements, in particular the OSPAR and Helsinki Conventions, and initiatives pursued under them. This subject, for example, has recently been included in the Action Plan for Annex V to the 1992 OSPAR Convention on the Protection and Conservation of the Ecosystems and Biological Diversity of the Maritime Area as a human activity requiring assessment. It is also recognised that certain ecologically sensitive areas may not be designated under international, European, or national rules but nonetheless require particular consideration within the assessment procedures described in these Guidelines."), style = "info"),
                    #_______________________________________________________________________________
                    ## ADMINISTRATIVE FRAMEWORK ##
                    bsCollapsePanel(
                      ## Title
                      "Administrative framework", 
                      ## Text
                      p("It is recommended that countries have an appropriate framework for the management of sand and gravel extraction and that they define and implement their own administrative framework with due regard to these guidelines. There should be a designated regulatory authority to:"),
                      ## Bullets
                      tags$ul(
                        tags$li("issue authorisation having fully considered the potential environmental effects;"),
                        tags$li("be responsible for compliance monitoring;"),
                        tags$li("develop the framework for monitoring;"),
                        tags$li("enforce conditions.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## ENVIRONMENTAL IMPACT ASSESSMENT ##
                    bsCollapsePanel(
                      ## Title
                      "Environmental impact assessment", 
                      ## Text
                      "The extraction of sand and gravel from the seabed can have significant physical and biological effects on the marine and coastal environment. The significance and extent of the environmental effects will depend upon a range of factors including the location of the extraction area, the nature of the surface and underlying sediment, coastal processes, the design, method, rate, amount and intensity of extraction, and the sensitivity of habitats and assorted biodiversity, fisheries and other uses in the locality. These factors are considered in more detail below. Particular consideration should be given to sites designated under international, European, national and local legislation, in order to avoid unacceptable disturbance or deterioration of these areas for the habitats, species, and other designated features.", 
                      br(),br(),
                      "To enable the organisation(s) responsible for authorising extraction to evaluate the nature and scale of the effects and to decide whether a proposal can proceed, it is necessary that an adequate assessment of the environmental effects be carried out. It is important, for example, to determine whether the application is likely to have an effect on the coastline, or have potential impact on fisheries and the marine environment.",
                      br(),br(),
                      "The Baltic Marine Environment Protection Commission (Helsinki Commission) adopted HELCOM Recommendation 19/1 on 26 March 1998. This recommends to the Governments of Contracting Parties that an EIA should be undertaken in all cases before an extraction is authorised. For EU member states, the extraction of minerals from the seabed falls within Annex II of the “Directive on the Assessment of the Effects of Certain Public and Private Projects on the Environment” (85/337/EEC) As an Annex II activity, an EIA is required if the Member State takes the view that one is necessary. It is at the discretion of the individual Member States to define the criteria and/or threshold values that need to be met to require an EIA. The Directive was amended in March 1997 by Directive 97/11/EC. Member States are obliged to transpose the requirements of the Directive into national legislation by March 1999.",
                      br(),br(),
                      "It is recommended that the approach adopted within the EU be followed. Member States should therefore set their own thresholds for deciding whether and when an EIA is required, but it is recommended that an EIA always be undertaken where extraction is proposed in areas designated under international, European, or national rules and in other ecologically sensitive areas. For NATURA 2000 sites, Article 6 of the Habitats Directive contains special requirements in this respect.",
                      br(),br(),
                      "Where an EIA is considered appropriate, the level of detail required to identify the potential impacts on the environment should be carefully considered and identified on a site-specific basis. An EIA should normally be prepared for each extraction area, but in cases where multiple operations in the same area are proposed, a single impact assessment for the whole area may be more appropriate, which takes account of the potential for any cumulative impacts. In such cases, consideration should be given to the need for a strategic environmental assessment.",
                      br(),br(),
                      "Consultation is central to the EIA process. The framework for the content of the EIA should be established by early consultation with the regulatory authority, statutory consultees, and other interested parties. Where there are potential transboundary issues, it will be important to undertake consultation with the other countries likely to be affected, and the relevant Competent Authorities are encouraged to establish procedures for effective communication.",
                      br(),br(),
                      "As a general guide, it is likely that the following topics considered below will need to be addressed.",
                      
                      style = "info"),
                    #_______________________________________________________________________________
                    ## DESCRIPTION OF PHYSICAL SETTING ##
                    bsCollapsePanel(
                      ## Title
                      "Description of the physical setting", 
                      ## Text
                      "The proposed extraction area should be identified by geographical location, and described in terms of:",br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("the bathymetry and topography of the general area;"),
                        tags$li("the distance from the nearest coastlines;"),
                        tags$li("the geological history of the deposit;"),
                        tags$li("the source of the material;"),
                        tags$li("type of material;"),
                        tags$li("sediment particle size distribution;"),
                        tags$li("extent and volume of the deposit;"),
                        tags$li("the stability and/or natural mobility of the deposit;"),
                        tags$li("thickness of the deposit and evenness over the proposed extraction area;"),
                        tags$li("the nature of the underlying deposit, and any overburden;"),
                        tags$li("local hydrography including tidal and residual water movements;"),
                        tags$li("wind and wave characteristics;"),
                        tags$li("average number of storm days per year;"),
                        tags$li("estimate of bed-load sediment transport (quantity, grain size, direction);"),
                        tags$li("topography of the seabed, including occurrence of bedforms;"),
                        tags$li("existence of contaminated sediments and their chemical characteristics;"),
                        tags$li("natural (background) suspended sediment load under both tidal currents and wave action.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## DESCRIPTION OF THE BIOLOGICAL SETTING ##
                    bsCollapsePanel(
                      ## Title
                      "Description of the biological setting", 
                      ## Text
                      "The biological setting of the proposed extraction site and adjacent areas should be described in terms of:",br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("the flora and fauna within the area likely to be affected by aggregate dredging (e.g., pelagic and benthic community structure), taking into account temporal and spatial variability;"),
                        tags$li("information on the fishery and shellfishery resources including spawning areas, with particular regard to benthic spawning fish, nursery areas, over-wintering grounds for ovigerous crustaceans, and known routes of migration;"),
                        tags$li("trophic relationships (e.g., between the benthos and demersal fish populations by stomach content investigations);"),
                        tags$li("presence of any areas of special scientific or biological interest in or adjacent to the proposed extraction area, such as sites designated under local, national or international regulations (e.g., Ramsar sites, the UNEP ”Man and the Biosphere” Reserves, World Heritage sites, Marine Protected Areas (MPAs) Marine Nature Reserves, Special Protection Areas (Birds Directive), or the Special Areas of Conservation (Habitats Directive)).")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## DESCRIPTION OF THE PROPOSED AGGREGATE DREDGING ACTIVITY##
                    bsCollapsePanel(
                      ## Title
                      "Description of the proposed aggregate dredging activity", 
                      ## Text
                      "The assessment should include, where appropriate, information on:",br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("the total volume to be extracted; "),
                        tags$li("proposed maximum annual extraction rates and dredging intensity;"),
                        tags$li("expected lifetime of the resource and proposed duration of aggregate dredging;"),
                        tags$li("aggregate dredging equipment to be used;"),
                        tags$li("spatial design and configuration of aggregate dredging (i.e., the maximum depth of deposit removal, the shape and area of resulting depression);"),
                        tags$li("substrate composition on cessation of aggregate dredging;"),
                        tags$li("proposals to phase (zone) operations;"),
                        tags$li("whether on-board screening (i.e., rejection of fine or coarse fractions) will be carried out;"),
                        tags$li("number of dredgers operating at a time;"),
                        tags$li("routes to be taken by aggregate dredgers to and from the proposed extraction area;"),
                        tags$li("time required for aggregate dredgers to complete loading;"),
                        tags$li("number of days per year on which aggregate dredging will occur;"),
                        tags$li("whether aggregate dredging will be restricted to particular times of the year or parts of the tidal cycle;"),
                        tags$li("direction of aggregate dredging (e.g., with or across tide).")),
                      ## Text
                      "It may be appropriate, when known also to include details of the following:",br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("energy consumption and gaseous emissions;"),
                        tags$li("ports for landing materials;"),
                        tags$li("servicing ports;"),
                        tags$li("on-shore processing and onward movement;"),
                        tags$li("project-related employment.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## INFORMATION REQUIRED FOR PHYSICAL IMPACT ASSESSMENT##
                    bsCollapsePanel(
                      ## Title
                      "Information required for physical impact assessment", 
                      ## Text
                      "To assess the physical impacts, the following should be considered:",br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("implications of extraction for coastal and offshore processes, including possible effects on beach draw down, changes to sediment supply and transport pathways, changes to wave and tidal climate;"),
                        tags$li("changes to the seabed topography and sediment type"),
                        tags$li("exposure of different substrates;"),
                        tags$li("changes to the behaviour of bedforms within the extraction and adjacent areas;"),
                        tags$li("potential risk of release of contaminants by aggregate dredging, and exposure of potentially toxic natural substances;"),
                        tags$li("transport and settlement of fine sediment disturbed by the aggregate dredging equipment on the seabed, and from hopper overflow or on-board processing and its impact on normal and maximum suspended load;"),
                        tags$li("the effects on water quality mainly through increases in the amount of fine material in suspension;"),
                        tags$li("implications for local water circulation resulting from removal or creation of topographic features on the seabed;"),
                        tags$li("the time scale for potential physical “recovery” of the seabed.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## INFORMATION REQUID FOR BIOLOGICAL IMPACT ASSESSMENT ##
                    bsCollapsePanel(
                      ## Title
                      "Information required for biological impact assessment",
                      ## Text
                      "To assess the biological impact, the following information should be considered:", br(),br(),
                      ## Bullets
                      tags$ul(tags$li("changes to the benthic community structure, and to any ecologically sensitive species or habitats that may be particularly vulnerable to extraction operations;"),
                              tags$li("effects of aggregate dredging on pelagic biota;"),
                              tags$li("effects on the fishery and shellfishery resources including spawning areas, with particular regard to benthic spawning fish, nursery areas, over-wintering grounds for ovigerous crustaceans, and known routes of migration;"),
                              tags$li("effects on trophic relationships (e.g., between the benthos and demersal fish populations);"),
                              tags$li("Effects on sites designated under local, national or international regulations (see above);"),
                              tags$li("predicted rate and mode of recolonisation, taking into account initial community structure, natural temporal changes, local hydrodynamics, and any predicted change of sediment type;"),
                              tags$li("effects on marine flora and fauna including seabirds and mammals;"),
                              tags$li(" effects on the ecology of boulder fields/stone reefs.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## INTERFERENCE WITH OTHER LEGITIMATE USERS OF THE SEA ##
                    bsCollapsePanel(
                      ## Title
                      "Interference with other legitimate uses of the sea",
                      ## Text
                      "The assessment should consider the following in relation to the proposed programme of extraction:", br(),br(),
                      ## Bullets
                      tags$ul(tags$li("commercial fisheries;"),
                              tags$li("shipping and navigation lanes;"),
                              tags$li("military exclusion zones;"),
                              tags$li("offshore oil and gas activities;"),
                              tags$li("engineering uses of the seabed (e.g., adjacent extraction activities, undersea cables and pipelines including associated safety and exclusion zones);"),
                              tags$li("areas designated for the disposal of dredged or other materials;"),
                              tags$li("location in relation to existing or proposed aggregate extraction areas;"),
                              tags$li("location of wrecks and war-graves in the area and general vicinity;"),
                              tags$li("wind farms;"),
                              tags$li("areas of heritage, nature conservation, archaeological and geological importance;"),
                              tags$li("recreational uses;"),
                              tags$li("general planning policies for the area (international, national, and local);"),
                              tags$li("Any other legitimate use of the sea.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## EVALUATION OF IMPACTS ##
                    bsCollapsePanel(
                      ## Title
                      "Evaluation of impacts", 
                      ## Text
                      "When evaluating the overall impact, it is necessary to identify and quantify the marine and coastal environmental consequences of the proposal. The EIA should evaluate the extent to which the proposed extraction operation is likely to affect other interests of acknowledged importance. Consideration should also be given to the assessment of the potential for cumulative impacts on the marine environment. In this context, cumulative impacts might occur as a result of aggregate dredging at a single site over time, from multiple sites in close proximity, or in combination with effects from other human activities (e.g., fishing and disposal of harbour dredgings).", br(), br(),
                      "It is recommended that a risk assessment be undertaken. This should include consideration of worst-case scenarios, and indicate uncertainties and assumptions used in their evaluation.", br(), br(),
                      "The environmental consequences should be summarised as an impact hypothesis. The assessment of some of the potential impacts requires predictive techniques, and it will be necessary to use appropriate mathematical models. Where such models are used, there should be sufficient explanation of the nature of the model, including its data requirements, its limitations and any assumptions made in the calculations, to enable assessment of its suitability for the particular modelling exercise.", br(), br(),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## MITIGATION MEASURES ##
                    bsCollapsePanel(
                      ## Title
                      "Mitigation measures",
                      ## Text
                      "The impact hypothesis should include consideration of the steps that might be taken to mitigate the effects of extraction activities. These may include: ", br(),br(),
                      ## Bullets
                      tags$ul(tags$li("the selection of aggregate dredging equipment and timing of aggregate dredging operations to limit impact upon the biota (such as birds, benthic communities, any particularly sensitive species and habitats, and fish resources);"),
                              tags$li("modification of the depth and design of aggregate dredging operations to limit changes to hydrodynamics and sediment transport and to minimise the effects on fishing;"),
                              tags$li("spatial and temporal zoning of the area to be authorised for extraction or scheduling extraction to protect sensitive fisheries or to respect access to traditional fisheries;"),
                              tags$li("preventing on-board screening or minimising material passing through spillways when outside the dredging area to reduce the spread of the sediment plume;"),
                              tags$li(" agreeing exclusion areas to provide refuges for important habitats or species, or other sensitive areas.")),
                      ## Text
                      "Evaluation of the potential impacts of the aggregate dredging proposal, taking into account any mitigating measures, should enable a decision to be taken on whether or not the application should proceed. In some cases it will be appropriate to monitor certain effects as the aggregate dredging proceeds. The EIA should form the basis for the monitoring plan. ", br(),br(),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## AUTHORISATION ISSUE ##
                    bsCollapsePanel(
                      ## Title
                      "Authorisation issue",
                      ## Text
                      "When an aggregate extraction operation is approved, then an authorisation should be issued in advance (which may take the form of a permit, licence or other form of regulatory approval). In granting an authorisation, the immediate impact of aggregate extraction occurring within the boundaries of the extraction site, such as alterations to the local physical and biological environment, is accepted by the regulatory authority. Notwithstanding these consequences, the conditions under which an authorisation for aggregate extraction is issued should be such that environmental change beyond the boundaries of the extraction site are as far below the limits of allowable environmental change as practicable. The operation should be authorised subject to conditions which further ensure that environmental disturbance and detriment are minimised.", br(),br(),
                      "he authorisation is an important tool for managing aggregate extraction and will contain the terms and conditions under which aggregate extraction may take place, as well as provide a framework for assessing and ensuring compliance.",br(),br(),
                      "Authorisation conditions should be drafted in plain and unambiguous language and will be designed to ensure that:",br(),br(),
                      tags$ul(
                        tags$p("a) the material is only extracted from within the selected extraction site;"),
                        tags$p("b) any mitigation requirements are complied with; and "),
                        tags$p("c) any monitoring requirements are fulfilled and the results reported to the regulatory authority.")),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## MONITORING COMPLIANCE WITH CONDITIONS ATTACHED TO THE AUTHORISATION ##
                    bsCollapsePanel(
                      ## Title
                      "Monitoring compliance with conditions attached to the authorisation", 
                      ## Text
                      "An essential requirement for the effective control of marine aggregate extraction is the monitoring of dredging activities to ensure conformity with the authorisation requirements. This has been achieved in several ways, e.g., an Electronic Monitoring System or Black Box. The information provided will allow the regulatory authority to monitor the activities of aggregate dredging vessels to ensure compliance with particular conditions in the authorisation.",br(),br(),
                      "The information collected and stored will depend on the requirements of the individual authorities and the regulatory regime under which the permission is granted, e.g., EIA, Habitats, Birds Directives of the EU. ",br(),br(),
                      "The minimum requirements for the monitoring system should include:",br(),br(),
                      ## Bullets
                      tags$ul(
                        tags$li("an automatic record of the date, time and position of all aggregate dredging activity;"),
                        tags$li(" position to be recorded to within a minimum of 100 metres in latitude and longitude or other agreed coordinates using a satellite-based navigation system;"),
                        tags$li("there should be an appropriate level of security;"),
                        tags$li("the frequency of recording of position should be appropriate to the status of the vessel, i.e., less frequent records when the vessel is in harbour or in transit to the aggregate dredging area e.g., every 30 minutes, and more frequently when dredging, e.g., every 30 seconds.")),
                      ## Text
                      "The above are considered to be reasonable minimum requirements to enable the regulatory authority to monitor the operation of the authorisation in accordance with any conditions attached. Individual countries may require additional information for compliance monitoring at their own discretion.",br(),br(),
                      "The records can also be used by the aggregate dredging company to improve utilisation of the resources. The information is also an essential input into the design and development of appropriate environmental monitoring programmes and research into the physical and biological effects of aggregate dredging, including combined/cumulative impacts (see section above).",br(),br(),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## ENVIRONMENTAL MONITORING ##
                    bsCollapsePanel(
                      ## Title
                      "Environmental monitoring",
                      ## Text
                      "Sand and gravel extraction inevitably disturbs the marine environment. The extent of the disturbance and its environmental significance will depend on a number of factors. In many cases, it will not be possible to predict, in full, the environmental effects at the outset, and a programme of monitoring may be needed to demonstrate the validity of the EIA’s predictions, the effectiveness of any conditions imposed on the authorisation, and therefore the absence of unacceptable impacts on the marine environment.",br(),br(),
                      "The level of monitoring should depend on the relative importance and sensitivity of the surrounding area. Monitoring requirements should be site-specific, and should be based, wherever possible, on the findings of the EIA. To be cost-effective, monitoring programmes should have clearly defined objectives derived from the impact hypothesis developed during the EIA process. The results should be reviewed at regular intervals against the stated objectives, and the monitoring exercise should then be continued, revised, or even terminated.",br(),br(),
                      "It is also important that the baseline and subsequent monitoring surveys take account of natural variability. This can be achieved by comparing the physical and biological status of the areas of interest with suitable reference sites located away from the influence of the aggregate dredging effects, and of other anthropogenic disturbance. Suitable locations should be identified as part of the EIA’s impact hypothesis.",br(),br(),
                      "A monitoring programme may include assessment of a number of effects. When developing the programme, a number of questions should be addressed, including:",br(),br(),
                      ## Bullets
                      tags$ul(tags$li("What are the environmental concerns that the monitoring programme seeks to address?"),
                              tags$li("What measurements are necessary to identify the significance of a particular effect?"),
                              tags$li("What are the most appropriate locations at which to take samples or observations for assessment?"),
                              tags$li("How many measurements are required to produce a statistically sound programme?"),
                              tags$li("What is the appropriate frequency and duration of monitoring?")),
                      ## Text
                      "The regulatory authority is encouraged to take account of relevant research information in the design and modification of monitoring programmes.",br(),br(),
                      "The spatial extent of sampling should take account of the area designated for extraction and areas outside which may be affected. In some cases, it may be appropriate to monitor more distant locations where there is some question about a predicted nil effect. The frequency and duration of monitoring may depend upon the scale of the extraction activities and the anticipated period of consequential environmental changes, which may extend beyond the cessation of extraction activities.",br(),br(),
                      "Information gained from field monitoring (or related research studies) should be used to amend or revoke the authorisation, or refine the basis on which the aggregate extraction operation is assessed and managed. As information on the effects of marine aggregate dredging becomes more available and a better understanding of impacts is gained, it may be possible to revise the monitoring necessary. It is therefore in the interest of all concerned that monitoring data are made widely available. Reports should detail the measurements made, results obtained, their interpretation, and how these data relate to the monitoring objectives.",br(),br(),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## REPORTING FRAMEWORK ##
                    bsCollapsePanel(
                      ## Title
                      "Reporting Framework", 
                      ## Text
                      "It is recommended that the national statistics on aggregate dredging activity continue to be collated annually by the ICES Working Group on the Effects of Extraction of Marine Sediments on the Marine Ecosystem (WGEXT).", 
                      style = "info"),
                    #_______________________________________________________________________________
                    ## DEFINITIONS ##
                    bsCollapsePanel(
                      ## Title
                      "Definitions",
                      ## Text
                      "In these Guideline, “marine sediment extraction” is intended to refer to the extraction of marine sands and gravels (or “aggregates”) from the seabed for use in the construction industry (where they often directly replace materials extracted from land-based sources), and for use in flood and coastal defence, beach replenishment, fill and land reclamation projects. It is recognised that other materials are also extracted from the seabed, such as stone, shell materials, and maerl, and similar considerations to those set out in the Guidelines should also apply to them. The Guidelines do not apply to navigational dredging (e.g., maintenance or capital dredging operations).", br(),br(),
                      "In these Guidelines, the term “authorisation” is used in preference to “permit” or “license” and is intended to replace both terms. The legal regime under which marine extraction operations are authorized and regulated differs from country to country, and the terms permit and license may have a specific connotation within national legal regimes, and also under rules of international law. The term “authorisation” is thus used to mean any use of permits, licenses, or other forms of regulatory approval.", br(),br(),
                      "The ecosystem approach will be elaborated by further work in both OSPAR and ICES. The following definition has been used elsewhere “the comprehensive integrated management of human activities based on best available scientific knowledge about the ecosystem and its dynamics, in order to identify and take action on influences which are critical to the health of marine ecosystems, thereby achieving sustainable use of ecosystem goods and services and maintenance of ecosystem integrity.”", br(),br(),
                      style = "info"),
                    #_______________________________________________________________________________
                    ## REVISION OF GUIDELINES ##
                    bsCollapsePanel(
                      ## Title
                      "Revision of Guidelines", 
                      ## Text
                      "WGEXT will continue to review any new information, conclusions, and understandings from scientific research projects, any reports from countries on their experiences with the implementation of the Guidelines and, where appropriate, will revise the Guidelines accordingly.", 
                      style = "info")
                    #_______________________________________________________________________________
                  )
         ),
         #tabPanel("Revisions Log", "Content for tab 2")
         #tabPanel("Revisions Log", tableOutput("mtcars_kable"))
         tabPanel("Revisions Log", dataTableOutput("mtcars_kable"))
       )
     )
#   )
# )           

################################################################################
      ),#tabitem 'publications' end
######################
tabItem( tabName = "blog",
         h2("Blog Page"),
         uiOutput("blogPosts")
         #p("Welcome to the blog page! Here you can add your blog content.")
)
      #____________________________________________
    )#tabItems end
    #____________________________________________
    
  )#dashboardbody closend
  #____________________________________________
  
)#dashboardPage close


#_______________________________________________________________________________

server <- function(input, output, session) {
  #set.seed(122)
  #histdata <- rnorm(500)
#_______________________________________________________________________________
  

#_______________________________________________________________________________
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
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
  output$blogPosts <- renderUI({
    renderBlogPosts(blog_content)
  })
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
  
  publications_sel <- reactive({
    View(publications)
    if(is.null(input$subjectInput)|| input$subjectInput==""){
      publication2.5 <- subset( publications,authors %in% input$authorInput)
    publication3 <- unique(publication2.5[,1:6])
  }else{
    publication2 <- subset( publications,tag_name %in% input$subjectInput)
    publication2.5 <- subset( publication2,authors %in% input$authorInput)
    publication3 <- unique(publication2.5[,1:6])
  }
    
    
    
    
    return( publication3)
  })
  
  
  ## Publications table: papers
  output$mytable = DT::renderDataTable(
    publications_sel() %>%
      filter(pub_type == "paper")%>% arrange(desc(Year))%>% select(2,3),
    #options = list(pageLength = 10, lengthChange = FALSE),
    ## Remove col header
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")),
    rownames= FALSE,
    escape = FALSE
  )
  
  ## Publications table:report
  output$mytable_report = DT::renderDataTable(
    publications_sel() %>%
      filter(pub_type == "report")%>% arrange(desc(Year))%>% select(2,3),
    #options = list(pageLength = 10, lengthChange = FALSE),
    ## Remove col header
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")),
    rownames= FALSE,
    escape = FALSE
  )
  
  ## Publications table:PhD
  output$mytable_phd = DT::renderDataTable(
    publications_sel() %>%
      filter(pub_type == "phd")%>% arrange(desc(Year))%>% select(2,3),
    #options = list(pageLength = 10, lengthChange = FALSE),
    ## Remove col header
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")),
    rownames= FALSE,
    escape = FALSE
  )
  ## Publications table:website
  output$mytable_website = DT::renderDataTable(
    publications_sel() %>%
      filter(pub_type == "website")%>% arrange(desc(Year))%>% select(2,3),
    #options = list(pageLength = 10, lengthChange = FALSE),
    ## Remove col header
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")),
    rownames= FALSE,
    escape = FALSE
  )
  #__________________________________________________________________________________________
  management_sel <- reactive({
    
    
    
    #management2 <- subset( management,country_countryname %in% input$country2Input)
    #management2 <- subset( management,Country %in% input$country2Input)
    #management3 <- subset( management2,question %in% input$questionInput)
    management3 <- subset( management,question %in% input$questionInput)
    return(management3)
  })
  
  ## Management table
  output$managementtable = DT::renderDataTable(
    #management
    management_sel()[,c(1,4)],
    
## Remove table headers
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      searching = FALSE#,  # Removes the search box
      #headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")# remove table column headers
      ),
    rownames= FALSE,
    escape = FALSE

  )
  
  output$selected_element <- renderText({
    paste( input$questionInput)
  })
  #__________________________________________________________________________________________
  guidelines_sel <- reactive({
    
    
    #management2 <- subset( management,country_countryname %in% 'United Kingdom')
    guidelines2 <- subset( guidelines,question %in% input$questionInput)
    guidelines3 <- as.data.frame(guidelines2[,2])
    colnames(guidelines3)[1] <- 'Advice'
    
    return(guidelines3)
  })
  
  ## Management table
  output$guidelinestable = 
    renderText({
      paste( guidelines_sel())#"You have chosen:",
    })  
#    DT::renderDataTable(
#    guidelines_sel(), 
#    options = list(
#      stripe = FALSE,
#      pageLength = 10,
#      lengthChange = FALSE,
#      searching = FALSE,  # Removes the search box
#      style="bootstrap",
#      dom = 't',  # Removes table controls (search box, pagination, etc.)
     
#headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }") ),
#rownames = FALSE,
#)
  #__________________________________________________________________________________________
  #### GUIDELINES TABLE UPDATES ####
  ## Create guidelines update table 
  pred_var <- data.frame(
    Section=c(""),
    Change=c(paste("Existing ",tags$a(href="https://doi.org/10.17895/ices.pub.5398","Guidelines for the Management of Marine Sediment Extraction")," digitised and uploaded to MAAP")),
    Date=c("March 2025"),
    Version=c('1.0'))
  
  # Convert the 'Change' column to HTML
  pred_var$Change <- lapply(pred_var$Change, HTML)
  
  ## Create Revisions Log table using kable 
 # output$mtcars_kable <- function() {
 #   pred_var %>%
 #     knitr::kable("html") %>%
  #    kable_styling("striped", full_width = T) }
    #__________________________________________________________________________________________
    # Create Revisions Log table using DT
    output$mtcars_kable <- DT::renderDataTable({
      datatable(pred_var, escape = FALSE, options = list(dom = 't', paging = FALSE, searching = FALSE))
    })
    #_________________________________________________________________________________________
 
  
  ## Repeat of question on main body
  output$selected_question <- renderText({
    paste( input$questionInput)#"You have chosen:",
  })
  

  
}

shinyApp(ui, server)
