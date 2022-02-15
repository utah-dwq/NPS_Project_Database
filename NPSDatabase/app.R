library(readxl)
library(shiny)
library(leaflet)
library(wqTools)
library(shinycssloaders)
library(shinyjs)
library(htmltools)
library(lubridate)
library(DT)


data = readxl::read_xlsx("C:\\Users\\ehinman\\Documents\\GitHub\\NPS_Project_Database\\NPSDatabase\\data\\NPS_projects_draft_JB.xlsx")
colnames(data) = gsub(" ", ".", colnames(data))
data$Year.Awarded = paste0(as.character(data$Year.Awarded),"-01-01")
data$Year.Awarded = as.Date(data$Year.Awarded, format = "%Y-%m-%d")
location = na.omit(unique(data[,c("Project.Latitude","Project.Longitude")]))
location = wqTools::assignPolys("wmu_poly", columns = "Mgmt_Unit",data=location, lat = "Project.Latitude", long = "Project.Longitude")
location = wqTools::assignPolys("au_poly", columns = "AU_NAME", data = location, lat = "Project.Latitude", long = "Project.Longitude")

data = merge(data, location, all.x = TRUE)
data$Mgmt_Unit = as.character(data$Mgmt_Unit)
data$Mgmt_Unit[data$Watershed=="Statewide"] = "Statewide"
data$Mgmt_Unit[is.na(data$Mgmt_Unit)] = "No Location Available"
data$Project.Latitude[data$Mgmt_Unit=="No Location Available"] = jitter(rep(41.979736, length(data$Project.Latitude[data$Mgmt_Unit=="No Location Available"])), factor = 0.01)
data$Project.Longitude[data$Mgmt_Unit=="No Location Available"] = jitter(rep(-114.014642, length(data$Project.Longitude[data$Mgmt_Unit=="No Location Available"])), factor = 0.01)
data$Project.Latitude[data$Mgmt_Unit=="Statewide"] = jitter(rep(39.419220, length(data$Project.Latitude[data$Mgmt_Unit=="Statewide"])), factor = 0.01)
data$Project.Longitude[data$Mgmt_Unit=="Statewide"] = jitter(rep(-111.950684, length(data$Project.Longitude[data$Mgmt_Unit=="Statewide"])), factor = 0.01)

data$Project.Type = as.factor(data$Project.Type)

# save(data, file = "C:\\Users\\ehinman\\Documents\\GitHub\\NPS_Project_Database\\NPSDatabase\\Draft_NPS_tool_data.Rdata")

# load("data\\Draft_NPS_tool_data.Rdata")
# write.csv(data, "NPS_DB_Tool_data.csv", row.names = FALSE)
# data = read.csv("NPS_DB_Tool_data.csv")
data$Year.Awarded = as.Date(data$Year.Awarded, format = "%d/%m/%Y")
data$Mgmt_Unit = as.character(data$Mgmt_Unit)
type_pal = colorFactor("Spectral", data$Project.Type)

# Define UI for application that draws a histogram
ui <- fluidPage(title = "Nonpoint Source Project Database",
                useShinyjs(),
                titlePanel(title=div(img(width="5%",height="5%",src="dwq_logo_small.png"), "Nonpoint Source Project Database")),
                fluidRow(
                    column(6,
                           strong("Use the watershed unit, project type, funding sources, and date range widgets to filter to projects of interest. Projects that meet the filters will display on the map."),
                           br(),
                           br(),
                           fluidRow(
                             div(id = "rst",
                                 column(3, selectInput("ws", "Watershed Unit", choices = c("All", unique(data$Mgmt_Unit)), multiple = TRUE)), # map filter 1
                                 column(3, selectInput("ptype","Project Type", choices = c("All",unique(as.character(data$Project.Type))), multiple = TRUE)), # map filter 2
                                 column(2, checkboxGroupInput("funding", "Funding Source(s)", choiceNames = c("Federal 319","State NPS"), choiceValues = c("319","NPS"), selected = c("319","NPS")))),
                                 column(4, sliderInput("date", "Date Range", min = min(data$Year.Awarded), max = max(data$Year.Awarded), value = c(min(data$Year.Awarded),max(data$Year.Awarded)), timeFormat = "%Y", step = 730))
                           ),
                           fluidRow(
                             # column(2, actionButton("search","Search Map")),
                                    column(3, align="right", actionButton("resetsrch", "Reset Search")),
                                    column(4, align="center", actionButton("selall", "Select All Mapped Projects")),
                                    column(3, align="left", actionButton("clearsel", "Clear Project Selection"))),
                           br(),
                           fluidRow(column(12,
                                           strong("Hover over markers for basic project information, and click on markers to see project information in the right pane. Click on the layers icon in the upper right corner to turn on watershed unit and/or HUC12 boundaries."),
                             leafletOutput("projmap")%>%withSpinner(),tags$head(tags$style(".leaflet-top {z-index:999!important;}"))))), # map
                    column(6, 
                           strong("For now, only this table displays when sites are selected, but I would like to eventually show summary statistics for all selected projects below."),
                           fluidRow(
                             DT::dataTableOutput("projdata")
                           ), # Data table 1
                           fluidRow()) # Data table(s) 2
                )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive values to be used throughout app
  appdata = reactiveValues()
  
  # Original map layout
  output$projmap = renderLeaflet({
    ut_poly = wqTools::ut_poly
    wmu_poly = wqTools::wmu_poly
    au_poly = wqTools::au_poly
    huc12_poly = wqTools::huc12_poly
    
    map=leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE, dragging=TRUE))
    map=leaflet::setView(map, lat=39.419220, lng=-111.950684, zoom = 6)
    map=leaflet::addProviderTiles(map, "Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))
    map=leaflet::addProviderTiles(map,"Esri.WorldImagery", group = "Satellite", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))
    
    map=addMapPane(map,"ut_poly", zIndex = 410)
    map=addMapPane(map,"wmu_poly", zIndex = 412)
    map=addMapPane(map, "au_poly", zIndex = 413)
    map=addMapPane(map,"huc12_poly", zIndex = 415)
    map=addMapPane(map, "highlight", zIndex = 418)
    map=addMapPane(map,"proj_markers", zIndex = 420)
    
    map=leaflet::addPolygons(map, data=wmu_poly,group="Watershed Units",smoothFactor=2,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "wmu_poly"),
                             popup=wmu_poly$Mgmt_Unit)
    map=leaflet::addPolygons(map, data=au_poly,group="Assessment Units",smoothFactor=2,fillOpacity = 0.1,weight=3,color="pink", options = pathOptions(pane = "au_poly"),
                             popup=au_poly$AU_NAME)
    map=leaflet::addPolygons(map, data=ut_poly,group="State Boundary",smoothFactor=2,fillOpacity = 0.1,weight=3,color="purple", options = pathOptions(pane = "ut_poly"))
    map=leaflet::addPolygons(map, data=huc12_poly,group="HUC12 Boundaries",smoothFactor=2,fillOpacity = 0.1,weight=3,color="yellow", options = pathOptions(pane = "huc12_poly"),
                             popup=huc12_poly$HUC12)
    map=leaflet::addLayersControl(map,
                                  position ="topright",
                                  baseGroups = c("Topo","Satellite"),overlayGroups = c("Watershed Units", "Assessment Units", "State Boundary", "HUC12 Boundaries"),
                                  options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
    map=hideGroup(map, "State Boundary")
    map=hideGroup(map, "Assessment Units")
    map=hideGroup(map, "Watershed Units")
    map=hideGroup(map, "HUC12 Boundaries")
  })
  
  # Create reactive dataset following map filters
  observe({
    req(input$ptype, input$ws)
    if("All"%in%input$ws){
      ws = unique(data$Mgmt_Unit)
    }else{ws = input$ws}
    if("All"%in%input$ptype){
      ptype = unique(data$Project.Type)
    }else{ptype = input$ptype}
    pdat = subset(data, data$Project.Type%in%ptype&data$Mgmt_Unit%in%ws)
    pdat = subset(pdat, pdat$Year.Awarded>=input$date[1]&pdat$Year.Awarded<=input$date[2])
    pdat = subset(pdat, pdat$Funding.Source %in% c(input$funding))
    bounds = c(max(pdat$Project.Longitude), max(pdat$Project.Latitude), min(pdat$Project.Longitude), min(pdat$Project.Latitude))

    appdata$pdat = pdat
    appdata$bounds = bounds
    appdata$sel_projs = vector()
  })
  
  proxy = leafletProxy("projmap")
  
  # Change map according to reactive dataset built from filters
  observe({
    req(appdata$pdat)
    proxy = clearMarkers(proxy)
    pdat = appdata$pdat
    labs <- lapply(seq(nrow(pdat)), function(i) {
      paste0("Name: ",pdat[i,"Project.Title"],"<br>",
             "Year Awarded: ",lubridate::year(pdat[i, "Year.Awarded"]),"<br>",
             "Amount Awarded: $",pdat[i, "Amount.Awarded"],"<br>",
             "Basin: ", pdat[i, "Mgmt_Unit"], "<br>",
             "Assessment Unit: ", pdat[i, "AU_NAME"])
    })
    proxy%>%addCircleMarkers(data = pdat, layerId = pdat$Project.Title, lat = pdat$Project.Latitude, lng = pdat$Project.Longitude,
                             color = "black", weight = 3, opacity = 0.6, fillColor = type_pal(pdat$Project.Type), fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                             group = "ProjSites", options = pathOptions(pane = "proj_markers"))%>%
      flyToBounds(lng1 = appdata$bounds[1], lat1 = appdata$bounds[2], lng2 = appdata$bounds[3], lat2 = appdata$bounds[4], leafletOptions(maxZoom = 10))%>%
      removeControl(layerId = "legend")%>%
      addLegend("bottomright", pal = type_pal, values = data$Project.Type,layerId = "legend",
                title = "Project Category",
                opacity = 0.6)
  })
  
  #### ADD SEARCH BAR
  
  # Map marker click (to identify selected sites will also select 2 sites w/ identical (round(lat/long, 4) but different MLIDs
  observeEvent(input$projmap_marker_click, {
    site_click <- input$projmap_marker_click
    siteid=site_click$id
    pdat = appdata$pdat
  if(!is.null(siteid)){
    lat = unique(pdat[pdat$Project.Title == siteid,'Project.Latitude'])
    lat = round(as.numeric(paste(lat),4))
    long = unique(pdat[pdat$Project.Title == siteid,'Project.Longitude'])
    long = round(as.numeric(paste(long),4))
    latlong_matches=pdat[round(pdat$Project.Latitude,4)==lat & round(pdat$Project.Longitude,4)==long,]
    if(dim(latlong_matches)[1] > 1){siteid = as.character(unique(latlong_matches$Project.Title))}

    if(!is.null(siteid)){
      if(any(siteid %in% appdata$sel_projs)){
        appdata$sel_projs=appdata$sel_projs[!appdata$sel_projs %in% siteid]
      }else{
        appdata$sel_projs=append(appdata$sel_projs, siteid)
      }
    }
  }
  })
  
  # Update map marker highlights
  observeEvent(appdata$sel_projs, ignoreNULL=F, {
    req(appdata$pdat)
    highdat = appdata$pdat[appdata$pdat$Project.Title %in% appdata$sel_projs,]
    proxy %>%
      clearGroup(group='highlight') %>%
      addCircleMarkers(data=highdat,lat = highdat$Project.Latitude, lng = highdat$Project.Longitude,
                       group='highlight', options = pathOptions(pane = "highlight"), radius = 20, color='chartreuse', opacity = 0.75, fillOpacity = 0.4)
  })

  # Reset filters and map, easier than deleting values from drop down
  observeEvent(input$resetsrch,{
    updateSliderInput(session, "date", "Date Range", min = min(data$Year.Awarded), max = max(data$Year.Awarded), value = c(min(data$Year.Awarded),max(data$Year.Awarded)), timeFormat = "%Y", step = 730)
    reset("rst")
    proxy = clearMarkers(proxy)
    proxy=leaflet::setView(proxy, lat=39.419220, lng=-111.950684, zoom = 6)
    appdata$pdat = NULL
    appdata$tabledata = NULL
  })
  
  observeEvent(input$selall, {
    req(appdata$pdat)
    appdata$sel_projs = appdata$pdat$Project.Title
  })
  
  observeEvent(input$clearsel,{
    appdata$sel_projs = NULL
    appdata$tabledata = NULL
  })
  
  observe({
    req(appdata$sel_projs)
    tabledata = subset(appdata$pdat, appdata$pdat$Project.Title%in%appdata$sel_projs)
    tabledata = tabledata[,!names(tabledata)%in%c("Watershed", "Project.Type_old", "Mgmt_Unit", "AU_NAME")]
    tabledata <<- tabledata
    appdata$tabledata = tabledata
    })
  
  output$projdata <- DT::renderDT(appdata$tabledata,
                                  colnames = c("Title","Year Awarded","Funding Source","Sponsor","Contact","Project Type","Amount Awarded","Latitude", "Longitude", "Final Report","BMPs","Notes"),
                                  rownames = FALSE,selection='none',filter="top",
                                  options = list(scrollY = '700px', paging = FALSE, scrollX='700px', dom = "t"))

}

# Run the application 
shinyApp(ui = ui, server = server)
