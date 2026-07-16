##This should detect and install missing packages before loading them####

list.of.packages <- c("shiny","ggmap", "excelR", 'rhandsontable', 'DT', 'tidycensus', 'tidyverse', 'dplyr', 'janitor', 'reshape2', 'tidyselect',
                      'promises', 'future', 'shinythemes', 'future', 'gridExtra')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
future::plan(multisession)

source('~/workspace/setup-acmt.R')
source('external_data-presets.R')
source('external_data-file_loader.R')
source('~/workspace/ACMT_shiny/summary_tables.R')
source('~/workspace/GeocoderACMT.R')
#file.exists('~/workspace/ACMT_shiny/data_pull_settings/ACMT_shiny-functions.R')
source('~/workspace/ACMT_shiny/data_pull_settings/ACMT_shiny-functions.R')
source('~/workspace/ACMT_shiny/data_pull_settings/ACMT_shiny_external_data_functions.R')
source('~/workspace/ACMT_shiny/data_pull_settings/cdc_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/acs_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/walk_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/mrfei_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/park_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/crimerisk_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/sidewalk_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/rpp_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/gentrification_data_settings.R')
source('~/workspace/ACMT_shiny/data_pull_settings/nlcd_data_settings.R')

library(dplyr)
library(magrittr)

data.preview.choices<-c('Show geocoded dataset', 'Show environmental measures data pull', 'Show missingness/count summary')

### SHINY UI ####
ui<-shinyUI(
  fluidPage(theme=shinytheme('cerulean'),
  
  tags$style("#shiny-notification-status_notif {position: fixed; top: 45%; left: 50%; width: 15em; opacity: 1;}"),
  tags$style("#shiny-notification-process_notif {position: fixed; top: 50%;  left: 50%; width: 15em; opacity: 1;}"),
  tags$style("#shiny-notification-stop_message {position: fixed; top: 55%;  left: 50%; width: 15em; opacity: 1;}"),
  
  navbarPage(title='Environmental Measures for ACMT_shiny',
### INTRODUCTION ####
   tabPanel(title='INTRODUCTION', 
          h1('Introduction'),
          fluidRow(style='padding-left:50px; padding-right:50px', 
          p('The ACMT can be used to gather data on the social and built environment surrounding a specific address or lat/lon point. 
          :'),
          uiOutput('measureList'),   
          p('Note that while most of the datasets only have one year of data available, 
               you will need to choose specific years of data for the ACS data, the CDC PLACES data, and the NLCD data.'),
            )
          ),
          
### DATA UPLOAD & GEOCODING ####
  tabPanel('UPLOAD & GEOCODE', 

           navlistPanel(widths=c(3, 9),
                        'Data Upload Process',
                        
                        tabPanel('Upload Dataset',
                                 ##upload instructions ##
                                 
                                 fluidRow(style = 'padding-left:30px',
                                   h4(em('1. Select the type of dataset you will upload:')), 
                                   radioButtons('upload_dataset', 'Data upload type', 
                                                 choices=c('Participant id & address (no geocodes)', 'Geocoded participant data')), 
                                 ),
                                 fluidRow(style = 'padding-left:30px',
                                   h4(em('2. Check your file with example dataset below')),
                                   div(tableOutput('example_table_preview'), style = "padding: 10px; font-size: 95%; width: 70%")),
                                 fluidRow(style = 'padding-left:30px',
                                      h4(em('3. Save the file as a .csv file'))),
                                 fluidRow(style = 'padding-left:30px',
                                      h4(em('4. Upload file below')),
                                          #1 Selector for file upload ##
                                      column(width=8, 
                                             wellPanel(
                                         strong('Upload datafile.'), 
                                         em('Note you will receive an error if the dataset is formatted incorrectly'),
                                          fileInput('datafile', 'Choose CSV file',
                                                      accept=c('text/csv', 'text/comma-separated-values,text/plain')), 
                                          ))
                                   )
                               ),
                                
                        tabPanel('Preview Uploaded Data', 
                                  
                                 fluidRow(style='padding-left:60px',
                                   column(4, offset=2,
                                          h4(em('Preview of the uploaded dataset:')),
                                          div(DT::dataTableOutput('filetable_preview'), style = "padding: 10px; font-size: 95%; width: 250%"))
                                 )
                        ), 
                        tabPanel('Geocode Data', 
                                 fluidRow(style = 'padding-left:30px',
                                   h4(em('If your data has not yet been geocoded, click the button below to run the geocoding function')),
                                   ),
                                 fluidRow(style='padding-left:30px', 
                                          actionButton('geocodeButton', 'Geocode dataset'))
                                 # button to combine/rename address fields into one columns (may add this for a later iteration)
                        ),
                        ### Check ratings ####
                        tabPanel(
                          'Check Geocode Ratings',
                          fluidRow(style='padding-left:30px',
                                   h4(em('If you used the ACMT geocoder, you can verify the geocodes and update any address errors to improve geocodes.'))
                                   ),
                          fluidRow(style='padding-left:30px',
                                   column(width=4,
                                          h4("1. Load/Refresh data:"),
                                          tags$ul(
                                            tags$li("Click button to upload geocoded data or to refresh after updating dataset")
                                          ),
                                          div(actionButton('refreshgeocode', 'Load/refresh geocoded data'), style='display:center-align')
                                   ),
                                   column(width=4,
                                          wellPanel(
                                            h4("2. Select participant ID to map"),
                                            selectInput('idNumber', 'Select ID', "")
                                          )
                                   ),
                                   column(width=4,
                                          h4("3. Click to generate a map for the selected ID: "),
                                          br(),
                                          div(actionButton('mapButton', 'Generate map')), 
                                   )
                          ),
                          
                          conditionalPanel(
                            condition="input.idNumber !=''", 
                            fluidRow(
                              column(width=12,
                                     DT::dataTableOutput('dataset_geocoded2')
                              )
                            )
                          ),
                          br(),
                          
                          conditionalPanel(
                            condition="input.mapButton>0 & input.idNumber != 'Select a single id to map'",
                            fluidRow(
                              column(width=4,
                                     tags$h4(strong("For ratings >0, follow these steps to check and update:")),
                                     wellPanel(
                                       style='background-color: #34495E; color:white; font-size:16px',
                                       tags$h4(strong("4. Check map:")),
                                       tags$ul(
                                         tags$li(" Mapped to correct street?"),
                                         tags$li(" Mapped to the correct city?"),
                                         tags$li(" Any obvious errors? (i.e., mapped into water, mapped into area with no houses)")), 
                                         em("if it is mapping incorrectly, start by cleaning up the address if possible (see instructions below)")
                                       ),
                                     wellPanel(
                                       style='background-color: #34495E; color:white; font-size:16px',
                                       h4(strong("5. Update address:")), 
                                       tags$ul(
                                         tags$li("Double click address cell to update address text"),
                                         tags$li("After updating, click", strong("'Load geocoded data'"), "to refresh data"),
                                         tags$li("Click", strong("'Generate map'"), "to update geocodes and re-map")
                                       ),
                                        em("If the address is correctly written, but geocode is mapping incorrectly, you can manually update the location (see instructions below the map)")
                                     ),
                                     wellPanel(
                                       style='background-color: #fff; font-size:16px',
                                       radioButtons('geocode_check', "6. Update geocoding notes (only necessary when you can't get a rating to 0)", choices=c('Geocode looks accurate (correct street/city)', 'Geocode does not look accurate')),
                                       actionButton('geocode_notes', 'Update Notes'),
                                     )
                              ), 
                              column(width=8,
                                     #shiny::plotOutput("map", inline=TRUE)
                                     leafletOutput('map', width='700px', height='700px'),
                                     wellPanel(textOutput('maplabel'), width=12), 
                                     wellPanel(
                                       style='background-color: #34495E; color:white; font-size:16px',
                                       tags$h3("Manually update address location:"),
                                       tags$li("You can click on the correct location on the map to update the geocode 
                                                (you should be positive that this is the correct location before clicking)"),
                                       tags$li("Once you have clicked on the map, click the 'Update geocode' button below to confirm the udpates,
                                               then reload your data and latitude and longitude values will reflect the location that you clicked on."),
                                       tableOutput('updated_geocode'),
                                       actionButton('update_geocode', 'Update geocode')
                                     )
                              )
                            ), 
                          ),
                          class="p-3 border border-top- rounded-bottom"
                        )
           ) #end of navList
           
           ),
### Walkability Index UI ####
  tabPanel('WALKABILITY', 

      sidebarPanel(
        h2(strong('Instructions')),
           ## Step 1 ##
           fluidRow(style = 'padding-left:30px', 
                    h3('1. Load geocoded dataset'),
                    p(em('Click the button to load your geocoded dataset')),
           ),
           fluidRow(style = 'padding:5px',
                    column(width=3,
                           ## load geocoded dataset button
                           div(style = 'padding-left: 10px', actionButton('loaddata_walk', 'Load Geocoded data'))
                    )
           ),
           ## Step 2 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('2. Set years of interest for data pull '),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectyear_walk', 'Select years', choices=walk_years, selected=walk_selected_years, inline=TRUE)),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectradii_walk', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
           ),
           ## Step 3 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('3. Run loop to pull Walkability variables'), 
                    ##button to run loop to pull data
                    div(style='padding:10px', actionButton('pull_walk', 'Pull data')),
                    div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_walk_message')),
                         ),
        width=3,style='min-width:200px',
        ),
      mainPanel(
        
        h1('Walkability Data'),
        
        fluidRow(style='padding-left: 30px',
                 textOutput('walk_description')
                 ), 
        
           ## Step 4 ##
           conditionalPanel(condition='input.loaddata_walk!=0',
        fluidRow(style = 'padding-left:30px',
                    h3('Preview data'),
           ),
           ##print head of processed data
          fluidRow(
            column(width=4, 
            ##button to get status of data pull
             div(style='padding:10px', actionButton('status_walk', 'Show PROGRESS', stype='color: #0E6655'))
             ),
          column(width=4, 
             ##button to pause the loop
             div(style='padding:10px', actionButton('stop_walk', 'STOP data pull', style='color:#AD2222'))
          ),
          ),
        p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
          fluidRow(
                 div(style='padding:5px', selectInput('show_data_walk', 'Data Preview', choices=data.preview.choices, width='300px')),
               div(DT::dataTableOutput('dataset_walk'), style = "padding: 10px; font-size: 95%; width: 100%"), 
          )
      )
      ),
  ),
### CDC PLACES Index UI ####
tabPanel('PLACES', 
         
         sidebarPanel(
           h2(strong('Instructions')),
           ## Step 1 ##
           fluidRow(style = 'padding-left:30px', 
                    h3('1. Load geocoded dataset'),
                    p(em('Click the button to load your geocoded dataset')),
           ),
           fluidRow(style = 'padding:5px',
                    column(width=3,
                           ## load geocoded dataset button
                           div(style = 'padding-left: 10px', actionButton('loaddata_cdc', 'Load Geocoded data'))
                    )
           ),
           ## Step 2 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('2. Set years of interest for data pull '),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectyear_cdc', 'Select years', choices=cdc_years, selected=cdc_selected_years, inline=TRUE)),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectradii_cdc', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
           ),
           ## Step 3 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('3. Run loop to pull CDC PLACES variables'), 
                    ##button to run loop to pull data
                    div(style='padding:10px', actionButton('pull_cdc', 'Pull data')),
                    div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_cdc_message')),
           ),
           width=3,style='min-width:200px',
         ),
         mainPanel(
           
           h1('CDC PLACES Data'),
           
           fluidRow(style='padding-left: 30px',
                    textOutput('cdc_description')
           ), 
           
           ## Step 4 ##
           conditionalPanel(condition='input.loaddata_cdc!=0',
                            fluidRow(style = 'padding-left:30px',
                                     h3('Preview data'),
                            ),
                            ##print head of processed data
                            fluidRow(
                              column(width=4, 
                                     ##button to get status of data pull
                                     div(style='padding:10px', actionButton('status_cdc', 'Show PROGRESS', stype='color: #0E6655'))
                              ),
                              column(width=4, 
                                     ##button to pause the loop
                                     div(style='padding:10px', actionButton('stop_cdc', 'STOP data pull', style='color:#AD2222'))
                              ),
                            ),
                            p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                            fluidRow(
                              div(style='padding:5px', selectInput('show_data_cdc', 'Data Preview', choices=data.preview.choices, width='300px')),
                              div(DT::dataTableOutput('dataset_cdc'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                            )
           )
         ),
),
### NLCD UI ####
  tabPanel('NLCD',
           
                  sidebarPanel(
                      h2(strong('Instructions')),
                      ## Step 1 ##
                      fluidRow(style = 'padding-left:30px', 
                               h3('1. Load geocoded dataset'),
                               p(em('Click the button to load your geocoded dataset')),
                      ),
                      fluidRow(style = 'padding:5px',
                               column(width=3,
                                      ## load geocoded dataset button
                                      div(style = 'padding-left: 10px', actionButton('loaddata_nlcd', 'Load Geocoded data'))
                               )
                      ),
                      ## Step 2 ##
                      fluidRow(style = 'padding:30px; padding-top:0px',
                               h3('2. Set years of interest for data pull '),
                               tags$div(style='padding-left:30px', align='left', class='multicol', 
                                        checkboxGroupInput('selectyear_nlcd', 'Select years', choices=nlcd_years, selected=nlcd_selected_years, inline=TRUE)),
                               tags$div(style='padding-left:30px', align='left', class='multicol', 
                                        checkboxGroupInput('selectradii_nlcd', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
                      ),
                      ## Step 3 ##
                      fluidRow(style = 'padding:30px; padding-top:0px',
                               h3('3. Run loop to pull NLCD variables'), 
                               ##button to run loop to pull data
                               div(style='padding:10px', actionButton('pull_nlcd', 'Pull data')),
                               div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_nlcd_message')),
                      ),
                      width=3,style='min-width:200px',
                    ),
                    mainPanel(
                      
                      h1('National Land Cover Database Data'),
                      
                      fluidRow(style='padding-left: 30px',
                               textOutput('nlcd_description')
                      ), 
                      
                      ## Step 4 ##
                      conditionalPanel(condition='input.loaddata_nlcd!=0',
                                       fluidRow(style = 'padding-left:30px',
                                                h3('Preview data'),
                                       ),
                                       ##print head of processed data
                                       fluidRow(
                                         column(width=4, 
                                                ##button to get status of data pull
                                                div(style='padding:10px', actionButton('status_nlcd', 'Show PROGRESS', stype='color: #0E6655'))
                                         ),
                                         column(width=4, 
                                                ##button to pause the loop
                                                div(style='padding:10px', actionButton('stop_nlcd', 'STOP data pull', style='color:#AD2222'))
                                         ),
                                       ),
                                       p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                                       fluidRow(
                                         div(style='padding:5px', selectInput('show_data_nlcd', 'Data Preview', choices=data.preview.choices, width='300px')),
                                         div(DT::dataTableOutput('dataset_nlcd'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                                       )
                      )
                    ),
           ),
### MRFEI UI ####
tabPanel('MRFEI', 
         
         sidebarPanel(
           h2(strong('Instructions')),
           ## Step 1 ##
           fluidRow(style = 'padding-left:30px', 
                    h3('1. Load geocoded dataset'),
                    p(em('Click the button to load your geocoded dataset')),
           ),
           fluidRow(style = 'padding:5px',
                    column(width=3,
                           ## load geocoded dataset button
                           div(style = 'padding-left: 10px', actionButton('loaddata_mrfei', 'Load Geocoded data'))
                    )
           ),
           ## Step 2 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('2. Set years of interest for data pull '),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectyear_mrfei', 'Select years', choices=mrfei_years, selected=mrfei_selected_years, inline=TRUE)),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectradii_mrfei', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
           ),
           ## Step 3 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('3. Run loop to pull Modified Retail Food Environment Index variables'), 
                    ##button to run loop to pull data
                    div(style='padding:10px', actionButton('pull_mrfei', 'Pull data')),
                    div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_mrfei_message')),
           ),
           width=3,style='min-width:200px',
         ),
         mainPanel(
           
           h1('Modified Retail Food Environment Index Data'),
           
           fluidRow(style='padding-left: 30px',
                    textOutput('mrfei_description')
           ), 
           
           ## Step 4 ##
           conditionalPanel(condition='input.loaddata_mrfei!=0',
                            fluidRow(style = 'padding-left:30px',
                                     h3('Preview data'),
                            ),
                            ##print head of processed data
                            fluidRow(
                              column(width=4, 
                                     ##button to get status of data pull
                                     div(style='padding:10px', actionButton('status_mrfei', 'Show PROGRESS', stype='color: #0E6655'))
                              ),
                              column(width=4, 
                                     ##button to pause the loop
                                     div(style='padding:10px', actionButton('stop_mrfei', 'STOP data pull', style='color:#AD2222'))
                              ),
                            ),
                            p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                            fluidRow(
                              div(style='padding:5px', selectInput('show_data_mrfei', 'Data Preview', choices=data.preview.choices, width='300px')),
                              div(DT::dataTableOutput('dataset_mrfei'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                            )
           )
         ),
),
### PARKSERVE UI ####
  tabPanel("PARKSERVE", 
           sidebarPanel(
             h2(strong('Instructions')),
             ## Step 1 ##
             fluidRow(style = 'padding-left:30px', 
                      h3('1. Load geocoded dataset'),
                      p(em('Click the button to load your geocoded dataset')),
             ),
             fluidRow(style = 'padding:5px',
                      column(width=3,
                             ## load geocoded dataset button
                             div(style = 'padding-left: 10px', actionButton('loaddata_parks', 'Load Geocoded data'))
                      )
             ),
             ## Step 2 ##
             fluidRow(style = 'padding:30px; padding-top:0px',
                      h3('2. Set years of interest for data pull '),
                      tags$div(style='padding-left:30px', align='left', class='multicol', 
                               checkboxGroupInput('selectyear_parks', 'Select years', choices=parks_years, selected=parks_selected_years, inline=TRUE)),
                      tags$div(style='padding-left:30px', align='left', class='multicol', 
                               checkboxGroupInput('selectradii_parks', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
             ),
             ## Step 3 ##
             fluidRow(style = 'padding:30px; padding-top:0px',
                      h3('3. Run loop to pull ParkServe variables'), 
                      ##button to run loop to pull data
                      em('The first time you pull the ParkServe data, the Parkserve dataset will need to download and process, which will take about 20 minutes, 
                         before data is pulled for your dataset'),
                      div(style='padding:10px', actionButton('pull_parks', 'Pull data')),
                      div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_parks_message')),
             ),
             width=3,style='min-width:200px',
           ),
           mainPanel(
             
             h1('PARKSERVE Data'),
             
             fluidRow(style='padding-left: 30px',
                      textOutput('parks_description')
             ), 
             
             ## Step 4 ##
             conditionalPanel(condition='input.loaddata_parks!=0',
                              fluidRow(style = 'padding-left:30px',
                                       h3('Preview data'),
                              ),
                              ##print head of processed data
                              fluidRow(
                                column(width=4, 
                                       ##button to get status of data pull
                                       div(style='padding:10px', actionButton('status_parks', 'Show PROGRESS', stype='color: #0E6655'))
                                ),
                                column(width=4, 
                                       ##button to pause the loop
                                       div(style='padding:10px', actionButton('stop_parks', 'STOP data pull', style='color:#AD2222'))
                                ),
                              ),
                              p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                              fluidRow(
                                div(style='padding:5px', selectInput('show_data_parks', 'Data Preview', choices=data.preview.choices, width='300px')),
                                div(DT::dataTableOutput('dataset_parks'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                              )
             )
           ),
  ), 

### American Community Survey Index UI ####
tabPanel('ACS', 
         
         sidebarPanel(
           h2(strong('Instructions')),
           ## Step 1 ##
           fluidRow(style = 'padding-left:30px', 
                    h3('1. Load geocoded dataset'),
                    p(em('Click the button to load your geocoded dataset')),
           ),
           fluidRow(style = 'padding:5px',
                    column(width=3,
                           ## load geocoded dataset button
                           div(style = 'padding-left: 10px', actionButton('loaddata_acs', 'Load Geocoded data'))
                    )
           ),
           ## Step 2 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('2. Set years of interest for data pull '),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectyear_acs', 'Select years', choices=acs_years, selected=acs_selected_years, inline=TRUE)),
                    tags$div(style='padding-left:30px', align='left', class='multicol', 
                             checkboxGroupInput('selectradii_acs', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
           ),
           ## Step 3 ##
           fluidRow(style = 'padding:30px; padding-top:0px',
                    h3('3. Run loop to pull American Community Survey variables'), 
                    ##button to run loop to pull data
                    div(style='padding:10px', actionButton('pull_acs', 'Pull data')),
                    div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_acs_message')),
           ),
           ## Step 4 ##
           fluidRow(style=  'padding:30px; padding-top:0px',
                    h3('4. Pull County GEOIDs for each participant'),
                    div(style='padding:10px', actionButton('pull_county_GEOID', 'Pull GEOIDs for county'))
           ),
           width=3,style='min-width:200px',
         ),
         mainPanel(
           
           h1('American Community Survey Data'),
           
           fluidRow(style='padding-left: 30px',
                    textOutput('acs_description'),
           ), 
           
           ## Step 4 ##
           conditionalPanel(condition='input.loaddata_acs!=0',
                            fluidRow(style = 'padding-left:30px',
                                     h3('Preview data'),
                            ),
                            ##print head of processed data
                            fluidRow(
                              column(width=4, 
                                     ##button to get status of data pull
                                     div(style='padding:10px', actionButton('status_acs', 'Show PROGRESS', stype='color: #0E6655'))
                              ),
                              column(width=4, 
                                     ##button to pause the loop
                                     div(style='padding:10px', actionButton('stop_acs', 'STOP data pull', style='color:#AD2222'))
                              ),
                            ),
                            p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                            fluidRow(
                              div(style='padding:5px', selectInput('show_data_acs', 'Data Preview', choices=data.preview.choices, width='300px')),
                              div(DT::dataTableOutput('dataset_acs'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                            )
           )
         ),
),

### RPP UI ####
  tabPanel('RPP', 
           
           sidebarPanel(
             h2(strong('Instructions')),
             ## Step 1 ##
             fluidRow(style = 'padding-left:30px', 
                      h3('1. Load geocoded dataset'),
                      p(em('Click the button to load your geocoded dataset')),
             ),
             fluidRow(style = 'padding:5px',
                      column(width=3,
                             ## load geocoded dataset button
                             div(style = 'padding-left: 10px', actionButton('loaddata_rpp', 'Load Geocoded data'))
                      )
             ),
             ## Step 2 ##
             fluidRow(style = 'padding:30px; padding-top:0px',
                      h3('2. Set years of interest for data pull '),
                      tags$div(style='padding-left:30px', align='left', class='multicol', 
                               checkboxGroupInput('selectyear_rpp', 'Select years', choices=rpp_years, selected=rpp_selected_years, inline=TRUE)),
                      tags$div(style='padding-left:30px', align='left', class='multicol', 
                               checkboxGroupInput('selectradii_rpp', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
             ),
             ## Step 3 ##
             fluidRow(style = 'padding:30px; padding-top:0px',
                      h3('3. Run loop to pull RPP variables'), 
                      ##button to run loop to pull data
                      div(style='padding:10px', actionButton('pull_rpp', 'Pull data')),
                      div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_rpp_message')),
             ),
             width=3,style='min-width:200px',
           ),
           mainPanel(
             
             h1('Regional Price Parity Data'),
             
             fluidRow(style='padding-left: 30px',
                      textOutput('rpp_description')
             ), 
             
             ## Step 4 ##
             conditionalPanel(condition='input.loaddata_rpp!=0',
                              fluidRow(style = 'padding-left:30px',
                                       h3('Preview data'),
                              ),
                              ##print head of processed data
                              fluidRow(
                                column(width=4, 
                                       ##button to get status of data pull
                                       div(style='padding:10px', actionButton('status_rpp', 'Show PROGRESS', stype='color: #0E6655'))
                                ),
                                column(width=4, 
                                       ##button to pause the loop
                                       div(style='padding:10px', actionButton('stop_rpp', 'STOP data pull', style='color:#AD2222'))
                                ),
                              ),
                              p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                              fluidRow(
                                div(style='padding:5px', selectInput('show_data_rpp', 'Data Preview', choices=data.preview.choices, width='300px')),
                                div(DT::dataTableOutput('dataset_rpp'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                              )
             )
           ),
  ),
### Gentrification UI ####
  tabPanel('GENTRIFICATION',

sidebarPanel(
  h2(strong('Instructions')),
  ## Step 1 ##
  fluidRow(style = 'padding-left:30px', 
           h3('1. Load geocoded dataset'),
           p(em('Click the button to load your geocoded dataset')),
  ),
  fluidRow(style = 'padding:5px',
           column(width=3,
                  ## load geocoded dataset button
                  div(style = 'padding-left: 10px', actionButton('loaddata_gentrification', 'Load Geocoded data'))
           )
  ),
  ## Step 2 ##
  fluidRow(style = 'padding:30px; padding-top:0px',
           h3('2. Set years of interest for data pull '),
           tags$div(style='padding-left:30px', align='left', class='multicol', 
                    checkboxGroupInput('selectyear_gentrification', 'Select years', choices=gentrification_years, selected=gentrification_selected_years, inline=TRUE)),
           tags$div(style='padding-left:30px', align='left', class='multicol', 
                    checkboxGroupInput('selectradii_gentrification', 'Select radii', choices=c(500, 1000, 5000), selected=c(500, 1000, 5000), inline=TRUE)),
  ),
  ## Step 3 ##
  fluidRow(style = 'padding:30px; padding-top:0px',
           h3('3. Run loop to pull Gentrification variables'), 
           ##button to run loop to pull data
           div(style='padding:10px', actionButton('pull_gentrification', 'Pull data')),
           div(style='padding-left: 10px; font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_gentrification_message')),
  ),
  width=3,style='min-width:200px',
),
mainPanel(
  
  h1('GENTRIFICATION Data'),
  
  fluidRow(style='padding-left: 30px',
           textOutput('gentrification_description')
  ), 
  
  ## Step 4 ##
  conditionalPanel(condition='input.loaddata_gentrification!=0',
                   fluidRow(style = 'padding-left:30px',
                            h3('Preview data'),
                   ),
                   ##print head of processed data
                   fluidRow(
                     column(width=4, 
                            ##button to get status of data pull
                            div(style='padding:10px', actionButton('status_gentrification', 'Show PROGRESS', stype='color: #0E6655'))
                     ),
                     column(width=4, 
                            ##button to pause the loop
                            div(style='padding:10px', actionButton('stop_gentrification', 'STOP data pull', style='color:#AD2222'))
                     ),
                   ),
                   p(em('note: this process will take a while - if you need to stop the process to shut down your computer, press the STOP data pull button. 
                         Interrupting may take a few minutes as the process finishes the step it is currently on.')), 
                   fluidRow(
                     div(style='padding:5px', selectInput('show_data_gentrification', 'Data Preview', choices=data.preview.choices, width='300px')),
                     div(DT::dataTableOutput('dataset_gentrification'), style = "padding: 10px; font-size: 95%; width: 100%"), 
                   )
  )
)
  ), 
#### Data Pull Progress ####
    tabPanel('Overall Progress',
     h2('Current Data Pull Progress Summary'), 
     em('The table below shows your current progress for each dataset'),
     fluidRow(
       column(width=2,
       actionButton('refreshprogress', 'Refresh Progress Summary')),
       column(width=3,
       actionButton('create_report', 'Create final summary report', 
                          style="color: #fff; background: #059501"))
     ),
     fluidRow(
       tags$div(style='padding:30px;padding-top:0px;font-size:85%; width:40%', uiOutput("tables"))     )
     
    )
)
))




## SERVER actions ####
server<-function(input, output, session) {

## introduction functions ####
output$measureList<-renderUI(HTML(markdown::renderMarkdown(text = paste(paste0("- ", measures.list, "\n"), collapse = ""))))

## Show example data table
example_table<-reactiveValues(data=example_address)

observeEvent(input$upload_dataset, {
  if(input$upload_dataset == 'Participant id & address (no geocodes)'){
     example_table$data <-example_address}
  if(input$upload_dataset == 'Geocoded participant data'){
    example_table$data<-example_geocoded
  }
})

output$example_table_preview<-renderTable({example_table$data})

## Upload data file ###
filedata <- reactive({
  req(input$datafile)
  tryCatch({
    infile <- input$datafile
    #if (is.null(infile)) {
    # User has not uploaded a file yet
    #  return(NULL)
    #}
    read.csv(infile$datapath)
    
  }, error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error',id = 'status_notif', duration=NULL)
    return()}
  )
})

filetable<-reactiveValues(data=data.frame(Message='Upload data to preview'))

# save data after uploading
observeEvent(input$datafile, {
  if(input$upload_dataset == 'Participant id & address (no geocodes)'){
    tryCatch({data<-filedata() %>% dplyr::select(id, address)
    }, 
             error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error', id='status_notif', duration=NULL)
               return()}
    )
    saveData(data=data, fileName='dataset_address.csv')
    filetable$data<-data
    saveData(data=data, fileName='dataset_address.csv')}
  if(input$upload_dataset == 'Geocoded participant data'){
    tryCatch({data<-filedata() %>% dplyr::select(id, lat, long)%>%mutate(address=NA)
    },
             error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error', id='status_notif', duration=NULL)
               return()}
    )
    saveData(data=data, fileName='dataset_geocoded.csv')
    filetable$data<-data}
})

#This previews the CSV data file or geocoded datafile ##
output$filetable_preview<- DT::renderDataTable({filetable$data}, editable=FALSE, 
                                               rownames=FALSE,
                                               options = list(
                                                 searching = FALSE,
                                                 pageLength = 10,
                                                 autowidth=FALSE,
                                                 scrollX=TRUE
                                               ))

##  Geocoded dataset ####

## function to geocode uploaded dataset  
geocode_data<-reactive({
  raw_data<-read.csv('~/workspace/ACMT_shiny/dataset_address.csv')
  filetable$data<-withProgress(geocode_loop(raw_data), message='Geocoding dataset')
})    

# save data after geocoding
observeEvent(input$geocodeButton, {
  data<-geocode_data()
  showNotification('Geocoding Complete', id='status_notif', duration=NULL)
  data<-data %>% mutate(geocode_notes='')
  saveData(data=data, fileName='dataset_geocoded.csv')}
)

##upload geocoded dataset from ACMT_shiny folder ##
upload_geocoded_data<-eventReactive(
  input$refreshgeocode,
  {loadData('dataset_geocoded.csv')%>%dplyr::select(-any_of('X'))%>%
      mutate(address=as.character(address)) }
  #id<-input$idNumber
  #if(id=='Select a single id to map'){geocoded_data}
  #else{geocoded_data[geocoded_data$id==id,]}
)

##Filtered version of geocoded dataset
filter_uploaded_data<-reactive({if(input$idNumber=='Select a single id to map' | input$idNumber=='' | is.null(input$idNumber)){upload_geocoded_data()}
  else{upload_geocoded_data()[upload_geocoded_data()$id==input$idNumber,]}
})

## page 2 -- check ratings and maps for ratings > 10 
output$dataset_geocoded2<-DT::renderDataTable(
  {filter_uploaded_data()},
  editable=TRUE, 
  options = list(
    rownames=FALSE,
    paging = TRUE,
    searching = TRUE,
    fixedColumns = FALSE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'Bfrtip',
    buttons = c('csv', 'excel')
  ), 
  class='display'
)


##### Update Table values ##
observeEvent(input$dataset_geocoded2_cell_edit, {
  dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(address=as.character(address), geocode_notes=as.character(geocode_notes))
  str(input$dataset_geocoded2_cell_edit)
  value<-input$dataset_geocoded2_cell_edit$value
  column<-input$dataset_geocoded2_cell_edit$col
  if(input$idNumber=='Select a single id to map'){
    row<-input$dataset_geocoded2_cell_edit$row
    id<-filter_uploaded_data()$id[row]
  }
  else{id<-input$idNumber
  #print(row_number(filter_uploaded_data()[filter_uploaded_data()$id==id]))
  #   row<-row_number(filter_uploaded_data()[filter_uploaded_data()$id==id])
  }
  dataset_geocoded[dataset_geocoded$id==id,][column]<-value
  
  ## re-geocode ##
  if(colnames(filter_uploaded_data()[column])=='address'){
    
 new_lat_long<-tryCatch({geocode(value)}, 
                 error=function(x){
                  list(latitude=NA, longitude=NA, rating=NA) ##imput NA value if geocoding is unsuccessful
                                          })
    dataset_geocoded$lat[dataset_geocoded$id==id]<-new_lat_long$lat
    dataset_geocoded$long[dataset_geocoded$id==id]<-new_lat_long$long
    dataset_geocoded$rating[dataset_geocoded$id==id]<-new_lat_long$rating
  }
  saveData(data=dataset_geocoded, fileName='dataset_geocoded.csv')
  write.csv(dataset_geocoded%>%dplyr::select(id, rating, geocode_notes), '~/workspace/ACMT_shiny/data_pull_measures/geocode_ratings_notes.csv', row.names=FALSE)
  filter_uploaded_data()
})

#update Selector input with participant IDs ##
observe({
  if(input$refreshgeocode==0)
    return()
  isolate({
    #if(input$refreshgeocode>0){
    updateSelectInput(session, 'idNumber', 
                      label='Choose ID for mapping', 
                      choices=c('Select a single id to map', upload_geocoded_data()$id), 
                      selected=c('Select a single id to map')) 
    
  })
  # else{updateSelectInput(session, 'idNumber', 
  #                         label='Choose id for mapping', 
  #                        choices=c('Select a single id to map', filedata()$id), 
  #                       selected=c('Select a single id to map'))  }
})


#Function to create plot ##
vals <- reactiveValues(id = 1, lat=NULL, long=NULL, address=NULL, rating=NULL, z = 15, side_len = .007)

## Generate map button
observeEvent(list(input$mapButton, input$idNumber), {
  vals$id<-input$idNumber
  vals$z<-input$z
  vals$side_len<-input$side_len
  vals$lat<-filter_uploaded_data()$lat[filter_uploaded_data()$id==input$idNumber]
  vals$long<-filter_uploaded_data()$long[filter_uploaded_data()$id==input$idNumber]
  vals$address<-filter_uploaded_data()$address[filter_uploaded_data()$id==input$idNumber]
  vals$rating<-filter_uploaded_data()$rating[filter_uploaded_data()$id==input$idNumber]
  
})

mapdata<-eventReactive(list(input$mapButton, input$idNumber),{
  if(input$idNumber=='Select a single id to map' | input$idNumber=='' | is.null(vals$address)){
    showNotification('Select 1 ID number to')
    return()
  }
  else{
    #check_geocode_for_address(address=vals$address, id=vals$id, z=vals$z, side_len=vals$side_len)
    check_geocode(lat=vals$lat, long=vals$long, address=vals$address, rate=vals$rating, id=vals$id, z=vals$z, side_len=vals$side_len)
    #check_geocode_for_address(address=filter_uploaded_data()$address[filter_uploaded_data()$id==input$idNumber], 
    #                          id=input$idNumber, side_len=input$side_len, z=input$z)
  }
})

#output$map<-renderPlot({
output$map<-renderLeaflet({
  withProgress(mapdata(), message = 'Mapping address for selected id')
})

output$maplabel<-renderText({
  if(input$mapButton==0){''}
  if(input$mapButton>0){
    paste(paste('ID = ', vals$id, sep=''), paste('address = ', vals$address, sep=''), paste("Rating =", vals$rating, sep = ""), sep=' | ')
  }
})

## Reactive to handle user inputs (update geocode by clicking on map)
proxy_geocode<-reactiveValues(data=data.frame(Message='Click on map to update latitude and longitude'))

output$updated_geocode<-renderTable({proxy_geocode$data})

#output$updated_geocode<- DT::renderDataTable({proxy_geocode$data}, editable=FALSE, 
#                                               rownames=FALSE,
#                                               options = list(
#                                                 searching = FALSE,
#                                                 pageLength = 11,
#                                                 autowidth=FALSE,
#                                                 scrollX=FALSE
#                                               ))

observeEvent(input$map_click, {
  click<-input$map_click
  leafletProxy('map')%>%
    clearMarkers()%>%
    addCircleMarkers(lng=click$lng, lat=click$lat)
  dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(geocode_notes=as.character(geocode_notes))
  id<-input$idNumber
  showNotification('Click the "Update Geocode" button to update the latitude and longitude to the clicked location', type='warning', id='status_notif')
  proxy_geocode$data$Message='Lat/Long for clicked location:'
  proxy_geocode$data$lat<-round(as.numeric(click$lat), 4)
  proxy_geocode$data$long<-round(as.numeric(click$lng), 4)
})  
  
observeEvent(input$update_geocode, {
  dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(geocode_notes=as.character(geocode_notes))
  id<-input$idNumber
  #calculate distance between old and new points on first udpate:
  if(!('old_lat' %in% colnames(dataset_geocoded))){
    dataset_geocoded$old_lat<-NA
    dataset_geocoded$old_long<-NA
    dataset_geocoded$distance_old_new<-NA
  }
  if(is.na(dataset_geocoded$old_lat[dataset_geocoded$id==id])){
        dist<-round(distm(c( dataset_geocoded$long[dataset_geocoded$id==id],dataset_geocoded$lat[dataset_geocoded$id==id]),
              c(proxy_geocode$data$long, proxy_geocode$data$lat), fun = distHaversine), 2)
  #save distance, and old lat/long values
  dataset_geocoded$distance_old_new[dataset_geocoded$id==id]<-dist
  dataset_geocoded$old_lat[dataset_geocoded$id==id]<-dataset_geocoded$lat[dataset_geocoded$id==id]
  dataset_geocoded$old_long[dataset_geocoded$id==id]<-dataset_geocoded$long[dataset_geocoded$id==id]
  }
  #re-calculate distance (don't rewrite old_lat or new_lat)
  if(!is.na(dataset_geocoded$old_lat[dataset_geocoded$id==id])){
    dist<-round(distm(c(dataset_geocoded$old_long[dataset_geocoded$id==id],dataset_geocoded$old_lat[dataset_geocoded$id==id]),
                      c(proxy_geocode$data$long, proxy_geocode$data$lat), fun = distHaversine), 2)
}
  #add new lat/long values to lat/long fields
  dataset_geocoded$lat[dataset_geocoded$id==id]<-proxy_geocode$data$lat
  dataset_geocoded$long[dataset_geocoded$id==id]<-proxy_geocode$data$long
  #dataset_geocoded$rating[dataset_geocoded$id==id]<-NA
  dataset_geocoded$distance_old_new[dataset_geocoded$id==id]<-dist
  if(!(grepl('manually updated lat/long', dataset_geocoded$geocode_notes[dataset_geocoded$id==id]))){
    if(!is.na(dataset_geocoded$geocode_notes[dataset_geocoded$id==id])){
  dataset_geocoded$geocode_notes[dataset_geocoded$id==id]<-paste0(dataset_geocoded$geocode_notes[dataset_geocoded$id==id], ', ',
                                                                 'manually updated lat/long on the map')}
    else(dataset_geocoded$geocode_notes[dataset_geocoded$id==id]<-'manually updated lat/long on the map')
  }
  proxy_geocode$data<-data.frame(Message='Click on map to update latitude and longitude')
  saveData(data=dataset_geocoded, fileName='dataset_geocoded.csv')
  write.csv(dataset_geocoded%>%dplyr::select(id, rating, distance_old_new, geocode_notes), '~/workspace/ACMT_shiny/data_pull_measures/geocode_ratings_notes.csv', row.names = FALSE)
})

observeEvent(input$geocode_notes,{
  
  dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(geocode_notes=as.character(geocode_notes))
  id<-input$idNumber
  dataset_geocoded$geocode_notes[dataset_geocoded$id==id]<-input$geocode_check
  saveData(data=dataset_geocoded, fileName='dataset_geocoded.csv')
  write.csv(dataset_geocoded%>%dplyr::select(id, rating, geocode_notes), '~/workspace/ACMT_shiny/data_pull_measures/geocode_ratings_notes.csv', row.names = FALSE)
  ## upload notes column dependent on radio buttons
})

  
## COMMON FUNCTIONS ####  
#Status File functions
  status_file <- tempfile()
  current_id<-tempfile()
  
  get_status <- function(){
    scan(status_file, what = "character",sep="\n")
  }
  
  get_current_id<-function(){
    scan(current_id, what="character", sep="\n")
  }
  
  set_status <- function(msg){
    write(msg, status_file)
  }
  set_current_id<-function(msg){
    write(msg, current_id)
  }
  
  fire_interrupt <- function(){
    set_status("Data process stopping, please wait")
  }
  
  fire_ready <- function(){
    set_status("Ready...")
    set_current_id('Click the pull data button to begin or resume data pull')
  }
  
  fire_running <- function(perc_complete){
    if(missing(perc_complete))
      msg <- "Running..."
    else
      msg <- paste0(perc_complete, "% Complete")
    set_status(msg)
  }
  
  auto_status<-function(id=id, radius, year){
    req(id)
    msg<-paste0('Currently processing id # ', id, ' for radius = ', radius, ' and year = ', year)
    set_current_id(msg)
  }
  
  interrupted <- function(){
    get_status() == "Data process stopping, please wait"
  }
  
  # Delete file at end of session
  onStop(function(){
    if(file.exists(status_file))
      unlink(status_file)
    if(file.exists(current_id))
      unlink(current_id)
  })  
  
## Create Status File ####
fire_ready()
nclicks <- reactiveVal(0)
result_val <- reactiveVal()
  
### American Community Survey  Server Functions #### ####
#1. Upload Geocoded Data ####
output$acs_description<-renderText(acs_description)

load_geocode_acs<-eventReactive(input$loaddata_acs,{
  source('~/workspace/ACMT_shiny/data_pull_settings/acs_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
})

#pull or create acs data (automatically with load data)
observeEvent(input$pull_acs,{
  if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==FALSE){
    showNotification('Creating acs data frame', duration=5, type='message', id='process_notif') 
    dataset_acs<-create_dataset(variable_list=acs_vars)
    write.csv(dataset_acs, '~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv', row.names = FALSE)
  }
  else{
    showNotification('Importing acs data frame', duration=5, type='message', id='status_notif')
  }
})

load_acs<-reactive({
  dataset_acs<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(acs_vars[1]:radius)
  dataset_acs
})

#3. Loop acs Process ####
observeEvent(input$pull_acs,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning', id='status_notif')
    return(NULL)
  }
  
  if(input$loaddata_acs==0){
    showNotification('Upload geocoded data to pull acs data', type='error')
    return(NULL)
  }
  
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling acs data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of acs data, using geocoded data: 
  #loop to pull acs data: 
  years<-as.numeric(input$selectyear_acs)
  radius_vector <- as.numeric(input$selectradii_acs) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_acs()
  dataset_acs<-
    load_acs()
  
  N<-nrow(dataset_geocoded)
  N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
  
  #future promise loop here:
  result <- future({
    print("Pulling acs data...")
    # Long Running Task - acs data pull
    for(i in 1:N){
      print(paste0("Currently processing ", i, ' out of ', N))
      #check for interrupted data process:
      id<-dataset_geocoded$id[i]
      latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
      longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
      print(id) #print the number to keep track of progress
      print(c(latitude, longitude))
      
      for(y in 1:length(years)){
        year<-years[y]
        print(year)
        
        for(r in 1:length(radius_vector)){
          # Check for user interrupts
          if(interrupted()){ 
            print("Stopping...")
            stop("User Interrupt")
            removeNotification(id='stop_message')
            #set clicks back to 0
            nclicks(0)
          }
          
          radius<-radius_vector[r]
          print(radius)
          
          # Notify status file of progress
          fire_running(round(100*i/N, 2))
          auto_status(id=dataset_geocoded$id[i], radius, year)
          
          #check for existing data in dataset:
          tryCatch({
            if(length(dataset_acs$id) != 0){
              if(id %in% dataset_acs$id[dataset_acs$year==year & dataset_acs$radius==radius]) next #skip the row if the data is already there
            }
            
            dataset_acs<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(acs_vars[1]:radius)
            suppressMessages(
              suppressWarnings(
                environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = codes_of_acs_variables_to_get, 
                                                                external_data_name_to_info_list=NULL, fill_missing_GEOID_with_zero = TRUE, set_var_list = TRUE)
              )
            )
            
            environmental_measures<-merge(environmental_measures,as.data.frame(acs_vars)%>%rename(names=acs_vars), by='names', all.y=TRUE)
            acs_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius) %>% dplyr::select(acs_vars, id, year, radius )
            
            #combine 
            dataset_acs<-rbind(dataset_acs, acs_measures)
            
            write.csv(dataset_acs, '~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv', row.names = FALSE)
            
          },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
        }
      }
      
    }
    
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/acs_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/acs_missingness.csv')
    
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for another Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

##Function to pull County GEOID####
observeEvent(input$pull_county_GEOID, {
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning', id='status_notif')
    return(NULL)
  }
  
  if(input$loaddata_acs==0){
    showNotification('Upload geocoded data to pull acs data', type='error')
    return(NULL)
  }
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling County GEOIDs..."))
  
  fire_running()
  
  #loop to pull county GEOIDs: 
  dataset_geocoded<-load_geocode_acs()
  dataset_GEOID_county<-pull_county_geoid(dataset_geocoded)
  
  result_val(NULL)
  showNotification('county GEOID pull complete', id='status-notif', type='message')
  nclicks(0)
  
})


#4. Show acs table & Status ####
preview_acs<-reactiveValues(data=data.frame())

observeEvent(input$show_data_acs, {
  if(input$show_data_acs=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_acs$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_acs$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  
  if(input$show_data_acs=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==TRUE){
    preview_acs$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_acs=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==TRUE){
    preview_acs$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_acs=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==TRUE){
    preview_acs$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_acs !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==FALSE){
    preview_acs$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}

})


output$dataset_acs<-DT::renderDataTable({preview_acs$data}, editable=FALSE, 
                                         rownames=FALSE,
                                         options = list(
                                           searching = FALSE,
                                           pageLength = 10,
                                           #dom = 't', 
                                           autowidth=FALSE,
                                           scrollX=TRUE
                                           #,
                                           #columnDefs = list(list(targets='_all', width='900px'))
                                         )
)

output$dataset_acs_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_acs,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_acs=='Show geocoded dataset') {
    preview_acs$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_acs=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==TRUE){
    preview_acs$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_acs=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==TRUE){
    preview_acs$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_acs=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==TRUE){
    preview_acs$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_acs !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_acs.csv')==FALSE){
    preview_acs$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_acs,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})

### Walkability  Server Functions #### ####
#1. Upload Geocoded Data ####
output$walk_description<-renderText(walk_description)

load_geocode_walk<-eventReactive(input$loaddata_walk,{
  source('~/workspace/ACMT_shiny/data_pull_settings/walk_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) &!is.na(long))
  })

#pull or create walk data (automatically with load data)
observeEvent(input$pull_walk,{
  if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==FALSE){
    showNotification('Creating walk data frame', duration=5, type='message', id='process_notif') 
    dataset_walk<-create_dataset(variable_list=walk_vars)
    write.csv(dataset_walk, '~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv', row.names = FALSE)
  }
  else{
    showNotification('Importing walk data frame', duration=5, type='message', id='status_notif')
  }
})

load_walk<-reactive({
  dataset_walk<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(walk_vars[1]:radius)
  dataset_walk
})

#3. Loop walk Process ####
observeEvent(input$pull_walk,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning')
    return(NULL)
  }
  
  if(input$loaddata_walk==0){
    showNotification('Upload geocoded data to pull walk data', type='error')
    return(NULL)
  }
  
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling walk data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of walk data, using geocoded data: 
  #loop to pull walk data: 
  years<-as.numeric(input$selectyear_walk)
  radius_vector <- as.numeric(input$selectradii_walk) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_walk()
  dataset_walk<-
    load_walk()

  N<-nrow(dataset_geocoded)
  N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
  
  #future promise loop here:
  result <- future({
    print("Pulling walk data...")
    # Long Running Task - walk data pull
    for(i in 1:N){
      print(paste0("Currently processing ", i, ' out of ', N))
      #check for interrupted data process:
      id<-dataset_geocoded$id[i]
      latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
      longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
      print(id) #print the number to keep track of progress
      print(c(latitude, longitude))
      
      for(y in 1:length(years)){
        year<-years[y]
        print(year)
        
        for(r in 1:length(radius_vector)){
          # Check for user interrupts
          if(interrupted()){ 
            print("Stopping...")
            stop("User Interrupt")
            removeNotification(id='stop_message')
            #set clicks back to 0
            nclicks(0)
          }
          
          radius<-radius_vector[r]
          print(radius)
          
          # Notify status file of progress
          fire_running(round(100*i/N, 2))
          auto_status(id=dataset_geocoded$id[i], radius, year)
          
          #check for existing data in dataset:
          tryCatch({
            if(length(dataset_walk$id) != 0){
              if(id %in% dataset_walk$id[dataset_walk$year==year & dataset_walk$radius==radius]) next #skip the row if the data is already there
            }
            
            dataset_walk<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(walk_vars[1]:radius)
            suppressMessages(
              suppressWarnings(
                environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NULL, 
                                                                external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
              )
            )
            walk_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
            
            #combine 
            dataset_walk<-rbind(dataset_walk, walk_measures)
            
            write.csv(dataset_walk, '~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv', row.names = FALSE)
            
          },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
        }
      }
      
      
      
    }
    
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/walk_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/walk_missingness.csv')
    
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for another Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show walk table & Status ####
preview_walk<-reactiveValues(data=data.frame())
observeEvent(input$show_data_walk, {
  if(input$show_data_walk=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_walk$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_walk$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  
 if(input$show_data_walk=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==TRUE){
    preview_walk$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
    dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
 if(input$show_data_walk=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==TRUE){
     preview_walk$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
     mutate_if(is.numeric, round, digits=3)%>%table_summary()
  }
 if(input$show_data_walk=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==TRUE){
    preview_walk$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
    }
  if(input$show_data_walk !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==FALSE){
    preview_walk$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}

})


output$dataset_walk<-DT::renderDataTable({preview_walk$data}, editable=FALSE, 
                                        rownames=FALSE,
                                        options = list(
                                          searching = FALSE,
                                          pageLength = 10,
                                          #dom = 't', 
                                          autowidth=FALSE,
                                          scrollX=TRUE
                                          #,
                                          #columnDefs = list(list(targets='_all', width='900px'))
                                          )
)

output$dataset_walk_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
  req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_walk,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_walk=='Show geocoded dataset') {
    preview_walk$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_walk=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==TRUE){
    preview_walk$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_walk=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==TRUE){
    preview_walk$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_walk=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==TRUE){
    preview_walk$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_walk !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_walk.csv')==FALSE){
    preview_walk$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_walk,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
 else{
  showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
  print("Cancel")
  fire_interrupt()
  }
})

### CDC Places Server Functions #### ####
#1. Upload Geocoded Data ####
output$cdc_description<-renderText(cdc_description)

load_geocode_cdc<-eventReactive(input$loaddata_cdc,{
  source('~/workspace/ACMT_shiny/data_pull_settings/cdc_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) &!is.na(long))
})

#pull or create cdc data (automatically with load data)
observeEvent(input$pull_cdc,{
  if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==FALSE){
    showNotification('Creating cdc data frame', duration=5, type='message', id='process_notif') 
    dataset_cdc<-create_dataset(variable_list=cdc_vars)
    write.csv(dataset_cdc, '~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv', row.names = FALSE)
  }
  else{
    showNotification('Importing cdc data frame', duration=5, type='message', id='status_notif')
  }
})

load_cdc<-reactive({
  dataset_cdc<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(cdc_vars[1]:radius)
  dataset_cdc
})

#3. Loop cdc Process ####
observeEvent(input$pull_cdc,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning')
    return(NULL)
  }
  
  if(input$loaddata_cdc==0){
    showNotification('Upload geocoded data to pull cdc data', type='error')
    return(NULL)
  }
  
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling cdc data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of cdc data, using geocoded data: 
  #loop to pull cdc data: 
  years<-as.numeric(input$selectyear_cdc)
  radius_vector <- as.numeric(input$selectradii_cdc) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_cdc()
  #print(states)
  dataset_cdc<-
    load_cdc()
  
  N<-nrow(dataset_geocoded)
  N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
  
  #future promise loop here:
  result <- future({
    print("Pulling cdc data...")
    # Long Running Task - cdc data pull
    for(i in 1:N){
      print(paste0("Currently processing ", i, ' out of ', N))
      #check for interrupted data process:
      id<-dataset_geocoded$id[i]
      latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
      longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
      print(id) #print the number to keep track of progress
      print(c(latitude, longitude))
      
      for(y in 1:length(years)){
        year<-years[y]
        print(year)
        
        for(r in 1:length(radius_vector)){
          # Check for user interrupts
          if(interrupted()){ 
            print("Stopping...")
            stop("User Interrupt")
            removeNotification(id='stop_message')
            #set clicks back to 0
            nclicks(0)
          }
          
          radius<-radius_vector[r]
          print(radius)
          
          # Notify status file of progress
          fire_running(round(100*i/N, 2))
          auto_status(id=dataset_geocoded$id[i], radius, year)
          
          #check for existing data in dataset:
          tryCatch({
            if(length(dataset_cdc$id) != 0){
              if(id %in% dataset_cdc$id[dataset_cdc$year==year & dataset_cdc$radius==radius]) next #skip the row if the data is already there
            }
            
            dataset_cdc<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(cdc_vars[1]:radius)
            print(states)
            suppressMessages(
              suppressWarnings(
                environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NULL, 
                                                                external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
              )
            )
            cdc_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
            
            #combine 
            dataset_cdc<-rbind(dataset_cdc, cdc_measures)
            
            write.csv(dataset_cdc, '~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv', row.names = FALSE)
            
          },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
        }
      }
      
      
      
    }
    
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/cdc_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/cdc_missingness.csv')
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show cdc table & Status ####
preview_cdc<-reactiveValues(data=data.frame())

observeEvent(input$show_data_cdc, {
  if(input$show_data_cdc=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_cdc$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_cdc$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  if(input$show_data_cdc=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==TRUE){
    preview_cdc$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_cdc=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==TRUE){
    preview_cdc$data=suppressWarnings(read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.))
  }
  if(input$show_data_cdc=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==TRUE){
    preview_cdc$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_cdc !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==FALSE){
    preview_cdc$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})


output$dataset_cdc<-DT::renderDataTable({preview_cdc$data}, editable=FALSE, 
                                         rownames=FALSE,
                                         options = list(
                                           searching = FALSE,
                                           pageLength = 10,
                                           #dom = 't', 
                                           autowidth=FALSE,
                                           scrollX=TRUE
                                           #,
                                           #columnDefs = list(list(targets='_all', width='900px'))
                                         )
)

output$dataset_cdc_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_cdc,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_cdc=='Show geocoded dataset') {
    preview_cdc$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_cdc=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==TRUE){
    preview_cdc$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_cdc=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==TRUE){
    preview_cdc$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_cdc=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==TRUE){
    preview_cdc$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_cdc !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_cdc.csv')==FALSE){
    preview_cdc$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_cdc,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})


### Modified retail food environment  Server Functions #### ####
#1. Upload Geocoded Data ####
output$mrfei_description<-renderText(mrfei_description)

load_geocode_mrfei<-eventReactive(input$loaddata_mrfei,{
  source('~/workspace/ACMT_shiny/data_pull_settings/mrfei_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) &!is.na(long))
})

#pull or create mrfei data (automatically with load data)
observeEvent(input$pull_mrfei,{
  if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==FALSE){
    showNotification('Creating mrfei data frame', duration=5, type='message', id='process_notif') 
    dataset_mrfei<-create_dataset(variable_list=mrfei_vars)
    write.csv(dataset_mrfei, '~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv', row.names = FALSE)
  }
  else{
    showNotification('Importing mrfei data frame', duration=5, type='message', id='status_notif')
  }
})

load_mrfei<-reactive({
  dataset_mrfei<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(mrfei_vars[1]:radius)
  dataset_mrfei
})

#3. Loop mrfei Process ####
observeEvent(input$pull_mrfei,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning')
    return(NULL)
  }
  
  if(input$loaddata_mrfei==0){
    showNotification('Upload geocoded data to pull mrfei data', type='error')
    return(NULL)
  }
  
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling mrfei data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of mrfei data, using geocoded data: 
  #loop to pull mrfei data: 
  years<-as.numeric(input$selectyear_mrfei)
  radius_vector <- as.numeric(input$selectradii_mrfei) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_mrfei()
  dataset_mrfei<-
    load_mrfei()
  
  N<-nrow(dataset_geocoded)
  N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
  
  #future promise loop here:
  result <- future({
    print("Pulling mrfei data...")
    # Long Running Task - mrfei data pull
    for(i in 1:N){
      print(paste0("Currently processing ", i, ' out of ', N))
      #check for interrupted data process:
      id<-dataset_geocoded$id[i]
      latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
      longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
      print(id) #print the number to keep track of progress
      print(c(latitude, longitude))
      
      for(y in 1:length(years)){
        year<-years[y]
        print(year)
        
        for(r in 1:length(radius_vector)){
          # Check for user interrupts
          if(interrupted()){ 
            print("Stopping...")
            stop("User Interrupt")
            removeNotification(id='stop_message')
            #set clicks back to 0
            nclicks(0)
          }
          
          radius<-radius_vector[r]
          print(radius)
          
          # Notify status file of progress
          fire_running(round(100*i/N, 2))
          auto_status(id=dataset_geocoded$id[i], radius, year)
          
          #check for existing data in dataset:
          tryCatch({
            if(length(dataset_mrfei$id) != 0){
              if(id %in% dataset_mrfei$id[dataset_mrfei$year==year & dataset_mrfei$radius==radius]) next #skip the row if the data is already there
            }
            
            dataset_mrfei<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(mrfei_vars[1]:radius)
            suppressMessages(
              suppressWarnings(
                environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=2017, codes_of_acs_variables_to_get = NULL, 
                                                                external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
              )
            )
            mrfei_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
            
            #combine 
            dataset_mrfei<-rbind(dataset_mrfei, mrfei_measures)
            
            write.csv(dataset_mrfei, '~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv', row.names = FALSE)
            
          },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
        }
      }
      
      
      
    }
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/mrfei_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/mrfei_missingness.csv')
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show mrfei table & Status ####
preview_mrfei<-reactiveValues(data=data.frame())

observeEvent(input$show_data_mrfei, {
  if(input$show_data_mrfei=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_mrfei$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_mrfei$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  if(input$show_data_mrfei=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==TRUE){
    preview_mrfei$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_mrfei=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==TRUE){
    preview_mrfei$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_mrfei=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==TRUE){
    preview_mrfei$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_mrfei !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==FALSE){
    preview_mrfei$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})


output$dataset_mrfei<-DT::renderDataTable({preview_mrfei$data}, editable=FALSE, 
                                         rownames=FALSE,
                                         options = list(
                                           searching = FALSE,
                                           pageLength = 10,
                                           #dom = 't', 
                                           autowidth=FALSE,
                                           scrollX=TRUE
                                           #,
                                           #columnDefs = list(list(targets='_all', width='900px'))
                                         )
)

output$dataset_mrfei_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_mrfei,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_mrfei=='Show geocoded dataset') {
    preview_mrfei$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_mrfei=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==TRUE){
    preview_mrfei$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_mrfei=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==TRUE){
    preview_mrfei$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_mrfei=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==TRUE){
    preview_mrfei$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_mrfei !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_mrfei.csv')==FALSE){
    preview_mrfei$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_mrfei,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})



##PARKSERVE data #### -- **** NEED TO ADD SHP PRE_PROCESS STEP AND TEST THE SHP PROCESSING **** ####
#1. Upload Geocoded Data ####
output$parks_description<-renderText(parks_description)

load_geocode_parks<-eventReactive(input$loaddata_parks,{
  source('~/workspace/ACMT_shiny/data_pull_settings/park_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
})

#pull or create parks data (automatically with load data)
observeEvent(input$pull_parks,{
  if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==FALSE){
    showNotification('Creating parks data frame', duration=5, type='message', id='process_notif') 
    dataset_parks<-create_dataset(variable_list=parks_vars)
    write.csv(dataset_parks, '~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv', row.names = FALSE)
  }
  else{
    showNotification('Importing parks data frame', duration=5, type='message', id='status_notif')
  }
})

load_parks<-reactive({
  dataset_parks<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(parks_vars[1]:radius)
  dataset_parks
})

#3. Loop parks Process ####
observeEvent(input$pull_parks,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning')
    return(NULL)
  }
  
  if(input$loaddata_parks==0){
    showNotification('Upload geocoded data to pull parks data', type='error')
    return(NULL)
  }
  
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling parks data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of parks data, using geocoded data: 
  #loop to pull parks data: 
  years<-as.numeric(input$selectyear_parks)
  radius_vector <- as.numeric(input$selectradii_parks) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_parks()
  dataset_parks<-
    load_parks()
  #Download & Process parks data  
  if(exists('park_shp')==TRUE){
    showNotification('Park shapefile has already been created -- ready to pull data', duration=5, id='status_notif')
  }
  
  if(exists('park_shp')==FALSE){
  showNotification('loading parks data (this may take around 20 minutes', duration=10, id='status_notif')
  fire_running("Currently downloading ParkServe data")
  park_shp<-prepare_park_data()
  removeNotification(id='status_notif')
  showNotification('Raw ParkServe data has downloaded and processed, now pulling measures for dataset', duration=10, id='status_notif')
    }
  fire_running()
  
  
  N<-nrow(dataset_geocoded)
  N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
  
  #future promise loop here:
  result <- future({
    print("Pulling parks data...")
    # Long Running Task - parks data pull
    for(i in 1:N){
      print(paste0("Currently processing ", i, ' out of ', N))
      #check for interrupted data process:
      id<-dataset_geocoded$id[i]
      latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
      longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
      print(id) #print the number to keep track of progress
      print(c(latitude, longitude))
      
      for(y in 1:length(years)){
        year<-years[y]
        print(year)
        
        for(r in 1:length(radius_vector)){
          # Check for user interrupts
          if(interrupted()){ 
            print("Stopping...")
            stop("User Interrupt")
            removeNotification(id='stop_message')
            #set clicks back to 0
            nclicks(0)
          }
          
          radius<-radius_vector[r]
          print(radius)
          
          # Notify status file of progress
          fire_running(round(100*i/N, 2))
          auto_status(id=dataset_geocoded$id[i], radius, year)
          
          #check for existing data in dataset:
          tryCatch({
            if(length(dataset_parks$id) != 0){
              if(id %in% dataset_parks$id[dataset_parks$year==year & dataset_parks$radius==radius]) next #skip the row if the data is already there
            }
            
            dataset_parks<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(parks_vars[1]:radius)
            suppressMessages(
              suppressWarnings(
                environmental_measures<-data.frame(park_proportion=get_proportion_in_shapefile(long=longitude, lat=latitude, radius_meters=radius, shp_processed = park_shp), 
                                                   distance_park=get_distance_to_shapefile(long=longitude, lat=latitude, radius_meters=radius, shp_processed = park_shp))
              )
            )
            parks_measures<-environmental_measures %>%mutate(id=id, year=year, radius=radius)
            
            #combine 
            dataset_parks<-rbind(dataset_parks, parks_measures)
            
            write.csv(dataset_parks, '~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv', row.names = FALSE)
            
          },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
        }
      }
    }
    
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/parks_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/parks_missingness.csv')
    
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show parks table & Status ####
preview_parks<-reactiveValues(data=data.frame())

observeEvent(input$show_data_parks, {
  if(input$show_data_parks=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_parks$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_parks$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  if(input$show_data_parks=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==TRUE){
    preview_parks$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_parks=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==TRUE){
    preview_parks$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_parks=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==TRUE){
    preview_parks$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_parks !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==FALSE){
    preview_parks$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})


output$dataset_parks<-DT::renderDataTable({preview_parks$data}, editable=FALSE, 
                                         rownames=FALSE,
                                         options = list(
                                           searching = FALSE,
                                           pageLength = 10,
                                           #dom = 't', 
                                           autowidth=FALSE,
                                           scrollX=TRUE
                                           #,
                                           #columnDefs = list(list(targets='_all', width='900px'))
                                         )
)

output$dataset_parks_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_parks,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_parks=='Show geocoded dataset') {
    preview_parks$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_parks=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==TRUE){
    preview_parks$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_parks=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==TRUE){
    preview_parks$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_parks=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==TRUE){
    preview_parks$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_parks !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_parks.csv')==FALSE){
    preview_parks$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_parks,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})




### RPP Server Functions #### 
#1. Upload Geocoded Data ####
output$rpp_description<-renderText(rpp_description)

load_geocode_rpp<-eventReactive(input$loaddata_rpp,{
  source('~/workspace/ACMT_shiny/data_pull_settings/rpp_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) &!is.na(long))
})

#3. Loop rpp Process ####
observeEvent(input$pull_rpp,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning', id='status_notif')
    return(NULL)
  }
  
  if(input$loaddata_rpp==0){
    showNotification('Upload geocoded data to pull rpp data', type='error', id='status_notif')
    return(NULL)
  }
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling rpp data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of rpp data, using geocoded data: 
  years<-as.numeric(input$selectyear_rpp)
  radius_vector <- as.numeric(input$selectradii_rpp) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_rpp()

  pull_rpp_ids(dataset_geocoded)
  
  
  #pull or create cdc data (automatically with load data)
    if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==FALSE){
      showNotification('Creating rpp data frame', duration=5, type='message', id='process_notif') 
      dataset_rpp<-create_dataset(variable_list=rpp_vars)%>%dplyr::select(-radius)
      write.csv(dataset_rpp, '~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv', row.names = FALSE)
    }
    else{
      showNotification('Importing rpp data frame', duration=5, type='message', id='status_notif')
      datset_rpp<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')
    }
  
  result <- future({
    print("Pulling rpp data...")
  
  for(y in 1:length(years)){
    year<-years[y]
    auto_status(id='all ids', radius='', year=year)
    if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
      dataset_rpp<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(-any_of('X'))}
    if(length(dataset_rpp$year) != 0){
      if(year %in% dataset_rpp$year) next #skip the row if the data is already there
    }
        rpp<-read.csv('~/workspace/ACMT_shiny/price_parity_processed.csv')%>%dplyr::select(-any_of('X'))
        rpp_year<-rpp[rpp$year==year,]
        msa_state_dataset<-read.csv('~/workspace/ACMT_shiny/msa_state_dataset.csv')%>%dplyr::select(-any_of('X'))
        rpp_measures<-merge(msa_state_dataset%>%dplyr::select(state_geoid, msa_geoid, GEOID_pp, id), rpp_year, by.x='GEOID_pp', by.y='FIPS', all.x= TRUE)
        
        dataset_rpp<-rbind(dataset_rpp, rpp_measures)
        write.csv(dataset_rpp, '~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv', row.names = FALSE)
        
  }
    # Notify status file of progress
    fire_running('complete')
      
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/rpp_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/rpp_missingness.csv')
    
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                      showNotification('Data pull Completed', type='message', id='status_notif')
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show rpp table & Status ####
preview_rpp<-reactiveValues(data=data.frame())

observeEvent(input$show_data_rpp, {
  if(input$show_data_rpp=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_rpp$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_rpp$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  if(input$show_data_rpp=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
    preview_rpp$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(id,year, everything())%>%
      dplyr::select(-any_of('X'))
  }
  if(input$show_data_rpp=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
    preview_rpp$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(id, year, everything())%>%
      dplyr::select(-any_of('X'))%>%dplyr::select(-GEOID_pp, -state_geoid, -msa_geoid, -GeoName)%>%mutate(radius=NA) %>% table_summary(.)
  }
  if(input$show_data_rpp=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
    preview_rpp$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(id, year, everything())%>%
      dplyr::select(-any_of('X'))%>%dplyr::select(-GEOID_pp, -state_geoid, -msa_geoid, -GeoName)%>% group_by(year) %>%dplyr::summarise(count_na=sum(is.na(.)), 
                                                                                                                               count_total=n())%>%arrange(year)
  }
  if(input$show_data_rpp !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==FALSE){
    preview_rpp$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})


output$dataset_rpp<-DT::renderDataTable({preview_rpp$data}, editable=FALSE, 
                                             rownames=FALSE,
                                             options = list(
                                               searching = FALSE,
                                               pageLength = 10,
                                               #dom = 't', 
                                               autowidth=FALSE,
                                               scrollX=TRUE
                                               #,
                                               #columnDefs = list(list(targets='_all', width='900px'))
                                             )
)

output$dataset_rpp_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_rpp,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_rpp=='Show geocoded dataset') {
    preview_rpp$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_rpp=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
    preview_rpp$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(id, year, everything())%>%
      dplyr::select(-any_of('X'))
  }
  if(input$show_data_rpp=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
    preview_rpp$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(id, year, everything())%>%
      dplyr::select(-any_of('X'))%>%dplyr::select(-GEOID_pp, -state_geoid, -msa_geoid, -GeoName)%>%mutate(radius=NA) %>% table_summary(.)
  }
  if(input$show_data_rpp=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==TRUE){
    preview_rpp$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')%>%dplyr::select(id, year, everything())%>%
      dplyr::select(-any_of('X'))%>%dplyr::select(-GEOID_pp, -state_geoid, -msa_geoid, -GeoName)%>% group_by(year) %>% dplyr::summarise(count_na=sum(is.na(.)), 
                                                                                                                       count_total=n())%>%arrange(year)
  }
  if(input$show_data_rpp !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_rpp.csv')==FALSE){
    preview_rpp$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_rpp,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})





### GENTRIFICATION Server Functions #### 
#1. Upload Geocoded Data ####
output$gentrification_description<-renderText(gentrification_description)

load_geocode_gentrification<-eventReactive(input$loaddata_gentrification,{
  source('~/workspace/ACMT_shiny/data_pull_settings/gentrification_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) &!is.na(long))
})

#3. Loop gentrification Process ####
observeEvent(input$pull_gentrification,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning', id='status_notif')
    return(NULL)
  }
  
  if(input$loaddata_gentrification==0){
    showNotification('Upload geocoded data to pull gentrification data', type='error', id='status_notif')
    return(NULL)
  }
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling gentrification data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of gentrification data, using geocoded data: 
  years<-as.numeric(input$selectyear_gentrification)
  dataset_geocoded<-load_geocode_gentrification()
  
  dataset_gentrification<-data.frame()
  
  result <- future({
    print("Pulling gentrification data...")
    
    dataset_gentrification<-pull_gentrification(dataset_geocoded)
    
    # Notify status file of progress
    fire_running('complete')
    auto_status(id='all ids', radius='', year=years)
    
    write.csv(dataset_gentrification, '~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv', row.names = FALSE)
    
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/gentrification_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/gentrification_missingness.csv')
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                      showNotification('Data pull Completed', type='message', id='status_notif')
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show gentrification table & Status ####
preview_gentrification<-reactiveValues(data=data.frame())

observeEvent(input$show_data_gentrification, {
  if(input$show_data_gentrification=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_gentrification$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_gentrification$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  if(input$show_data_gentrification=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==TRUE){
    preview_gentrification$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')%>%dplyr::select(id,everything())%>%
      dplyr::select(-any_of('X'))
  }
  if(input$show_data_gentrification=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==TRUE){
    preview_gentrification$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')%>%dplyr::select(id, everything())%>%
      dplyr::select(-any_of('X'))%>%mutate(year=NA, radius=NA) %>% table_summary(.)
  }
  if(input$show_data_gentrification=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==TRUE){
    preview_gentrification$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')%>%dplyr::select(id, everything())%>%
      dplyr::select(-any_of('X'))%>% dplyr::summarise(count_na=sum(is.na(.)), count_total=n())
  }
  if(input$show_data_gentrification !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==FALSE){
    preview_gentrification$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})


output$dataset_gentrification<-DT::renderDataTable({preview_gentrification$data}, editable=FALSE, 
                                        rownames=FALSE,
                                        options = list(
                                          searching = FALSE,
                                          pageLength = 10,
                                          #dom = 't', 
                                          autowidth=FALSE,
                                          scrollX=TRUE
                                          #,
                                          #columnDefs = list(list(targets='_all', width='900px'))
                                        )
)

output$dataset_gentrification_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_gentrification,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_gentrification=='Show geocoded dataset') {
    preview_gentrification$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_gentrification=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==TRUE){
    preview_gentrification$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')%>%dplyr::select(id, everything())%>%
      dplyr::select(-any_of('X'))
  }
  if(input$show_data_gentrification=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==TRUE){
    preview_gentrification$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')%>%dplyr::select(id,everything())%>%
      dplyr::select(-any_of('X'))%>%mutate(radius=NA, year=NA) %>% table_summary(.)
  }
  if(input$show_data_gentrification=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==TRUE){
    preview_gentrification$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')%>%dplyr::select(id, everything())%>%
      dplyr::select(-any_of('X'))%>%dplyr::summarise(count_na=sum(is.na(.)), 
                                                                                                                       count_total=n())
  }
  if(input$show_data_gentrification !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_gentrification.csv')==FALSE){
    preview_gentrification$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_gentrification,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})






### NATIONAL LAND COVER DATABASE ####
#1. Upload Geocoded Data ####
output$nlcd_description<-renderText(nlcd_description)

load_geocode_nlcd<-eventReactive(input$loaddata_nlcd,{
  source('~/workspace/ACMT_shiny/data_pull_settings/nlcd_data_settings.R')
  loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat) &!is.na(long))
})

#pull or create nlcd data (automatically with load data)
observeEvent(input$pull_nlcd,{
  if(file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==FALSE){
    showNotification('Creating nlcd data frame', duration=5, type='message', id='process_notif') 
    dataset_nlcd<-create_dataset(variable_list=nlcd_vars)
    write.csv(dataset_nlcd, '~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv', row.names = FALSE)
  }
  else{
    showNotification('Importing nlcd data frame', duration=5, type='message', id='status_notif')
  }
})

load_nlcd<-reactive({
  dataset_nlcd<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(nlcd_vars[1]:radius)
  dataset_nlcd
})

#3. Loop nlcd Process ####
observeEvent(input$pull_nlcd,{
  
  # Don't do anything if analysis is already being run
  if(nclicks() != 0){
    showNotification("Already pulling data", type='warning')
    return(NULL)
  }
  
  if(input$loaddata_nlcd==0){
    showNotification('Upload geocoded data to pull nlcd data', type='error')
    return(NULL)
  }
  
  
  # Increment clicks and prevent concurrent analyses
  nclicks(nclicks() + 1)
  
  result_val(data.frame(Status="Pulling nlcd data..."))
  
  fire_running()
  
  ##reactive values here:
  #initial pull of nlcd data, using geocoded data: 
  #loop to pull nlcd data: 
  years<-as.numeric(input$selectyear_nlcd)
  radius_vector <- as.numeric(input$selectradii_nlcd) #set the radius for the area of interest
  dataset_geocoded<-load_geocode_nlcd()
  dataset_nlcd<-
    load_nlcd()
  
  N<-nrow(dataset_geocoded)
  N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
  
  #future promise loop here:
  result <- future({
    print("Pulling nlcd data...")
    # Long Running Task - nlcd data pull
    for(i in 1:N){
      print(paste0("Currently processing ", i, ' out of ', N))
      #check for interrupted data process:
      id<-dataset_geocoded$id[i]
      latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
      longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
      print(id) #print the number to keep track of progress
      print(c(latitude, longitude))
      
      for(y in 1:length(years)){
        year<-years[y]
        print(year)
        nlcd_data<-pull_nlcd_data(year=year, label='nlcd landcover')
        
        for(r in 1:length(radius_vector)){
          # Check for user interrupts
          if(interrupted()){ 
            print("Stopping...")
            stop("User Interrupt")
            removeNotification(id='stop_message')
            #set clicks back to 0
            nclicks(0)
          }
          
          radius<-radius_vector[r]
          print(radius)
          
          # Notify status file of progress
          fire_running(round(100*i/N, 2))
          auto_status(id=dataset_geocoded$id[i], radius, year)
          
          #check for existing data in dataset:
          tryCatch({
            if(length(dataset_nlcd$id) != 0){
              if(id %in% dataset_nlcd$id[dataset_nlcd$year==year & dataset_nlcd$radius==radius]) next #skip the row if the data is already there
            }
            
            dataset_nlcd<-read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(open_water:radius)
            suppressMessages(
              suppressWarnings(
                #create buffer shapefile
                environmental_measures<-pull_nlcd_measures(longitude, latitude, radius, variable_list=nlcd_vars, nlcd_data=nlcd_data)
              )
            )
            nlcd_measures<-environmental_measures %>% dplyr::select(legend, Freq) %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
            
            #combine 
            dataset_nlcd<-rbind(dataset_nlcd, nlcd_measures)
            
            write.csv(dataset_nlcd, '~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv', row.names = FALSE)
            
          },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
        }
      }
      
      
      
    }
    # export summary tables
    summary_table_function(dataset_path='~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv', 
                           summary_path='~/workspace/ACMT_shiny/data_pull_summaries/nlcd_summary.csv', 
                           missingness_path='~/workspace/ACMT_shiny/data_pull_summaries/nlcd_missingness.csv')
    
    #Some results
  }) %...>% result_val()
  
  # Catch inturrupt (or any other error) and notify user
  result <- catch(result,
                  function(e){
                    result_val(NULL)
                    print(e$message)
                    removeNotification(id='stop_message')
                    showNotification(e$message, type='warning', id='status_notif')
                  })
  
  # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
  result <- finally(result,
                    function(){
                      fire_ready() 
                      nclicks(0)
                    })
  
  # Return something other than the promise so shiny remains responsive
  NULL
})

#4. Show nlcd table & Status ####
preview_nlcd<-reactiveValues(data=data.frame())

observeEvent(input$show_data_nlcd, {
  if(input$show_data_nlcd=='Show geocoded dataset' & file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==TRUE) {
    preview_nlcd$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(file.exists('~/workspace/ACMT_shiny/dataset_geocoded.csv')==FALSE){
    (preview_nlcd$data<-data.frame(message='Geocoded dataset does not yet exist, please upload data proir to pulling environmental measures'))} 
  if(input$show_data_nlcd=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==TRUE){
    preview_nlcd$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
      mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_nlcd=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==TRUE){
    preview_nlcd$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
      mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_nlcd=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==TRUE){
    preview_nlcd$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
      mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_nlcd !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==FALSE){
    preview_nlcd$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})


output$dataset_nlcd<-DT::renderDataTable({preview_nlcd$data}, editable=FALSE, 
                                         rownames=FALSE,
                                         options = list(
                                           searching = FALSE,
                                           pageLength = 10,
                                           #dom = 't', 
                                           autowidth=FALSE,
                                           scrollX=TRUE
                                           #,
                                           #columnDefs = list(list(targets='_all', width='900px'))
                                         )
)

output$dataset_nlcd_message<-renderTable(
  if(is.null(result_val())){data.frame(Status='Status: Ready to pull data')}
  else{
    req(result_val())
  }, 
  colnames=FALSE
)

# Show status notifications
observeEvent(input$status_nlcd,{
  print("Status")
  print('Current ID')
  showNotification(id='status_notif', get_status(), type='message')
  showNotification(id='process_notif', get_current_id(), type='message')
  
  if(input$show_data_nlcd=='Show geocoded dataset') {
    preview_nlcd$data=loadData('dataset_geocoded.csv') %>% dplyr::select(id, lat, long)%>%filter(!is.na(lat) & !is.na(long))
  }
  if(input$show_data_nlcd=='Show environmental measures data pull' & file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==TRUE){
    preview_nlcd$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
      mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
  }
  if(input$show_data_nlcd=='Show measure summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==TRUE){
    preview_nlcd$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
      mutate_if(is.numeric, round, digits=3)%>%table_summary(.)
  }
  if(input$show_data_nlcd=='Show missingness/count summary'& file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==TRUE){
    preview_nlcd$data=read.csv('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
      mutate_if(is.numeric, round, digits=3)%>%table_missingness(.)
  }
  if(input$show_data_nlcd !='Show geocoded dataset' &file.exists('~/workspace/ACMT_shiny/data_pull_measures/dataset_nlcd.csv')==FALSE){
    preview_nlcd$data=data.frame(message='Dataframe not yet created, click the Pull data button to create dataset and begin pulling environmental measures')}
  
})

#save uploaded folder to the ACMT_shiny folder
observeEvent(input$stop_nlcd,{
  if(get_status()=='Ready...'){
    showNotification('Data Pull is not currently running', duration=5, type='warning')
  }
  else{
    showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
    print("Cancel")
    fire_interrupt()
  }
})




#### Progress Table ####
# create the progress summmary function #  
progress_summary<-function(){  
  
  files <- list.files(path="~/workspace/ACMT_shiny/data_pull_measures", pattern="dataset", full.names=TRUE)
  files<-files[!grepl("county", files)]
  
  if(length(files)==0){
    data_summary<-list(Status=data.frame(total_n='No data pulled'))
  }
  
  if(length(files)>0){
    #get dataset names
    dataset_names<-lapply(files, function(x) {
      toupper(gsub("\\..*", "", str_replace(x, '(.*?)dataset_(.*?)', ''))) # get dataset name
    })
    
    data_summary<-lapply(files, function(x) {
      t <- read.csv(x, header=TRUE) # load file
      # apply function
      if('radius' %in% colnames(t)){
      t %>% group_by(year, radius) %>% dplyr::summarise(total_n=n())}
      else if(!('radius' %in% colnames(t)) & 'year' %in% colnames(t)){
        t%>%group_by(year) %>% dplyr::summarise(total_n=n())
      }
    })
    
    
  names(data_summary)<-dataset_names
  }
  lapply(names(data_summary), function(x) {
    table_title<-as.character(x)
    output[[x]] = renderDataTable({data_summary[[x]]}, 
                                  editable=FALSE, rownames=FALSE,
                                  caption=table_title,
                                  options=list(dom = 't'))
  })
  
  output$tables <- renderUI({
    lapply(names(data_summary), dataTableOutput)
  })
}

#show table when the page loads
#progress_summary()

#refresh table with the button
observeEvent(input$refreshprogress, {
  progress_summary()})

observeEvent(input$create_report, {
  withProgress(
source('~/workspace/ACMT_shiny/summary_tables.R'), 
message='Creating final summary report'
)
  showNotification('Data Summary Report Complete', id='status_notif', duration=NULL)
})


}

shinyApp(ui, server)
