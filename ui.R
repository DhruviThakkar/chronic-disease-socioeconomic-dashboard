#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(plotly)
library(jsonlite)
library(leaflet)
library(highcharter)
library(shinyjs)

# Defining UI for application 
fluidPage(
  
  tags$head(    # Adding CSS 
    tags$style(HTML("
                    body {
                      background-color: #0b0b0b;
                      color: #fff7f3;
                      font-size: 14px;
                      font-family: Open Sans, verdana, arial, sans-serif;
                    }
                    
                    .container-fluid{
                    margin-top: 48px;
                    padding-right: 48px;
                    padding-left: 48px;
                    }
                    
                    .leaflet .info{
                    background: transperent !important;
                    }
                    
                    .selectize-input{
                    background: #fff7f3;
                    color: #0b0b0b;
                    }
                    
                  
                    
                    .selectize-dropdown{
                    background: #fff7f3;
                    }
                    
                    .selectize-input.full{
                    background: #fff7f3
                    }
                    
                    .selectize-control.single .selectize-input.input-active{
                    background: #fff7f3
                    }
                    
                    .selectize-input.focus{
                    border-color: none;
                    box-shadow: none;
                    }
                    
                    .selectize-dropdown .selected{
                    background-color: #7a0177;
                    }
                    
                    .btn-default{
                    background-color: #fff7f3;
                    border-color: None;
                    color: #0b0b0b;
                    }
                    
                    .btn-default:focus {
                    background-color: #fff7f3;
                    border-color: None;
                    color: #0b0b0b;
                    }
                    
                    .btn-default:hover{
                    background-color: #fff7f3;
                    color: #0b0b0b;
                    }
                    
                    text.highcharts-title,
                    .highcharts-title{
                    font-family: Open Sans, verdana, arial, sans-serif;
                    font-size: 12px;
                    color: #fff7f3;
                    }
                    
                    text {
                    font-family: Open Sans, verdana, arial, sans-serif;
                    color: #fff7f3;
                    }
                    
                    
                    
                    .leaflet .info {
                      background-color: #0b0b0b;
                      color: #fff7f3; /* Legend background color */
                      font-size: 11px;        /* Legend font size */
                    }
                    
                    .leaflet .legend svg text {
                        fill: #fff7f3;
                    }
                    
                    
                    .leaflet-control .legend .legend-title {
                      font-weight: bold;      /* Make title bold */
                    }
                    
                    .leaflet-container .leaflet-control-attribution{
                    font-size: 11px;  /*Changing font size of leaflet copyright*/
                    background: #0b0b0b
                    }
                    
                    .heading{
                    margin-bottom: 48px;
                    padding-right: 56px;
                    padding-left: 56px;
                    text-align: center;
                    color: #fde0dd;
                    }
                    
                    .select-info{
                    margin-bottom: 80px;
                    }
                    
                    .select-info p{
                    margin-bottom: 32px;
                    }
                    
                    
                    .paragraph-class {
                    text-align: justify;
                    }
                    
                    .reset-btn{
                    display: flex;
                    justify-content: flex-end;
                    margin-bottom: 24px;
                    }
                    
                    .title-sankey{
                    font-family: Open Sans, verdana, arial, sans-serif;
                    font-size: 17px;
                    color: #fff7f3;
                    text-align: center;
                    margin-bottom: 0px;
                    }
                    
                    # 
                    # .highchart,
                    # .html-widget,
                    # .html-widget-output,
                    # .shiny-report-size,
                    # .html-fill-item,
                    # .shiny-bound-output{
                    # height: 350px;
                    # }
                    # 
                    # 
                    # #sankeyChart{
                    # height: 350px;
                    # }
                    
                    .sankey-label{
                    display: flex;
                    justify-content: space-between;
                    }
                    
                    .click{
                    font-weight: bold;
                    }
                    
                    .combine_title{
                    text-align: center;
                    font-size: 24px;
                    }
                    
                    .data-source{
                    display: flex;
                    font-size: 9px;
                    }
                   
                    "))
    
  ),
  
 # setBackgroundColor("#160417"),
  fluidRow(
    column(width = 12,
           tags$h1(
             class = "heading",  # Add your custom class here for the h1
             strong("Impact of Socioeconomic Barriers on Cardiovascular 
             Diseases and Healthcare Access")
           ),
           tags$p(
             class = "paragraph-class",  # Adding paragraph class 
             "This visualization provides policymakers with insights into how 
             healthcare coverage access and cost affordability impact health 
             outcomes. The narrative visualization begins by illustrating overall 
             trends in healthcare, followed by an exploration of income 
             disparities in medical cost affordability and access to healthcare 
             coverage. It then highlights the impact of these income disparities 
             on health outcomes, specifically in terms of disease prevalence. 
             Additionally, the visualization offers in-depth, state-level details, 
             enabling policymakers to design targeted interventions based on 
             localized data. This structured approach allows for a holistic
             understanding of how socioeconomic factors influence health, 
             supporting data-informed, region-specific policy decisions."
           ),
    ),
    style = "margin-bottom: 48px"
  ), 
  
  fluidRow(
    column(
      width = 8,
      plotlyOutput("healthCoveragePlot")  # Line Chart
    ),
    column(
      width = 4,
      tags$div(
        class = "select-info",  # Adding class
        
        selectInput(
          inputId = "break_out_category",
          label = "Select Category: ",
          choices = c('Overall', 'Age Group','Race/Ethnicity', 'Gender'),
          selected = "Overall"  # Default Selection
        ),
      ),
      tags$div(
        uiOutput("dynamicText")  # placeholder for dynamic paragraph
      ),
     
    ),
    style = "margin-bottom: 80px"
  ),
 fluidRow(
   column(
     width = 12,
     tags$p(
       class = "paragraph-class",
       "The above graphs clearly indicate an overall increase in the proportion 
       of individuals with healthcare coverage over the years. However, income 
       disparity remains a significant issue, as evidenced by the bubble chart 
       below."     
       ),
     tags$p(
       class = "paragraph-class",
       "The bubble chart illustrates the relationship between income categories 
       and healthcare access. This visualization effectively displays the proportion 
       of individuals with health insurance alongside those unable to afford medical 
       care because of the cost within each income bracket."
     )
   ),
   style = "margin-bottom: 48px"
 ),
 fluidRow(
   column(
     width = 8,
     plotlyOutput("bubbleChart")  # Bubble Chart
   ),
   column(
     width = 4,
     tags$p(
       class = "paragraph-class",
       "The bubble chart illustrates a clear correlation between household 
       income and access to healthcare. As household income rises, the proportion 
       of individuals with health insurance/coverage also increases, while the 
       inability to afford medical care decreases. This trend highlights the 
       significant barriers lower-income households face in accessing affordable 
       healthcare. These findings underscore the urgent need for targeted policies 
       and programs aimed at addressing health disparities related to socioeconomic status.",
       style = "margin-top: 72px"
     )
   ),
   style = "margin-bottom: 80px"
 ),
 fluidRow(
   column(
     width = 12,
     tags$p(
       class = "paragraph-class",
       "The bar chart and Sankey diagram below further illustrate how income 
       disparity impacts healthcare outcomes. On the left, the bar chart displays 
       the proportion of individuals in each income category. Meanwhile, the 
       Sankey diagram on the right reveals a clear trend: as income increases, 
       the prevalence of cardiovascular diseases and associated risk factors, 
       such as high blood pressure, high cholesterol, and diabetes, decreases. 
       When hovering over the disease conditions, the width of the flow paths 
       connecting disease prevalence to income categories increases, emphasizing 
       this relationship. Thus, it can be concluded that income not only influences 
       insurance coverage and medical affordability but also significantly 
       affects the prevalence of diseases."
     )
   ),
   style = "margin-bottom: 48px"
 ),
 
 fluidRow(
   column(
     width = 12,
     tags$div(
       class = "combine_title",
       "The Impact of Income on Cardiovascular and Chronic Disease Prevalence"
     ),
     tags$div(
       class = "click",
       "Click on a Bar"
     )
   ),
   style = "margin-bottom: 32px"
 ), 
 
 fluidRow(
   column(
     width = 5,
     tags$div(
       class = "reset-btn",
       useShinyjs(),
       # code to reset plotlys event_data("plotly_click", source="A") to NULL -> executed upon action button click
       extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-bar_click', 'null'); }",functions=c("resetClick")),
       actionButton("reset", "Reset plot"),
     ),
   )
 ),
 
  fluidRow(
    column(
      width = 5,
      plotlyOutput("incomeBarPlot")  # Bar Chart
    ),
    column(
      width = 7,
      tags$div(
        class = "title-sankey",
        "Impact of Income on Prevalence of CVD and its Risk"
      ),
      highchartOutput("sankeyChart"),
      tags$div(
        class = "sankey-label",
        tags$div(
          "Income Categories"
        ),
        tags$div(
          "Disease Condition"
        )
      )
    ),
    style = "margin-bottom: 80px"
  ),
 fluidRow(
   column(
     width = 12,
     tags$p(
       class = "paragraph-class",
       "For a more detailed view, the choropleth map illustrates the proportion 
       of the low-income population in each state. Accompanying this, the bar graph 
       displays the prevalence of various disease conditions within those states. 
       This visual pairing highlights a critical trend: states with a higher 
       proportion of low-income households tend to exhibit greater prevalence of 
       diseases. By clicking on a specific state on the choropleth map, users 
       can easily correlate the socioeconomic landscape with health outcomes, 
       gaining insights into how economic factors contribute to public health 
       disparities. This relationship underscores the importance of targeted health 
       interventions and policies aimed at addressing the unique challenges faced 
       by low-income populations in different regions."
     ),
     tags$div(
       class = "click",
       "Click on a State"
     )
   ),
   style = "margin-bottom: 48px"
 ),

  fluidRow(
    column(
      width = 8,
      class = "custom-class",   # Apply custom class 
      tags$div(
        class = "title-sankey",
        "Proportion of Low Income Population in each State"
      ),
      leafletOutput("map")
    ),
    column(
      width = 4,
      plotlyOutput("stateplot")
    ),
    style = "margin-bottom: 80px"
  ),
 fluidRow(
   column(
     width = 12,
     tags$p(
       class = "paragraph-class",
       "By understanding the correlation between income levels and disease prevalence, 
       policymakers can design targeted interventions that address the specific 
       needs of communities. This localized approach allows for more effective 
       allocation of resources, ensuring that programs and services are tailored 
       to the demographics and health issues prevalent in each state. "
     )
   ),
   style = "margin-bottom: 64px"
 ),
 fluidRow(
   column(
     width = 12,
     tags$p(
       "Data Source: ",
       style = "font-size: 11px"
     ),
     tags$div(
       class = "data-source",
       tags$p(
         "1. "
       ),
       tags$a(
         href = "https://data.cdc.gov/Heart-Disease-Stroke-Prevention/Behavioral-Risk-Factor-Surveillance-System-BRFSS-N/ikwk-8git/about_data",       # Link of data source
         " Heart Disease & Stroke Prevention", 
         target = "_blank"
       ),  # Opens link in a new tab
       tags$p(
         ", data from Centers for Disease Control and Prevention, updated April 10, 2024 "
       )
     ),
     tags$div(
       class = "data-source",
       tags$p(
         "2. "
       ),
       tags$a(
         href = "https://data.cdc.gov/Behavioral-Risk-Factors/BRFSS-Table-of-Health-Care-Access-Coverage/f7a2-7inb/about_data",       # Link of data source
         " Behavioral Risk Factors ", 
         target = "_blank"
       ),  # Opens link in a new tab
       tags$p(
         ", data from Centers for Disease Control and Prevention, updated September 19, 2024 "
       )
     ),
     tags$div(
       class = "data-source",
       tags$p(
         "3. "
       ),
       tags$a(
         href = "https://catalog.data.gov/dataset/2022-cartographic-boundary-file-shp-united-states-1-20000000",       # Link of data source
         " 2022 Cartographic Boundary File (SHP) ", 
         target = "_blank"
       ),  # Opens link in a new tab
       tags$p(
         ", United States, updated December 14, 2023 "
       )
     )
   )
   
 ),
 style = "margin-bottom: 40px"
)

