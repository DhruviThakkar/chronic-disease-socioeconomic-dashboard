#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# PLEASE INSTALL FOLLOWING PACKAGES IF YOU NEED TO

# install.packages("shiny")
# install.packages("htmltools")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("sfarrow")
# install.packages("leaflet")
# install.packages("sf")
# install.packages("highcharter")
# install.packages("RColorBrewer")
# install.packages("shinyjs")
# install.packages("jsonlite")
# install.packages("shinyWidgets")

library(shiny)
library(htmltools)
library(dplyr)
library(plotly)
library(sfarrow)
library(leaflet)
library(sf)
library(highcharter)
library(RColorBrewer)
library(shinyjs)
library(shinyWidgets)

geo_data <- st_read_parquet("DVP_33322910_data.parquet")  # Reading .csv file

dataframe <- geo_data %>% filter(Topic!='LowIncome' )
dataframe <- dataframe %>% st_set_geometry(NULL)
geo_data <- geo_data %>% filter(Topic == 'LowIncome'& Locationdesc!='Puerto Rico')
high_col <- dataframe %>% filter(Topic == "Cholesterol High", Response=="Yes")
high_bp <- dataframe %>% filter(Topic == "High Blood Pressure", Response=="Yes")
dia <- dataframe %>% filter(Topic == "Diabetes", Response=="Yes")
cardio <- dataframe %>% filter(Topic == "Cardiovascular Disease", Response=="Yes")
combined_data <- bind_rows(high_col, high_bp, dia, cardio)

# Defining server logic

function(input, output, session) {
  
  # Create a reactive expression to filter and summarize the data based on the 
  # selected Break Out Category
  
  healthcare_coverage_summary <- reactive({
    
    # Filtering the data frame based on the selected category
    
    healthcare_coverage_data <- dataframe %>%  
      filter(Topic == "Health Care Coverage",
             Response == "Yes",
             Break_Out_Category == input$break_out_category)  # using the selected category
    
    # Grouping by Year and calculate the mean Data_vale
    
    healthcare_coverage_data %>%
      group_by(Year = as.integer(Year), Break_Out) %>%
      summarise(mean_coverage = mean(Data_value, na.rm = TRUE))
    
  })
  
  # Define custom colors for each Break_Out category
  line_colors <- c(
    "Overall" = "#f768a1",
    "Female" = "#fa9fb5",
    "Male" = "#c51b8a",
    "18-24" = "#ae017e",
    "25-34" = "#dd3497",
    "35-44" = "#f768a1",
    "45-54" = "#fa9fb5",
    "55-64" = "#fcc5c0",
    "65+" = "#feebe2",
    "American Indian" = "#fff7f3",
    "Asian" = "#fde0dd",
    "Black" = "#fcc5c0",
    "Hispanic" = "#fa9fb5",
    "Multiracial" = "#f768a1",
    "Native Hawaiian" = "#dd3497",
    "Other" = "#ae017e",
    "White" = "#7a0177"
  )
  
  # Rendering the line chart
  
  output$healthCoveragePlot <- renderPlotly({
    
    # Preparing data for hover text
    
    data_summary <- healthcare_coverage_summary()
    
    # Dynamically changing the title using input$break_out_category
    if (is.null(input$break_out_category) || input$break_out_category == "Overall") {
      line_plot_title <- "Overall Trends in Healthcare Coverage (2011 - 2022)"
    } else {
      line_plot_title <- paste("Trends in Healthcare Coverage by", input$break_out_category, "(2011 - 2022)")
    }
    
    plot_ly(
      data = data_summary,
      x = ~Year,
      y = ~mean_coverage,
      color = ~Break_Out,  # using Break_out for stratified lines
      colors = line_colors, # applying custom colors
      type = 'scatter',
      mode = 'line+markers',
      line = list(color = ~Break_Out),
      marker = list(size=8),
      text = ~paste("Year:", Year, 
                    "<br>Coverage (%):", round(mean_coverage, 2)), # customizing hover text
      hoverinfo = "text",  # using above text on hover
      hoverlabel = list(
        bgcolor = "#fff7f3", # background color for tooltip
        font = list(
          color = "#0b0b0b",  # font color
          size = 11  # font size
        )
      )
    ) %>%
      layout(
        
        title = list(
          text = line_plot_title,
          font = list(color = "#fff7f3")
        ),
        xaxis = list(
          tickvals = seq(2011, 2022),  # setting tick values and text
          ticktext = seq(2011, 2022),
          title = list(
            text = "Year", 
            font = list(color = "#fff7f3")
          ),
          tickfont = list(color = "#fff7f3")
          ),
        yaxis = list(
          title = list(
            text = "Proportion of People with Healthcare Coverage (%)",
            font = list(color = "#fff7f3")
          ),
          tickfont = list(color = "#fff7f3")
        ),
        showlegend = TRUE, # Show legend for stratified lines
        legend = list(
          font = list(
            color = "#fff7f3"  # Change legend font color here
          )
        ),
        plot_bgcolor="#0b0b0b",
        paper_bgcolor = "#0b0b0b"
        
      )
  })
  
  # chaning text based on the selected category
  output$dynamicText <- renderUI({
    text <- switch(input$break_out_category,
                   "Overall" = "The line graph titled illustrates the proportion 
                   of individuals with access to healthcare coverage over the 
                   past 12 years. The data indicates a consistent increase in 
                   access to healthcare coverage, rising from approximately 82% 
                   in 2011 to around 92% in 2022.",
                   "Age Group" = "The graph depicts the proportion of individuals 
                   with healthcare coverage over a 12-year period, stratified by 
                   age group. The data reveals an overall upward trend in coverage 
                   across most age groups; however, younger adults, aged 18-24 and 
                   adults aged 25-34 consistently exhibit lower coverage rates. 
                   In contrast, older age groups have maintained generally higher 
                   coverage rates throughout the entire period, highlighting a 
                   disparity in access between younger and older populations.",
                   "Race/Ethnicity" = "The graph illustrates the proportion of individuals 
                   with healthcare coverage over a 12-year period, categorized by race and ethnicity.
                   Overall, there has been an upward trend in healthcare coverage for most racial and ethnic groups 
                   from 2011 to 2022. The White, Asian and Amerian Indian population consistently exhibited the 
                   highest proportion of coverage throughout this period. In contrast, the Hispanic population 
                   had lower coverage rates compared to their White and Asian counterparts. While progress has been 
                   made in enhancing 
                   healthcare coverage for various racial and ethnic groups, significant disparities remain, 
                   indicating a need for targeted interventions to address the specific needs these populations.",
                   "Gender" = "The graph shows the proportion of individuals with healthcare coverage over a 12-year period, 
                   categorized by gender. Overall, both males and females have experienced an upward trend in healthcare 
                   coverage during this time.  While the coverage rates for both genders have risen, there has been 
                    a consistent gender gap, with women generally having higher rates of healthcare coverage 
                    compared to men throughout the time period."
                   )
    
    # Returning the updated paragraph as a p tag
    tags$p(
      class = "paragraph-class",  # Adding paragraph class 
      text
    )
  })
  
  output$bubbleChart <- renderPlotly({
    
    # Define your desired order for income categories
    desired_order <- c("Less than $15,000", "$15,000-$24,999", "$25,000-$34,999", 
                       "$35,000-$49,999", "$50,000-$99,999", "$100,000-$199,999", 
                       "$150,000-$199,999", "More than $200,000")
    
    # Preparing data 
    df_afford_care <- dataframe %>%
      filter(Question == "Afford_Care", Response=="Yes", Break_Out_Category == "Household Income")
    
    df_afford_care_summary <- df_afford_care %>%
      group_by(Break_Out, Response) %>%
      summarise(average_value = round(mean(Data_value, na.rm = TRUE), 2))
    
    # Summarize health coverage by income group
    df_health_cover <- dataframe %>%
      filter(Question == "Adults_Health_Coverage", Response == "Yes", Break_Out_Category == "Household Income") %>%
      group_by(Break_Out, Response) %>%
      summarise(average_value = round(mean(Data_value, na.rm = TRUE), 2))
    
    # Adding a new column to distinguish between the two datasets
    df_afford_care_summary <- df_afford_care_summary %>%
      mutate(Category = "Inability to Afford Medical Care")
    
    df_health_cover <- df_health_cover %>%
      mutate(Category = "Health Coverage")
    
    # Combine both summaries into one data frame
    combined_data_cover <- bind_rows(df_afford_care_summary, df_health_cover)
    
    combined_data_cover <- combined_data_cover %>%
      mutate(Break_Out = factor(Break_Out, levels = desired_order))
    
    # Create custom hover text for each point
    combined_data_cover <- combined_data_cover %>%
      mutate(hover_text = paste("Income Group: ", Break_Out,
                                "<br>Category: ", Category,
                                "<br>Average Value: ", round(average_value, 2), "%"))
    
    #Creating bubble chart
    
    plot_ly(data = combined_data_cover, 
            x = ~Break_Out, 
            y = ~average_value, 
            size = ~average_value*2 , 
            color = ~Category, 
            colors = c("#fa9fb5", "#c51b8a"),
            type = 'scatter',
            mode = 'markers',
            text = ~hover_text,  # Use the custom hover text
            hoverinfo = 'text',  # Display only the custom hover text
            hoverlabel = list(
              bgcolor = "#fff7f3", # background color for tooltip
              font = list(
                color = "#0b0b0b",  # font color
                size = 11  # font size
              )
            ),
            marker = list(opacity = 1, line = list(width = 0.5, color = 'black'))) %>%
      layout(
        title = list(
          text = "Medical Care Affordability and Health Coverage by Household Income",
          font = list(color = "#fff7f3")
        ),
             xaxis = list(
               title = list(
                 text = "Household Income", 
                 font = list(color = "#fff7f3")
               ),
               tickfont = list(color = "#fff7f3")
              ),
             yaxis = list(
               title = list(
                 text = "Proportion (%)",
                 font = list(color = "fff7f3")
               ),
               tickfont = list(color = "#fff7f3")
             ),
             showlegend = TRUE,
              legend = list(
                font = list(
                  color = "#fff7f3"  # Change legend font color here
                )
              ),
             plot_bgcolor="#0b0b0b",
             paper_bgcolor = "#0b0b0b")
  })
  
  # creating a reactive expression to summarize income data
  
  income_summary <- reactive({
    income_data <- dataframe %>% 
      filter(Topic == "Income", Break_Out_Category == "Overall") %>%
      group_by(Response) %>%
      summarise(total_people = sum(Data_value, na.rm = TRUE)) %>%
      mutate(proportion = total_people / sum(total_people) * 100) %>% # Proportion in %
      ungroup()
    
    # Specifying the desired order of income categories
    income_order <- c("Less than $15,000", "$15,000-$24,999", "$25,000-$34,999", 
                      "$35,000-$49,999", "$50,000-$99,999", "$100,000-$199,999",
                      "More than $200,000")
    
    # Converting 'Response' column into a factor with the specified order
    income_data$Response <- factor(income_data$Response, levels = income_order)
    
    return(income_data)
  })
  
  # Reactive value to store the hovered income category
  #hovered_income_category <- reactiveVal(NULL)
  
  
  
  # Rendering the bar chart for income
  output$incomeBarPlot <- renderPlotly({
    income_data <- income_summary()
    
    color_palette <- c(
      "Less than $15,000" = "#7a0177",
      "$15,000-$24,999" = "#ae017e",
      "$25,000-$34,999" = "#dd3497",
      "$35,000-$49,999" = "#f768a1",
      "$50,000-$99,999" = "#fa9fb5",
      "$100,000-$199,999" = "#fcc5c0",
      "More than $200,000" = "#feebe2"
    )
    
    # Assign colors based on the Response category
    bar_colors <- color_palette[income_data$Response]
    
    # bar_colors <- sapply(income_data$Response, function(cat) {
    #   
    #   base_color <- color_palette[cat]
    #   if (!is.null(hovered_income_category()) ) {
    #     if( hovered_income_category() == cat){
    #       # If this bar is hovered, use full opacity
    #       adjustcolor(base_color, alpha.f = 1)
    #     }
    #     else {
    #       # Reduce opacity for non-hovered bars
    #       adjustcolor(base_color, alpha.f = 0.1)
    #     }
    #   }else{
    #     color_palette[cat]
    #   }
    # })
    
    plot_ly(
      data = income_data,
      x = ~proportion,
      y = ~Response,
      source = "bar_click",
      type = 'bar',
      orientation = 'h',
      marker = list(color = bar_colors),
      hovertemplate = paste(
        "<b>Household Income:</b> %{y}<br>",
        "<b>Proportion:</b> %{x:.2f}%<extra></extra>"
      ),
      hoverlabel = list(
        bgcolor = "#fff7f3", # background color for tooltip
        font = list(
          color = "#0b0b0b",  # font color
          size = 11  # font size
        )
      )
    ) %>%
      layout(
        title = list(
          text = "Proportion of people in each Income Category (2011 - 2022)",
          font = list(color = "#fff7f3")
        ),
        xaxis = list(
          title = list(
            text = "Proportion (%)", 
            font = list(color = "#fff7f3")
          ),
          tickfont = list(color = "#fff7f3")
        ),
        yaxis = list(
          title = list(
            text = "Household Income",
            font = list(color = "#fff7f3")
          ),
          tickfont = list(color = "#fff7f3")
        ),
        margin = list(l = 150),
        plot_bgcolor="#0b0b0b",
        paper_bgcolor = "#0b0b0b"
      )
  })
  
  # Update the hovered category when hovering over a bar
  # observeEvent(event_data("plotly_hover", source = "bar_click"), {
  #   event <- event_data("plotly_hover", source = "bar_click")
  #   
  #   if (!is.null(event)) {
  #     hovered_income_category(event$y)  # Store the hovered income category
  #   }
  # })
  # 
  # # Update the hovered category when hovering over a bar
  # observeEvent(event_data("plotly_unhover", source = "bar_click"), {
  #   hovered_income_category(NULL)  
  #   event <- event_data("plotly_unhover", source = "bar_click")
  #   print(event)
  #   
  # })
  
  # observeEvent(event_data("plotly_hover", source = "bar_click"), {
  #   event <- event_data("plotly_hover", source = "bar_click")
  #   if (!is.null(event)) {
  #     print(event$y)
  #   }
  # })
  # 
  
  
  # Initialize click_income_category as a reactive value
  click_income_category <- reactiveVal(NULL)
  
  # Observe the click event to update the reactive value
  observeEvent(event_data("plotly_click", source = "bar_click"), {
    click_data <- event_data("plotly_click", source = "bar_click")
    
    if (!is.null(click_data)) {
      # Set the reactive value based on the clicked income category
      click_income_category(click_data$y)
    }
  })
   observeEvent(input$reset, {
     
     js$resetClick()
     click_income_category(NULL) 
   })
   output$sankeyChart <- renderHighchart({
     combined_data$Break_Out <- factor(combined_data$Break_Out, 
                                       levels = c(
                                         "More than $200,000",
                                         "$100,000-$199,999",
                                         "$50,000-$99,999",
                                         "$35,000-$49,999",
                                         "$25,000-$34,999",
                                         "$15,000-$24,999", 
                                         "Less than $15,000"
                                       ))
     combined_data <- combined_data %>%
       arrange(Break_Out)
     # Prepare the data
     sankey_data <- combined_data %>%
       filter(Break_Out_Category == 'Household Income') %>%
       group_by(Break_Out, Topic) %>%
       summarise(mean_value = round(mean(Data_value, na.rm = TRUE), 2))  # Calculate mean Data_value for each combination
     
     df_sankey <- sankey_data %>% 
       select(Break_Out, Topic, mean_value) %>%
       rename(from = Break_Out, to = Topic, weight = mean_value)
     
     node_colors <- c(
       "Less than $15,000" = "#7a0177",
       "$15,000-$24,999" = "#ae017e",
       "$25,000-$34,999" = "#dd3497",
       "$35,000-$49,999" = "#f768a1",
       "$50,000-$99,999" = "#fa9fb5",
       "$100,000-$199,999" = "#fcc5c0",
       "More than $200,000" = "#feebe2",
       "Cardiovascular Disease" = "#feebe2",
       "Cholesterol High" = "#fbb4b9",
       "Diabetes" = "#f768a1",
       "High Blood Pressure" = "#ae017e"
     )
     
     unique_nodes <- unique(c(as.character(df_sankey$from), df_sankey$to))
     
     # Create the Sankey diagram
     highchart() %>%
       hc_chart(type = "sankey") %>%
       hc_add_series(
         data = list_parse(df_sankey),  # Convert the dataframe to a list
         type = "sankey",
         name = "Flow",
         nodes = lapply(unique_nodes, function(node) {
           list(
             id = node,
             color =if (is.null(click_income_category())){
               as.character(node_colors[node])
             } else{
               if (node == click_income_category()) {
               adjustcolor(as.character(node_colors[node]), alpha.f = 1)
             } 
               else {
               adjustcolor(as.character(node_colors[node]), alpha.f = 0.1)  # Reduce opacity to 0.1
             }
               }
           )
         })
       ) %>%
       # hc_title(text = "Impact of Income on Prevalence of CVD and its Risk",
       #          style = list(color = "#fff7f3")) %>%  # changing font color 
       hc_tooltip(
         useHTML = TRUE,
         pointFormat = '<b>{point.from}</b> to <b>{point.to}</b>: <br>{point.weight}',
         style = list(
           color = "#0b0b0b" # Customize text color
         ),
         backgroundColor = "#fff7f3"  # Set tooltip background color
        
       ) 
   })

  
  # Defining custom colors
  custom_colors <- c("#ae017e", "#f768a1", "#fbb4b9", "#feebe2")
  
  # Define custom intervals (replace with your desired break points)
  custom_intervals <- c(8, 11, 14, 17, 20)  # For example, intervals of 10%
  
  # Use colorBin with custom intervals
  paletteBin <- colorBin(palette = custom_colors, 
                         domain = geo_data$Data_value, 
                         bins = custom_intervals)
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     scrollWheelZoom = FALSE,
                                     doubleClickZoom = FALSE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng = -96.25, lat = 38.50, zoom = 4) %>%
      addPolygons(data = geo_data,
                  color = "black", # border of state
                  weight = 1,  # weight of state borders
                  smoothFactor = 1.5,
                  fillOpacity = 1,
                  fillColor = ~paletteBin(geo_data$Data_value), # Use binned color palette,
                  layerId = geo_data$Locationdesc,
                  label = ~htmlEscape(
                    paste0(
                      "State: ", geo_data$Locationdesc, 
                      ", Percentage: ", round(geo_data$Data_value, 2)
                    )
                  ),
                  labelOptions = labelOptions(
                    style = list(
                      "color" = "#fff7f3",            # Set font color
                      "font-weight" = "bold",       # Set font weight
                      "font-size" = "11px",         # Set font size
                      "background-color" = "#ae017e"   # Set label background color
                    )
                  )
                  # Add hover text for popups
                  # Add tooltip for hover
                  ) %>% 
      # Add the legend for the color scale
      addLegend("bottomright", 
                pal = paletteBin, 
                values = geo_data$Data_value, 
                title = "Percentage (%)",
                opacity = 1,
                labFormat = labelFormat(prefix = ""),
                ) 
  })
  
  observeEvent(input$map_shape_click, {
    clicked_state <- input$map_shape_click$id
    
    output$stateplot <- renderPlotly({
      
      d_palette <- c(
        "Cardiovascular Disease" = "#7a0177",
        "Cholesterol High" = "#c51b8a",
        "Diabetes" = "#f768a1",
        "High Blood Pressure" = "#fbb4b9"
      )
      
      
      
      prevalence_data <- combined_data %>% 
        filter(Locationdesc==clicked_state & Break_Out %in% c(
          "Less than $15,000", "$15,000-$24,999"
          )) %>% 
        group_by(Locationdesc,Topic) %>% 
        summarise(Data_value = mean(Data_value))
      
      # Assign colors based on the Response category
      d_bar_colors <- d_palette[prevalence_data$Topic]
      
      bar_title <- paste("Prevanlence (%) of Disease/Condition in", clicked_state)
      
      plot_ly(
        data = prevalence_data,
        x = ~Topic,          # X-axis is the Topic (disease names)
        y = ~Data_value,     # Y-axis is the averaged data value
        type = 'bar',        # Bar chart
        marker = list(color = d_bar_colors),  # Custom color for bars
        hovertemplate = paste(
          "<b>Prevalence :</b> %{y:.2f}%<extra></extra>"
        ),
        hoverlabel = list(
          bgcolor = "#fff7f3", # background color for tooltip
          font = list(
            color = "#0b0b0b",  # font color
            size = 11  # font size
          )
        )
      ) %>%
        layout(
          title = list(
            text = bar_title,
            font = list(color = "#fff7f3")
          ),
          xaxis = list(
            title = list(
              text = "Disease Condition", 
              font = list(color = "#fff7f3")  # Label for the x-axis
            ),
            tickfont = list(color = "#fff7f3")  
          ),
          yaxis = list(
            title = list(
              text = "Prevalance (%)", 
              font = list(color = "#fff7f3")
            ),
            tickfont = list(color = "#fff7f3")  # Label for the y-axis
          ),
          plot_bgcolor="#0b0b0b",
        paper_bgcolor = "#0b0b0b"
        )
    })
    

    
  })
  
}


