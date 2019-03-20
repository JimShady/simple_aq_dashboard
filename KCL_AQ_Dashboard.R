## Libraries
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(openair)
library(xtable)

## Get the data
file_to_view <- read_csv('test_data.csv')


modelled <- file_to_view %>% 
  select(-SiteID, -month, -Year, -Type, -SiteName,- MeasuredNOX, -MeasuredNO2, -MeasuredPM10, -MeasuredPM25, -MeasuredO3) %>% 
  rename_all(tolower) %>%
  gather(key = 'pollutant', value = 'modelled', 3:7) %>%
  mutate(sitetype = tolower(sitetype)) %>%
  mutate(pollutant = gsub('model', '', pollutant),
         sitetype  = case_when(grepl('heathrow'          , sitetype) ~ 'Heathrow',
                               grepl('traffic'           , sitetype) ~ 'Kerbside',
                               grepl('kerbside'          , sitetype) ~ 'Kerbside',
                               grepl('industrial'        , sitetype) ~ 'Industrial',
                               grepl('roadside'          , sitetype) ~ 'Roadside',
                               grepl('suburban'          , sitetype) ~ 'Suburban background',
                               grepl('urban background'  , sitetype) ~ 'Urban background'))

measured <- file_to_view %>% 
  select(-SiteID, -month, -Year, -Type, -SiteName,- ModelNOx, -ModelNO2, -ModelPM10, -ModelPM25, -ModelO3) %>% 
  rename_all(tolower) %>%
  gather(key = 'pollutant', value = 'measured', 3:7) %>%
  mutate(sitetype = tolower(sitetype)) %>%
  mutate(pollutant = gsub('measured', '', pollutant),
         sitetype  = case_when(grepl('heathrow'          , sitetype) ~ 'Heathrow',
                               grepl('traffic'           , sitetype) ~ 'Kerbside',
                               grepl('kerbside'          , sitetype) ~ 'Kerbside',
                               grepl('industrial'        , sitetype) ~ 'Industrial',
                               grepl('roadside'          , sitetype) ~ 'Roadside',
                               grepl('suburban'          , sitetype) ~ 'Suburban background',
                               grepl('urban background'  , sitetype) ~ 'Urban background'))

data     <- left_join(modelled, measured) %>% filter(modelled != 0 & measured != 0)

table_data <- file_to_view %>% rename_all(tolower) %>% select(-siteid, -month, -year, -type, -sitename)

rm(modelled, measured)

#Sidebar
ui <- fluidPage(
  
  # App title ----
 # headerPanel("Plot options"),

  fluidRow(
  # Sidebar panel for inputs ----
    column(4,
    
    ## Choose site type
    pickerInput(input="sitetype",
                label="sitetype:",
                choices=unique(data$sitetype),
                options = list(`actions-box` = TRUE),
                multiple = T,
                selected = unique(data$sitetype)),
    
    ## Choose pollutant
    selectInput("pollutant", "pollutant:",  unique(data$pollutant)),
    
    ## Choose sitecode
    pickerInput(input="sitecode",
                label="sitecode:",
                choices=sort(unique(data$sitecode)),
                options = list(`actions-box` = TRUE),
                multiple = T,
                selected = unique(data$sitecode))
    
  ),
  
  # Main panel for displaying outputs ----
     column(8,
    

    # The plot panel    
    plotOutput("plot", width=700, height=700,
               click = "plot_click",
               dblclick = "plot_dblclick",
               hover = "plot_hover",
               brush = "plot_brush"
    ),
    # The table panel
    verbatimTextOutput("Summary stats"),
    tableOutput("table"),
    
    # The mouse info panel
    tableOutput("info")
    )

  )
)

# Get the thing running
server <- function(input, output) {
  
  ## Title label thing
  output$title <- renderText({
    input$pollutant
  })
  
    ## The plot
  output$plot <- renderPlot(
    {
      ggplot(filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype & sitecode %in% input$sitecode), aes(x = measured, y = modelled, label = sitecode, colour = sitetype)) + 
        geom_point() +
        coord_fixed() +
        xlab('Measured') +
        ylab('Modelled') +
        xlim(-5,
             max(c(filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype & sitecode %in% input$sitecode)$modelled,
                   filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype)$measured))) +
        ylim(-5,
             max(c(filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype & sitecode %in% input$sitecode)$modelled,
                   filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype)$measured))) +
        theme(legend.title     = element_blank(),
              panel.border     = element_blank(),
              axis.title       = element_text(size=14, colour = 'black'),
              axis.text        = element_text(size=14, colour = 'black'),
              legend.text      = element_text(size=12, colour = 'black'),
              legend.position  = 'right',
              legend.spacing.x = unit(0.7, 'cm'),
              plot.margin      = unit(c(0,0,0,0), "cm"),
              panel.spacing     = unit(c(-1,-1,-1,-1), "cm"),
              axis.line        = element_line(colour='black'),
              panel.background = element_rect(fill = 'black'),
              panel.grid.major = element_line(colour = 'grey'),
              panel.grid.minor = element_blank()) +
        geom_abline(intercept = 0, slope = 1, colour='white') + 
        geom_abline(intercept = 0, slope = 2, linetype="dashed", colour='white') +
        geom_abline(intercept = 0, slope = 0.5, linetype="dashed", colour='white') +
        guides(guide_legend(ncol = 4))
      
        })
  
    h3('test')
  
    ## The table data
    output$table <-renderTable({
      filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype & sitecode %in% input$sitecode) %>%
      modStats(mod = 'modelled',
               obs = 'measured') %>%
        as_tibble() %>% select(-default)
                    
    }, align='l')
    
    ## Making a new table from the mouse interactions
    output$info <- renderTable({
      # With base graphics, need to tell it what the x and y variables are.
      clicked_data <- brushedPoints(filter(data, pollutant == input$pollutant, sitetype %in% input$sitetype & sitecode %in% input$sitecode),
                 input$plot_brush)
      select(clicked_data, sitecode, sitetype, modelled, measured)
      # nearPoints() also works with hover and dblclick events
    })
  

}

# Run the thing
shinyApp(ui, server)