# libraries
library(shiny)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shinythemes)

# data
df <- readxl::read_excel("./excel_figure3.xlsx")
data_ib <- readxl::read_excel("data_ib.xlsx")
data_ca <- readxl::read_excel("data_ca.xlsx")
data_ci <- readxl::read_excel("data_ci.xlsx")

# server
server <- function(input, output) {
  output$data_table <- DT::renderDataTable({
    total_df <- readxl::read_excel("./total_excel_for_shiny_app.xlsx")
    DT::datatable(total_df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$plotPubTrend <- renderPlotly({
    df4 %>%
      plot_ly(x = ~`Publication year`, y = ~count_per_year, type = 'bar', color = ~Group, colors = "Paired") %>%
      layout(title = "A. Study Distribution by Publication Year",
             xaxis = list(title = "Publication Year"),
             yaxis = list(title = "Number of Studies"))
  })
  
  output$plotPieChart <- renderPlotly({
    df5 %>%
      plot_ly(labels = ~Group, values = ~Percentage, type = 'pie') %>%
      layout(title = "B. Study Distribution by MIA type")
  })
  
  output$plotSampleSize <- renderPlotly({
    df4 %>%
      plot_ly(x = ~`Publication year`, y = ~`Sample size`, type = 'scatter', mode = 'markers',
              color = ~Group, colors = "Paired", marker = list(size = 10),
              text = ~paste("First Author:", `First author`, "<br>Sample Size:", `Sample size`),
              hoverinfo = "text") %>%
      layout(title = "C. Study Distribution by Sample Size",
             xaxis = list(title = "Publication Year"),
             yaxis = list(title = "Sample Size", range = c(0, input$yAxisLimitC)))
  })
  
  output$plot_ib <- renderPlotly({ plot_ly(data_ib, x = ~brain_outcome, y = ~count, type = 'bar', color = ~finding, colors = c("Significant finding" = "mediumpurple3", "Null finding" = "seagreen3"),
                                           text = ~paste('Author Names:', author_names, '<br>Developmental Period:', developmental_period, '<br>Imaging modality:', modality, '<br>N:', count),
                                           hoverinfo = 'text', split = ~finding) %>%
      layout(title = "Summary of findings for exposure: inflammatory biomarkers",
             xaxis = list(title = "Brain Outcome"),
             yaxis = list(title = "Number of participants"),
             barmode = 'group')
  })
  
  output$plot_ca <- renderPlotly({ plot_ly(data_ca, x = ~brain_outcome, y = ~count, type = 'bar', color = ~finding, colors = c("Significant finding" = "mediumpurple3", "Null finding" = "seagreen3"),
                                           text = ~paste('Author Names:', author_names, '<br>Developmental Period:', developmental_period, '<br>Imaging modality:', modality,'<br>N:', count),
                                           hoverinfo = 'text', split = ~finding) %>%
      layout(title = "Summary of findings for exposure: chorioamnionitis",
             xaxis = list(title = "Brain Outcome"),
             yaxis = list(title = "Number of participants"),
             barmode = 'group')
  })
  
  output$plot_ci <- renderPlotly({ plot_ly(data_ci, x = ~brain_outcome, y = ~count, type = 'bar', color = ~finding, colors = c("Significant finding" = "mediumpurple3", "Null finding" = "seagreen3"),
                                           text = ~paste('Author Names:', author_names, '<br>Developmental Period:', developmental_period, '<br>Imaging modality:', modality,'<br>N:', count),
                                           hoverinfo = 'text', split = ~finding) %>%
      layout(title = "Summary of findings for exposure: common infections",
             xaxis = list(title = "Brain Outcome"),
             yaxis = list(title = "Number of participants", range = c(0, input$yAxisLimitD)),
             barmode = 'group')
  })
}
