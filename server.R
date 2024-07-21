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

# clean up df to be in right format for figure 
df2 <- rename(df, "Sample size" = N) #rename var name 
df2$`Publication year` <- as.numeric(df2$`Publication year`) #make numeric 
df2$Grouping <- as.factor(df2$Grouping) #make factor
df2 <- rename(df2, 'Group' = Grouping)

# count number of studies per year 
df4 <- df2 %>%
  group_by(`Publication year`) %>%
  mutate(count_per_year = n())

# create prevalence figure per infection type 
df5 <- df4 %>%
  group_by(Group) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count / sum(count) * 100)

# create data results
data_ib <- data.frame(
  brain_outcome = c('Cerebral lesions', 'White matter lesions', 'White matter microstructure', 'White matter microstructure', 'Anterior cingulate cortex'),
  finding = c('Significant finding', 'Significant finding' ,'Significant finding', 'Null finding', 'Significant finding'),
  modality = c('sMRI', 'sMRI', 'DTI', 'DTI', 'fMRI'),
  count = c(120, 144 ,481, 129, 76),
  author_names = c('Duggan et al, Tian et al', 'Duggan et al, Yoon et al' ,'Conole et al, Sanders et al, Rasmussen et al', 'Conole et al' ,'Spann et al, Goldstein et al'),
  developmental_period = c('Infancy, infancy', 'Infancy, infancy', 'Infancy (preterm), infancy, childhood', 'Infancy', 'Infancy, Adulthood')
)
data_ib$modality <- factor(data_ib$modality, levels = c("sMRI", "DTI", "fMRI"))
data_ib$finding <- factor(data_ib$finding)

data_ca <- data.frame(
  brain_outcome = c('White matter microstructure', 'White matter microstructure', 'White matter lesions', 'White matter lesions', 'Cerebral lesions', 'Cerebral lesions'),
  finding = c('Significant finding', 'Null finding','Significant finding', 'Null finding','Significant finding', 'Null finding'),
  modality = c('DTI', 'DTI', 'sMRI', 'sMRI', 'sMRI', 'sMRI'),
  count = c(210, 92, 573, 157, 740, 388),
  author_names = c('Anblagan et al, Gaudet et al, Pogribna et al', 'Chau et al', 'Basu et al, Qin et al, Xu et al, Shevell et al', 'Baud et al, Chau et al, Basu et al', 'Budal et al, Jain et al, Jenster et al', 'Granger et al, Qin et al, Xu et al'),
  developmental_period = c('Infancy (preterm), Infancy (preterm), Infancy (preterm)', 'Infancy (preterm)', 'Infancy (preterm), infancy (preterm), infancy (preterm), childhood (term)', 'Infancy (preterm), Infancy (preterm), infancy (preterm)', 'Infancy (preterm), infancy (preterm), infancy (term)', 'Infancy (preterm), infancy (preterm), infancy (preterm)')
)
data_ca$modality <- factor(data_ca$modality, levels = c("sMRI", "DTI"))
data_ca$finding <- factor(data_ca$finding)

data_ci <- data.frame(
  brain_outcome = c('Cerebral lesions', 'Cerebral lesions', 'White matter lesions', 'White matter lesions'),
  finding = c('Significant finding', 'Null finding', 'Significant finding', 'Null finding'),
  modality = c('sMRI'),
  count = c(320, 58, 505, 464),
  author_names = c('Kurokawa et al, McKissic et al, Zeng et al, Capretti et al, Oosterom et al, Roee et al, Diebler et al, Turner et al', 'Roee et al, Virkola et al', 'McKissic et al, Capretti et al, Diebler et al', 'Dammann et al'),
  developmental_period = c('Infancy, infancy, infancy, infancy, infancy, infancy,infancy,infancy', 'Infancy, infancy', 'Infancy, infancy, infancy', 'Infancy')
)
data_ci$modality <- factor(data_ci$modality, levels = c("sMRI"))
data_ci$finding <- factor(data_ci$finding)

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
