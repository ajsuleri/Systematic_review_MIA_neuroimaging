library(shiny)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shinythemes)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
  /* General Body Styling */
  body {
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; /* Modern, clean font */
    background-color: #f4f6f9; /* Light background color */
    color: #333333; /* Dark text color for better readability */
  }

  /* Customize tab color */
  .nav-tabs > li > a {
    background-color: #004080; /* Dark blue background */
    color: #ffffff; /* White text */
    border: none; /* No border */
    font-weight: bold; /* Bold text for better readability */
    transition: background-color 0.3s ease, color 0.3s ease; /* Smooth transition for background color and text color */
  }

  /* Active tab customization */
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover {
    background-color: #002244; /* Slightly darker blue on active */
    color: #ffffff;
    border-bottom: solid 3px #ffffff; /* Thicker bottom border on active */
  }

  /* Panel borders and padding */
  .tab-content {
    border: solid 2px #003366; /* Dark blue border */
    padding: 20px; /* Increased padding */
    border-radius: 10px; /* More rounded corners for the panel */
    background-color: #ffffff; /* White background for better contrast */
    box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); /* Subtle shadow for a lifted look */
  }

  /* Ensure all main panels have a consistent layout and spacing */
  .tab-pane {
    margin-top: 20px; /* Increased space between tabs and content */
  }

  /* Customizing the sidebar */
  .well {
    border: 1px solid #003366; /* Dark blue border for sidebar */
    background-color: #f8f9fa; /* Lighter gray background for contrast */
    padding: 20px; /* Increased padding */
    border-radius: 10px; /* Rounded corners for sidebar */
    box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); /* Subtle shadow for a lifted look */
  }

  /* Customize action button */
  .btn {
    background-color: #003366; /* Dark blue background */
    color: #ffffff; /* White text */
    border: none; /* No border */
    padding: 12px 24px; /* Padding for a larger button */
    font-size: 16px; /* Larger font size */
    font-weight: bold; /* Bold text */
    border-radius: 5px; /* Rounded corners */
    transition: background-color 0.3s ease, transform 0.3s ease; /* Smooth transition for background color and transform */
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); /* Subtle shadow */
  }

  .btn:hover {
    background-color: #002244; /* Slightly darker blue on hover */
    transform: scale(1.05); /* Slightly larger on hover */
  }

  /* Customize help text */
  .help-block {
    color: #003366; /* Dark blue text */
    font-size: 14px; /* Slightly larger font size */
    line-height: 1.5; /* Increased line height for better readability */
    margin-top: 10px; /* Margin for spacing */
  }

  /* Customize slider */
  .irs-bar {
    background-color: #003366; /* Dark blue slider bar */
    border: 1px solid #003366; /* Dark blue border */
  }

  .irs-bar-edge {
    background-color: #003366; /* Dark blue slider bar edge */
    border: 1px solid #003366; /* Dark blue border */
  }

  .irs-single {
    background-color: #004080; /* Darker blue for the single handle */
    color: #ffffff; /* White text */
    border-radius: 5px; /* Rounded corners */
  }

  .irs-grid-text {
    color: #333333; /* Dark text for grid */
  }

  /* Customize data table */
  .dataTables_wrapper .dataTables_length,
  .dataTables_wrapper .dataTables_filter,
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_paginate {
    color: #333333; /* Dark text for data table controls */
  }

  .dataTables_wrapper .dataTables_paginate .paginate_button {
    background-color: #003366; /* Dark blue background */
    color: #ffffff !important; /* White text */
    border: none; /* No border */
    transition: background-color 0.3s ease; /* Smooth transition for background color */
  }

  .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
    background-color: #002244; /* Slightly darker blue on hover */
  }

  /* Tooltip styling */
  .plotly .modebar-btn:hover {
    background-color: #003366 !important; /* Dark blue background */
    color: #ffffff !important; /* White text */
  }
"))
  ),
  titlePanel("The Association Between Maternal Immune Activation and Brain Structure and Function in Human Offspring: A Systematic Review"),
  sidebarLayout(
    sidebarPanel(
      helpText("Hi reader, this Shiny app provides detailed visualizations for figures 2-5 of the following manuscript: 'The association between maternal immune activation and brain structure and function in human offspring: a systematic review'. Check out the paper: https://www.nature.com/articles/s41380-024-02760-w. Queries about the manuscript or app can be sent to: a.suleri@erasmusmc.nl."),
      actionButton("show_details", "Show/Hide Details"),
      conditionalPanel(
        condition = "input.show_details % 2 == 1",
        tags$p("Figure 2 from the manuscript is split into descriptive figure A-C in here which represents the publication trends and sample sizes. Figures 3-5 from the main manuscript are detailed in Result plots A-C. These plots provide more interactive details that supplement the information within the manuscript. Moving the mouse to the upper right corner of a plot will show additional interactive features of the plot."),
        sliderInput("yAxisLimitC", "Set Y-axis limit for sample size (Descriptives C):", min = 50, max = 2500, value = 50, step = 100)
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table: Studies included in review", DT::dataTableOutput("data_table")),
                  tabPanel("Descriptives (figure 2): A. Publication Trend", plotlyOutput("plotPubTrend")),
                  tabPanel("Descriptives (figure 2): B. Study Distribution", plotlyOutput("plotPieChart")),
                  tabPanel("Descriptives (figure 2): C. Sample Size Trend", 
                           fluidRow(column(8, plotlyOutput("plotSampleSize")),
                                    column(4, textOutput("numericSampleSize")))),
                  tabPanel("Results (figure 3): A. Qualitative synthesis for MIA exposure - inflammatory biomarkers", plotlyOutput("plot_ib")),
                  tabPanel("Results (figure 4): B. Qualitative synthesis for MIA exposure - chorioamnionitis", plotlyOutput("plot_ca")),
                  tabPanel("Results (figure 5): C. Qualitative synthesis for MIA exposure - common infections", 
                           fluidRow(column(8, plotlyOutput("plot_ci")),
                                    column(4, textOutput("numericSampleSize"))))
      )
    )
  )
)
