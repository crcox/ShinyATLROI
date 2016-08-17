library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("ATL ROI Explorer"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope",
                  "Slope:",
                  min = -2,
                  max = 0,
                  value = 0,
                  step=0.1),
      sliderInput("rotation",
                  "Rotation:",
                  min = -45,
                  max = 45,
                  value = 0,
                  step=5),
      sliderInput("intercept",
                  "AP-intercept:",
                  min = -30,
                  max = 30,
                  value = 45),
      fluidRow(
        verbatimTextOutput("sag_hover_info")
      ),
      fluidRow(
        verbatimTextOutput("cor_hover_info")
      ),
      fluidRow(
        verbatimTextOutput("roi_intersection_by_region_rh")
      ),
      fluidRow(
        verbatimTextOutput("roi_intersection_by_region_lh")
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("sagitalPlot", hover=hoverOpts(id = "sagital_plot_hover")),
      plotOutput("coronalPlot", hover=hoverOpts(id = "coronal_plot_hover")),
      actionButton("save_roi","Export ROI to csv...")
    )
  )
))
