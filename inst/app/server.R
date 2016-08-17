library('shiny')
library('dplyr')
library('svGUI')
library('ShinyATLROI')

data("temporal_lobes_atlas")
data("temporal_lobes_key")

atlas <- temporal_lobes_atlas
key <- temporal_lobes_key

sagital_projection <- sagital_project(atlas, key)
head(sagital_projection)
# Define server logic required to draw plot
R <- reactiveValues()
shinyServer(function(input, output) {
  reactiveA <- reactive({
    coronal_plane(atlas, input, R)
  })
  output$sagitalPlot <- shiny::renderPlot({
    sagitalPlot(sagital_projection, reactiveA(), input)
  })
  output$coronalPlot <- shiny::renderPlot({
    coronalPlot(reactiveA(), input)
  })
  output$sag_hover_info <- shiny::renderPrint({
      cat("Sagital Region under cursor:\n")
      print(
        unique(sagital_projection[
        round(as.numeric(input$sagital_plot_hover$x)) == sagital_projection$y &
          round(as.numeric(input$sagital_plot_hover$y)) == sagital_projection$z &
          input$sagital_plot_hover$panelvar1 == sagital_projection$hemisphere
        ,'value_full'])
      )
  })
  output$cor_hover_info <- shiny::renderPrint({
    if (abs(input$slope) > 0) {
      m <- 1/input$slope
      if (abs(m) > 1) {
        dp <- filter(R$coronal_slice,y==roi_boundary_y)
      } else {
        dp <- filter(R$coronal_slice,z==roi_boundary_z)
      }
    } else {
      dp <- filter(R$coronal_slice,y==ceiling(input$intercept))
    }
    cat("Coronal Region under cursor:\n")
    print(
      unique(dp[
      round(as.numeric(input$coronal_plot_hover$x)) == dp$x &
        round(as.numeric(input$coronal_plot_hover$y)) == dp$z
      ,'value_full'])
    )
  })
  output$roi_intersection_by_region_rh <- shiny::renderPrint({
    cat("Intersection with ROI by region (Right Hemisphere):\n")
    roi_info <- R$coronal_slice %>%
      filter(hemisphere=='right') %>%
      group_by(value) %>%
      summarize(
        size=n(),
        count_in_roi=sum(y<=roi_boundary_y),
        pct_in_roi=count_in_roi/size
      ) %>% ungroup()
    print(roi_info)
  })
  output$roi_intersection_by_region_lh <- shiny::renderPrint({
    cat("Intersection with ROI by region (Left Hemisphere):\n")
    roi_info <- R$coronal_slice %>%
      filter(hemisphere=='left') %>%
      group_by(value) %>%
      summarize(
        size=n(),
        count_in_roi=sum(y<=roi_boundary_y),
        pct_in_roi=count_in_roi/size
      ) %>% ungroup()
    print(roi_info)
  })
  observeEvent(input$save_roi, {
    roi <- dplyr::filter(R$coronal_slice, y<=roi_boundary_y)
    roi <- dplyr::select(roi, x,y,z,as.numeric(value))
    filepath <- svDialogs::dlgSave(default="~/", title="Save ATL ROI as...", filters=svDialogs::dlgFilters["All",])$res
    write.csv(x=roi,file = filepath,quote = FALSE,row.names = F)
  })
})
