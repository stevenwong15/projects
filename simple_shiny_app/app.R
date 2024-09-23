library(data.table)
library(magrittr)
library(ggplot2)
library(shiny)

DT <- fread("CollegeAdmissions_Data.csv")[, .(
	college_name = name,
	parent_income_percentile = factor(
		par_income_bin,
		levels = par_income_bin,
		labels = par_income_lab),
	relative_attendance_rate = rel_attend
	)]
college_names <- unique(DT$college_name)

ui <- pageWithSidebar(
  headerPanel('College Attendance Rate by Family Income'),
  sidebarPanel(
    selectInput('college_name', 'College Name', college_names)
  ),
  mainPanel(
    plotOutput('plot')
  )
)

server <- function(input, output) {
	output$plot <- renderPlot({
		DT[college_name == input$college_name] %>%
			ggplot(aes(
				x = parent_income_percentile, 
				y = relative_attendance_rate, 
				group = 1)) +
			geom_line() + 
			geom_point() + 
			expand_limits(y = 0) + 
			labs(
				x = "Parent Income Percentile",
				y = "Relative Attendance Rate")
	})
}

shinyApp(ui = ui, server = server)

# rsconnect::deployApp("~/Documents/projects/simple_shiny_app/")
