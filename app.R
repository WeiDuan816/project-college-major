library(shiny)

visualization_page <- tabPanel(
  "Visualizations",
  titlePanel("Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("data_category", label = "Select Category",
                  c("Comparison of Majors by Income", "Employment Rates of Majors")),
      selectInput("major_category", label = "Choose Major to Visualize",
                  c("Agriculture & Natural Resources", "Arts","Business",
                    "Biology & Life Science", "Communications & Journalism",
                    "Computers and Mathematics", "Education", "Engineering", "Health",
                    "Humanities & Liberal Arts", "Industrial Arts & Consumer Services",
                    "Interdisciplinary", "Law & Public Policy", "Physical Sciences",
                    "Psychology & Social Work", "Social Science")
        
      )
    ),
    mainPanel(
      p("These specific data visualizations methods are effective to represent the 
        data major surrounding majors because they are continous (income),
        and rates (employment).")
    )
  )
)

ui <- navbarPage(
  "Hierarchy in College Majors",
  visualization_page
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
