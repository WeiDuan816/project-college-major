library(shiny)

visualization_page <- tabPanel(
  "Visualizations",
  titlePanel(h1("Visualizations", align = "center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_category", label = "Select Category to Compare Majors",
                  c("Income", "Employment Rates")),
         checkboxGroupInput("major_category", label = "Choose Majors to Visualize",
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

conclusion_page <- tabPanel(
  "Conclusion",
  titlePanel(h1("Conclusion", align = "center")),
  fluidPage(
    h4("Is there an income disparity between different majors or major subjects?"),
    p("Yes"),
    h4("Is there an employment disparity between different majors or major subjects?"),
    p("Yes"),
    h4("Limitations"),
    p("There are other potential confounding variables. For example, it has been well 
      documented that there is a wage gap between men and women, so a major that is 
      predominately male would most likely have a greater average income than the same 
      exact major if it were majority female. Additionally, race and national origin has 
      a similar effect, as some races are more likely to get paid more many or hired then 
      other races. The hope is that using a large dataset would help eliminate these 
      effects, but if a major has a different race composition than another major, it 
      could have an effect on the results."),
    h4("Areas for More Research or Analysis"),
    p("Evaluating and analyzing different majors while taking into account the various 
      confounding factors. Additionally, looking at potential market fluctuations that 
      influence the supply and demand of specific jobs or majors. These fluctuations could 
      come from many different trends including technology, improved education, global 
      warming, and other social, scientific, and political trends.")
  )
)

ui <- navbarPage(
  "Hierarchy in College Majors",
  visualization_page,
  conclusion_page
)

server <- function(input, output) {
  
}

shinyApp(ui, server)
