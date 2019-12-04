
library(shiny)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(ggthemes)
library(extrafont)
library(scales)
library(stringr)
library(lintr)

about_page <- tabPanel(
  "About",
  titlePanel(h1("About Project", align = "center")),
  fluidPage(
    h4("Overview"),
    p("This is a project that focuses on using shiny ot analyze and present information about the Congress API
      This API includes things such as members, votes, bills and nominations. Its purpose is to present all data related to
      congressional activity. This task will also present two visualizations of this data in order to better show to the viewer
      what the data means in a way that they can understand. This shiny application is meant to show a mastering of shiny and the 
      ability to take a raw data set and turn in into something that you can present."),
    h4("Affiliation"),
    p("Kathleen Dolan INFO-201A:Technical Foundations of Informatics, The Information School, University of Washington, Autumn 2019"),
    h4("Reflective Statement"),
    p("The most challenging aspect of this project for me was figuring out the right way to present the data in the visualizations, because there were so many options
      and it was challinging to figure out what visualizations summarized the data the best in a way that was easy for someone to understand and observe who was not familiar
      with this data set. It was challenging because there was so much available data and categories that deciding which would be more impactful was then more difficult.")
    )
)

visualization_page <- tabPanel(
  "Querys by State",
  titlePanel(h1("States Representitives", align = "center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("State", label = "State",
                  c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont", "Virginia","Washington","West Virginia","Wisconsin","Wyoming")),
    ),
    mainPanel(
      p("These pages show the various descriptions of the representitives for each state in which the members are representitives for.")
    )
  )
)

summary_page <- tabPanel(
  "Summary",
  titlePanel(h1("Summary Stats", align = "center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", label = "Select State to compare representitives",
                  c("Representitives by Gender", "Representitives by Party")),
      checkboxGroupInput("State", label = "Choose State to
                            Visualize",
                         c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont", "Virginia","Washington","West Virginia","Wisconsin","Wyoming")
      )
    ),
    mainPanel(
      plotOutput("plot1"),
      p("These specific data visualizations are representitive of the distributions of both gender and party amoung the representitves for specific states.")
    )
  )                       
 )

ui <- navbarPage("About Project",
                 about_page,
                 visualization_page,
                 summary_page
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    categories <- input$state
    categories[length(categories) + 1] <- "All States"
    plotdf <- df[match(categories, df$state), ]
    
    ggplot() + geom_bar(aes(y = female, x = "female",
                            fill = as.double(total)),
                        data = plotdf, stat = "identity")+
      labs(x = "Gender", y = "Count",
           fill = "Total Members in the House") +
      ggtitle("Comparison of Gender of House Reps by State")
  })
}



shinyApp(ui, server)

