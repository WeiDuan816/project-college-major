library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
library(scales)
library(stringr)

df <- read.csv("major.csv", stringsAsFactors = FALSE) %>%
  group_by(Major_category) %>%
  summarize(total = sum(Total),
            employed = sum(Employed),
            employedFull = sum(Employed_full_time_year_round),
            unemployed = sum(Unemployed)
            )
df[nrow(df) + 1, ] = c("All Categories", sum(df$total), sum(df$employed),
                       sum(df$employedFull), sum(df$unemployed)) 
df <- mutate(df, unemploymentRate = round(as.double(unemployed) / as.double(total), 4) * 100) %>%
  mutate(category_abbr = word(Major_category, 1))

home_page <- tabPanel(
  "Home", 
  titlePanel(h1("Hierarchy In College Majors", align = "center")), 
  fluidPage(
    h4("Introduction"), 
    p("Many people believe that majors can be categorized in one of two ways:
      the ones that make money and the ones that don't. Because of this
      categorization, a hierarchy has been made of the major system, where
      the ones that lead to the highest paying jobs are on top and everything
      else is beneath them. The problem that we want to analyze is this
      currently existing hierarchy and whether or not your major determines
      your income / employment status upon graduating. Through data, we hope
      to investigate whether all the emphasis behind the importance of picking
      particular majors to ensure higher incomes is valid."),
    p("This issue affects many people, including college students, professors,
      universities, and companies that hire employees directly out of college.
      Often times these hirings use college major as an initial screening
      process, but interviews also influence these decisions. Because of this,
      more and more students are congregating to majors that 'make money' such
      as STEM majors, and away from majors in the arts. Oftentimes, this
      decreases the funding that particular departments receive, which can
      affect the quality of their program."),
    h4("Our Data Analysis Plan"),
    p("We analyzed the problem by visualizing how different categories of
      majors correlate to after-graduation employment opportunities, and how
      the income levels vary among these categories. To do this, we used a bar
      graph that compares the incomes of each major category that you choose
      with each other. We also used pie charts for each individual major
      category that compares employment and unemployment rates after graduation
      within that category.")
    )
    )

visualization_page <- tabPanel(
  "Visualizations",
  titlePanel(h1("Visualizations", align = "center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_category", label = "Select Category to Compare Majors",
                  c("Employment Rates", "Income")),
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
      plotOutput("majorPlot"),
      p("These specific data visualizations methods are effective to represent the 
        data major surrounding majors because they are continous (income),
        and rates (employment). However, the rates can be converted into continuous 
        variables by multiplying the rate by the total number of people in a major.")
    )
  )
)

data_page <- tabPanel(
  "Data",
  titlePanel(h1("Data", align = "center")),
  fluidPage(
    h4("College Majors Dataset"),
    p("This dataset provides information about recently graduated college students,
      their majors, and their employment status.
      It has 79,668,7996 observations and 11 attributes."),
    h4("Graduate Student Majors Dataset"),
    p("This dataset provides information about both recently graduated college students
      and those attending graduate school. It provides information about their majors and
      their employment status. It has 59,233,874 observations and 22 attributes."),
    h4("Who created it?"),
    p("The data were collected and organized by United States' Census Bureau"),
    h4("Why was it created?"),
    p("A college degree is never a guarantee of economic success.
      But researchers believe that through careful choice of major,
      students can take at least some steps toward boosting their odds.
      Research has also shown that many schools do little push students to
      make informed choices about what to study."),
    h4("Where did you access it?"),
    p("The data can be downloaded from the website - census.gov,
      and we accessed through github."),
    h4("What represents an “observation”?"),
    p("Each observation represents a specific major, which contains information including
      number of people, employment, income earnings, etc."),
    h4("What “variables” does each observation have?"),
    p("Specific majors, major catgories, number of people employed and unemployed,
      and median, 25th percentile, 75th percentile income earnings. In addition,
      graduate students dataset also has comparable data values for people
      who do not have a graduate degree."),
    h4("How big is the data set?
       If it is a sub-set of a larger data set, how was the sub-set created?"),
    p("The college major dataset contains 79,668,7996 observations and 11 attributes,
      and the graduate students dataset contains 59,233,874 observations and
      22 attributes."),
    h4("If the data set represents people,
       who has been included and who has been excluded?"),
    p("Undergraduate and graduate students were included.")
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
  home_page,
  visualization_page,
  data_page,
  conclusion_page
)

server <- function(input, output) {
  output$majorPlot <- renderPlot({
    categories <- input$major_category
    categories[length(categories) + 1] = "All Categories"
    plotdf <- df[match(categories, df$Major_category), ]
    
    ggplot() + geom_bar(aes(y = unemploymentRate, x = category_abbr, fill = as.double(total)),
                        data = plotdf, stat = "identity") +
      geom_text(data = plotdf, aes(x = category_abbr, y = unemploymentRate,
                label = paste0(unemploymentRate, "%")), size = 5, color = "white",
                vjust = 1.5) +
      labs(x = "Major Category", y = "Unemployment Rate", fill = "Total Students in Major") +
      ggtitle("Comparison of Majors by Unemployment Rate")
  })
}

shinyApp(ui, server)
