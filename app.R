library(shiny)
library(magrittr)

recipe_df <- readr::read_delim("data/recipe_data.psv", delim = "|")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "A Shiny Recipe Book",
  theme = shinythemes::shinytheme("slate"),
  tabPanel(
    "About",
    h1("Welcome!"),
    h2("A modern book of recipes"),
    "You know those family recipe books? Of course you do. These books are a great way to pass down beloved (or not) family recipes over generations. However, these books can become damaged or lost and the recipes can become outdated due to changes in food culture and/or lack of digital accessibility.",
    "This web app is to serve as a modern recipe book - a way to save digital recipes over time. In addition, it's a way to record general information about those recipes to help inspire meals.",
    h2("Development information"),
    "This web app was built using R, Shiny, and DT - special thanks to Winston Chang for Yihue Xie for their work in developing these packages.",
    "The code for the web app can be found here."
  ),
  tabPanel(
    "View Recipes"
  ),
  tabPanel(
    "Add a Recipe",
    sidebarLayout(
      sidebarPanel(
        h3("Enter Recipe Information"),
        textInput("nr_title", "Title", value = ""),
        textInput("nr_website", "Website", value = ""),
        textInput("nr_source", "Source", value = ""),
        selectInput(
            "nr_course", 
            "Course", 
            choices = c("Appetizer", "Breakfast", "Dessert", "Entree", "Side", "Snack", "Other"),
            selected = "Other"
        ),
        selectInput(
            "nr_category", 
            "Category", 
            choices = c(
              "Bowl", "Burger", "Salad", "Pasta", "Pizza", "Sandwich", "Quesadilla", 
              "Taco", "Soup", "Other", "Rice", "Risotto"),
            selected = "Other"
        ),
        selectInput(
            "nr_cuisine", 
            "Type", 
            choices = c(
              "American", "Asian", "British/Irish", "Cajun", "Italian", 
              "Mediterranean", "Mexican", "Southern", "Other"
            ),
            selected = "Other"
        ),
        sliderInput("nr_rating", "Rating", min = 0, max = 10, value = 5, round = T, ticks = F),
        sliderInput("nr_difficulty", "Difficulty", min = 0, max = 10, value = 5, round = T, ticks = F),
        actionButton("nr_submit", "Submit")
      ),
      mainPanel(
        DT::dataTableOutput("recipes_df")
      )
    )
  ),
  tabPanel(
    "Find Similar Recipes",
    sidebarLayout(
      sidebarPanel(
        h3("Select A Recipe"),
        selectizeInput(
          "sr_title", 
          "Title", 
          choices = recipe_df$Title, 
          selected = NULL, multiple = FALSE
        )      
      ),
      mainPanel(
        DT::dataTableOutput("similar_recipes_df")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(
    input$sr_title,
    {
    output$similar_recipes_df <- DT::renderDataTable(
      {
        html_to_title <- function(x) {
          stringr::str_sub(
            x, 
            stringr::str_locate(x, ">")[1] + 1, 
            stringr::str_locate(x, "</")[1] - 1
          )
        }
        html_to_title_v <- Vectorize(html_to_title)
        df <- readr::read_delim("data/recipe_data.psv", delim = "|")
        df %>% 
          dplyr::select(Title, Source, Course, Category, Cuisine, Rating, Difficulty) %>% 
          dplyr::mutate_if(is.character, as.factor) %>% 
          cluster::daisy(metric = "gower", stand = TRUE) %>% 
          as.matrix() %>% 
          tibble::as_tibble() %>% 
          magrittr::set_colnames(df[['Website']]) %>% 
          dplyr::mutate(Recipe1 = df[['Website']]) %>%
          tidyr::gather("Recipe2", "Gower", -Recipe1) %>% 
          dplyr::filter(
            html_to_title_v(Recipe1) == input$sr_title,
            Gower != 0
          ) %>% 
          dplyr::arrange(Gower) %>% 
          dplyr::mutate(Similarity = round(1 - Gower, 4)) %>% 
          dplyr::select(Recipe = Recipe2, Similarity) %>% 
          DT::datatable(
            rownames = FALSE, 
            escape = FALSE, 
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: center;',
              htmltools::withTags(
                div(
                  HTML(
                    'Similarity scores provided by Gower\'s Similarity Coefficient <a href=\"http://members.cbio.mines-paristech.fr/~jvert/svn/bibli/local/Gower1971general.pdf\" target=\"_blank\" class=\"class2\">Gower (1971)</a>'
                  )
                )
              )
            ),
            style = "bootstrap"
          )
        }, server = FALSE
      )
    }
  )
  output$recipes_df <- DT::renderDataTable(
    {
      df <- readr::read_delim("data/recipe_data.psv", delim = "|")
      df %>% 
        dplyr::select(-Title) %>% 
        DT::datatable(rownames = FALSE, escape = FALSE, style = "bootstrap") %>% 
        DT::formatStyle(
          'Rating',
          color = DT::styleInterval(c(3, 7), c('red', 'white', 'lightgreen'))
        )
    }, server = FALSE
  )
  recipe_df_proxy <- DT::dataTableProxy('recipes_df')
  observeEvent(
    input$nr_submit, 
    {
      new_row <- tibble::tibble(
          Title = input$nr_title,
          Website = paste0('<a href="', input$nr_website, '" target="_blank" class="class2">', input$nr_title,'</a>'),
          Source = input$nr_source,
          Course = input$nr_course,
          Category = input$nr_category,
          Cuisine = input$nr_cuisine,
          Rating = input$nr_rating,
          Difficulty = input$nr_difficulty
      )
      new_row %>% 
        dplyr::bind_rows(
          readr::read_delim("data/recipe_data.psv", delim = "|")
        ) %>% 
        readr::write_delim("data/recipe_data.psv", delim = "|")
      recipes_df_proxy %>% DT::addRow(new_row)
      updateTextInput(session, "nr_title", value = "")
      updateTextInput(session, "nr_website", value = "")
      updateTextInput(session, "nr_source", value = "")
      updateSelectInput(session, "nr_course", selected = "Other")
      updateSelectInput(session, "nr_category", selected = "Other")
      updateSelectInput(session, "nr_cuisine", selected = "Other")
      updateSliderInput(session, "nr_rating", value = 5)
      updateSliderInput(session, "nr_difficulty", value = 5)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
