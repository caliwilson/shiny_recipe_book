library(shiny)
library(magrittr)

recipe_df <- readr::read_delim("data/recipe_data.psv", delim = "|")
sources <- unique(recipe_df$Source)
cuisines <- unique(recipe_df$Cuisine)
categories <- unique(recipe_df$Category)
courses <- unique(recipe_df$Course)

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
    "The code for the web app can be found here.",
    h2("Download data"),
    "Download the recipe data as a pipe-separated text file.",
    downloadButton("download", "Download Recipe Book")
  ),
  tabPanel(
    "View Recipes",
    sidebarLayout(
      sidebarPanel(
        h3("Filter Recipes"),
        selectizeInput("fr_source", "Source", sort(sources), selected = sort(sources), multiple = T, options = NULL),
        selectizeInput("fr_course", "Course", sort(courses), selected = sort(courses), multiple = T, options = NULL),
        selectizeInput("fr_category", "Category", sort(categories), selected = sort(categories), multiple = T, options = NULL),
        selectizeInput("fr_cuisine", "Cuisine", sort(cuisines), selected = sort(cuisines), multiple = T, options = NULL),
        sliderInput("fr_rating_range", "Rating Range", min = 0, max = 10, value = c(0, 10), ticks = F),
        sliderInput("fr_difficulty_range", "Difficulty Range", min = 0, max = 10, value = c(0, 10), ticks = F)
      ),
      mainPanel(
        DT::dataTableOutput("filter_recipes_df")
      )
    )
  ),
  tabPanel(
    "Add a Recipe",
    sidebarLayout(
      sidebarPanel(
        h3("Enter Recipe Information"),
        textInput("nr_title", "Title", value = ""),
        textInput("nr_website", "Website", value = ""),
        selectizeInput(
          "nr_source", 
          "Source", 
          choices = sort(sources),
          selected = NULL,
          multiple = T,
          options = list(create = T, maxItems = 1)
        ),
        selectizeInput(
            "nr_course", 
            "Course", 
            choices = sort(courses),
            selected = "Other",
            multiple = T,
            options = list(create = T, maxItems = 1)
        ),
        selectizeInput(
            "nr_category", 
            "Category", 
            choices = sort(categories),
            selected = "Other",
            multiple = T,
            options = list(create = T, maxItems = 1)
        ),
        selectizeInput(
            "nr_cuisine", 
            "Cuisine", 
            choices = sort(cuisines),
            selected = "Other",
            multiple = T,
            options = list(create = T, maxItems = 1)
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
          choices = sort(recipe_df$Title), 
          selected = NULL, multiple = FALSE
        )      
      ),
      mainPanel(
        DT::dataTableOutput("similar_recipes_df")
      )
    )
  )
)

server <- function(input, output, session) {
  output$download <- downloadHandler(
    filename = "recipe_book_data.psv",
    content = function(file) {
      readr::write_delim(
        readr::read_delim("data/recipe_data.psv", delim = "|"),
        file,
        delim = "|"
      )
    }
  )
  observeEvent(
    c(
      input$fr_source,
      input$fr_course,
      input$fr_category,
      input$fr_cuisine,
      input$fr_difficulty_range,
      input$fr_rating_range
    ),
    {
      output$filter_recipes_df <- DT::renderDataTable(
        {
          df <- readr::read_delim("data/recipe_data.psv", delim = "|")
          df %>% 
            dplyr::select(Recipe = Website, Source, Course, Category, Cuisine, Rating, Difficulty) %>% 
            dplyr::filter(
              Source %in% input$fr_source,
              Course %in% input$fr_course,
              Category %in% input$fr_category,
              Cuisine %in% input$fr_cuisine,
              dplyr::between(Difficulty, input$fr_difficulty_range[1], input$fr_difficulty_range[2]),
              dplyr::between(Rating, input$fr_rating_range[1], input$fr_rating_range[2])
            ) %>% 
            DT::datatable(rownames = F, escape = F, style = "bootstrap")  %>% 
            DT::formatStyle(
              'Rating',
              color = DT::styleInterval(c(3, 7), c('red', 'white', 'lightgreen'))
            )
        }, server = F
      )
    }
  )
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
                    'Similarity scores provided by Gower\'s Similarity Coefficient <a href=\"http://members.cbio.mines-paristech.fr/~jvert/svn/bibli/local/Gower1971general.pdf\" target=\"_blank\" class=\"class2\">(Gower 1971)</a>'
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
        dplyr::rename(Recipe = Website) %>% 
        DT::datatable(rownames = FALSE, escape = FALSE, style = "bootstrap") %>% 
        DT::formatStyle(
          'Rating',
          color = DT::styleInterval(c(3, 7), c('red', 'white', 'lightgreen'))
        )
    }, server = FALSE
  )
  recipes_df_proxy <- DT::dataTableProxy('recipes_df')
  observeEvent(
    input$nr_submit, 
    {
      # Update data
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
      new_df <- new_row %>% 
        dplyr::bind_rows(
          readr::read_delim("data/recipe_data.psv", delim = "|")
        ) 
      readr::write_delim(new_df, "data/recipe_data.psv", delim = "|")
      
      # Update UI
      new_titles <- sort(new_df$Title)
      new_sources <- sort(unique(new_df$Source))
      new_cuisines <- sort(unique(new_df$Cuisine))
      new_categories <- sort(unique(new_df$Category))
      new_courses <- sort(unique(new_df$Course))
      
      updateTextInput(session, "nr_title", value = "")
      updateTextInput(session, "nr_website", value = "")
      updateSelectizeInput(session, "nr_source", selected = NULL, options = list(create = TRUE))
      updateSelectizeInput(session, "nr_course", choices = new_courses, selected = "Other", options = list(create = TRUE))
      updateSelectizeInput(session, "nr_category", choices = new_categories, selected = "Other", options = list(create = TRUE))
      updateSelectizeInput(session, "nr_cuisine", choices = new_cuisines, selected = "Other", options = list(create = TRUE))
      updateSliderInput(session, "nr_rating", value = 5)
      updateSliderInput(session, "nr_difficulty", value = 5)
      updateSelectizeInput(session, "fr_source", choices = new_sources, selected = new_sources)
      updateSelectizeInput(session, "fr_course", choices = new_courses, selected = new_courses)
      updateSelectizeInput(session, "fr_category", choices = new_categories, selected = new_categories)
      updateSelectizeInput(session, "fr_cuisine", choices = new_cuisines, selected = new_cuisines)
      updateSelectizeInput(session, "sr_title", choices = new_titles)
      
      # Update table
      recipes_df_proxy %>% DT::addRow(new_row)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
