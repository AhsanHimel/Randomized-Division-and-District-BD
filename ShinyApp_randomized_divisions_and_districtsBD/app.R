library(rvest)
library(shiny)
library(tidyverse)

content <- read_html("https://en.wikipedia.org/wiki/Districts_of_Bangladesh")
tables <- content %>% html_table(fill = TRUE)
dist_table <- tables[[2]][,-2]
dist_table <- separate(data = dist_table, col = District,
                       into =  c("District", "Status"), sep = "\\s(\\w+)$")
districts <- c(dist_table$District)
divisions <- unique(dist_table$Division)


RandLocation <- function(division_no, district_no, seed){
    # setting seed if given
    if (!missing(seed)) set.seed(seed)
    
    number_of_divi = division_no
    number_of_dist = district_no
    
    # randomly assigning districts to each divisions from the table
    rand <- by(dist_table$District, 
               dist_table$Division, 
               sample, size = number_of_dist, replace = F)
    
    tab <- data.frame(Division = character(0),
                      Districts = character(0))
    row <- 0
    for(i in sample(divisions, size = number_of_divi)){
        # selecting divisions
        tab[row+1,1] <- i
        tab[row+1,2] <- paste(rand[[i]], collapse = ", ")
        row <- row + 1
    }
    return(tab)
}

ui <- fluidPage(
    tags$div(id="title",
             titlePanel("Randomized Divisions and Districts in Bangladesh", 
                        windowTitle = "Random Locations in BD")
    ),
    h3("Md. Ahsanul Islam"),
    hr(), 
    tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(
            HTML("
           #title {text-align: center; color: #922B21 }
           h3 {text-align: center; font-size: 16px}
           ")
        )
    ),
    sidebarLayout(
        position = "left",
        
        sidebarPanel(
            selectInput(inputId = "division",
                        label = "Number of Divisions",
                        # value = 2, min = 1, max = 8, step = 1
                        choices = c(1:8), selected = 4, size = 8, selectize = F
            ),
            selectInput(inputId = "district",
                        label = "Number of Districts",
                        # value = 3, min = 1, max = min(table(dist_table$Division)),
                        choices = c(1:min(table(dist_table$Division))), 
                        selected = 3, size = min(table(dist_table$Division)), 
                        selectize = F
            ),
            numericInput(inputId = "seed",
                         label = "Select Seed:",
                         value = round(runif(1, -10000, 100000)), min = -Inf, max = Inf,
                         step = 1
            )
        ),
        
        mainPanel(
            tableOutput(outputId = "result")
        )
    )
)


server <- function(input, output) {
    
    output$result <- renderTable({ RandLocation(division_no = input$division, 
                                                district_no = input$district,
                                                seed = input$seed) })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
