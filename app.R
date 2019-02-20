library(shiny)
library(tidyverse)
library(lubridate)
library("here")


# Hack to force the app to London time. 
# Can make this not hard-coded if anyone ever needs that.

time_zone <- "Europe/London" 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Magic Addiction App"),
  
  # 
  actionButton("magic", "I cast too much magic"),
  actionButton("drama", "I use a drama point to stop this madness"),
  
  # 
  mainPanel(
    tableOutput("addiction")
  )
)

master_addiction_deck <- read_lines(here("data/mage.txt"))
addiction_duration <- seconds(70)

duration_to_mm_ss <- function(duration){
  duration <- as.numeric(duration)
  
  minutes <- if_else(duration<Inf, floor(duration/60), NA_real_)
  seconds <- if_else(duration<Inf, duration %% 60, NA_real_)
  
  if_else(duration<Inf, 
          paste0(minutes, ":", seconds),
          "Expired")
}


server <- function(input, output, session) {
  
  react <- reactiveValues()
  
  if(!file.exists(here("data/results_table.rds"))){
    results_table <- tibble(
      addict = c("placeholder"),
      end_time = c(now(time_zone) - addiction_duration)
    ) %>% filter(addict != "placeholder") %>%
      write_rds(here("data/results_table.rds"))
  }
  
  react$results_table <- read_rds(here("data/results_table.rds"))
  
  
  if(!file.exists(here("data/personal_deck.rds"))){
    personal_deck <- master_addiction_deck %>%
      write_rds(here("data/personal_deck.rds"))
  }
  
  react$personal_deck <- read_rds(here("data/personal_deck.rds"))
  
  observeEvent(input$magic, {
   new_addiction <- sample(length(personal_deck), 1)
   
   react$results_table <- bind_rows(
     react$results_table,
     tibble(
       addict = personal_deck[new_addiction],
       end_time = now(time_zone) + addiction_duration
     )
   ) %>%
     write_rds(here("data/results_table.rds"))
   
   react$personal_deck <- personal_deck[-new_addiction] %>%
     write_rds(here("data/personal_deck.rds"))
   
   if(length(personal_deck)== 0){
     react$personal_deck <- master_addiction_deck %>%
       write_rds(here("personal_deck.rds"))
   }
   
  }
  )
  
  observeEvent(input$drama,{
    react$results_table <- react$results_table %>%
      mutate(end_time = if_else(
        end_time > now(time_zone), end_time - addiction_duration, end_time
      )) %>%
      write_rds(here("data/results_table.rds"))
  })
  
  output$addiction <- renderTable({
    
    invalidateLater(1000, session)
    react$results_table %>%
      mutate(time_remaining = 
               difftime(end_time, now(time_zone), units = "secs") %>%
               round() %>%
               if_else(.>0, ., Inf)
      ) %>%
      arrange(time_remaining) %>%
      mutate(time_remaining = duration_to_mm_ss(time_remaining)) %>%
      mutate(end_time = paste0(end_time)) 
  })
}


# Run the application 
shinyApp(ui = ui, server = server)