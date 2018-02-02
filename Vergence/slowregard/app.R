library(ggplot2)
library(dplyr)
library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)
library(shiny)





model<- load_model_hdf5('C:/Users/setup/Dropbox/deeplearning/slowregard1/SlowRegard1.hd5')
text<- readRDS('C:/Users/setup/Dropbox/deeplearning/slowregard1/slowregard1text.RDS')
chars<- readRDS('C:/Users/setup/Dropbox/deeplearning/slowregard1/slowregard1chars.RDS')


maxlen<-20
sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

diversity=1

# poem_length=as.integer(runif(n=1,min=200,max=2000))
poem_length=2000
# for(diversity in c(0.80)){  

cat(sprintf("diversity: %f ---------------\n\n", diversity))

start_index <- sample(1:(length(text) - maxlen), size = 1)
sentence <- text[start_index:(start_index + maxlen - 1)]
generated <- ""

#alternative way to seed with custom string:
sentence<-"just what was a wheel thinking"
# sentence<-"hey liz, what's up??" #must be 20 characters
# sentence<-"can't feel\nyour face"
cat(sentence)
sentence<-strsplit(sentence,split=NULL)[[1]]

# diversity=1

for(i in 1:poem_length){
  
  x <- sapply(chars, function(x){
    as.integer(x == sentence)
  })
  x <- array_reshape(x, c(1, dim(x)))
  
  preds <- predict(model, x)
  next_index <- sample_mod(preds, diversity)
  next_char <- chars[next_index]
  
  generated <- str_c(generated, next_char, collapse = "")
  sentence <- c(sentence[-1], next_char)
  
}

library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel('Slow Regard of Deep Learning'),
  mainPanel(
    tags$style(type="text/css", "#Output_text {white-space: pre-wrap;}"),
    textOutput("Output_text")
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$Output_text <- renderText({ 
   generated
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
