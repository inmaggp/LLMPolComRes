## Script to replicate the analysis of the main project developed in Markdown.

library(writexl)
library(openxlsx)
library(scales)
library(dplyr)
library(stringr)
library(stringi)
library(RColorBrewer)
library(ggplot2)
library(caret)
library(viridis)
library(tibble)
library(readr)
library(irr)
library(readxl)
library("text")
textrpp_install()
textrpp_initialize() # Press 1 in the console
library(sentencepiece)

## Load data
main_data <- read_excel("Data/Main Data.xlsx",sheet='Final')

#change the variable name, same name, sequence, to the results of transformers
names(main_data)[names(main_data) == "Full Text"] <- 'sequence'


## Function to clean text
clean_text <- function(text) {
  # Convert to uppercase: consistency, pattern recognition and reduction of complexity
  text <- tolower(text)
  # Remove encoding artifacts (UTF-8 misinterpreted issues)
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  text <- stri_replace_all_regex(text, "[\\p{So}\\p{Cn}]", "")  # Elimina símbolos y caracteres desconocidos
  text <- str_replace_all(text, "\\b(i'm)\\b", "i am")
  text <- str_replace_all(text, "\\b(you're)\\b", "you are")
  text <- str_replace_all(text, "\\b(he's)\\b", "he is")
  text <- str_replace_all(text, "\\b(she's)\\b", "she is")
  text <- str_replace_all(text, "\\b(it's)\\b", "it is")
  text <- str_replace_all(text, "\\b(we're)\\b", "we are")
  text <- str_replace_all(text, "\\b(they're)\\b", "they are")
  #text <- str_replace_all(text, "[^\x01-\x7F]", "")
  # text <- str_replace_all(text, "[\\p{So}\\p{Cn}]", "")
  # Replace contractions for the verb 'to have'
  text <- str_replace_all(text, "\\b(i've)\\b", "i have")
  text <- str_replace_all(text, "\\b(you've)\\b", "you have")
  text <- str_replace_all(text, "\\b(he's)\\b", "he has")
  text <- str_replace_all(text, "\\b(she's)\\b", "she has")
  text <- str_replace_all(text, "\\b(it's)\\b", "it has")
  text <- str_replace_all(text, "\\b(we've)\\b", "we have")
  text <- str_replace_all(text, "\\b(they've)\\b", "they have")
  # Remove user mentions
  #text <- str_remove(text, "^rt\\s+@\\S+\\s*")  
  #text <- str_remove_all(text, "@\\S+")
  # Remove hashtags
  #text <- str_remove_all(text, "#\\S+")
  # Remove URLs
  text <- str_remove_all(text, "http\\S+|www\\S+")
  # New line to remove everything after 'HTTP' until a space
  text <- str_remove_all(text, "HTTP\\S*") # Removes everything from 'https:' until the first space
  # Remove problematic characters (single quotes, double quotes, apostrophes, etc.)
  text <- str_remove_all(text, "[\"'`´^¨]")
  # Remove non-alphanumeric characters, except spaces
  #    text <- str_remove_all(text, "[^\\w\\s]")
  # Remove non-alphanumeric characters **EXCEPT @ and #**
  text <- str_replace_all(text, "[^\\w\\s@#]", "")
  # Remove extra spaces
  text <- str_squish(text)
  
  return(text)
}


# Apply cleaning to the "sequence" column
main_data$clean_text <- clean_text(main_data$sequence)
if (anyNA(main_data$sequence)) {
  warning("The 'sequence' column contains missing values (NA). It is recommended to handle these values before proceeding.")
  # Optional: Remove rows with missing values
  main_data <- main_data[!is.na(main_data$sequence), ]
}

main_data$clean_text <- ifelse(main_data$clean_text == "", "empty sentence", main_data$clean_text)

# Uncomment to save as xlsx 
#write_xlsx(list("Final" = main_data), "Data/Main Data.xlsx")


# Define labels, prompts and model
labels= c("Conspiratorial Logic", "The Economy in General","Donald Trump","Joe Biden","Democrats","Republicans","MAGA","Jews and Antisemitism in the US","Healthcare","Reproductive Rights","Homelessness","Immigration","Climate Change","Electric Vehicles","Elections","January 6 Insurrection","Race Relations","Resistance to Social Change or Traditional Values")
hypothesis=c("This text is about {}.")
models=c("facebook/bart-large-mnli")


ZSC_main <- function(main_data, labels, hypothesis) {
for (m in seq_along(models)) {
  for (h in seq_along(hypothesis)) {
    for (l in seq_along(labels)) {
      # name of the variable for each classification
      variable_name <- paste0("h", h, "_l", l)
      main_data[[variable_name]] = 0
      # textZeroShot for each combination of parameters
      results <- textZeroShot(
        sequences = main_data$clean_text,
        candidate_labels = labels[l],
        hypothesis_template = hypothesis[h],
        multi_label = FALSE,
        model = models[m],
        device = "cpu",
        tokenizer_parallelism = FALSE,
        logging_level = "error",
        return_incorrect_results = FALSE,
        set_seed = 1001L)
        main_data[[variable_name]]<-results$scores_x_1
      }
  }
}
  return(main_data)
}

#1-4400 sin problema
pilot_data_1_200 <- ZSC_main(main_data[1:200, ], labels, hypothesis)
# Uncomment to save pilot_data_1_200
write_xlsx(pilot_data_1_200,'Data/main_data_1_200.xlsx')
