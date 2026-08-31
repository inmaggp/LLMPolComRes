#### Functions
library(openxlsx)
library(readr)
library(readxl)
library("text")
textrpp_install()
textrpp_initialize()
textrpp_initialize(save_profile = FALSE)
library(sentencepiece)
#pilot_data <- read_excel("Data/Pilot Data.xlsx",sheet='Complete Data All Issues')

#change the variable name, same name, sequence, to the results of transformers
names(pilot_data)[names(pilot_data) == "Tweets based"] <- 'sequence'

#Set the hypothesis and labels to clasify
labels= c("Immigration")#, "Health Care", "Foreign Affairs",'Elections',"Civil Rights","Culture","Trump","Climate Change")
hypothesis=c("This text is about {}.")#, "This text focuses on {}.", "The main subject of this text is {}", "This text is related to {}")



ZSC <- function(pilot_data, labels, hypothesis) {
  for (h in seq_along(hypothesis)) {
    for (l in seq_along(labels)) {
      # Name of the variable for each classification
      variable_name <- paste0("h", h, "_l", l)
      # Perform zero-shot classification for each combination
      result <- textZeroShot(
        sequences = pilot_data$TEXT,
        candidate_labels = labels[l],
        hypothesisemplate = hypothesis[h],
        multi_label = FALSE,
        model = "facebook/bart-large-mnli",
        device = "cpu",
        tokenizer_parallelism = FALSE,
        logging_level = "error",
        return_incorrect_results = FALSE,
        set_seed = 1001L
      )
      
      # Store results in new column
      pilot_data[[variable_name]] <- result$scores_x_1
    }
  }
  return(pilot_data)
}
