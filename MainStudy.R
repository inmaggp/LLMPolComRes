## Script to replicate the analysis of the main project developed in Markdown.
library(tidyr)
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
# This function takes some time to run. To avoid issues, it is recommended not to run it on the entire dataset at once. A sample is provided below.
## Test with a random sample of 200 sentences.
main_data_1_200 <- ZSC_main(main_data[1:200, ], labels, hypothesis)
# Uncomment to save main_data_1_200
#write_xlsx(main_data_1_200,'Data/main_data_1_200.xlsx')


############ Metrics' analysis
# Path to the Excel file
excel_file <- "Data/Main_classification.xlsx"
LLM <- read_excel(excel_file, sheet="BART")%>%
  select(ID,sequence, h1_l1, h1_l2, h1_l3, h1_l4, h1_l5, h1_l6, h1_l7, h1_l8, h1_l9, h1_l10, h1_l11, h1_l12, h1_l13, h1_l14, h1_l15, h1_l16, h1_l17, h1_l18)

human <- read_excel(excel_file)%>%
  select(ID,sequence, ConsTruth, ConsCodeA, ConsCodeB, EconTruth, EconCodeA, EconCodeB, TrumpTruth, TrumpCodeA, TrumpCodeB, BidenTruth, BidenCodeA, BidenCodeB, DemsTruth, DemsCodeA, DemcCodeB, GOPTruth, GOPCodeA, GOPCodeB, MAGATruth, MAGACodeA, MAGACodeB, JewsTruth, JewsCodeA, JewsCodeB, HealthTruth, HealthCodeA, HealthCodeB, ReproTruth, ReproCodeA, ReproCodeB, HomelessTruth, HomelessCodeA, HomeLessCodeB, ImmTruth, ImmCodeA, ImmCodeB, ClimateTruth, ClimateCodeA, ClimateCodeB, ElecVTruth, ElecVCodeA, ElecVCodeB, ElectionsTruth, ElectionsCodeA, ElectionsCodeB, Jan6Truth, Jan6CodeA, Jan6CodeB, RaceTruth, RaceCodeA, RaceCodeB, SocChgTruth, SocChgCodeA, SocChgCodeB)

thresholds <- c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7)

# Function to apply threshold
apply_threshold <- function(value, threshold) {
  ifelse(value >= threshold, 1, 0)
}
# Columns to apply the threshold. It considers only the relevant columns that include classifications.
classifications <- setdiff(colnames(LLM), c("ID", "sequence"))
# Initialize a list to store the DataFrames
threshold_dataframes <- list()
for (threshold in thresholds) {
  # Generate DataFrame name dynamically (ensure integer conversion)
  df_name <- sprintf("LLM_%02d", as.integer(threshold * 100))
  
  # Create a copy of the DataFrame and apply the threshold
  LLM_copy <- LLM
  LLM_copy[classifications] <- lapply(LLM_copy[classifications], apply_threshold, threshold)
  LLM_copy$TH <- threshold
  
  # Store in the list
  threshold_dataframes[[df_name]] <- LLM_copy
}
# Uncomment following lines to save different binarizations as Excel
# wb <- createWorkbook()
# for (name in names(threshold_dataframes)) { 
# addWorksheet(wb, name)  
# writeData(wb, name, threshold_dataframes[[name]])}
# saveWorkbook(wb, "Data/main_binarizations.xlsx", overwrite = TRUE)

# Organize the variables to proceed with the comparison between the binarized BART classifications and those provided by the human coders.

# Make a copy of 'human' without 'ID' and 'TEXT'
human_copy <- human %>% select(matches("Truth$"))
human_all <- human %>% select(-"ID", -"sequence")

cols_data1 <- setdiff(names(LLM_copy), c("ID", "sequence", "clean_text", "TH"))


# Create an empty results DataFrame to store, for each threshold and each issue, different goodness-of-fit metrics:
# The consensus value of the human coders is considered as the ground truth.

results_df <- data.frame(Issue=character(), FP=integer(), FN=integer(), TP=integer(), TN=integer(), TH=numeric(), recall=numeric(), precision=numeric(), accuracy=numeric(), f1 = numeric(),kappa=numeric(), kappa_fleiss=numeric(), kappa_C1C2=numeric(),kappa_C1BART=numeric(), kappa_C2BART=numeric(), stringsAsFactors = FALSE)
for(df_name in names(threshold_dataframes)){
  LLM_copy <- threshold_dataframes[[df_name]]
  TH <- unique(LLM_copy$TH)
  for (k in 1:18) {  # 18 labels/topics per hypothesis
    # Define true/predicted labels
    y_true <- human_copy[k]
    y_pred <- LLM_copy[[cols_data1[k]]]
    selected_columns <- human_all[, c(3*k-1, 3*k)]     
    selected_columns <- cbind(y_pred, selected_columns)
    
    #first column in selected_columns is BART classification
    #second column in selected_columns is Coder1 classification
    #third column in selected_columns is Coder2 classification
    # Compute confusion matrix
    cm <- table(unlist(y_true), y_pred)
    FP <- cm[1,2]
    FN <- cm[2,1]
    TP <- cm[2,2]
    TN <- cm[1,1]
    # Compute classification metrics
    precision <- ifelse((TP + FP) != 0, TP / (TP + FP), 0)
    recall <- ifelse((TP + FN) != 0, TP / (TP + FN), 0)
    accuracy <- ifelse((TP + TN + FP + FN) != 0, (TP + TN) / (TP + TN + FP + FN), 0)
    f1_score <- ifelse((precision + recall) != 0, (2 * precision * recall) / (precision + recall), 0)
    # Compute Cohen's Kappa
    kappa_stat <- kappa2(data.frame(y_true, y_pred))$value
    kappa_C1C2 <- kappa2(data.frame(selected_columns[,2], selected_columns[,3]))$value
    kappa_C1BART <- kappa2(data.frame(selected_columns[,2], y_pred))$value
    kappa_C2BART <- kappa2(data.frame(selected_columns[,3], y_pred))$value
    kappa_fleiss=kappam.fleiss(as.matrix(selected_columns))$value
    # Create a new row with the results      
    new_row <- data.frame(
      Issue = labels[k], FP = FP, FN = FN, TP = TP, TN = TN,TH=TH,
      recall = recall, precision = precision, accuracy = accuracy, 
      f1 = f1_score, kappa=kappa_stat,
      kappa_fleiss=kappa_fleiss,kappa_C1C2=kappa_C1C2,
      kappa_C1BART=kappa_C1BART,kappa_C2BART=kappa_C2BART
    )
    
    # Append new row to results DataFrame
    results_df <- bind_rows(results_df, new_row)
    # Define class labels
    classes <- c("Class 0", "Class 1")
    
  }}

# Uncomment to save ´results_df´as a XLSX file.
#write_xlsx(results_df, "Results/Main Project/BART/results_main_BART.xlsx")
results_main_BART = results_df

## Metrics representation
colors <- c("Recall" = "#FFD700", "Precision" = "#66CD00", "Accuracy" = "#FF3030", "F1" = "#00BFFF", "Human Consensus-BART 2 codes kappa"="#D15FEE", "Humans-BART 3 codes kappa"="#FFC0CB", "Code 1-Code 2 kappa"="#C1CDC1", "Code 1-BART kappa"="#8B0000", "Code 2-BART kappa"="#27408B")
unique_combinations <- unique(results_main_BART$Issue)


#### Plot recall, accuracy, precision and f1 across the threshold

for (i in 1:length(unique_combinations)) {
  top <- unique_combinations[i]
  # Filter the data for the current combination
  subset <- results_main_BART[results_main_BART$Issue == top, ]
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = recall, color = "Recall"), linewidth =  1) +
    geom_point(aes(y = recall, color = "Recall"), size =  5) +
    geom_line(aes(y = precision, color = "Precision"), linewidth =  1) +
    geom_point(aes(y = precision, color = "Precision"), size =  5) +
    geom_line(aes(y = accuracy, color = "Accuracy"), linewidth =  1) +
    geom_point(aes(y = accuracy, color = "Accuracy"), size =  5) +
    geom_line(aes(y = f1, color = "F1"), linewidth =  1) +
    geom_point(aes(y = f1, color = "F1"), size =  5) +
    scale_color_manual(values = colors) +  
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Topic: ", top), x = "Threshold", y = "Value", color = "Metric") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = NA), legend.position = "bottom", text = element_text(size =  12))
  filename <- sprintf("Results/Main Project/BART/Metrics Evolution/%s_Evolution_of_Metrics.png",  top)
  ## Uncomment to save the figure
  #ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}


#### Plot recall, accuracy, precision, f1 and human consensus-BART kappa across the threshold
for (i in 1:length(unique_combinations)) {
  top <- unique_combinations[i]
  # Filter the data for the current combination
  subset <- results_main_BART[results_main_BART$Issue == top, ]
  # Define the graphic
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = recall, color = "Recall"), linewidth =  1) +
    geom_point(aes(y = recall, color = "Recall"), size =  5) +
    geom_line(aes(y = precision, color = "Precision"), linewidth =  1) +
    geom_point(aes(y = precision, color = "Precision"), size =  5) +
    geom_line(aes(y = accuracy, color = "Accuracy"), linewidth =  1) +
    geom_point(aes(y = accuracy, color = "Accuracy"), size =  5) +
    geom_line(aes(y = f1, color = "F1"), linewidth =  1) +
    geom_point(aes(y = f1, color = "F1"), size =  5) +
    geom_line(aes(y = kappa, color = "Human Consensus-BART 2 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa, color = "Human Consensus-BART 2 codes kappa"), size =  5) +
    scale_color_manual(values = colors) + 
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Topic: ", top), x = "Threshold", y = "Value", color = "Metric") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = NA), legend.position = "bottom", text = element_text(size =  12))+
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  filename <- sprintf("Results/Main Project/BART/Metrics Evolution with Kappa/%s_Evolution_of_Metrics_kappa.png",  top)
  #ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}


#### Plot recall, accuracy, precision, f1 and Fleiss kappa across the threshold

for (i in 1:length(unique_combinations)) {
  topic <- unique_combinations[i]
  
  # Filter the data for the current combination
  subset <- results_main_BART[results_main_BART$Issue == topic, ]
  # Define the graphic
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = recall, color = "Recall"), linewidth =  1) +
    geom_point(aes(y = recall, color = "Recall"), size =  5) +
    geom_line(aes(y = precision, color = "Precision"), linewidth =  1) +
    geom_point(aes(y = precision, color = "Precision"), size =  5) +
    geom_line(aes(y = accuracy, color = "Accuracy"), linewidth =  1) +
    geom_point(aes(y = accuracy, color = "Accuracy"), size =  5) +
    geom_line(aes(y = f1, color = "F1"), linewidth =  1) +
    geom_point(aes(y = f1, color = "F1"), size =  5) +
    geom_line(aes(y = kappa_fleiss, color = "Humans-BART 3 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_fleiss, color = "Humans-BART 3 codes kappa"), size =  5) +
    scale_color_manual(values = colors) + 
    scale_y_continuous(limits=c(-0.09,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Topic: ", topic),
         x = "Threshold", y = "Value", color = "Metric")+
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  filename <- sprintf("Results/Main Project/BART/With Kappa Fleiss/%s_Evolution_of_Metrics_kappa_fleiss.png", topic)
  #ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}


#### Plot different combinations of Cohen’s kappas and Fleiss’ Kappa

for (i in 1:length(unique_combinations)) {
  topic <- unique_combinations[i]
  
  # Filter the data for the current combination
  subset <- results_main_BART[results_main_BART$Issue == topic, ]  
  # Define the graphic
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = kappa_C1C2, color = "Code 1-Code 2 kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_C1C2, color = "Code 1-Code 2 kappa"), size =  5) +
    geom_line(aes(y = kappa_C1BART, color = "Code 1-BART kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_C1BART, color = "Code 1-BART kappa"), size =  5) +
    geom_line(aes(y = kappa_C2BART, color = "Code 2-BART kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_C2BART, color = "Code 2-BART kappa"), size =  5) +
    geom_line(aes(y = kappa_fleiss, color = "Humans-BART 3 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_fleiss, color = "Humans-BART 3 codes kappa"), size =  5) +
    geom_line(aes(y = kappa, color = "Human Consensus-BART 2 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa, color = "Human Consensus-BART 2 codes kappa"), size =  5) +
    scale_color_manual(values = colors) + 
    scale_y_continuous(limits=c(-0.09,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Topic: ", topic),
         x = "Threshold", y = "Value", color = "Metric") +
    scale_fill_manual(
      labels = label_wrap(35))+
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  
  filename <- sprintf("Results/Main Project/BART/kappas evolution/%s_Evolution_of_kappas.png", topic)
  #ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
} 


## Analysis of 0.4 threshold, BART, Main Analysis, Figure 1.
results_04 <- results_main_BART[results_main_BART$TH == "0.4",]
results_04$Issue <- factor(results_04$Issue, levels = unique(results_04$Issue))

results_long <- results_04 %>%
  pivot_longer(
    cols=c(recall,precision,accuracy,f1,kappa_fleiss,kappa),
    names_to="Metric",
    values_to="Value"
  )

results_long$Metric <- factor(
  results_long$Metric,
  levels=c("accuracy","precision","recall",
           "f1","kappa_fleiss","kappa"),
  labels=c("Accuracy",
           "Precision",
           "Recall",
           "F1",
           "Humans-BART 3 codes kappa",
           "Human Consensus-BART 2 codes kappa")
)
#We generate a large, high‑resolution plot for saving and publication, and a smaller simplified version for direct visualization inside RStudio.
plot <- ggplot(results_long,
               aes(Issue, Value, fill=Metric))+
  
  geom_col(position=position_dodge(.8),
           width=.5,
           alpha=.35)+
  
  geom_point(aes(colour=Metric),
             position=position_dodge(.8),
             size=8)+
  
  scale_fill_manual(values=colors)+
  scale_colour_manual(values=colors)+
  
  scale_y_continuous(limits=c(-0.1,1),
                     breaks=seq(0,1,.2))+
  
  labs(x="",y="Value",
       fill="Metric",
       colour="Metric")+
  
  theme_minimal()+
  theme(
    legend.position = "bottom",
    text = element_text(size = 42),
    axis.text.x = element_text(size = 44, angle = 60, hjust = 1, face='bold'),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 44),
    legend.title = element_text(size = 44),
    legend.text = element_text(size = 42)
  )

#ggsave("Results/Main Project/BART/New_BarsPoints4_main_BART.png",      plot,width=48,height=27,dpi=300,bg="white")
# RStudio-friendly version with smaller text and smaller legend symbols.
# This version ensures the full plot renders correctly inside RStudio.

plot_view <- ggplot(results_long,
                    aes(Issue, Value, fill = Metric)) +
  
  # Large but balanced bars
  geom_col(position = position_dodge(width = 0.55),
           width = 0.4,        # wide bars, but not too wide
           alpha = 0.4) +      # lighter fill for clarity
  
  # Small points for clarity
  geom_point(aes(colour = Metric),
             position = position_dodge(width = 0.55),
             size = 1.5) +
  
  # Manual color scales
  scale_fill_manual(values = colors) +
  scale_colour_manual(values = colors) +
  
  # Y-axis limits and breaks
  scale_y_continuous(limits = c(-0.1, 1),
                     breaks = seq(0, 1, 0.2)) +
  
  # Axis labels
  labs(x = "",
       y = "Value",
       fill = "Metric",
       colour = "Metric") +
  
  # Prevent clipping of long labels
  coord_cartesian(clip = "off") +
  
  # Compact legend (single guide)
  guides(
    fill = guide_legend(
      override.aes = list(size = 1.2, alpha = 0.6),
      ncol = 3
    )
  ) +
  
  # Theme adjustments for RStudio visibility
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "lines"),
    legend.spacing.x = unit(0.15, "lines"),
    
    # General text size
    text = element_text(size = 9),
    
    # X-axis labels
    axis.text.x = element_text(size = 6, angle = 55, hjust = 1, face = "bold", margin = margin(t = 2)),
    
    # Y-axis labels
    axis.text.y = element_text(size = 8),
    
    # Axis titles
    axis.title = element_text(size = 9),
    
    # Legend text
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    
    # Panel margins (balanced)
    plot.margin = margin(60, 60, 60, 60)
  )
print(plot_view)
