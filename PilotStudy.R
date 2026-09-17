#### Script to replicate the analysis of the pilot dataset developed in Markdown.

library(writexl)
library(openxlsx)
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
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
pilot_data <- read_excel('Data/Pilot Data.xlsx', sheet='Complete Data All Issues')

#Set the hypothesis and labels to clasify
labels= c("Immigration", "Health Care", "Foreign Affairs",'Elections',"Civil Rights","Culture","Trump","Climate Change")
hypothesis=c("This text is about {}.", "This text focuses on {}.", "The main subject of this text is {}", "This text is related to {}")

ZSC <- function(pilot_data, labels, hypothesis) {
  for (h in seq_along(hypothesis)) {
    for (l in seq_along(labels)) {
      # Name of the variable for each classification
      variable_name <- paste0("h", h, "_l", l)
      # Perform zero-shot classification for each combination
      result <- textZeroShot(
        sequences = pilot_data$sequence,
        candidate_labels = labels[l],
        hypothesis_template = hypothesis[h],
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
# Uncomment to apply ZSC. Warning: it takes long
#pilot_data=ZSC(pilot_data,labels,hypothesis)

# Uncomment to save the pilot_data list as an .xlsx file.
#write.xlsx(pilot_data, file = "Data/Pilot_classification_new.xlsx")


############ Metrics' analysis
# Path to the Excel file
excel_file <- "Data/Pilot_classification.xlsx"
LLM <- read_excel(excel_file)%>%
  select(ID,sequence, h1_l1, h1_l2, h1_l3, h1_l4, h1_l5, h1_l6, h1_l7, h1_l8, h2_l1, h2_l2, h2_l3, h2_l4, h2_l5, h2_l6, h2_l7, h2_l8, h3_l1, h3_l2, h3_l3, h3_l4, h3_l5, h3_l6,  h3_l7, h3_l8, h4_l1, h4_l2, h4_l3, h4_l4, h4_l5, h4_l6, 
         h4_l7, h4_l8)
human <- read_excel(excel_file)%>%
  select(ID,sequence, Imm_ConsensusTruth, Imm_Code1, Imm_Code2, HealthCare_ConsensusTruth, HealthCare_Code1, HealthCare_Code2, ForAff_ConsensusTruth, ForAff_Code1, ForAff_Code2, Elections_ConsensusTruth, Elections_Code1, Elections_Code2, CivRights_ConsensusTruth, CivRights_Code1, CivRights_Code2, Culture_ConsensusTruth, Culture_Code1, Culture_Code2, Trump_ConsensusTruth, Trump_Code1, Trump_Code2, Env_ConsensusTruth, Env_Code1, Env_Code2)


## LLaMA classifications are obtained by running "Perform LLaMA.ipynb"
LLaMa <- read_excel(excel_file,sheet='LLaMa')
thresholds <- c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7)

# Function to apply threshold
apply_threshold <- function(value, threshold) {
  ifelse(value >= threshold, 1, 0)
}
# Columns to apply the threshold
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
# saveWorkbook(wb, "Data/binarizations_LLM.xlsx", overwrite = TRUE)


# Human topics
cols_data2 <- c('l1', 'l2', 'l3', 'l4', 'l5', 'l6', 'l7', 'l8')
cols_data1 <- c('h1_l1', 'h1_l2', 'h1_l3', 'h1_l4', 'h1_l5', 'h1_l6', 'h1_l7', 'h1_l8', 'h2_l1', 'h2_l2', 'h2_l3', 'h2_l4', 'h2_l5', 'h2_l6', 'h2_l7', 'h2_l8', 'h3_l1', 'h3_l2', 'h3_l3', 'h3_l4', 'h3_l5', 'h3_l6', 'h3_l7', 'h3_l8', 'h4_l1', 'h4_l2', 'h4_l3', 'h4_l4', 'h4_l5', 'h4_l6', 'h4_l7', 'h4_l8')

# Make a copy of 'human' without 'ID' and 'sequence'
human_copy <- human %>% select('Imm_ConsensusTruth','HealthCare_ConsensusTruth','ForAff_ConsensusTruth','Elections_ConsensusTruth','CivRights_ConsensusTruth','Culture_ConsensusTruth','Trump_ConsensusTruth','Env_ConsensusTruth')
human2 <- subset(human, select=-c(ID,sequence))
# Rename columns
colnames(human_copy) <- cols_data2


##### results_df. 
#### Analysis of results of LLM, pilot data
results_df <- data.frame(Issue=character(), FP=integer(), FN=integer(), TP=integer(), TN=integer(), TH=numeric(), recall=numeric(), precision=numeric(), accuracy=numeric(), f1 = numeric(),kappa=numeric(), topic=character(),hypothesis=character(), kappa_fleiss=numeric(), kappa_C1C2=numeric(),kappa_C1BART=numeric(), kappa_C2BART=numeric(), stringsAsFactors = FALSE)
for(df_name in names(threshold_dataframes)){
  LLM_copy <- threshold_dataframes[[df_name]]
  TH <- unique(LLM_copy$TH)
  for (i in 0:3) {  # 4 hypotheses
    for (k in 1:8) {  # 8 labels/topics per hypothesis
      c <- i * 8 + k  # Adjust index accordingly
      # Define true/predicted labels
      y_true <- human_copy[[cols_data2[k]]]
      y_pred <- LLM_copy[[classifications[c]]]
      selected_columns <- human2[, c(3*k-1, 3*k)]     
      #first column in selected_columns is BART classification
      #second column in selected_columns is Coder1 classification
      #third column in selected_columns is Coder2 classification
      selected_columns <- cbind(y_pred, selected_columns)
      # Compute confusion matrix
      cm <- table(
        factor(y_true, levels = c(0, 1)),
        factor(y_pred, levels = c(0, 1))
      )
      FP <- cm[1,2]
      FN <- cm[2,1]
      TP <- cm[2,2]
      TN <- cm[1,1]
      # Compute classification metrics
      precision <- ifelse((TP+FP) != 0, TP / (TP+FP), 0)
      recall <- ifelse((TP+FN) != 0, TP / (TP+FN), 0)
      accuracy <- ifelse((TP+TN+FP+FN) != 0, (TP+TN) / (TP+TN +FP+FN), 0)
      f1_score <- ifelse((precision+recall) != 0, (2*precision*recall)/(precision+recall), 0)
      # Compute Cohen's Kappa
      kappa_stat <- kappa2(data.frame(y_true, y_pred))$value
      kappa_C1C2 <- kappa2(data.frame(selected_columns[,2],selected_columns[,3]))$value
      kappa_C1BART <- kappa2(data.frame(selected_columns[,2], y_pred))$value
      kappa_C2BART <- kappa2(data.frame(selected_columns[,3], y_pred))$value
      kappa_fleiss=kappam.fleiss(as.matrix(selected_columns))$value
      # Extract 'hypothesis' from 'Issue' column (only numbers, removing 'h')
      hypothesis <- as.character(str_extract(classifications[c], "(?<=h)\\d+"))
      # Extract 'topic' from 'Issue' column (only numbers, removing 'l')
      topic <- as.character(str_extract(classifications[c], "(?<=l)\\d+"))
      # Create a new row with the results      
      new_row <- data.frame(Issue = classifications[c], FP = FP, FN = FN, TP = TP, TN = TN, TH = TH, recall = recall, precision = precision, accuracy = accuracy, f1 = f1_score, kappa=kappa_stat,topic=topic, hypothesis=hypothesis, kappa_fleiss=kappa_fleiss,kappa_C1C2=kappa_C1C2, kappa_C1BART=kappa_C1BART,kappa_C2BART=kappa_C2BART)
      # Append new row to results DataFrame
      results_df <- bind_rows(results_df, new_row) 
      # Define class labels
      classes <- c("Class 0", "Class 1")
      # Create heatmap of the confusion matrix
      cm_matrix <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE)
      cm_df <- as.data.frame(as.table(cm_matrix))
      cm_plot<-ggplot(cm_df, aes(Var2, Var1, fill = Freq))+geom_tile(color = "white")+scale_fill_gradientn(colors = c("LightYellow", "yellow", "orange", "red"), guide = "none")+geom_text(aes(label = Freq), color = "black", size =  5) + theme_minimal() + labs(title = paste("Evolution of Metrics for Prompt", hypothesis, "and Topic", topic),x = "Predicted Labels", y = "True Labels") +scale_x_discrete(labels = classes) +scale_y_discrete(labels = classes) + theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.background = element_rect(fill = "white", color = NA))  # Ensure white background
      # Uncomment to print the plot
      # print(cm_plot)
      # Uncomment to save the figure
      filename <- sprintf("Results/Pilot/Confusion Matrices/TH=%s_Confusion_Matrix_%s.png", TH, gsub("[ \n.]", "_",classifications[c]))
      #ggsave(filename=filename,plot=cm_plot,dpi=300,width=5,height=5,bg="white")
    }}}
## Uncomment to save as XLSX
#write_xlsx(results_df, "Results/Pilot/LLM/results_pilot_LLM.xlsx")
results_pilot_LLM = results_df

### Metrics evolution. LLM
#Set the hypothesis and labels to clasify
colors <- c("Recall" = "#FFD700", "Precision" = "#66CD00", "Accuracy" = "#FF3030", "F1" = "#00BFFF", "Human Consensus-LLM 2 codes kappa"="#D15FEE", "Humans-LLM 3 codes kappa"="#FFC0CB", "Code 1-Code 2 kappa"="#C1CDC1", "Code 1-LLM kappa"="#8B0000", "Code 2-LLM kappa"="#27408B")
# Obtain unique combinations of hypotheses and topics
unique_combinations <- unique(results_pilot_LLM[, c("hypothesis", "topic")])

# Iterate over each unique combination of hypotheses and topics
for (i in 1:nrow(unique_combinations)) {
  hyp <- unique_combinations$hypothesis[i]
  top <- unique_combinations$topic[i]
  
  # Filter the data for the current combination
  subset <- results_df[results_df$hypothesis == hyp & results_df$topic == top, ]
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
    labs(title = paste("Prompt:", hypothesis[as.numeric(unique_combinations$hyp[i])], " Topic:", labels[as.numeric(unique_combinations$top[i])]), x = "Threshold", y = "Value", color = "Metric") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = NA), legend.position = "bottom", text = element_text(size =  12))
  filename <- sprintf("Results/Pilot/LLM/Metrics Evolution2/%s_%s_Evolution_of_Metrics.png", hyp, top)
  #ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  # Uncomment to plot
  print(plot)
}


# Iterate over each unique combination of hypotheses and topics
#plot also kappa Human Consensus-LLM kappa.
#Figure 1 in the Appendix, specifically for topic Healthcare.
for (i in 1:nrow(unique_combinations)) {
  hyp <- unique_combinations$hypothesis[i]
  top <- unique_combinations$topic[i]
  # Filter the data for the current combination
  subset <- results_df[results_df$hypothesis == hyp & results_df$topic == top, ]
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
    geom_line(aes(y = kappa, color = "Human Consensus-LLM 2 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa, color = "Human Consensus-LLM 2 codes kappa"), size =  5) +
    scale_color_manual(values = colors) + 
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Prompt:", hypothesis[as.numeric(unique_combinations$hyp[i])], " Topic:", labels[as.numeric(unique_combinations$top[i])]),
         x = "Threshold", y = "Value", color = "Metric") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  filename <- sprintf("Results/Pilot/LLM/Metrics Evolution with Kappa/%s_%s_Evolution_of_Metrics_kappa.png", hyp, top)
 # ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}


#plot also kappa fleiss
for (i in 1:nrow(unique_combinations)) {
  hyp <- unique_combinations$hypothesis[i]
  top <- unique_combinations$topic[i]
  
  # Filter the data for the current combination
  subset <- results_df[results_df$hypothesis == hyp & results_df$topic == top, ]
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = recall, color = "Recall"), linewidth =  1) +
    geom_point(aes(y = recall, color = "Recall"), size =  5) +
    geom_line(aes(y = precision, color = "Precision"), linewidth =  1) +
    geom_point(aes(y = precision, color = "Precision"), size =  5) +
    geom_line(aes(y = accuracy, color = "Accuracy"), linewidth =  1) +
    geom_point(aes(y = accuracy, color = "Accuracy"), size =  5) +
    geom_line(aes(y = f1, color = "F1"), linewidth =  1) +
    geom_point(aes(y = f1, color = "F1"), size =  5) +
    geom_line(aes(y = kappa_fleiss, color = "Humans-LLM 3 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_fleiss, color = "Humans-LLM 3 codes kappa"), size =  5) +
    scale_color_manual(values = colors) + 
    scale_y_continuous(limits=c(-0.07,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Prompt:", hypothesis[as.numeric(unique_combinations$hyp[i])], " Topic:", labels[as.numeric(unique_combinations$top[i])]),
         x = "Threshold", y = "Value", color = "Metric") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  filename <- sprintf("Results/Pilot/LLM/With Kappa Fleiss/%s_%s_Evolution_of_Metrics_kappa_fleiss.png", hyp, top)
 # ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}

## kappas evolution

for (i in 1:nrow(unique_combinations)) {
    hyp <- unique_combinations$hypothesis[i]
    top <- unique_combinations$topic[i]
    
    # Filter the data for the current combination
    subset <- results_df[results_df$hypothesis == hyp & results_df$topic == top, ]
    
    # Define the graphic
    plot <- ggplot(subset, aes(x = TH)) +
      geom_line(aes(y = kappa_C1C2, color = "Code 1-Code 2 kappa"), linewidth =  1) +
      geom_point(aes(y = kappa_C1C2, color = "Code 1-Code 2 kappa"), size =  5) +
      geom_line(aes(y = kappa_C1BART, color = "Code 1-LLM kappa"), linewidth =  1) +
      geom_point(aes(y = kappa_C1BART, color = "Code 1-LLM kappa"), size =  5) +
      geom_line(aes(y = kappa_C2BART, color = "Code 2-LLM kappa"), linewidth =  1) +
      geom_point(aes(y = kappa_C2BART, color = "Code 2-LLM kappa"), size =  5) +
      geom_line(aes(y = kappa_fleiss, color = "Humans-LLM 3 codes kappa"), linewidth =  1) +
      geom_point(aes(y = kappa_fleiss, color = "Humans-LLM 3 codes kappa"), size =  5) +
      geom_line(aes(y = kappa, color = "Human Consensus-LLM 2 codes kappa"), linewidth =  1) +
      geom_point(aes(y = kappa, color = "Human Consensus-LLM 2 codes kappa"), size =  5) +
      scale_color_manual(values = colors) + 
      scale_y_continuous(limits=c(-0.07,1),breaks=seq(0,1,by = 0.1))+
      labs(title = paste("Prompt:", hypothesis[as.numeric(unique_combinations$hyp[i])], " Topic:", labels[as.numeric(unique_combinations$top[i])]),
           x = "Threshold", y = "Value", color = "Metric") +
      scale_fill_manual(
        labels = label_wrap(35))+
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA),
            legend.position = "bottom",
            text = element_text(size =  12))
    
    filename <- sprintf("Results/Pilot/LLM/kappas evolution/%s_%s_Evolution_of_kappas.png", hyp, top)
 #   ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
    print(plot)
  }  
  


#### Explore f1
# Step 1: Create a list to store the tables for each hypothesis
hypothesis_tables <- list()

# Step 2: Loop through each hypothesis to create the corresponding table
for (hyp in unique(results_df$hypothesis)) {
  
  # Filter the data for the current hypothesis
  hypothesis_data <- results_df %>%
    filter(hypothesis == hyp) 
  
  # Step 3: For each topic, find the maximum F1 score and the corresponding thresholds and accuracy
  max_f1_table <- hypothesis_data %>%
    group_by(topic) %>%
    # Find the maximum F1 score for each topic
    mutate(max_f1 = max(f1)) %>%
    # Filter rows where F1 is equal to the maximum F1 score
    filter(f1 == max_f1) %>%
    # Arrange by TH (threshold) to keep all thresholds where the max F1 is achieved
    arrange(topic, TH) %>%
    # Summarize the results for each topic, keeping the max F1, thresholds, and accuracy
    summarise(
      max_f1 = first(max_f1),  # Maximum F1 score (all rows will have the same value)
      thresholds = paste(TH, collapse = ", "),  # Thresholds where max F1 is achieved
      accuracy = first(accuracy)  # Accuracy corresponding to max F1 score
    ) %>%
    ungroup()
  
  # Store the table for the current hypothesis in the list
  hypothesis_tables[[hyp]] <- max_f1_table
}

# Now hypothesis_tables contains a table for each hypothesis
# Access the table for a specific hypothesis like this:
# hypothesis_tables[["Hypothesis1"]]

print(hypothesis_tables[[1]]) #This text is about
print(hypothesis_tables[[2]]) #This text focuses on
print(hypothesis_tables[[3]]) #The main subject of this text is
print(hypothesis_tables[[4]]) #This text is related to

## Focus on th=0.4 Appendix B, Figure 3, Top.  LLM

## TH = 0.4 data:
results_04 <- results_pilot_LLM[results_pilot_LLM$TH == "0.4",]
results_04H1 <- results_04[results_04$hypothesis == "1",]
results_04H1$topic_names<-c('Immigration','Health Care','Foreign Affairs', 'Elecctions', 'Civil Rights','Culture','Trump','Climate Change')


results_long <- results_04H1 %>%
  pivot_longer(
    cols=c(accuracy,precision,recall,
           f1,kappa_fleiss,kappa),
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
           "Humans-LLM 3 codes kappa",
           "Human Consensus-LLM 2 codes kappa")
)

plot <- ggplot(results_long,
               aes(topic_names, Value, fill=Metric))+
  geom_col(position=position_dodge(.8),
           width=.5,
           alpha=.35)+
  
  geom_point(aes(colour=Metric),
             position=position_dodge(.8),
             size=9)+
  
  scale_fill_manual(values=colors)+
  scale_colour_manual(values=colors)+
  
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.2))+
  
  labs(x="",y="Value",
       fill="Metric",
       colour="Metric")+
  
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 0),    
    legend.spacing.y = unit(0.2, "cm"),     
    plot.margin = margin(b = 2, t = 5),   
    text = element_text(size = 28),
    axis.text.x = element_text(size = 28, angle = 60, hjust =0.8, vjust=1, face='bold'),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 28)
  )

# Uncomment to save with the same size and appearance as in the paper
#ggsave("Results/Pilot/LLM/New_BarsPoints4_pilot_LLM.png", plot,width=23,height=13,bg="white")

#The following version ensures the full plot renders correctly inside RStudio.
plot_view <- ggplot(results_long,
                    aes(topic_names, Value, fill = Metric)) +
  
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
  scale_y_continuous(limits = c(0, 1),
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

### Correlations analysis. Tabla A2 Appendix
# Generate names with structure hypothesis/topics
column_names <- as.vector(outer(labels, hypothesis, paste, sep = "_"))
# Rename variables to automatically calculate correlation matrices
colnames(LLM)[3:34] <- column_names

# List to store the correlation matrices
correlation_matrices <- list()

# Iterate over the topics
for (topic in labels) {
  # Select the columns for the topic with the different hypotheses
  columns_to_select <- paste(topic, hypothesis, sep = "_")
  selected_columns <- LLM[, columns_to_select]
  
  # Calculate the correlation matrix
  cor_matrix <- cor(selected_columns, use = "complete.obs")  
  # Use "complete.obs" to omit NA values if needed
  
  # Store the correlation matrix in the list
  correlation_matrices[[topic]] <- cor_matrix
}

# View the correlation matrix for a specific topic, for example, "Immigration"
print(correlation_matrices[["Immigration"]])
corr_foraff = correlation_matrices[["Foreign Affairs"]]
corr_culture = correlation_matrices[["Culture"]]
corr_immigration = correlation_matrices[["Immigration"]]
corr_trump = correlation_matrices[["Trump"]]
corr_health = correlation_matrices[["Health Care"]]
corr_elections = correlation_matrices[["Elections"]]
corr_civ_rights= correlation_matrices[["Civil Rights"]]
corr_EnvEnCli = correlation_matrices[["Climate Change"]]


#### LLaMa Analysis
classifications_LLaMa <- setdiff(colnames(LLaMa), c("ID", "sequence"))
threshold_llama <- list()
for (threshold in thresholds) {
  # Generate DataFrame name dynamically (ensure integer conversion)
  df_name <- sprintf("LLaMa_%02d", as.integer(threshold * 100))
  
  # Create a copy of the DataFrame and apply the threshold
  LLaMa_copy <-LLaMa
  LLaMa_copy[classifications_LLaMa] <- lapply(LLaMa_copy[classifications_LLaMa], apply_threshold, threshold)
  LLaMa_copy$TH <- threshold
  
  # Store in the list
  threshold_llama[[df_name]] <-LLaMa_copy
}

# Save different binarizations as Excel
#wb <- createWorkbook()
#for (name in names(threshold_llama)) { 
#  addWorksheet(wb, name)  
#  writeData(wb, name, threshold_llama[[name]])}
#saveWorkbook(wb, "Data/binarizations_LLaMa.xlsx", overwrite = TRUE)

# organize the variables
cols_llama <- c("Immigration","Health Care","Foreign Affairs","Elections","Civil Rights","Culture","Trump","Climate Change" )

# Make a copy of 'human' without 'ID' and 'sequence'
human_copy <- human %>% select('Imm_ConsensusTruth','HealthCare_ConsensusTruth','ForAff_ConsensusTruth','Elections_ConsensusTruth','CivRights_ConsensusTruth','Culture_ConsensusTruth','Trump_ConsensusTruth','Env_ConsensusTruth')
human2 <- subset(human, select=-c(ID,sequence))
# Rename columns
colnames(human_copy) <- cols_data2

# Create an empty results DataFrame where we store, for each threshold and each hypothesis–issue combination, different goodness-of-fit metrics
## LLaMa VS Human
results_pilot_LLaMa <- data.frame(Issue = character(), FP = integer(), FN = integer(), TP = integer(),TN = integer(), TH = numeric(), recall = numeric(), precision = numeric(), accuracy = numeric(), f1 = numeric(),kappa=numeric(),
                                  topic=character(),hypothesis=character(),kappa_fleiss=numeric(), kappa_C1C2=numeric(),kappa_C1LLaMa=numeric(), kappa_C2LLaMa=numeric(),stringsAsFactors = FALSE)

for(df_name in names(threshold_llama)){
  LLaMa_copy <- threshold_llama[[df_name]]
  TH <- unique(LLaMa_copy$TH)
  for (k in 1:8) {  # 8 labels/topics per hypothesis
    # c <-  k  # Adjust index accordingly
    # Define true/predicted labels
    y_true <- human_copy[[cols_data2[k]]]
    y_pred <-LLaMa_copy[[cols_llama[k]]]
    
    selected_columns <- human2[, c(3*k-1, 3*k)]     
    # selected_columns <- human2[, seq(2, ncol(human2), by = 2)] 
    selected_columns <- cbind(y_pred, selected_columns)
    # Compute confusion matrix
    cm <- table(y_true, y_pred)
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
    kappa_C1C2 <- kappa2(data.frame(selected_columns[,2],selected_columns[,3]))$value
    kappa_C1LLaMa <- kappa2(data.frame(selected_columns[,2], y_pred))$value
    kappa_C2LLaMa <- kappa2(data.frame(selected_columns[,3], y_pred))$value
    kappa_fleiss=kappam.fleiss(as.matrix(selected_columns))$value
    # Extract 'hypothesis' from 'Issue' column (only numbers, removing 'h')
    hypothesis <- as.character(str_extract(cols_data1[k], "(?<=h)\\d+"))
    # Extract 'topic' from 'Issue' column (only numbers, removing 'l')
    topic <- as.character(str_extract(cols_data1[k], "(?<=l)\\d+"))
    
    # Create a new row with the results      
    new_row <- data.frame(
      Issue = cols_data2[k], FP = FP, FN = FN, TP = TP, TN = TN, TH = TH,
      recall = recall, precision = precision, accuracy = accuracy, 
      f1 = f1_score, kappa=kappa_stat,topic=topic, hypothesis=hypothesis, kappa_fleiss=kappa_fleiss,kappa_C1C2=kappa_C1C2, kappa_C1LLaMa=kappa_C1LLaMa,kappa_C2LLaMa=kappa_C2LLaMa
    )
    
    # Append new row to results DataFrame
    results_pilot_LLaMa <- bind_rows(results_pilot_LLaMa, new_row)
  }
}
# Uncomment to save ´results_df´as a XLSX file.
# write_xlsx(results_pilot_LLaMa, "Results/Pilot/LLaMa/results_pilot_LLaMa.xlsx")

colors_LLaMa <- c("Recall" = "#FFD700", "Precision" = "#66CD00", "Accuracy" = "#FF3030", "F1" = "#00BFFF", "Human Consensus-LLM 2 codes kappa"="#D15FEE", "Humans-LLM 3 codes kappa"="#FFC0CB", "Code 1-Code 2 kappa"="#C1CDC1", "Code 1-LLM kappa"="#8B0000", "Code 2-LLM kappa"="#27408B","Human Consensus-LLaMa 2 codes kappa"="#D15FEE", "Humans-LLaMa 3 codes kappa"="#FFC0CB", "Code 1-Code 2 kappa"="#C1CDC1", "Code 1-LLaMa kappa"="#8B0000", "Code 2-LLaMa kappa"="#27408B")  
unique_combinations_LLaMa <- unique(results_pilot_LLaMa[, c("hypothesis", "topic")])
labels_LLaMa= c("Immigration", "Health Care", "Foreign Affairs",'Elections',"Civil Rights","Culture","Trump","Climate Change")
hypothesis_LLaMa=c("This text is about {}.")

#### Plot recall, accuracy, precision and f1 across the threshold for LLaMa

# Iterate over each unique combination of hypotheses and topics
for (i in 1:nrow(unique_combinations_LLaMa)) {
  hyp <- unique_combinations_LLaMa$hypothesis[i]
  top <- unique_combinations_LLaMa$topic[i]
  
  # Filter the data for the current combination
  subset <- results_pilot_LLaMa[results_pilot_LLaMa$hypothesis == hyp & results_pilot_LLaMa$topic == top, ]
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = recall, color = "Recall"), linewidth =  1) +
    geom_point(aes(y = recall, color = "Recall"), size =  5) +
    geom_line(aes(y = precision, color = "Precision"), linewidth =  1) +
    geom_point(aes(y = precision, color = "Precision"), size =  5) +
    geom_line(aes(y = accuracy, color = "Accuracy"), linewidth =  1) +
    geom_point(aes(y = accuracy, color = "Accuracy"), size =  5) +
    geom_line(aes(y = f1, color = "F1"), linewidth =  1) +
    geom_point(aes(y = f1, color = "F1"), size =  5) +
    scale_color_manual(values = colors_LLaMa) +  
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Prompt:", hypothesis_LLaMa[as.numeric(unique_combinations_LLaMa$hyp[i])], " Topic:", labels_LLaMa[as.numeric(unique_combinations_LLaMa$top[i])]), x = "Threshold", y = "Value", color = "Metric") + theme_minimal() + theme(panel.background = element_rect(fill = "white", color = NA), legend.position = "bottom", text = element_text(size =  12))
  filename <- sprintf("Results/Pilot/LLaMa/Metrics Evolution/%s_%s_Evolution_of_Metrics.png", hyp, top)
  # Uncomment to save
  #ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  # Uncomment to plot
  print(plot)
}

#### Plot recall, accuracy, precision, f1 and human consensus-LLaMa kappa across the threshold for LLaMa
for (i in 1:nrow(unique_combinations_LLaMa)) {
  hyp <- unique_combinations_LLaMa$hypothesis[i]
  top <- unique_combinations_LLaMa$topic[i]
  # Filter the data for the current combination
  subset <- results_pilot_LLaMa[results_pilot_LLaMa$hypothesis == hyp & results_pilot_LLaMa$topic == top, ]
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
    geom_line(aes(y = kappa, color = "Human Consensus-LLaMa 2 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa, color = "Human Consensus-LLaMa 2 codes kappa"), size =  5) +
    scale_color_manual(values = colors_LLaMa) + 
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Prompt:", hypothesis_LLaMa[as.numeric(unique_combinations_LLaMa$hyp[i])], " Topic:", labels_LLaMa[as.numeric(unique_combinations_LLaMa$top[i])]),
         x = "Threshold", y = "Value", color = "Metric") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  # Uncomment to save the figure
  filename <- sprintf("Results/Pilot/LLaMa/Metrics Evolution with Kappa/%s_%s_Evolution_of_Metrics_kappa.png", hyp, top)
  #  ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}

#### Plot recall, accuracy, precision, f1 and Fleiss kappa across the threshold for LLaMa
for (i in 1:nrow(unique_combinations_LLaMa)) {
  hyp <- unique_combinations_LLaMa$hypothesis[i]
  top <- unique_combinations_LLaMa$topic[i]
  
  # Filter the data for the current combination
  subset <- results_pilot_LLaMa[results_pilot_LLaMa$hypothesis == hyp & results_pilot_LLaMa$topic == top, ]
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = recall, color = "Recall"), linewidth =  1) +
    geom_point(aes(y = recall, color = "Recall"), size =  5) +
    geom_line(aes(y = precision, color = "Precision"), linewidth =  1) +
    geom_point(aes(y = precision, color = "Precision"), size =  5) +
    geom_line(aes(y = accuracy, color = "Accuracy"), linewidth =  1) +
    geom_point(aes(y = accuracy, color = "Accuracy"), size =  5) +
    geom_line(aes(y = f1, color = "F1"), linewidth =  1) +
    geom_point(aes(y = f1, color = "F1"), size =  5) +
    geom_line(aes(y = kappa_fleiss, color = "Humans-LLaMa 3 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_fleiss, color = "Humans-LLaMa 3 codes kappa"), size =  5) +
    scale_color_manual(values = colors_LLaMa) + 
    scale_y_continuous(limits=c(-0.07,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Prompt:", hypothesis_LLaMa[as.numeric(unique_combinations_LLaMa$hyp[i])], " Topic:", labels_LLaMa[as.numeric(unique_combinations_LLaMa$top[i])]),
         x = "Threshold", y = "Value", color = "Metric") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  12))
  # Uncomment to save the plot
  filename <- sprintf("Results/Pilot/LLaMa/With Kappa Fleiss/%s_%s_Evolution_of_Metrics_kappa_fleiss.png", hyp, top)
 # ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}


#### Plot different combinations of Cohen's kappas and Fleiss' Kappa for LLaMa

for (i in 1:nrow(unique_combinations_LLaMa)) {
  hyp <- unique_combinations_LLaMa$hypothesis[i]
  top <- unique_combinations_LLaMa$topic[i]
  
  # Filter the data for the current combination
  subset <- results_pilot_LLaMa[results_pilot_LLaMa$hypothesis == hyp & results_pilot_LLaMa$topic == top, ]
  
  # Define the graphic
  plot <- ggplot(subset, aes(x = TH)) +
    geom_line(aes(y = kappa_C1C2, color = "Code 1-Code 2 kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_C1C2, color = "Code 1-Code 2 kappa"), size =  5) +
    geom_line(aes(y = kappa_C1LLaMa, color = "Code 1-LLaMa kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_C1LLaMa, color = "Code 1-LLaMa kappa"), size =  5) +
    geom_line(aes(y = kappa_C2LLaMa, color = "Code 2-LLaMa kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_C2LLaMa, color = "Code 2-LLaMa kappa"), size =  5) +
    geom_line(aes(y = kappa_fleiss, color = "Humans-LLaMa 3 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa_fleiss, color = "Humans-LLaMa 3 codes kappa"), size =  5) +
    geom_line(aes(y = kappa, color = "Human Consensus-LLaMa 2 codes kappa"), linewidth =  1) +
    geom_point(aes(y = kappa, color = "Human Consensus-LLaMa 2 codes kappa"), size =  5) +
    scale_color_manual(values = colors_LLaMa) + 
    scale_y_continuous(limits=c(-0.07,1),breaks=seq(0,1,by = 0.1))+
    labs(title = paste("Prompt:", hypothesis_LLaMa[as.numeric(unique_combinations_LLaMa$hyp[i])], " Topic:", labels_LLaMa[as.numeric(unique_combinations_LLaMa$top[i])]),
         x = "Threshold", y = "Value", color = "Metric") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom",
          text = element_text(size =  7))
  # Uncomment to save the plot 
  filename <- sprintf("Results/Pilot/LLaMa/kappas evolution/%s_%s_Evolution_of_kappas.png", hyp, top)
#  ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}

## Focus on th=0.4.  Appendix B, Figure 3, bottom. LLaMa, pilot.
results_04 <- results_pilot_LLaMa[results_pilot_LLaMa$TH == "0.4",]
results_04H1 <- results_04[results_04$hypothesis == "1",]
results_04H1$topic_names<-c('Immigration','Health Care','Foreign Affairs', 'Elecctions', 'Civil Rights','Culture','Trump','Climate Change')

results_long <- results_04H1 %>%
  pivot_longer(
    cols=c(accuracy,precision,recall,
           f1,kappa_fleiss,kappa),
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
           "Humans-LLaMa 3 codes kappa",
           "Human Consensus-LLaMa 2 codes kappa")
)
plot <- ggplot(results_long,
               aes(topic_names, Value, fill=Metric))+
  
  geom_col(position=position_dodge(.8),
           width=.5,
           alpha=.35)+
  
  geom_point(aes(colour=Metric),
             position=position_dodge(.8),
             size=9)+
  
  scale_fill_manual(values=colors_LLaMa)+
  scale_colour_manual(values=colors_LLaMa)+
  
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.2))+
  
  labs(x="",y="Value",
       fill="Metric",
       colour="Metric")+
  
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 0),    
    legend.spacing.y = unit(0.2, "cm"),     
    plot.margin = margin(b = 2, t = 5),   
    text = element_text(size = 28),
    axis.text.x = element_text(size = 28, angle = 60, hjust =0.8, vjust=1, face='bold'),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 28)
  )

# Uncomment to save with the same size and appearance as in the paper
#ggsave("Results/Pilot/LLaMa/New_BarsPoints4_pilot_LLaMa.png", plot,width=23,height=13,bg="white")

plot_view <- ggplot(results_long,
                    aes(topic_names, Value, fill = Metric)) +
  
  # Large but balanced bars
  geom_col(position = position_dodge(width = 0.55),
           width = 0.4,        # wide bars, but not too wide
           alpha = 0.4) +      # lighter fill for clarity
  
  # Small points for clarity
  geom_point(aes(colour = Metric),
             position = position_dodge(width = 0.55),
             size = 1.5) +
  
  # Manual color scales
  scale_fill_manual(values = colors_LLaMa) +
  scale_colour_manual(values = colors_LLaMa) +
  
  # Y-axis limits and breaks
  scale_y_continuous(limits = c(0, 1),
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

## Comparixson: BART vs LLaMa, Pilot analysis. Appendix B, Figure 1.
# Iterate over each unique topic

hypothesis_LLM=c("This text is about {}.", "This text focuses on {}.", "The main subject of this text is {}", "This text is related to {}")
unique_combinations_LLM = unique_combinations
for (i in 1:nrow(unique_combinations_LLaMa)) {
  
  top <- unique_combinations_LLaMa$topic[i]
  # Filter LLM data: ONLY hypothesis = 1
  subset_LLM <- results_pilot_LLM[
    results_pilot_LLM$hypothesis == 1 &
      results_pilot_LLM$topic == top, 
  ]
  
  # Filter LLaMa data
  subset_LLaMa <- results_pilot_LLaMa[
    results_pilot_LLaMa$hypothesis == 1 &
      results_pilot_LLaMa$topic == top, 
  ]
  plot <- ggplot() +
    
    # =========================
  # LLM: solid lines + circles
  # =========================
  geom_line(
    data = subset_LLM,
    aes(x = TH, y = recall, color = "Recall"),
    linewidth = 1
  ) +
    geom_point(
      data = subset_LLM,
      aes(x = TH, y = recall, color = "Recall"),
      shape = 16,
      size = 5
    ) +
    
    geom_line(
      data = subset_LLM,
      aes(x = TH, y = precision, color = "Precision"),
      linewidth = 1
    ) +
    geom_point(
      data = subset_LLM,
      aes(x = TH, y = precision, color = "Precision"),
      shape = 16,
      size = 5
    ) +
    
    geom_line(
      data = subset_LLM,
      aes(x = TH, y = accuracy, color = "Accuracy"),
      linewidth = 1
    ) +
    geom_point(
      data = subset_LLM,
      aes(x = TH, y = accuracy, color = "Accuracy"),
      shape = 16,
      size = 5
    ) +
    
    geom_line(
      data = subset_LLM,
      aes(x = TH, y = f1, color = "F1"),
      linewidth = 1
    ) +
    geom_point(
      data = subset_LLM,
      aes(x = TH, y = f1, color = "F1"),
      shape = 16,
      size = 5
    ) +
    
    # ============================
  # LLaMa: dotted lines + stars
  # ============================
  geom_line(
    data = subset_LLaMa,
    aes(x = TH, y = recall, color = "Recall"),
    linewidth = 1,
    linetype = "dotted"
  ) +
    geom_point(
      data = subset_LLaMa,
      aes(x = TH, y = recall, color = "Recall"),
      shape = 8,
      size = 5
    ) +
    
    geom_line(
      data = subset_LLaMa,
      aes(x = TH, y = precision, color = "Precision"),
      linewidth = 1,
      linetype = "dotted"
    ) +
    geom_point(
      data = subset_LLaMa,
      aes(x = TH, y = precision, color = "Precision"),
      shape = 8,
      size = 5
    ) +
    
    geom_line(
      data = subset_LLaMa,
      aes(x = TH, y = accuracy, color = "Accuracy"),
      linewidth = 1,
      linetype = "dotted"
    ) +
    geom_point(
      data = subset_LLaMa,
      aes(x = TH, y = accuracy, color = "Accuracy"),
      shape = 8,
      size = 5
    ) +
    
    geom_line(
      data = subset_LLaMa,
      aes(x = TH, y = f1, color = "F1"),
      linewidth = 1,
      linetype = "dotted"
    ) +
    geom_point(
      data = subset_LLaMa,
      aes(x = TH, y = f1, color = "F1"),
      shape = 8,
      size = 5
    ) +
    
    # =========================
  # General settings
  # =========================
  scale_color_manual(values = colors) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1)
    ) +
    
    labs(
      title = paste(
        "Prompt:", 
        hypothesis_LLM[as.numeric(unique_combinations_LLM$hyp[i])],
        "Topic:", 
        labels[as.numeric(unique_combinations_LLM$top[i])]
      ),
      x = "Threshold",
      y = "Value",
      color = "Metric"
    ) +
    
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      text = element_text(size = 12)
    )
  
  filename <- sprintf("Results/Pilot/Comparison/%s_%s_Evolution_of_metrics_comparison.png", hyp, top)
  #  ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, bg = "white")
  print(plot)
}
