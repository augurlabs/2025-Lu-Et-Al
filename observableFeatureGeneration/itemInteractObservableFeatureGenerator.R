# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Item Interaction Freq
triggerCombineData=read.csv("sampleRawDataset/JGUXTriggerEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(triggerCombineData)
str(triggerCombineData)
colnames(triggerCombineData)

U2triggerCombineData = triggerCombineData %>% dplyr::filter(str_detect(Scenenames, "U2"))

U2triggerCombineDataSelect = U2triggerCombineData %>% dplyr::select("playerName", "triggerObjectId")

U2ItemInterFreq = U2triggerCombineDataSelect %>% dplyr::group_by(playerName, triggerObjectId) %>% 
  dplyr::summarize(count = dplyr::n(), .groups = 'drop') %>% 
  pivot_wider(names_from = triggerObjectId, values_from = count, values_fill = list(count = 0)) %>%
  rename_with(~paste0("Item.", .)) %>%
  rename_with(~paste0(., ".Freq")) %>% data.frame()

colnames(U2ItemInterFreq)[1] = "playerName"

write.csv(U2ItemInterFreq,'observableFeatures/U2ItemInterFreq.csv', row.names = FALSE)

# Item Interaction Share
calculate_interaction_share <- function(data){
  
  node_columns <- names(data)[grepl("Freq$", names(data)) & names(data) != "successFreq" & names(data) != "failedFreq"]
  
  data <- data %>%
    rowwise() %>%
    dplyr::mutate(total_interaction = sum(dplyr::across(all_of(node_columns)), na.rm = TRUE)) %>%
    ungroup()
  
  data <- data %>%
    dplyr::mutate(dplyr::across(all_of(node_columns), ~ . / total_interaction, .names = "share_{.col}"))
  
  result <- data %>%
    select(playerName, starts_with("share_")) %>% data.frame()
  
  return(result)
}

U2ItemInteractionShare = calculate_interaction_share(U2ItemInterFreq)

write.csv(U2ItemInteractionShare,'observableFeatures/U2ItemInteractionShare.csv', row.names = FALSE)

# Item Interaction Speed
U2triggerCombineData

U2triggerCombineDataSelect = U2triggerCombineData %>% dplyr::select("playerName", "timestamp", "triggerObjectId")

# Function to calculate time duration per triggerObjectId for each player
calculate_interaction_duration <- function(data) {

  data <- data %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S")) %>%
    arrange(playerName, timestamp) %>%
    
    dplyr::group_by(playerName, triggerObjectId) %>%
    
    dplyr::summarize(
      start_time = min(timestamp),
      end_time = max(timestamp),
      .groups = 'drop'
    ) %>%
    
    dplyr::mutate(duration = as.numeric(difftime(end_time, start_time, units = "secs")))
  
  result <- data %>%
    select(playerName, triggerObjectId, duration)
  
  return(result)
}

U2triggerCombineDuration = calculate_interaction_duration(U2triggerCombineDataSelect)

U2triggerCombineDurationS = U2triggerCombineDuration %>%  
  pivot_wider(names_from = triggerObjectId, values_from = duration, values_fill = list(duration = 0)) %>%
  rename_with(~paste0("Item.", .)) %>%
  rename_with(~paste0(., ".Duration")) %>% data.frame()

write.csv(U2triggerCombineDurationS,'observableFeatures/U2ItemInteractionSpeed.csv', row.names = FALSE)





















