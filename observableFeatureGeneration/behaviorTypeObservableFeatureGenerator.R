# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Read the table and process
allCombineData=read.csv("sampleRawDataset/JGUXTable.csv",stringsAsFactors = FALSE)[,-1]
head(allCombineData)
str(allCombineData)
colnames(allCombineData)

#allCombineDataS = allCombineData[1:5000,]
#write.csv(allCombineDataS,'sampleRawDataset/JGUXTableS.csv', row.names = FALSE)


unique(allCombineData$type)

allCombineDataC = allCombineData %>%
  filter(!type %in% c("GameStartEvent", "StateUpdateEvent", "GameQuitEvent"))

unique(allCombineDataC$type)

allCombineDataC = allCombineDataC %>%
  dplyr::mutate(type = case_when(
    type == "QuestEvent" ~ "TaskCompletion",
    type == "TaskEvent" ~ "TaskCompletion",
    type == "DialogueNodeEvent" ~ "DialogueEvent",
    type == "ArfMenuOpenEvent" ~ "ToolInteraction",
    type == "ArfFeatureOpenEvent" ~ "ToolInteraction",
    type == "ArfFeatureCloseEvent" ~ "ToolInteraction",
    type == "ArfMenuCloseEvent" ~ "ToolInteraction",
    type == "ArgumentationOpenedEvent" ~ "ArgumentInteraction",
    type == "ArgumentationStartHoverEvent" ~ "ArgumentInteraction",
    type == "ArgumentationEndHoverEvent" ~ "ArgumentInteraction",
    type == "ArgumentationAddNodeEvent" ~ "ArgumentInteraction",
    type == "ArgumentationSuccessEvent" ~ "ArgumentInteraction",
    type == "ArgumentationContinuedEvent" ~ "ArgumentInteraction",
    type == "ArgumentationRewardEvent" ~ "ArgumentInteraction",
    type == "ArgumentationScrapEvent" ~ "ArgumentInteraction",
    type == "ArgumentationClosedEvent" ~ "ArgumentInteraction",
    type == "ArgumentationRemoveNodeEvent" ~ "ArgumentInteraction",
    type == "ArgumentationFailedEvent" ~ "ArgumentInteraction",
    type == "ArgumentationRetryEvent" ~ "ArgumentInteraction",
    type == "JumpEvent" ~ "MovementEvent",
    type == "ToggleHoverboardEvent" ~ "MovementEvent",
    type == "CREiBallSpawn" ~ "CREiSystem",
    type == "CREiBallRespawn" ~ "CREiSystem",
    type == "CREiCorrect" ~ "CREiSystem",
    type == "CREiReset" ~ "CREiSystem",
    type == "CREiIncorrect" ~ "CREiSystem",
    type == "CREiTargetScoreHit" ~ "CREiSystem",
    TRUE ~ type  
  ))

unique(allCombineDataC$type)
colnames(allCombineDataC)

# Behavior type frequency
U2allCombineDataCFreqS = allCombineDataC %>% dplyr::filter(str_detect(Scenenames, "U2")) %>% 
  dplyr::select(playerName, type)

U2BehaviorTypeFreq = U2allCombineDataCFreqS %>% dplyr::group_by(playerName, type) %>% 
  dplyr::summarize(count = dplyr::n(), .groups = 'drop') %>% 
  pivot_wider(names_from = type, values_from = count, values_fill = list(count = 0)) %>%
  rename_with(~paste0("BType.", .)) %>%
  rename_with(~paste0(., ".Freq")) %>% data.frame()

colnames(U2BehaviorTypeFreq)[1] = "playerName"

write.csv(U2BehaviorTypeFreq,'observableFeatures/U2BehabiorTypeFreq.csv', row.names = FALSE)

# Behavior type share
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

U2BehabiorTypeShare = calculate_interaction_share(U2BehaviorTypeFreq)

write.csv(U2BehabiorTypeShare,'observableFeatures/U2BehabiorTypeShare.csv', row.names = FALSE)

# Behavior type duration
colnames(allCombineDataC)

U2allCombineDataCS = allCombineDataC %>% dplyr::filter(str_detect(Scenenames, "U2")) %>% 
  dplyr::select(playerName, timestamp, type)

calculate_behavior_duration <- function(data) {
  
  data <- data %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S")) %>%
    dplyr::arrange(playerName, timestamp)
  
  data <- data %>%
    dplyr::group_by(playerName) %>%
    dplyr::mutate(
      type_chunk = cumsum(type != lag(type, default = first(type)))
    ) %>%
    ungroup()
  
  chunk_durations <- data %>%
    dplyr::group_by(playerName, type, type_chunk) %>%
    dplyr::summarize(
      start_time = min(timestamp),  
      end_time = max(timestamp),    
      .groups = 'drop'
    ) %>%
    
    dplyr::mutate(duration = as.numeric(difftime(end_time, start_time, units = "secs"))) %>%
    dplyr::group_by(playerName, type) %>%
    
    dplyr::summarize(total_duration = sum(duration), .groups = 'drop')
  
  return(chunk_durations)
}

U2BehaviorTypeDuration = calculate_behavior_duration(U2allCombineDataCS)

U2BehaviorTypeDurationS = U2BehaviorTypeDuration %>% 
  pivot_wider(names_from = type, values_from = total_duration, values_fill = list(count = 0))


write.csv(U2BehaviorTypeDurationS,'observableFeatures/U2BehabiorTypeDuration.csv', row.names = FALSE)



























