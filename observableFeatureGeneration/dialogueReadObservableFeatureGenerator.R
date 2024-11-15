# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Dialogue Reading Frequency
dialogueCombineData=read.csv("sampleRawDataset/JGUXDialogueEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(dialogueCombineData)
str(dialogueCombineData)

U2dialogueStartCombineData = dialogueCombineData %>% 
  dplyr::filter(dialogueEventType == "DialogueStartEvent") %>%
  dplyr::filter(str_detect(Scenenames, "U2"))
  
U2dialogueStartCombineDataSelect = U2dialogueStartCombineData %>% dplyr::select(playerName, dialogueID)

U2DialogueReadFreq = U2dialogueStartCombineDataSelect %>% dplyr::group_by(playerName, dialogueID) %>% 
  dplyr::summarize(count = dplyr::n(), .groups = 'drop') %>% 
  pivot_wider(names_from = dialogueID, values_from = count, values_fill = list(count = 0)) %>%
  rename_with(~paste0("Dial.", .)) %>% data.frame() %>%
  rename_with(~paste0(., ".Freq")) %>% data.frame()

colnames(U2DialogueReadFreq)[1] = "playerName"

write.csv(U2DialogueReadFreq,'observableFeatures/U2DialogueReadFreq.csv', row.names = FALSE)

# Dialogue Reading Speed
U2DialogueCombineData = dialogueCombineData %>% dplyr::filter(str_detect(Scenenames, "U2"))
str(U2DialogueCombineData)

U2DialogueCombineDataSelect = U2DialogueCombineData %>% dplyr::select(playerName, timestamp, dialogueID, dialogueEventType)
str(U2DialogueCombineDataSelect)

discretizeDialogueDuration <- function(data) {
  
  data <- data[order(data$playerName, data$dialogueID, data$timestamp), ]
  
  data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%S")
  
  start_events <- subset(data, dialogueEventType == "DialogueStartEvent")
  complete_events <- subset(data, dialogueEventType == "DialogueFinishEvent")
  
  merged_data <- merge(
    start_events[, c("playerName", "dialogueID", "timestamp")],
    complete_events[, c("playerName", "dialogueID", "timestamp")],
    by = c("playerName", "dialogueID"),
    suffixes = c("_start", "_end")
  )
  
  merged_data$duration <- abs(as.numeric(difftime(merged_data$timestamp_end, merged_data$timestamp_start, units = "secs")))
  
  first_durations <- merged_data[!duplicated(merged_data[, c("playerName", "dialogueID")]), ]
  
  first_durations <- first_durations %>%
    group_by(dialogueID) %>%
    mutate(
      duration_quantile = case_when(
        duration == 0 ~ 0,
        TRUE ~ as.integer(cut(duration, breaks = quantile(duration, probs = seq(0, 1, by = 0.25), na.rm = TRUE), 
                              include.lowest = TRUE, labels = FALSE))
      )
    ) %>%
    ungroup()
  
  return(first_durations[, c("playerName", "dialogueID", "duration", "duration_quantile")])
}

U2DialogueReadingSpeed <- discretizeDialogueDuration(U2DialogueCombineDataSelect)

U2DialogueReadingSpeedS = U2DialogueReadingSpeed %>% 
  dplyr::select(playerName, dialogueID, duration_quantile) %>%
  tidyr::pivot_wider(names_from = dialogueID,
                     values_from = duration_quantile,
                     values_fill = list(duration_quantile = 0)) %>%
  rename_with(~paste0("Dial.", .)) %>% data.frame() %>%
  rename_with(~paste0(., ".Speed")) %>% data.frame()

colnames(U2DialogueReadingSpeedS)[1] = "playerName"

write.csv(U2DialogueReadingSpeedS,'observableFeatures/U2DialogueReadingSpeedS.csv', row.names = FALSE)

# Dialogue Reading Share
U2DialogueReadingShare = U2DialogueReadingSpeed %>% 
  dplyr::select(playerName,dialogueID,duration) %>% tidyr::pivot_wider(names_from = dialogueID,
                            values_from = duration,
                            values_fill = list(duration = 0)) %>%
  rename_with(~paste0("Dial.", .)) %>% data.frame() %>%
  rename_with(~paste0(., ".Duration")) %>% data.frame()

colnames(U2DialogueReadingShare)[1] = "playerName"

calculate_interaction_share <- function(data){
  
  node_columns <- names(data)[grepl("Duration$", names(data)) & names(data) != "successFreq" & names(data) != "failedFreq"]
  
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

U2DialogueReadingShareS = calculate_interaction_share(U2DialogueReadingShare)

write.csv(U2DialogueReadingShareS,'observableFeatures/U2DialogueReadingShare.csv', row.names = FALSE)















