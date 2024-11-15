# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Argumentation related frequency
# Node interaction frequency
argNoteHoverStartCombineData=read.csv("sampleRawDataset/JGUXArgumentationStartHoverEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argNoteHoverStartCombineData)

argAddNodeCombineData=read.csv("sampleRawDataset/JGUXArgumentationAddNodeEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argAddNodeCombineData)

argNodeCombineData = bind_rows(argNoteHoverStartCombineData, argAddNodeCombineData)
str(argNodeCombineData)

#argNodeCombineData = argNodeCombineData %>% mutate(questandtask = paste(questID, taskKey, sep = ","))

# Filter the task table focusing on Unit 2
U3argNodeCombineData = argNodeCombineData %>% dplyr::filter(str_detect(Scenenames, "U3"))
colnames(U3argNodeCombineData)

U3argNodeCombineDataNeeded = U3argNodeCombineData %>% select("playerName","eventDescription.name")

removeMultiLetterRows <- function(data, columnName) {
  data %>%
    filter(nchar(get(columnName)) == 1)
}

U3argNodeCombineDataNeededCleaned = data.frame(removeMultiLetterRows(U3argNodeCombineDataNeeded, "eventDescription.name"))
str(U3argNodeCombineDataNeededCleaned)

countAndExtend <- function(data) {
  data %>%
    
    count(playerName, eventDescription.name) %>%
    
    pivot_wider(names_from = eventDescription.name, values_from = n, values_fill = list(n = 0))
}

U3argNodeCombineDataNeededCleanedS = U3argNodeCombineDataNeededCleaned %>% dplyr::group_by(playerName, eventDescription.name) %>% 
  dplyr::summarize(count = dplyr::n(), .groups = 'drop') %>% 
  pivot_wider(names_from = eventDescription.name, values_from = count, values_fill = list(count = 0)) %>%
  rename_with(~paste0("Node", .), everything()) %>%
  rename_with(~paste0(., "Freq")) %>% data.frame()

colnames(U3argNodeCombineDataNeededCleanedS)[1] = "playerName"

# Success submission frequency
argSuccessSubmitCombineData=read.csv("sampleRawDataset/JGUXArgumentationSuccessEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argSuccessSubmitCombineData)
unique(argSuccessSubmitCombineData$argSuccessTitle)

U3argSuccessSubmitCombineData = argSuccessSubmitCombineData %>% dplyr::filter(str_detect(Scenenames, "U3"))
str(U3argSuccessSubmitCombineData)  

U3argSuccessSubmitCombineDataC = U3argSuccessSubmitCombineData %>% 
  dplyr::group_by(playerName) %>% dplyr::summarize(successFreq = n()) %>% data.frame()

U3argFreqCombineData = U3argNodeCombineDataNeededCleanedS %>% 
  dplyr::full_join(U3argSuccessSubmitCombineDataC, by = "playerName")

U3argFreqCombineData[is.na(U3argFreqCombineData)] = 0

# Fail submission frequency
argFailedSubmitCombineData=read.csv("sampleRawDataset/JGUXArgumentationFailedEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argFailedSubmitCombineData)

U3argFailedSubmitCombineData = argFailedSubmitCombineData %>% dplyr::filter(str_detect(Scenenames, "U3"))
str(U3argFailedSubmitCombineData)  

U3argFailedSubmitCombineDataC = U3argFailedSubmitCombineData %>% 
  dplyr::group_by(playerName) %>% dplyr::summarize(failedFreq = n()) %>% data.frame()

U3argFreqCombineData = U3argFreqCombineData %>% 
  dplyr::full_join(U3argFailedSubmitCombineDataC, by = "playerName")

U3argFreqCombineData[is.na(U3argFreqCombineData)] = 0

write.csv(U3argFreqCombineData,'observableFeatures/U3ArgFrequency.csv', row.names = FALSE)

# Argumentation related speed
# Node hovering speed
argNoteHoverStartCombineData=read.csv("sampleRawDataset/JGUXArgumentationStartHoverEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argNoteHoverStartCombineData)

argNoteAddStartCombineData=read.csv("sampleRawDataset/JGUXArgumentationAddNodeEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argNoteAddStartCombineData)

argNoteAddStartCombineData$type = replace(argNoteAddStartCombineData$type, argNoteAddStartCombineData$type == "ArgumentationAddNodeEvent", "ArgumentationStartHoverEvent")

argNoteHoverEndCombineData=read.csv("sampleRawDataset/JGUXArgumentationEndHoverEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argNoteHoverEndCombineData)

argNoteAddEndCombineData=read.csv("sampleRawDataset/JGUXArgumentationRemoveNodeEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(argNoteAddEndCombineData)

argNoteAddEndCombineData$type = replace(argNoteAddEndCombineData$type, argNoteAddEndCombineData$type == "ArgumentationRemoveNodeEvent", "ArgumentationEndHoverEvent")

argNodeHoverCombineData = bind_rows(argNoteHoverStartCombineData, argNoteAddStartCombineData, argNoteHoverEndCombineData, argNoteAddEndCombineData)
str(argNodeHoverCombineData)
colnames(argNodeHoverCombineData)
unique(argNodeHoverCombineData$type)

argNodeHoverCombineDataSelect = argNodeHoverCombineData %>% 
  dplyr::select(playerName, timestamp, type, Scenenames, eventDescription.name)

U2argNodeHoverCombineDataSelect = argNodeHoverCombineDataSelect %>% dplyr::filter(str_detect(Scenenames, "U2"))

U2argNodeHoverCombineDataCleaned = data.frame(removeMultiLetterRows(U2argNodeHoverCombineDataSelect, "eventDescription.name"))
str(U2argNodeHoverCombineDataCleaned)
head(U2argNodeHoverCombineDataCleaned)
unique(U2argNodeHoverCombineDataCleaned$type)

calculateHoveringDuration <- function(data) {
  
  data <- data %>%
    mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S")) %>%
    arrange(playerName, eventDescription.name, timestamp)
  
  start_events <- data %>% filter(grepl("StartHoverEvent", type))
  end_events <- data %>% filter(grepl("EndHoverEvent", type))
  
  merged_data <- start_events %>%
    dplyr::select(playerName, eventDescription.name, timestamp_start = timestamp) %>%
    dplyr::mutate(event_id = dplyr::row_number()) %>%
    left_join(
      end_events %>%
        dplyr::select(playerName, eventDescription.name, timestamp_end = timestamp) %>%
        dplyr::mutate(event_id = dplyr::row_number()),
      by = c("playerName", "eventDescription.name", "event_id")
    ) %>%
    dplyr::mutate(duration = as.numeric(difftime(timestamp_end, timestamp_start, units = "secs"))) %>%
    dplyr::filter(!is.na(duration))
  
  total_hovering_duration <- merged_data %>%
    dplyr::group_by(playerName, eventDescription.name) %>%
    dplyr::summarize(sumhoveringtimeduration = sum(duration), .groups = 'drop')
  
  return(total_hovering_duration)
}

U2ArgHoveringDuration = calculateHoveringDuration(U2argNodeHoverCombineDataCleaned)
U2ArgHoveringDurationS = U2ArgHoveringDuration %>% 
  pivot_wider(names_from = eventDescription.name, values_from = sumhoveringtimeduration, values_fill = list(sumhoveringtimeduration = 0))%>%
  rename_with(~paste0("Node", .), everything()) %>%
  rename_with(~paste0(., "Speed")) %>% data.frame()

colnames(U2ArgHoveringDurationS)[1] = "playerName"

write.csv(U2ArgHoveringDurationS,'observableFeatures/U2ArgHoveringSpeed.csv', row.names = FALSE)

# Argumentation related share
# Node interaction share
U3argFreqCombineData = read.csv('observableFeatures/U3ArgFrequency.csv', stringsAsFactors = FALSE)

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

U3ArgShareCombineData = calculate_interaction_share(U3argFreqCombineData)

write.csv(U3ArgShareCombineData,'observableFeatures/U3ArgNodeInterShares.csv', row.names = FALSE)





















