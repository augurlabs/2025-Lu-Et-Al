# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Tool Menu Usage Frequency
menuFeatureStartCombineData=read.csv("sampleRawDataset/JGUXArfFeatureOpenEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(menuFeatureStartCombineData)

U2menuFeatureStartCombineData = menuFeatureStartCombineData %>% dplyr::filter(str_detect(Scenenames, "U2"))
str(U2menuFeatureStartCombineData)

U2menuFeatureStartCombineDataSelect = U2menuFeatureStartCombineData %>% dplyr::select(playerName, eventDescription)

U2FeatureUseFreq = U2menuFeatureStartCombineDataSelect %>% dplyr::group_by(playerName, eventDescription) %>% 
  dplyr::summarize(count = dplyr::n(), .groups = 'drop') %>% 
  pivot_wider(names_from = eventDescription, values_from = count, values_fill = list(count = 0)) %>%
  rename_with(~paste0(., ".Freq")) %>% data.frame()

colnames(U2FeatureUseFreq)[1] = "playerName"

write.csv(U2FeatureUseFreq,'observableFeatures/U2FeatureUseFreq.csv', row.names = FALSE)

# Tool Menu Usage Share
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

U2FeatureUsageShare = calculate_interaction_share(U2FeatureUseFreq)

write.csv(U2FeatureUsageShare,'observableFeatures/U2FeatureUsageShare.csv', row.names = FALSE)

# Tool Menu Usage Time duration
menuFeatureCloseCombineData=read.csv("sampleRawDataset/JGUXArfFeatureCloseEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(menuFeatureCloseCombineData)

menuFeatureCombineData = bind_rows(menuFeatureStartCombineData, menuFeatureCloseCombineData)

U2menuFeatureCombineData = menuFeatureCombineData %>% dplyr::filter(str_detect(Scenenames, "U2"))
str(U2menuFeatureCombineData)

U2menuFeatureCombineDataSelect = U2menuFeatureCombineData %>% dplyr::select(playerName, timestamp, type, eventDescription)
unique(U2menuFeatureCombineDataSelect$type)

calculateHoveringDuration <- function(data) {
  
  data <- data %>%
    mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S")) %>%
    arrange(playerName, eventDescription, timestamp)
  
  start_events <- data %>% filter(grepl("ArfFeatureOpenEvent", type))
  end_events <- data %>% filter(grepl("ArfFeatureCloseEvent", type))
  
  merged_data <- start_events %>%
    dplyr::select(playerName, eventDescription, timestamp_start = timestamp) %>%
    dplyr::mutate(event_id = dplyr::row_number()) %>%
    left_join(
      end_events %>%
        dplyr::select(playerName, eventDescription, timestamp_end = timestamp) %>%
        dplyr::mutate(event_id = dplyr::row_number()),
      by = c("playerName", "eventDescription", "event_id")
    ) %>%
    dplyr::mutate(duration = as.numeric(difftime(timestamp_end, timestamp_start, units = "mins"))) %>%
    dplyr::filter(!is.na(duration))
  
  total_hovering_duration <- merged_data %>%
    dplyr::group_by(playerName, eventDescription) %>%
    dplyr::summarize(sumhoveringtimeduration = abs(sum(duration)), .groups = 'drop')
  
  return(total_hovering_duration)
}

U2FeatureUsageDuration = calculateHoveringDuration(U2menuFeatureCombineDataSelect)
U2FeatureUsageDurationS = U2FeatureUsageDuration %>% 
  pivot_wider(names_from = eventDescription, values_from = sumhoveringtimeduration, values_fill = list(sumhoveringtimeduration = 0))%>%
  rename_with(~paste0(., ".Speed")) %>% data.frame()

colnames(U2FeatureUsageDurationS)[1] = "playerName"

write.csv(U2FeatureUsageDurationS,'observableFeatures/U2FeatureUsageDuration.csv', row.names = FALSE)



















