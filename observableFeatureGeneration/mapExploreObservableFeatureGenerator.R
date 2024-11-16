# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Read the table and process
allCombineData = read.csv("sampleRawDataset/JGUXTable.csv",stringsAsFactors = FALSE)[,-1]

# Map exploration size calculation
allCombineDataS = allCombineData %>% dplyr::select("playerName", "Scenenames", "PlayerpostionX", "PlayerpostionZ", "PlayerpostionY")
colnames(allCombineDataS)
allCombineDataS

calculate_explored_map_size <- function(data) {
  data %>%
    
    dplyr::group_by(playerName, Scenenames) %>%
    
    dplyr::summarize(
      min_x = min(PlayerpostionX, na.rm = TRUE),
      max_x = max(PlayerpostionX, na.rm = TRUE),
      min_y = min(PlayerpostionY, na.rm = TRUE),
      max_y = max(PlayerpostionY, na.rm = TRUE),
      min_z = min(PlayerpostionZ, na.rm = TRUE),
      max_z = max(PlayerpostionZ, na.rm = TRUE),
      diff_x = max_x - min_x,
      diff_y = max_y - min_y,
      diff_z = max_z - min_z,
      exploration_size = diff_x * diff_y * diff_z, 
      .groups = 'drop'
    ) %>%

    select(playerName, Scenenames, exploration_size) %>% 
    filter(exploration_size != 0) %>%
    data.frame()
}

explorationSizes <- calculate_explored_map_size(allCombineDataS)

explorationSizesS = explorationSizes %>% 
  pivot_wider(names_from = Scenenames, values_from = exploration_size, values_fill = list(exploration_size = 0)) %>%
  rename_with(~paste0("Map.", .)) %>%
  rename_with(~paste0(., ".Size")) %>% data.frame()

colnames(explorationSizesS)[1] = "playerName"

# Map exploration share
calculate_interaction_share <- function(data){
  
  node_columns <- names(data)[grepl("Size$", names(data)) & names(data) != "successFreq" & names(data) != "failedFreq"]
  
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

MapSizeShare = calculate_interaction_share(explorationSizesS)

write.csv(MapSizeShare,'observableFeatures/MapSizeShare.csv', row.names = FALSE)

# Map exploration duration
allCombineDataDurationS = allCombineData %>% dplyr::select("playerName", "Scenenames", "timestamp")
allCombineDataDurationS

calculate_exploration_duration <- function(data) {
  
  data <- data %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S")) %>%
    dplyr::arrange(playerName, timestamp)
  
  
  data <- data %>%
    dplyr::group_by(playerName) %>%
    dplyr::mutate(
      scene_chunk = cumsum(Scenenames != lag(Scenenames, default = first(Scenenames)))  
    ) %>%
    ungroup()
  
  
  chunk_durations <- data %>%
    dplyr::group_by(playerName, Scenenames, scene_chunk) %>%
    dplyr::summarize(
      start_time = min(timestamp),  
      end_time = max(timestamp),    
      duration = as.numeric(difftime(end_time, start_time, units = "secs")),  
      .groups = 'drop'
    )
  
  
  total_durations <- chunk_durations %>%
    dplyr::group_by(playerName, Scenenames) %>%
    dplyr::summarize(
      total_duration = sum(duration),  
      .groups = 'drop'
    ) %>% filter(total_duration != 0) %>%
    data.frame()
  
  return(total_durations)
}

mapExploreDuration = calculate_exploration_duration(allCombineDataDurationS)

mapExploreDurationS = mapExploreDuration %>% 
  pivot_wider(names_from = Scenenames, values_from = total_duration, values_fill = list(total_duration = 0)) %>%
  rename_with(~paste0("Map.", .)) %>%
  rename_with(~paste0(., ".Duration")) %>% data.frame()

write.csv(mapExploreDurationS,'observableFeatures/mapExploreDuration.csv', row.names = FALSE)





