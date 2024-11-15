# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Task dataset
TaskCombineData=read.csv("sampleRawDataset/JGUXTaskEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(TaskCombineData)

TaskCombineData = TaskCombineData %>% mutate(questandtask = paste(questID, taskKey, sep = ","))

# Filter the task table focusing on Unit 2
U2TaskCombineData = TaskCombineData %>% dplyr::filter(str_detect(Scenenames, "U2|CREi"))

# Function to extract unit string from Scenenames
#extractUnit <- function(data) {
#  data %>%
#    mutate(Unit = stringr::str_extract(Scenenames, "U[1-5]")) %>% 
#    fill(Unit, .direction = "down")
#}

#TaskCombineDataNew <- extractUnit(TaskCombineData)

U2TaskDurationNeededVar = U2TaskCombineData %>% dplyr::select("playerName","questandtask","taskEventType","timestamp")

U2TaskDurationNeededVar$timestamp = as.POSIXct(U2TaskDurationNeededVar$timestamp, format="%Y-%m-%d %H:%M:%S")

U2TaskDurationNeededVar

# Task duration and speed
discretizeTaskDuration <- function(data) {
  
  data <- data[order(data$playerName, data$questandtask, data$timestamp), ]
  
  data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%S")
  
  start_events <- subset(data, taskEventType == "TaskActiveEvent")
  complete_events <- subset(data, taskEventType == "TaskCompleteEvent")
  
  merged_data <- merge(
    start_events[, c("playerName", "questandtask", "timestamp")],
    complete_events[, c("playerName", "questandtask", "timestamp")],
    by = c("playerName", "questandtask"),
    suffixes = c("_start", "_end")
  )
  
  merged_data$duration <- as.numeric(difftime(merged_data$timestamp_end, merged_data$timestamp_start, units = "mins"))
  
  first_durations <- merged_data[!duplicated(merged_data[, c("playerName", "questandtask")]), ]
  
  first_durations <- first_durations %>%
    group_by(questandtask) %>%
    mutate(
      duration_quantile = case_when(
        duration == 0 ~ 0,
        TRUE ~ as.integer(cut(duration, breaks = quantile(duration, probs = seq(0, 1, by = 0.25), na.rm = TRUE), 
                              include.lowest = TRUE, labels = FALSE))
      )
    ) %>%
    ungroup()
  
  return(first_durations[, c("playerName", "questandtask", "duration", "duration_quantile")])
}

U2TaskCompletionDuration <- discretizeTaskDuration(U2TaskDurationNeededVar)

# Task speed observable features

U2TaskCompletionDurationSpeed = U2TaskCompletionDuration %>% dplyr::select(playerName, questandtask, duration_quantile)

U2TaskCompletionDurationSpeedL = U2TaskCompletionDurationSpeed %>% tidyr::pivot_wider(names_from = questandtask,
                                                                                      values_from = duration_quantile,
                                                                                      values_fill = list(duration_quantile = 0))

write.csv(U2TaskCompletionDurationSpeedL, "observableFeatures/U2TaskCompletionSpeed.csv", row.names = FALSE)

# Task share observable features

calculateDurationShare <- function(data) {

  totalDurationPerPlayer <- data %>%
    dplyr::group_by(playerName) %>%
    dplyr::summarize(totalDuration = sum(duration))
  
  dataWithShare <- data %>%
    dplyr::left_join(totalDurationPerPlayer, by = "playerName") %>%
    dplyr::mutate(durationShare = abs(duration / totalDuration)) %>%
    dplyr::select(playerName, questandtask, durationShare)
  
  return(dataWithShare)
}

U2TaskCompletionShare = calculateDurationShare(U2TaskCompletionDuration)

U2TaskCompletionShareL = U2TaskCompletionShare %>% tidyr::pivot_wider(names_from = questandtask,
                                                                      values_from = durationShare,
                                                                      values_fill = list(durationShare = 0))

write.csv(U2TaskCompletionShareL, "observableFeatures/U2TaskCompletionShare.csv", row.names = FALSE)







