# Load libraries
library(dplyr)    
library(tidyr)    
library(stringr)

# Hotkey frequency
hotkeyCombineData=read.csv("sampleRawDataset/JGUXHotkeyEventDetailCombine.csv",stringsAsFactors = FALSE)[,-1]
head(hotkeyCombineData)
unique(hotkeyCombineData$description)
colnames(hotkeyCombineData)

U2hotkeyCombineData = hotkeyCombineData %>% dplyr::filter(str_detect(Scenenames, "U2"))

U2hotkeyCombineDataSelect = U2hotkeyCombineData %>% dplyr::select("playerName","keyName")

U2hotkeyFreq = U2hotkeyCombineDataSelect %>% dplyr::group_by(playerName, keyName) %>% 
  dplyr::summarize(count = dplyr::n(), .groups = 'drop') %>% 
  pivot_wider(names_from = keyName, values_from = count, values_fill = list(count = 0)) %>%
  rename_with(~paste0(., "Freq")) %>% data.frame()

colnames(U2hotkeyFreq)[1] = "playerName"

write.csv(U2hotkeyFreq,'observableFeatures/U2hotkeyFreq.csv', row.names = FALSE)

# Hotkey share
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

U2hotkeyShare = calculate_interaction_share(U2hotkeyFreq)

write.csv(U2hotkeyShare,'observableFeatures/U2hotkeyShare.csv', row.names = FALSE)



















