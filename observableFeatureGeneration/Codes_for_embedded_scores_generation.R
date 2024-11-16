# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(mongolite)

# Fetch and save the table directly extract from the server
Returntidywholetablebyname <- function(connect, playerNamelist) {
  Wholegeneraltable <- list()  # Use a list for efficient row-binding
  
  for (playerName in playerNamelist) {
    # Retrieve data for the player
    InitialPlayer <- mhsEventsForPlayerNamed(connect, playerName)
    colnames(InitialPlayer)[1] <- "ItemID"
    
    # Process scene names
    ScenenameC <- sapply(InitialPlayer$sceneNames, function(temp) paste(temp, collapse = ","))
    ScenenameCF <- data.frame(Scenenames = unique(ScenenameC))
    
    # Extract player and camera positions
    PlayerpostionX <- InitialPlayer$playerPosition$x
    PlayerpostionZ <- InitialPlayer$playerPosition$z
    PlayerpostionY <- InitialPlayer$playerPosition$y
    CamerarotationX <- InitialPlayer$cameraDirection$x
    CamerarotationZ <- InitialPlayer$cameraDirection$z
    CamerarotationY <- InitialPlayer$cameraDirection$y
    
    # Process quest and task data
    if (nrow(ScenenameCF) == 1) {
      QuestTableF <- data.frame(QuestTableF = rep("NA", nrow(InitialPlayer)))
      TasktableF <- data.frame(TasktableF = rep("NA", nrow(InitialPlayer)))
    } else {
      # Process quests
      QuestTable <- sapply(InitialPlayer$quests, function(temp) paste(temp, collapse = ","))
      QuestTableF <- data.frame(QuestTableF = QuestTable)
      
      # Process tasks
      Tasktable <- sapply(InitialPlayer$taskItems, function(temp) {
        if (length(temp) == 0) {
          "NA"
        } else {
          paste(apply(temp, 1, function(row) paste(row, collapse = ",")), collapse = ";")
        }
      })
      TasktableF <- data.frame(TasktableF = Tasktable)
    }
    
    # Combine quest and task data
    QuestTask <- data.frame(QuestTableF, TasktableF)
    
    # Extract other data
    buildVersion <- InitialPlayer$buildVersion
    
    # Final table
    excludedCols <- c("sceneNames", "playerPosition", "cameraDirection", "taskItems", "quests")
    FinaltableT <- InitialPlayer[, !names(InitialPlayer) %in% excludedCols]
    FinaltableTF <- data.frame(
      FinaltableT,
      PlayerpostionX, PlayerpostionZ, PlayerpostionY,
      CamerarotationX, CamerarotationZ, CamerarotationY,
      QuestTask,
      ScenenameCF,
      buildVersion
    )
    
    Wholegeneraltable[[playerName]] <- FinaltableTF
  }
  
  # Combine all player tables into one data frame
  Wholegeneraltable <- do.call(rbind, Wholegeneraltable)
  return(Wholegeneraltable)
}

write.csv(argumentSuccessStates, "rawData/wholeGeneralTable.csv")

# Process argument success
process_argument_success <- function(data) {
  argumentSuccess <- data %>% filter(type == "ArgumentationSuccessEvent")
  
  argumentSuccessDetail <- bind_rows(lapply(1:nrow(argumentSuccess), function(i) {
    temp <- mhsEventDetailsById(conn, argumentSuccess$ItemID[i])
    extract_argument_details(temp, "success")
  }))
  
  argumentSuccessCombine <- cbind(argumentSuccess, argumentSuccessDetail)
  playerList <- unique(argumentSuccessCombine$playerName)
  
  argumentSuccessStates <- bind_rows(lapply(playerList, function(player) {
    temp <- argumentSuccessCombine %>% filter(playerName == player)
    bind_rows(lapply(unique(temp$argumentTitle), function(title) {
      temp1 <- temp %>% filter(argumentTitle == title)
      data.frame(
        playerName = player,
        teacherId = temp1$teacherId[1],
        argumentTitle = title,
        result = "success",
        maxAttemptTime = max(temp1$attemptTime),
        feedBacks = paste(unique(temp1$feedbackGot), collapse = ";"),
        successTrialSum = nrow(temp1)
      )
    }))
  }))
  
  write.csv(argumentSuccessStates, "rawData/argumentSuccessStatesNew.csv")
  argumentSuccessStates
}

# Process argument failure
process_argument_failure <- function(data) {
  argumentFailure <- data %>% filter(type == "ArgumentationFailedEvent")
  
  argumentFailureDetail <- bind_rows(lapply(1:nrow(argumentFailure), function(i) {
    temp <- mhsEventDetailsById(conn, argumentFailure$ItemID[i])
    
    claimChoseName <- temp$eventDescription$claim$name
    reasoningChoseName <- temp$eventDescription$solutionReasonings[[1]]$reasoning$name
    evidenceChoseNameList <- temp$eventDescription$solutionReasonings[[1]]$evidences[[1]]$name
    evidenceChoseName <- paste(evidenceChoseNameList[evidenceChoseNameList != ""], collapse = "")
    
    claimChoseContent <- temp$eventDescription$claim$description
    reasoningChoseContent <- temp$eventDescription$solutionReasonings[[1]]$reasoning$description
    evidenceChoseContentList <- temp$eventDescription$solutionReasonings[[1]]$evidences[[1]]$description
    evidenceChoseContent <- paste(evidenceChoseContentList[evidenceChoseContentList != ""], collapse = ",")
    
    data.frame(
      argumentTitle = temp$eventDescription$argument$title,
      argumentName = temp$eventDescription$argument$name,
      result = "failure",
      attemptTime = temp$eventDescription$argument$attempt,
      feedbackGot = ifelse(is.null(temp$eventDescription$feedback), " ", temp$eventDescription$feedback),
      argumentCombineChoice = paste(claimChoseName, reasoningChoseName, evidenceChoseName, sep = ","),
      argumentCombineContent = paste(claimChoseContent, reasoningChoseContent, evidenceChoseContent, sep = ";")
    )
  }))
  
  argumentFailureCombine <- cbind(argumentFailure, argumentFailureDetail)
  write.csv(argumentFailureCombine, "rawData/argumentFailureCombineNew.csv")
  
  playerList <- unique(argumentFailureCombine$playerName)
  argumentFailureStates <- bind_rows(lapply(playerList, function(player) {
    temp <- argumentFailureCombine %>% filter(playerName == player) %>% distinct(timestamp, .keep_all = TRUE)
    bind_rows(lapply(unique(temp$argumentTitle), function(title) {
      temp1 <- temp %>% filter(argumentTitle == title)
      data.frame(
        playerName = player,
        teacherId = temp1$teacherId[1],
        argumentTitle = title,
        result = "failure",
        maxAttemptTime = max(temp1$attemptTime),
        feedBacks = paste(unique(temp1$feedbackGot), collapse = ";"),
        failureTrialSum = nrow(temp1),
        argumentFailureChoice = temp1$argumentCombineChoice[1],
        argumentFailureContent = temp1$argumentCombineContent[1]
      )
    }))
  }))
  
  write.csv(argumentFailureStates, "rawData/argumentFailureStatesNew.csv")
  argumentFailureStates
}

# Main calculation
argumentSuccessStates <- process_argument_success(filedTestTwoNew)
argumentFailureStates <- process_argument_failure(filedTestTwoNew)

# Combine success and failure
argumentFullStatement <- full_join(argumentSuccessStates, argumentFailureStates, by = c("playerName", "argumentTitle"))
write.csv(argumentFullStatement, "rawData/argumentFullStatementNew.csv")

# Process first item score
calculate_first_item_score <- function(data) {
  relatedCol <- data %>% select(playerName, Argumentation.Tutorial_success, Argumentation.Tutorial_failure)
  playerList <- unique(relatedCol$playerName)
  
  firstItemScore <- bind_rows(lapply(playerList, function(player) {
    temp <- relatedCol %>% filter(playerName == player)
    tutorialArgScore <- ifelse(temp$Argumentation.Tutorial_success > 0 & temp$Argumentation.Tutorial_failure == 0, 1, 0)
    data.frame(playerName = player, tutorialArgScore = tutorialArgScore)
  }))
  
  firstItemScore
}

relatedCol <- argumentFullStatement %>%
  select(playerName, Argumentation.Tutorial_success, Argumentation.Tutorial_failure)

firstItemScore <- calculate_first_item_score(relatedCol)

# Save first item score
write.csv(firstItemScore, "firstItemScore.csv")

# 2nd embedded score: 4.2 argue which watershed is bigger
relatedCol1 <- argumentFullStatementNew %>%
  select(playerName, Watershed_success, Watershed_failure)

# Helper function to calculate the score for each player
calculate_bigger_arg_score <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  if (temp$Watershed_success > 0 & temp$Watershed_failure < 3) {
    biggerArgScore <- 2
  } else if (temp$Watershed_success > 0 & temp$Watershed_failure == 3) {
    biggerArgScore <- 1
  } else {
    biggerArgScore <- 0
  }
  data.frame(playerName = player, biggerArgScore = biggerArgScore)
}

# Generate scores for all players
secondItemScore <- bind_rows(lapply(unique(relatedCol1$playerName), calculate_bigger_arg_score, data = relatedCol1))

# View the resulting data frame structure
str(secondItemScore)

# Optionally save the results to a CSV file
write.csv(secondItemScore, "secondItemScore.csv", row.names = FALSE)

# 3rd embedded score: 3.1 convince bill the pollutant is nearby
relatedCol2 <- argumentFullStatementNew %>%
  select(playerName, Pollution.Upstream_success, Pollution.Upstream_failure)

# Helper function to calculate the upstream argument score for each player
calculate_upstream_arg_score <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  
  # Apply the conditions to determine the score
  upstreamArgScore <- ifelse(
    temp$Pollution.Upstream_success > 0 & temp$Pollution.Upstream_failure < 3, 2,
    ifelse(
      temp$Pollution.Upstream_success > 0 & temp$Pollution.Upstream_failure >= 3 & temp$Pollution.Upstream_failure < 6, 1,
      0
    )
  )
  
  data.frame(playerName = player, upstreamArgScore = upstreamArgScore)
}

# Generate scores for all players
thirdItemScore <- bind_rows(lapply(unique(relatedCol2$playerName), calculate_upstream_arg_score, data = relatedCol2))

# View the resulting data frame structure
str(thirdItemScore)

# Optionally save the results to a CSV file
write.csv(thirdItemScore, "thirdItemScore.csv", row.names = FALSE)

# 4th embedded score: 9.1 who flooded the armory
relatedCol3 <- argumentFullStatementNew %>%
  select(playerName, Flooding.the.armory_failure, Flooding.the.armory_success)

# Helper function to calculate the flooding armory score for each player
calculate_flood_armory_score <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  
  # Apply the conditions to determine the score
  floodArmoryScore <- ifelse(
    temp$Flooding.the.armory_success > 0 & temp$Flooding.the.armory_failure < 3, 2,
    ifelse(
      temp$Flooding.the.armory_success > 0 & temp$Flooding.the.armory_failure >= 3 & temp$Flooding.the.armory_failure < 6, 1,
      0
    )
  )
  
  data.frame(playerName = player, floodArmoryScore = floodArmoryScore)
}

# Generate scores for all players
forthItemScore <- bind_rows(lapply(unique(relatedCol3$playerName), calculate_flood_armory_score, data = relatedCol3))

# View the resulting data frame structure
str(forthItemScore)

# Optionally save the results to a CSV file
write.csv(forthItemScore, "forthItemScore.csv", row.names = FALSE)

# 5th embedded score: 5.1 convince Bill
relatedCol4 <- argumentFullStatementNew %>%
  select(playerName, Making.water.for.Bill_failure, Making.water.for.Bill_success)

# Helper function to calculate the convince Bill score for each player
calculate_convince_bill_score <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  
  # Apply the conditions to determine the score
  convinceBillScore <- ifelse(
    temp$Making.water.for.Bill_success > 0 & temp$Making.water.for.Bill_failure < 3, 2,
    ifelse(
      temp$Making.water.for.Bill_success > 0 & temp$Making.water.for.Bill_failure >= 3 & temp$Making.water.for.Bill_failure < 6, 1,
      0
    )
  )
  
  data.frame(playerName = player, convinceBillScore = convinceBillScore)
}

# Generate scores for all players
fifthItemScore <- bind_rows(lapply(unique(relatedCol4$playerName), calculate_convince_bill_score, data = relatedCol4))

# View the resulting data frame structure
str(fifthItemScore)

# Optionally save the results to a CSV file
write.csv(fifthItemScore, "fifthItemScore.csv", row.names = FALSE)

# 6th embedded score: CREi system (3 and all the rest)

ballIncor <- filedTestTwoNew %>% filter(type == "CREiIncorrect")
ballCor <- filedTestTwoNew %>% filter(type == "CREiCorrect")

# Function to process CREi details
process_crei_details <- function(data, outcome_label) {
  bind_rows(lapply(1:nrow(data), function(i) {
    temp <- mhsEventDetailsById(conn, data$ItemID[i])
    data.frame(
      rightType = temp$eventDescription$component$componentType,
      rightContent = temp$eventDescription$component$componentText,
      chooseArguType = temp$eventDescription$componentType,
      outcome = outcome_label
    )
  }))
}

# Process incorrect and correct details
ballIncorDetail <- process_crei_details(ballIncor, "incorrect")
ballCorDetail <- process_crei_details(ballCor, "correct")

# Combine details with original data
ballIncorCombine <- cbind(ballIncor, ballIncorDetail)
ballCorCombine <- cbind(ballCor, ballCorDetail)

# Combine incorrect and correct events
ballStatement <- rbind(ballIncorCombine, ballCorCombine)
write.csv(ballStatement, "rawData/ballStatementNew.csv", row.names = FALSE)

# Reload processed data if needed
ballStatement <- read.csv("rawData/ballStatement.csv", stringsAsFactors = FALSE)[, -1]

# Extract unique player names
playerListCREi <- unique(ballStatement$playerName)

# Calculate CREi numbers for each player
calculate_crei_numbers <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  incorrectNumber <- nrow(temp %>% filter(outcome == "incorrect"))
  correctNumber <- nrow(temp %>% filter(outcome == "correct"))
  
  incor.Evidence <- nrow(temp %>% filter(outcome == "incorrect", chooseArguType == "EVIDENCE"))
  incor.Reasoning <- nrow(temp %>% filter(outcome == "incorrect", chooseArguType == "REASONING"))
  incor.Claim <- nrow(temp %>% filter(outcome == "incorrect", chooseArguType == "CLAIM"))
  
  cor.Evidence <- nrow(temp %>% filter(outcome == "correct", chooseArguType == "EVIDENCE"))
  cor.Reasoning <- nrow(temp %>% filter(outcome == "correct", chooseArguType == "REASONING"))
  cor.Claim <- nrow(temp %>% filter(outcome == "correct", chooseArguType == "CLAIM"))
  
  data.frame(
    playerName = player,
    incorrectNumber = incorrectNumber,
    correctNumber = correctNumber,
    incor.Evidence = incor.Evidence,
    incor.Reasoning = incor.Reasoning,
    incor.Claim = incor.Claim,
    cor.Evidence = cor.Evidence,
    cor.Reasoning = cor.Reasoning,
    cor.Claim = cor.Claim
  )
}

# Generate CREi numbers for all players
CREiNumbers <- bind_rows(lapply(playerListCREi, calculate_crei_numbers, data = ballStatement))
write.csv(CREiNumbers, "rawData/CREiNumbersNew.csv", row.names = FALSE)

# Reload processed CREi numbers if needed
CREiNumbersNew <- read.csv("rawData/CREiNumbersCombineWithTC.csv", stringsAsFactors = FALSE)[, -1]

# Calculate CREi score for sixth item
relatedCol5 <- CREiNumbersNew %>%
  select(playerName, incorrectNumber, correctNumber)

calculate_crei_score <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  
  if (nrow(temp) == 0) {
    CREIScore <- 0
  } else {
    sumNumber <- sum(temp$incorrectNumber, temp$correctNumber)
    CREIScore <- (temp$correctNumber / sumNumber * 1) + (temp$incorrectNumber / sumNumber * (-1 / 3))
  }
  
  data.frame(playerName = player, CREIScore = CREIScore)
}

# Generate CREi scores for all players
sixthItemScore <- bind_rows(lapply(unique(relatedCol5$playerName), calculate_crei_score, data = relatedCol5))

# View structure and save the results
str(sixthItemScore)
write.csv(sixthItemScore, "sixthItemScore.csv", row.names = FALSE)

# 7th embedded score: 5.2 Jasper's proposal
# Select dialogue events
dialogueEvents <- filedTestTwo %>% filter(type == "DialogueEvent")
dialogueNodeEvents <- filedTestTwoNew %>% filter(type == "DialogueNodeEvent")

# Helper function to process dialogue details
process_dialogue_details <- function(events, conn) {
  bind_rows(lapply(1:nrow(events), function(i) {
    temp <- mhsEventDetailsById(conn, events$ItemID[i])
    temp
  }))
}

# Process dialogue details
dialogueDetail <- process_dialogue_details(dialogueEvents, conn)
dialogueCombine <- cbind(dialogueEvents, dialogueDetail)
write.csv(dialogueCombine, "rawData/dialogueCombine.csv", row.names = FALSE)

# Process dialogue node details
dialogueNodeDetail <- process_dialogue_details(dialogueNodeEvents, conn)
dialogueNodeCombine <- cbind(dialogueNodeEvents, dialogueNodeDetail)
write.csv(dialogueNodeCombine, "rawData/dialogueNodeCombineNew.csv", row.names = FALSE)

# Combine dialogue node versions
dialogueNodeCombine <- read.csv("rawData/dialogueNodeCombine.csv", stringsAsFactors = FALSE)[, -1]
dialogueNodeCombineNew <- read.csv("rawData/dialogueNodeCombineNew.csv", stringsAsFactors = FALSE)[, -1]
dialogueNodeAllVersion <- rbind(dialogueNodeCombine, dialogueNodeCombineNew)
write.csv(dialogueNodeAllVersion, "rawData/dialogueNodeAllVersion.csv", row.names = FALSE)

# Prepare critique data
dialogueReference <- returnTidyDialogueReferenceData(conn, "0.11.171")
dialogue5 <- c("Is there anything wrong with it?")
inDialogue4 <- dialogueReference[grep(dialogue5, dialogueReference$content), ]

# Filter dialogue data for Jasper's critique
dialogueNode5 <- dialogueNodeCombineNew %>% filter(dialogueID == 425)
dialogueNode5F <- dialogueNode5 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Add choice content
choiceID <- c(0, 1, 2)
choiceContent <- c("Agree with Jasper.", "Disagree, Jasper forgot a claim.", "Disagree, Jasper forgot evidence.")
dialogueChoice5 <- data.frame(choiceID, choiceContent)
dialogueNode5FWChoiceC <- left_join(dialogueNode5F, dialogueChoice5, by = "choiceID")

# Calculate frequencies of player choices
calculate_jasper_critique <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  if (nrow(temp) == 0) {
    return(NULL)
  }
  
  choices <- unique(temp$choiceContent)
  bind_rows(lapply(choices, function(choice) {
    number <- nrow(temp %>% filter(choiceContent == choice))
    data.frame(playerName = player, choiceContent = choice, number = number)
  }))
}

playerList <- unique(dialogueNodeCombineNew$playerName)
JasperCritique <- bind_rows(lapply(playerList, calculate_jasper_critique, data = dialogueNode5FWChoiceC))

# Reshape data
JasperCritiqueFirst <- spread(JasperCritique, choiceContent, number)
JasperCritiqueFirst[is.na(JasperCritiqueFirst)] <- 0

# Add understanding metric
JasperCritiqueFirst$understanding <- ifelse(
  JasperCritiqueFirst$`Agree with Jasper.` > 0 | JasperCritiqueFirst$`Disagree, Jasper forgot a claim.` > 0, 0, 1
)

write.csv(JasperCritiqueFirst, "rawData/JasperCritiqueFirstNew.csv", row.names = FALSE)

# Calculate the seventh item score
relatedCol6 <- JasperCritiqueFirst
calculate_seventh_item_score <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  if (nrow(temp) == 0) {
    JasperCritiqueScore <- 0
  } else {
    sumNumber <- sum(temp$`Agree with Jasper.`, temp$`Disagree, Jasper forgot a claim.`, temp$`Disagree, Jasper forgot evidence.`)
    JasperCritiqueScore <- (temp$`Disagree, Jasper forgot evidence.` / sumNumber * 1) + 
      (sum(temp$`Agree with Jasper.`, temp$`Disagree, Jasper forgot a claim.`) / sumNumber * 0)
  }
  data.frame(playerName = player, JasperCritiqueScore = JasperCritiqueScore)
}

seventhItemScore <- bind_rows(lapply(unique(relatedCol6$playerName), calculate_seventh_item_score, data = relatedCol6))

# Save the seventh item score
write.csv(seventhItemScore, "seventhItemScore.csv", row.names = FALSE)

# 8th embedded score: 3.2 Drill to the water table
dialogueNode8 <- dialogueNodeCombineNew %>% filter(dialogueID == 578)
dialogueNode81 <- dialogueNodeCombineNew %>% filter(dialogueID == 579)

# Helper function to calculate the drill room score for each player
calculate_drill_room_score <- function(player, data578, data579) {
  temp <- data578 %>% filter(playerName == player, dialogueNodeEventType == "DialogueNodeFinishEvent")
  temp1 <- data579 %>% filter(playerName == player, dialogueNodeEventType == "DialogueNodeFinishEvent")
  
  if (nrow(temp) == 0 && nrow(temp1) == 0) {
    drillRoomScore <- 0
  } else {
    sumNumber <- sum(nrow(temp), nrow(temp1))
    drillRoomScore <- (nrow(temp) / sumNumber * 1) + (nrow(temp1) / sumNumber * 0)
  }
  
  data.frame(playerName = player, drillRoomScore = drillRoomScore)
}

# Generate scores for all players
eighthItemScore <- bind_rows(lapply(playerList, calculate_drill_room_score, data578 = dialogueNode8, data579 = dialogueNode81))

# View the structure of the resulting score data
str(eighthItemScore)

# Save the results to a CSV file
write.csv(eighthItemScore, "eighthItemScore.csv", row.names = FALSE)

# 9th embedded score: 9.2 who flooded the warehouse
dialogue10 <- c("This is a pretty serious accusation")
inDialogue9 <- dialogueFreference[grep(dialogue10, dialogueFreference$content), ]

# Filter dialogue node data for dialogue ID 479
dialogueNode10 <- dialogueNodeCombineNew %>% filter(dialogueID == 479)
dialogueNode10F <- dialogueNode10 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Define choice IDs and corresponding content
choiceID <- c(0, 1, 2)
choiceContent <- c(
  "Agree, fountain caused the flooding.",
  "Disagree, reasoning doesn't support evidence.",
  "Disagree, Anderson using wrong evidence."
)
dialogueChoice10 <- data.frame(choiceID, choiceContent)

# Merge dialogue node data with choice content
dialogueNode10FWChoiceC <- left_join(dialogueNode10F, dialogueChoice10, by = "choiceID")

# Function to calculate critique frequencies for each player
calculate_critique_frequencies <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  
  if (nrow(temp) == 0) return(NULL)
  
  choices <- unique(temp$choiceContent)
  
  bind_rows(lapply(choices, function(choice) {
    number <- nrow(temp %>% filter(choiceContent == choice))
    data.frame(playerName = player, choiceContent = choice, number = number)
  }))
}

# Generate critique frequencies for all players
playerList <- unique(dialogueNodeCombineNew$playerName)
JasperCritique <- bind_rows(lapply(playerList, calculate_critique_frequencies, data = dialogueNode10FWChoiceC))

# Reshape data for analysis
andersonClaim <- spread(JasperCritique, choiceContent, number)
andersonClaim[is.na(andersonClaim)] <- 0

# Add understanding metric
andersonClaim$understanding <- ifelse(
  andersonClaim$`Agree, fountain caused the flooding.` > 0 |
    andersonClaim$`Disagree, reasoning doesn't support evidence.` > 0, 0, 1
)

# Save the results to a CSV file
write.csv(andersonClaim, "andersonClaimNew.csv", row.names = FALSE)

# 10th embedded score: which fountain should be overflow
# Extract dialogue reference containing the specific phrase
dialogue11 <- c("Which fountain do you think is the best?")
inDialogue10 <- dialogueFreference[grep(dialogue11, dialogueFreference$content), ]

# Filter dialogue node data for dialogue ID 398
dialogueNode11 <- dialogueNodeCombineNew %>% filter(dialogueID == 398)
dialogueNode11F <- dialogueNode11 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Define choice IDs and corresponding content
choiceID <- c(0, 1, 2, 3)
choiceContent <- c("Fountain 1", "Fountain 2", "Fountain 3", "Let me check again.")
dialogueChoice11 <- data.frame(choiceID, choiceContent)

# Merge dialogue node data with choice content
dialogueNode11FWChoiceC <- left_join(dialogueNode11F, dialogueChoice11, by = "choiceID")

# Function to calculate critique frequencies for each player
calculate_fountain_choices <- function(player, data) {
  temp <- data %>% filter(playerName == player)
  
  if (nrow(temp) == 0) return(NULL)
  
  choices <- unique(temp$choiceContent)
  
  bind_rows(lapply(choices, function(choice) {
    number <- nrow(temp %>% filter(choiceContent == choice))
    data.frame(playerName = player, choiceContent = choice, number = number)
  }))
}

# Generate critique frequencies for all players
playerList <- unique(dialogueNodeCombineNew$playerName)
JasperCritique <- bind_rows(lapply(playerList, calculate_fountain_choices, data = dialogueNode11FWChoiceC))

# Reshape data for analysis
fountainChoose <- spread(JasperCritique, choiceContent, number)
fountainChoose[is.na(fountainChoose)] <- 0

# Add understanding metric
fountainChoose$understanding <- ifelse(
  fountainChoose$`Fountain 2` > 0 | fountainChoose$`Fountain 3` > 0, 0, 1
)

# Save the results to a CSV file
write.csv(fountainChoose, "fountainChooseNew.csv", row.names = FALSE)

# 11th embedded score: 7.1 setup the first piece
# Extract dialogue reference containing the specific phrase
dialogue12 <- c("which one should we use")
inDialogue11 <- dialogueFreference[grep(dialogue12, dialogueFreference$content), ]

# Filter dialogue node data for dialogue ID 467
dialogueNode12 <- dialogueNodeCombineNew %>% filter(dialogueID == 467)
dialogueNode12F <- dialogueNode12 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Define choice IDs and corresponding content
choiceID <- c(0, 1)
choiceContent <- c("The Condenser", "The Evaporator")
dialogueChoice12 <- data.frame(choiceID, choiceContent)

# Merge dialogue node data with choice content
dialogueNode12FWChoiceC <- left_join(dialogueNode12F, dialogueChoice12, by = "choiceID")

# Display structure of the resulting dataset
str(dialogueNode12FWChoiceC)

write.csv(dialogueNode12FWChoiceC, "dialogueNode12FWChoiceC.csv", row.names = FALSE)

# 12th embedded score: 7.2 setup the second piece
# Extract dialogue reference containing the specific phrase
dialogue13 <- c("so what should this one be")
inDialogue12 <- dialogueFreference[grep(dialogue13, dialogueFreference$content), ]

# Filter dialogue node data for dialogue ID 469
dialogueNode13 <- dialogueNodeCombineNew %>% filter(dialogueID == 469)
dialogueNode13F <- dialogueNode13 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Define choice IDs and corresponding content
choiceID <- c(0, 1)
choiceContent <- c("The Condenser", "The Evaporator")
dialogueChoice13 <- data.frame(choiceID, choiceContent)

# Merge dialogue node data with choice content
dialogueNode13FWChoiceC <- left_join(dialogueNode13F, dialogueChoice13, by = "choiceID")

# Display structure of the resulting dataset
str(dialogueNode13FWChoiceC)

write.csv(dialogueNode13FWChoiceC, "dialogueNode13FWChoiceC.csv", row.names = FALSE)

# 13th embedded score: 7.3 setup the third piece
# Extract dialogue reference containing the specific phrase
dialogue14 <- c("which one of these will help us")
inDialogue13 <- dialogueFreference[grep(dialogue14, dialogueFreference$content), ]

# Filter dialogue node data for dialogue ID 471
dialogueNode14 <- dialogueNodeCombineNew %>% filter(dialogueID == 471)
dialogueNode14F <- dialogueNode14 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Define choice IDs and corresponding content
choiceID <- c(0, 1)
choiceContent <- c("The Condenser", "The Evaporator")
dialogueChoice14 <- data.frame(choiceID, choiceContent)

# Merge dialogue node data with choice content
dialogueNode14FWChoiceC <- left_join(dialogueNode14F, dialogueChoice14, by = "choiceID")

# Display structure of the resulting dataset
str(dialogueNode14FWChoiceC)

write.csv(dialogueNode14FWChoiceC, "dialogueNode14FWChoiceC.csv", row.names = FALSE)

# 14th embedded score: 7.4 setup the fourth piece
# Extract dialogue reference containing the specific phrase
dialogue15 <- c("Which one do you think?")
inDialogue14 <- dialogueFreference[grep(dialogue15, dialogueFreference$content), ]

# Filter dialogue node data for dialogue ID 473
dialogueNode15 <- dialogueNodeCombineNew %>% filter(dialogueID == 473)
dialogueNode15F <- dialogueNode15 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Define choice IDs and corresponding content
choiceID <- c(0, 1)
choiceContent <- c("The Condenser", "The Evaporator")
dialogueChoice15 <- data.frame(choiceID, choiceContent)

# Merge dialogue node data with choice content
dialogueNode15FWChoiceC <- left_join(dialogueNode15F, dialogueChoice15, by = "choiceID")

# Validate the structure of the resulting dataset
str(dialogueNode15FWChoiceC)

write.csv(dialogueNode15FWChoiceC, "dialogueNode15FWChoiceC.csv", row.names = FALSE)

# 15th embedded score: 1.2 Deliver Sam's supplies
dialogueNode22 <- dialogueNodeCombineNew %>% filter(dialogueID == 37)
dialogueNode22F <- dialogueNode22 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

dialogueNode23 <- dialogueNodeCombineNew %>% filter(dialogueID == 38)
dialogueNode23F <- dialogueNode23 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Assign filtered data to related columns
relatedCol20 <- dialogueNode22F
relatedCol201 <- dialogueNode23F

# Function to calculate crate delivery score for each player
calculate_crate_delivery_score <- function(player, correct_data, wrong_data) {
  temp <- correct_data %>% filter(playerName == player)
  temp1 <- wrong_data %>% filter(playerName == player)
  
  correctCount <- nrow(temp)
  wrongCount <- nrow(temp1)
  
  if (nrow(temp) == 0 && nrow(temp1) == 0) {
    crateDeliveryScore <- 0
  } else {
    sumNumber <- sum(correctCount, wrongCount)
    crateDeliveryScore <- (correctCount / sumNumber * 1) + (wrongCount / sumNumber * 0)
  }
  
  data.frame(playerName = player, crateDeliveryScore = crateDeliveryScore)
}

# Generate crate delivery scores for all players
twentyOneItemScore <- bind_rows(lapply(playerList, calculate_crate_delivery_score, 
                                       correct_data = relatedCol20, wrong_data = relatedCol201))

# View structure of the resulting data
str(twentyOneItemScore)

# Save the results to a CSV file
write.csv(twentyOneItemScore, "twentyOneItemScore.csv", row.names = FALSE)

# 16th embedded score: 7.1 install 4 pumps downstream of super tree
# Filter dialogue node data for dialogue IDs 447, 448, 449, and 450
dialogueNode24 <- dialogueNodeCombineNew %>% filter(dialogueID == 447)
dialogueNode24F <- dialogueNode24 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

dialogueNode25 <- dialogueNodeCombineNew %>% filter(dialogueID == 448)
dialogueNode25F <- dialogueNode25 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

dialogueNode26 <- dialogueNodeCombineNew %>% filter(dialogueID == 449)
dialogueNode26F <- dialogueNode26 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

dialogueNode27 <- dialogueNodeCombineNew %>% filter(dialogueID == 450)
dialogueNode27F <- dialogueNode27 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")

# Assign filtered data to related columns
relatedCol21 <- dialogueNode24F
relatedCol211 <- dialogueNode25F
relatedCol212 <- dialogueNode26F
relatedCol213 <- dialogueNode27F

# Function to calculate plant score for each player
calculate_plant_score <- function(player, correct_data, wrong_data_list) {
  temp <- correct_data %>% filter(playerName == player)
  wrongCounts <- sapply(wrong_data_list, function(data) nrow(data %>% filter(playerName == player)))
  
  correctCount <- nrow(temp)
  wrongCount <- sum(wrongCounts)
  
  if (nrow(temp) == 0 && wrongCount == 0) {
    plantScore <- 0
  } else {
    sumNumber <- sum(correctCount, wrongCount)
    plantScore <- (correctCount / sumNumber * 1) + (wrongCount / sumNumber * -0.5)
  }
  
  data.frame(playerName = player, plantScore = plantScore)
}

# Generate plant scores for all players
wrong_data_list <- list(relatedCol211, relatedCol212, relatedCol213)
twentyTwoItemScore <- bind_rows(lapply(playerList, calculate_plant_score, 
                                       correct_data = relatedCol21, 
                                       wrong_data_list = wrong_data_list))

# View the structure of the resulting data
str(twentyTwoItemScore)

# Save the results to a CSV file
write.csv(twentyTwoItemScore, "twentyTwoItemScore.csv", row.names = FALSE)

# 17th embedded score: 1.1 find the team
# Filter "TaskEvent" data and extract details
FTTwoTaskEvent <- filedTestTwoNew %>% filter(type == "TaskEvent")
taskDetail <- bind_rows(lapply(1:nrow(FTTwoTaskEvent), function(i) {
  temp <- mhsEventDetailsById(conn, FTTwoTaskEvent$ItemID[i])
  temp
}))

# Combine task data
taskCombine <- data.frame(FTTwoTaskEvent, taskDetail)
write.csv(taskCombine, "rawData/taskCombine.csv", row.names = FALSE)

# Load combined task data
taskCombine <- read.csv("rawData/taskCombine.csv", stringsAsFactors = FALSE)[, -1]

# Filter "Find the team" task data
findTeamtask <- taskCombine %>% filter(questID == 20, taskKey == "Find the team")
playerList <- unique(findTeamtask$playerName)

# Calculate time duration for "Find the Team" task
findTeamTimeDuration <- bind_rows(lapply(playerList, function(player) {
  temp <- findTeamtask %>% filter(playerName == player)
  taskCompleteList <- temp %>% filter(taskEventType == "TaskCompleteEvent") %>% arrange(timestamp)
  
  if (nrow(taskCompleteList) == 0) return(NULL)
  
  bind_rows(lapply(1:nrow(taskCompleteList), function(j) {
    endTime <- taskCompleteList$timestamp[j]
    temp1 <- temp %>% filter(timestamp < endTime, taskEventType == "TaskActiveEvent") %>% arrange(timestamp)
    
    if (nrow(temp1) == 0) return(NULL)
    
    startTime <- tail(temp1$timestamp, 1)
    timeDuration <- round(difftime(endTime, startTime, units = "mins"), 3)
    data.frame(playerName = player, startTime = startTime, endTime = endTime, timeDuration = timeDuration)
  }))
}))

# Load "HotkeyEvent" data and extract details
hotKeyEvents <- filedTestTwo %>% filter(type == "HotkeyEvent")
hotKeyDetail <- bind_rows(lapply(1:nrow(hotKeyEvents), function(i) {
  temp <- mhsEventDetailsById(conn, hotKeyEvents$ItemID[i])
  temp
}))

# Combine hotkey data
hotKeyCombine <- cbind(hotKeyEvents, hotKeyDetail)
write.csv(hotKeyCombine, "rawData/hotKeyCombine.csv", row.names = FALSE)

# Load combined hotkey data and convert timestamps
hotKeyCombineNew <- read.csv("rawData/hotKeyCombine.csv", stringsAsFactors = FALSE)[, -1]
hotKeyCombineNew$timestamp <- ymd_hms(hotKeyCombineNew$timestamp)

# Calculate map usage during "Find the Team" task
mapUseStatus <- bind_rows(lapply(playerList, function(player) {
  temp <- hotKeyCombineNew %>% filter(playerName == player)
  temp1 <- findTeamTimeDuration %>% filter(playerName == player)
  
  bind_rows(lapply(1:nrow(temp1), function(j) {
    startTime <- ymd_hms(temp1$startTime[j])
    endTime <- ymd_hms(temp1$endTime[j])
    mapCount <- nrow(temp %>% filter(timestamp >= startTime, timestamp <= endTime, keyName == "M"))
    data.frame(playerName = player, playTime = j, mapCount = mapCount)
  }))
}))

# Combine "Find the Team" time duration and map usage
findTheTeamStatus <- left_join(findTeamTimeDuration, mapUseStatus, by = c("playerName", "playTime"))
write.csv(findTheTeamStatus, "rawData/findTheTeamStatusNew.csv", row.names = FALSE)

# Calculate scores for each playthrough
twentyThreeItemScore <- bind_rows(lapply(playerList, function(player) {
  temp <- findTheTeamStatus %>% filter(playerName == player)
  
  bind_rows(lapply(1:nrow(temp), function(j) {
    timeScore <- ifelse(temp$timeDuration[j] < 3, 1, 0)
    mapScore <- ifelse(temp$mapCount[j] > 0, 0.5, 0)
    sumScore <- timeScore + mapScore
    data.frame(playerName = player, playTime = j, sumScore = sumScore)
  }))
}))

write.csv(twentyThreeItemScore, "rawData/findTheTeamScoreNew.csv", row.names = FALSE)

# Calculate average score for each player
findTheTeamScoreAverage <- bind_rows(lapply(playerList, function(player) {
  temp <- twentyThreeItemScore %>% filter(playerName == player)
  findTeamAveScore <- mean(temp$sumScore, na.rm = TRUE)
  data.frame(playerName = player, findTeamAveScore = findTeamAveScore)
}))

# View and save the average scores
write.csv(findTheTeamScoreAverage, "findTheTeamScoreAverage.csv", row.names = FALSE)
