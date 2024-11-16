# 1st embedded score: 3.1 time to argue

## argument success and failure
### argument success
filedTestTwoNew=read.csv("fieldTestTwoTable.csv",stringsAsFactors = FALSE)[,-1]
str(filedTestTwoNew)

argumentSuccess=filedTestTwoNew %>% filter(type=="ArgumentationSuccessEvent")

argumentSuccessDetail=c()
for(i in 1:nrow(argumentSuccess)){
  temp=mhsEventDetailsById(conn, argumentSuccess$ItemID[i])
  
  attemptTime=temp$eventDescription$argument$attempt
  argumentName=temp$eventDescription$argument$name
  argumentTitle=temp$eventDescription$argument$title
  feedbackGot=temp$eventDescription$feedback
  if(is.null(feedbackGot)){
    feedbackGot=" "
  }else{
    feedbackGot=feedbackGot
  }
  result="success"
  tempDataframe=data.frame(argumentTitle,argumentName,result,attemptTime,feedbackGot)
  argumentSuccessDetail=rbind(argumentSuccessDetail,tempDataframe)
}
#str(argumentSuccessDetail)
argumentSuccessCombine=cbind(argumentSuccess,argumentSuccessDetail)
colnames(argumentSuccessCombine)

playerList=unique(argumentSuccessCombine$playerName)

argumentSuccessTable=argumentSuccessCombine %>% select("playerName","timestamp","teacherId","argumentTitle","result","attemptTime","feedbackGot")

argumentSuccessStates=c()
for(i in 1:length(playerList)){
  temp=argumentSuccessTable %>% filter(playerName==playerList[i])
  argumentList=unique(temp$argumentTitle)
  argumetStates=c()
  for(j in 1:length(argumentList)){
    temp1=temp %>%  filter(argumentTitle==argumentList[j])
    playerName=temp1$playerName[1]
    teacherId=temp1$teacherId[1]
    argumentTitle=temp1$argumentTitle[1]
    result="success"
    maxAttemptTime=max(temp1$attemptTime)
    feedBackList=unique(temp1$feedbackGot)
    feedBacks=paste(feedBackList,sep=";")
    successTrialSum=nrow(temp1)
    tempDataframe=data.frame(playerName,teacherId,argumentTitle,result,maxAttemptTime,feedBacks,successTrialSum)
    argumetStates=rbind(argumetStates,tempDataframe)
  }
  argumentSuccessStates=rbind(argumentSuccessStates,argumetStates)
}

#str(argumentSuccessStates)

unique(argumentSuccessStates$argumentTitle)

write.csv(argumentSuccessStates,"rawData/argumentSuccessStatesNew.csv")
argumentSuccessStates=read.csv("rawData/argumentSuccessStates.csv")[,-1]
#str(argumentSuccessStates)

argumentSuccessStatesNew=read.csv("rawData/argumentSuccessStatesNew.csv",stringsAsFactors = FALSE)[,-1]
#str(argumentSuccessStatesNew)

argumentSuccess=argumentSuccessStatesNew %>% select("playerName","argumentTitle","successTrialSum")
argumentSuccessS=spread(argumentSuccess,argumentTitle,successTrialSum)

colnames(argumentSuccessS)[-1]=paste(colnames(argumentSuccessS)[-1],"success",sep="_")

playerList=unique(argumentSuccessS$playerName)

argumentCompleteStatus=c()
for(i in 1:length(playerList)){
  playerOne=argumentSuccessS %>% filter(playerName == playerList[i])
  playerOne1=playerOne[,!is.na(playerOne)]
  argumentNumber=ncol(playerOne1)-1
  totalArgumentNumber=ncol(playerOne)-1
  argumentCompletePercentage=round(argumentNumber/totalArgumentNumber,3)
  playerName=playerList[i]
  tempDataframe=data.frame(playerName,argumentCompletePercentage)
  argumentCompleteStatus=rbind(argumentCompleteStatus,tempDataframe)
}

argumentSuccessStatement=full_join(argumentCompleteStatus,argumentSuccessS,by="playerName")
argumentSuccessStatement[is.na(argumentSuccessStatement)]=0

write.csv(argumentSuccessStatement,"rawData/argumentSuccessStatementNew.csv")

### argument failure
argumentFailure=filedTestTwoNew %>% filter(type=="ArgumentationFailedEvent")
#str(argumentFailure)

argumentFailureDetail=c()
for(i in 1:nrow(argumentFailure)){
  temp=mhsEventDetailsById(conn, argumentFailure$ItemID[i])
  
  claimChoseName=temp$eventDescription$claim$name
  reasoningChoseName=temp$eventDescription$solutionReasonings[[1]]$reasoning$name
  evidenceChoseNameList=temp$eventDescription$solutionReasonings[[1]]$evidences[[1]]$name
  selectRows=evidenceChoseNameList[-which(evidenceChoseNameList=="")]
  evidenceChoseName=paste(selectRows,collapse = "")
  
  if(length(evidenceChoseName)==0){
    evidenceChoseName=""
  }else{
    evidenceChoseName=evidenceChoseName
  }
  
  choiceCombine=data.frame(claimChoseName,reasoningChoseName,evidenceChoseName)
  briefChoice=choiceCombine %>% unite("choiceCombine",c("claimChoseName","reasoningChoseName","evidenceChoseName"),sep = ",")
  argumentCombineChoice=briefChoice$choiceCombine
  
  claimChoseContent=temp$eventDescription$claim$description
  reasoningChoseContent=temp$eventDescription$solutionReasonings[[1]]$reasoning$description
  evidenceChoseContentList=temp$eventDescription$solutionReasonings[[1]]$evidences[[1]]$description
  selectCRows=evidenceChoseContentList[-which(evidenceChoseContentList=="")]
  evidenceChoseContent=paste(selectCRows,collapse=",") 
  
  if(length(evidenceChoseContent)==0){
    evidenceChoseContent=""
  }else{
    evidenceChoseContent=evidenceChoseContent
  }
  
  contentCombine=data.frame(claimChoseContent,reasoningChoseContent,evidenceChoseContent)
  briefContent=contentCombine %>% unite("contentCombine",c("claimChoseContent","reasoningChoseContent","evidenceChoseContent"),sep = ";")
  argumentCombineContent=briefContent$contentCombine
  
  attemptTime=temp$eventDescription$argument$attempt
  argumentName=temp$eventDescription$argument$name
  argumentTitle=temp$eventDescription$argument$title
  feedbackGot=temp$eventDescription$feedback
  
  if(is.null(feedbackGot)){
    feedbackGot=" "
  }else{
    feedbackGot=feedbackGot
  }
  
  result="failure"
  tempDataframe=data.frame(argumentTitle,argumentName,result,attemptTime,feedbackGot,argumentCombineChoice,argumentCombineContent)
  
  argumentFailureDetail=rbind(argumentFailureDetail,tempDataframe)
}
#str(argumentFailureDetail)

argumentFailureCombine=cbind(argumentFailure,argumentFailureDetail)
#colnames(argumentFailureCombine)
#unique(argumentFailureCombine$argumentTitle)

playerList=unique(argumentFailureCombine$playerName)
write.csv(argumentFailureCombine,"rawData/argumentFailureCombineNew.csv")

argumentFailureStates=c()
for(i in 1:length(playerList)){
  temp=argumentFailureCombine %>% filter(playerName==playerList[i])
  temp1=temp[!duplicated(temp$timestamp),]
  argumentList=unique(temp1$argumentTitle)
  argumetStates=c()
  for(j in 1:length(argumentList)){
    temp2=temp1 %>%  filter(argumentTitle==argumentList[j])
    playerName=temp2$playerName[1]
    teacherId=temp2$teacherId[1]
    argumentTitle=temp2$argumentTitle[1]
    result="failure"
    maxAttemptTime=max(temp1$attemptTime)
    feedBackList=unique(temp1$feedbackGot)
    feedBacks=paste(feedBackList,collapse = ";")
    failureTrialSum=nrow(temp2)
    argumentFailureChoice=temp2$argumentCombineChoice
    argumentFailureContent=temp2$argumentCombineContent
    tempDataframe=data.frame(playerName,teacherId,argumentTitle,result,maxAttemptTime,feedBacks,failureTrialSum,argumentFailureChoice,argumentFailureContent)
    argumetStates=rbind(argumetStates,tempDataframe)
  }
  argumentFailureStates=rbind(argumentFailureStates,argumetStates)
}

#str(argumentFailureStates)
#unique(argumentFailureStates$argumentTitle)
playerOne=argumentFailureStates %>% filter(playerName == playerList[100])

write.csv(argumentFailureStates,"rawData/argumentFailureStatesNew.csv")

argumentFailure=argumentFailureStates %>% select("playerName","argumentTitle","failureTrialSum")
argumentFailure1=argumentFailure[!duplicated(argumentFailure),]
argumentFailureS=spread(argumentFailure1,argumentTitle,failureTrialSum)

argumentFullStatement=full_join(argumentSuccessStatement,argumentFailureS,by="playerName")

argumentFullStatementNew=read.csv("rawData/argumentFullStatementNew.csv",stringsAsFactors = FALSE)[,-1]
str(argumentFullStatementNew)

relatedCol=argumentFullStatementNew %>% select("playerName","Argumentation.Tutorial_success","Argumentation.Tutorial_failure")

playerList=unique(relatedCol$playerName)

firstItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol %>% filter(playerName==playerList[i])
  if (temp$Argumentation.Tutorial_success >0 & temp$Argumentation.Tutorial_failure==0){
    tutorialArgScore=1
  }else{
    tutorialArgScore=0
  }
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,tutorialArgScore)
  firstItemScore=rbind(firstItemScore,tempDataFrame)
}

# 2nd embedded score: 4.2 argue which watershed is bigger
relatedCol1=argumentFullStatementNew %>% select("playerName","Watershed_success","Watershed_failure")
str(relatedCol1)

secondItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol1 %>% filter(playerName==playerList[i])
  if(temp$Watershed_success>0 & temp$Watershed_failure <3){
    biggerArgScore=2
  }else if(temp$Watershed_success>0 & temp$Watershed_failure==3){
    biggerArgScore=1
  }else{
    biggerArgScore=0
  }
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,biggerArgScore)
  secondItemScore=rbind(secondItemScore,tempDataFrame)
}


# 3rd embedded score: 3.1 convince bill the pollutant is nearby
relatedCol2=argumentFullStatementNew %>% select("playerName","Pollution.Upstream_success","Pollution.Upstream_failure")
str(relatedCol2)

thirdItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol2 %>% filter(playerName==playerList[i])
  if(temp$Pollution.Upstream_success>0 & temp$Pollution.Upstream_failure <3){
    upstreamArgScore=2
  }else if(temp$Pollution.Upstream_success>0 & temp$Pollution.Upstream_failure>=3 & temp$Pollution.Upstream_failure<6){
    upstreamArgScore=1
  }else{
    upstreamArgScore=0
  }
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,upstreamArgScore)
  thirdItemScore=rbind(thirdItemScore,tempDataFrame)
}

# 4th embedded score: 9.1 who flooded the armory
relatedCol3=argumentFullStatementNew %>% select("playerName","Flooding.the.armory_failure","Flooding.the.armory_success")
str(relatedCol3)

forthItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol3 %>% filter(playerName==playerList[i])
  if(temp$Flooding.the.armory_success>0 & temp$Flooding.the.armory_failure <3){
    floodArmoryScore=2
  }else if(temp$Flooding.the.armory_success>0 & temp$Flooding.the.armory_failure>=3 & temp$Flooding.the.armory_failure<6){
    floodArmoryScore=1
  }else{
    floodArmoryScore=0
  }
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,floodArmoryScore)
  forthItemScore=rbind(forthItemScore,tempDataFrame)
}

# 5th embedded score: 5.1 convince Bill

relatedCol4=argumentFullStatementNew %>% select("playerName","Making.water.for.Bill_failure","Making.water.for.Bill_success")
str(relatedCol4)

fifthItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol4 %>% filter(playerName==playerList[i])
  if(temp$Making.water.for.Bill_success>0 & temp$Making.water.for.Bill_failure <3){
    convinceBillScore=2
  }else if(temp$Making.water.for.Bill_success>0 & temp$Making.water.for.Bill_failure>=3 & temp$Making.water.for.Bill_failure<6){
    convinceBillScore=1
  }else{
    convinceBillScore=0
  }
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,convinceBillScore)
  fifthItemScore=rbind(fifthItemScore,tempDataFrame)
}

# 6th embedded score: CREi system (3 and all the rest)

ballIncor=filedTestTwoNew %>% filter(type=="CREiIncorrect")
str(ballIncor)
playerList=unique(filedTestTwoNew$playerName)

ballIncorDetail=c()
for(i in 1:nrow(ballIncor)){
  temp=mhsEventDetailsById(conn, ballIncor$ItemID[i])
  chooseArguType=temp$eventDescription$componentType
  rightType=temp$eventDescription$component$componentType
  rightContent=temp$eventDescription$component$componentText
  outcome="incorrect"
  tempDataFrame=data.frame(rightType,rightContent,chooseArguType,outcome)
  ballIncorDetail=rbind(ballIncorDetail,tempDataFrame)
}

ballIncorCombine=cbind(ballIncor,ballIncorDetail)

ballCor=filedTestTwoNew %>% filter(type=="CREiCorrect")
str(ballCor)
playerList=unique(filedTestTwoNew$playerName)

ballCorDetail=c()
for(i in 1:nrow(ballCor)){
  temp=mhsEventDetailsById(conn, ballIncor$ItemID[i])
  chooseArguType=temp$eventDescription$componentType
  rightType=temp$eventDescription$component$componentType
  rightContent=temp$eventDescription$component$componentText
  outcome="correct"
  tempDataFrame=data.frame(rightType,rightContent,chooseArguType,outcome)
  ballCorDetail=rbind(ballCorDetail,tempDataFrame)
}

ballCorCombine=cbind(ballCor,ballCorDetail)

ballStatement=rbind(ballIncorCombine,ballCorCombine)
write.csv(ballStatement,"rawData/ballStatementNew.csv")

ballStatement=read.csv("rawData/ballStatement.csv",stringsAsFactors = FALSE)[,-1]
str(ballStatement)

playerListCREi=unique(ballStatement$playerName)

CREiNumbers=c()
for (i in 1:length(playerListCREi)){
  temp=ballStatement %>% filter(playerName==playerListCREi[i])
  temp1=temp %>% filter(outcome=="incorrect")
  incorrectNumber=nrow(temp1)
  temp2=temp %>% filter(outcome=="correct")
  correctNumber=nrow(temp2)
  incor.Evidence1=temp1 %>% filter(chooseArguType == "EVIDENCE")
  incor.Evidence=nrow(incor.Evidence1)
  incor.Reasoning1=temp1 %>% filter(chooseArguType == "REASONING")
  incor.Reasoning=nrow(incor.Reasoning1)
  incor.Claim1=temp1 %>% filter(chooseArguType == "CLAIM")
  incor.Claim=nrow(incor.Claim1)
  cor.Evidence1=temp2 %>% filter(chooseArguType == "REASONING")
  cor.Evidence=nrow(cor.Evidence1)
  cor.Reasoning1=temp2 %>% filter(chooseArguType == "EVIDENCE")
  cor.Reasoning=nrow(cor.Reasoning1)
  cor.Claim1=temp2 %>% filter(chooseArguType == "CLAIM")
  cor.Claim=nrow(cor.Claim1)
  playerName=playerListCREi[i]
  tempDataFrame=data.frame(playerName,incorrectNumber,correctNumber,incor.Evidence,incor.Reasoning,incor.Claim,cor.Evidence,cor.Reasoning,cor.Claim)
  CREiNumbers=rbind(CREiNumbers,tempDataFrame)
}

write.csv(CREiNumbers,"rawData/CREiNumbersNew.csv")

CREiNumbersNew=read.csv("rawData/CREiNumbersCombineWithTC.csv",stringsAsFactors = FALSE)[,-1]
str(CREiNumbersNew)

relatedCol5=CREiNumbersNew %>% select("playerName","incorrectNumber","correctNumber")

sixthItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol5 %>% filter(playerName==playerList[i])
  
  if (nrow(temp)==0){
    CREIScore=0
  }else{
    sumNumber=sum(temp$incorrectNumber, temp$correctNumber)
    CREIScore=(temp$correctNumber/sumNumber * 1) + (temp$incorrectNumber/sumNumber * (-1/3))
  }
  
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,CREIScore)
  sixthItemScore=rbind(sixthItemScore,tempDataFrame)
}

# 7th embedded score: 5.2 Jasper's proposal
dialogueEvents=filedTestTwo %>% filter(type=="DialogueEvent")
str(dialogueEvents)

dialogueDetail=c()
for(i in 717271:nrow(dialogueEvents)){
  temp=mhsEventDetailsById(conn, dialogueEvents$ItemID[i])
  dialogueDetail=rbind.fill(dialogueDetail,temp)
}

dialogueDetail1=dialogueDetail
dialogueDetail2=rbind(dialogueDetail1,dialogueDetail)
nrow(dialogueDetail2)
dialogueCombine=cbind(dialogueEvents,dialogueDetail2)
str(dialogueCombine)

write.csv(dialogueCombine,"dialogueCombine.csv")

dialogueNodeEvents=filedTestTwoNew %>% filter(type=="DialogueNodeEvent")
str(dialogueNodeEvents)

dialogueNodeDetail=c()
for(i in 1:nrow(dialogueNodeEvents)){
  temp=mhsEventDetailsById(conn, dialogueNodeEvents$ItemID[i])
  dialogueNodeDetail=rbind.fill(dialogueNodeDetail,temp)
}

str(dialogueNodeDetail)
table(dialogueNodeDetail$choiceID)
dialogueNodeDetail1
dialogueNodeDetail2=rbind(dialogueNodeDetail1,dialogueNodeDetail)
str(dialogueNodeDetail2)
dialogueNodeDetail3=rbind(dialogueNodeDetail2,dialogueNodeDetail)
str(dialogueNodeDetail3)
dialogueNodeDetail4=rbind(dialogueNodeDetail3,dialogueNodeDetail)
str(dialogueNodeDetail4)

dialogueNodeCombine=cbind(dialogueNodeEvents,dialogueNodeDetail)
str(dialogueNodeCombine)
write.csv(dialogueNodeCombine,"rawData/dialogueNodeCombineNew.csv")

dialogueNodeCombine=read.csv("rawData/dialogueNodeCombine.csv",stringsAsFactors = FALSE)[,-1]
str(dialogueNodeCombine)

dialogueNodeCombineNew=read.csv("rawData/dialogueNodeCombineNew.csv",stringsAsFactors = FALSE)[,-1]
str(dialogueNodeCombineNew)

dialogueNodeAllVersion = rbind(dialogueNodeCombine,dialogueNodeCombineNew)
str(dialogueNodeAllVersion)
write.csv(dialogueNodeAllVersion,"dialogueNodeAllVersion.csv")

dialogueFreference=returnTidyDialogueReferenceData(conn, "0.11.171")

dialogue5=c("Is there anything wrong with it?")
inDialogue4=dialogueFreference[grep(dialogue5,dialogueFreference$content),]

dialogueNode5=dialogueNodeCombineNew %>% filter(dialogueID==425)
str(dialogueNode5)
table(dialogueNode5$choiceID)
dialogueNode5F=dialogueNode5 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode5F)

choiceID=c(0,1,2)
choiceContent=c("Agree with Jasper.","Disagree, Jasper forgot a claim.","Disagree, Jasper forgot evidence.")

dialogueChoice5=data.frame(choiceID,choiceContent)

dialogueNode5FWChoiceC=left_join(dialogueNode5F,dialogueChoice5,by="choiceID")

str(dialogueNode5FWChoiceC)

playerList=unique(dialogueNodeCombineNew$playerName)

JasperCritique=c()
for(i in 1:length(playerList)){
  temp=dialogueNode5FWChoiceC %>% filter(playerName==playerList[i])
  if(nrow(temp)==0){
    next
  }else{
    choices=unique(temp$choiceContent)
    frequencies=c()
    for(j in 1:length(choices)){
      temp1=temp %>% filter(choiceContent == choices[j])
      number=nrow(temp1)
      choiceContent=choices[j]
      playerName=playerList[i]
      tempDataFrame=data.frame(playerName, choiceContent, number)
      frequencies=rbind(frequencies,tempDataFrame)
    }
    JasperCritique=rbind(JasperCritique,frequencies)
  }
}

JasperCritiqueFirst=spread(JasperCritique,choiceContent,number)
JasperCritiqueFirst[is.na(JasperCritiqueFirst)]=0

JasperCritiqueFirst$understanding = ifelse(JasperCritiqueFirst$`Agree with Jasper.` > 0 | JasperCritiqueFirst$`Disagree, Jasper forgot a claim.` > 0, 0, 1)

write.csv(JasperCritiqueFirst,"rawData/JasperCritiqueFirstNew.csv")

JasperCritiqueFirst=read.csv("rawData/JasperCritiqueFirst.csv",stringsAsFactors = FALSE)[,-1]

relatedCol6=JasperCritiqueFirst
playerListCol6=unique(relatedCol6$playerName)

seventhItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol6 %>% filter(playerName==playerList[i])
  
  if (nrow(temp)==0){
    JasperCritiqueScore=0
  }else{
    sumNumber=sum(temp$`Agree with Jasper.`,temp$`Disagree, Jasper forgot a claim.`,temp$`Disagree, Jasper forgot evidence.`)
    JasperCritiqueScore=(temp$`Disagree, Jasper forgot evidence.`/sumNumber * 1) + (sum(temp$`Agree with Jasper.`,temp$`Disagree, Jasper forgot a claim.`)/sumNumber * 0)
  }
  
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,JasperCritiqueScore)
  seventhItemScore=rbind(seventhItemScore,tempDataFrame)
}

# 8th embedded score: 3.2 Drill to the water table
dialogueNode8=dialogueNodeCombineNew %>% filter(dialogueID==578)
str(dialogueNode8)

dialogueNode81=dialogueNodeCombineNew %>% filter(dialogueID==579)
str(dialogueNode81)

relatedCol7=dialogueNode8
relatedCol71=dialogueNode81

eighthItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol7 %>% filter(playerName==playerList[i]) %>% filter(dialogueNodeEventType=="DialogueNodeFinishEvent")
  temp1=relatedCol71 %>% filter(playerName==playerList[i]) %>% filter(dialogueNodeEventType=="DialogueNodeFinishEvent")
  if (nrow(temp)==0){
    drillRoomScore=0
  }else{
    sumNumber=sum(nrow(temp),nrow(temp1))
    drillRoomScore=(nrow(temp)/sumNumber * 1) + (nrow(temp1)/sumNumber * 0)
  }
  
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,drillRoomScore)
  eighthItemScore=rbind(eighthItemScore,tempDataFrame)
}

# 9th embedded score: 9.2 who flooded the warehouse
dialogue10=c("This is a pretty serious accusation")
inDialogue9=dialogueFreference[grep(dialogue10,dialogueFreference$content),]

dialogueNode10=dialogueNodeCombineNew %>% filter(dialogueID==479)
str(dialogueNode10)
table(dialogueNode10$choiceID)
dialogueNode10F=dialogueNode10 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode10F)

choiceID=c(0,1,2)
choiceContent=c("Agree, fountain caused the flooding.","Disagree, reasoning doesn't support evidence.","Disagree, Anderson using wrong evidence.")

dialogueChoice10=data.frame(choiceID,choiceContent)

dialogueNode10FWChoiceC=left_join(dialogueNode10F,dialogueChoice10,by="choiceID")

str(dialogueNode10FWChoiceC)

playerList=unique(dialogueNodeCombineNew$playerName)

JasperCritique=c()
for(i in 1:length(playerList)){
  temp=dialogueNode10FWChoiceC %>% filter(playerName==playerList[i])
  if(nrow(temp)==0){
    next
  }else{
    choices=unique(temp$choiceContent)
    frequencies=c()
    for(j in 1:length(choices)){
      temp1=temp %>% filter(choiceContent == choices[j])
      number=nrow(temp1)
      choiceContent=choices[j]
      playerName=playerList[i]
      tempDataFrame=data.frame(playerName, choiceContent, number)
      frequencies=rbind(frequencies,tempDataFrame)
    }
    JasperCritique=rbind(JasperCritique,frequencies)
  }
}

andersonClaim=spread(JasperCritique,choiceContent,number)
andersonClaim[is.na(andersonClaim)]=0

andersonClaim$understanding = ifelse(andersonClaim$`Agree, fountain caused the flooding.` > 0 | andersonClaim$`Disagree, reasoning doesn't support evidence.` > 0, 0, 1)

write.csv(andersonClaim,"rawData/andersonClaimNew.csv")

andersonClaim=read.csv("rawData/andersonClaim.csv",stringsAsFactors = FALSE)[,-1]

andersonClaimNew=read.csv("rawData/andersonClaimNew.csv",stringsAsFactors = FALSE)[,-1]

andersonClaimCombine=rbind(andersonClaim,andersonClaimNew)

write.csv(andersonClaimCombine,"rawData/andersonClaimCombine.csv")

# 10th embedded score: which fountain should be overflow
dialogue11=c("Which fountain do you think is the best?")
inDialogue10=dialogueFreference[grep(dialogue11,dialogueFreference$content),]

dialogueNode11=dialogueNodeCombineNew %>% filter(dialogueID==398)
str(dialogueNode11)
table(dialogueNode11$choiceID)
dialogueNode11F=dialogueNode11 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode11F)

choiceID=c(0,1,2,3)
choiceContent=c("Fountain 1","Fountain 2","Fountain 3","Let me check again.")

dialogueChoice11=data.frame(choiceID,choiceContent)

dialogueNode11FWChoiceC=left_join(dialogueNode11F,dialogueChoice11,by="choiceID")

str(dialogueNode11FWChoiceC)

playerList=unique(dialogueNodeCombineNew$playerName)

JasperCritique=c()
for(i in 1:length(playerList)){
  temp=dialogueNode11FWChoiceC %>% filter(playerName==playerList[i])
  if(nrow(temp)==0){
    next
  }else{
    choices=unique(temp$choiceContent)
    frequencies=c()
    for(j in 1:length(choices)){
      temp1=temp %>% filter(choiceContent == choices[j])
      number=nrow(temp1)
      choiceContent=choices[j]
      playerName=playerList[i]
      tempDataFrame=data.frame(playerName, choiceContent, number)
      frequencies=rbind(frequencies,tempDataFrame)
    }
    JasperCritique=rbind(JasperCritique,frequencies)
  }
}

fountainChoose=spread(JasperCritique,choiceContent,number)
fountainChoose[is.na(fountainChoose)]=0

fountainChoose$understanding = ifelse(fountainChoose$`Fountain 2` > 0 | fountainChoose$`Fountain 3` > 0, 0, 1)

write.csv(fountainChoose,"rawData/fountainChooseNew.csv")

fountainChoose=read.csv("rawData/fountainChoose.csv",stringsAsFactors = FALSE)[,-1]
unique(fountainChoose$playerName)

fountainChooseNew=read.csv("rawData/fountainChooseNew.csv",stringsAsFactors = FALSE)[,-1]
unique(fountainChooseNew$playerName)

fountainChooseCombine=rbind(fountainChoose,fountainChooseNew)
write.csv(fountainChooseCombine,"rawData/fountainChooseCombine.csv")

# 11th embedded score: 7.1 setup the first piece
dialogue12=c("which one should we use")
inDialogue11=dialogueFreference[grep(dialogue12,dialogueFreference$content),]

dialogueNode12=dialogueNodeCombineNew %>% filter(dialogueID==467)
str(dialogueNode12)
table(dialogueNode12$choiceID)
dialogueNode12F=dialogueNode12 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode12F)

choiceID=c(0,1)
choiceContent=c("The Condenser","The Evaporator")

dialogueChoice12=data.frame(choiceID,choiceContent)

dialogueNode12FWChoiceC=left_join(dialogueNode12F,dialogueChoice12,by="choiceID")

str(dialogueNode12FWChoiceC)

# 12th embedded score: 7.2 setup the second piece
dialogue13=c("so what should this one be")
inDialogue12=dialogueFreference[grep(dialogue13,dialogueFreference$content),]

dialogueNode13=dialogueNodeCombineNew %>% filter(dialogueID==469)
str(dialogueNode13)
table(dialogueNode13$choiceID)
dialogueNode13F=dialogueNode13 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode13F)

choiceID=c(0,1)
choiceContent=c("The Condenser","The Evaporator")

dialogueChoice13=data.frame(choiceID,choiceContent)

dialogueNode13FWChoiceC=left_join(dialogueNode13F,dialogueChoice13,by="choiceID")

str(dialogueNode13FWChoiceC)

# 13th embedded score: 7.3 setup the third piece
dialogue14=c("which one of these will help us")
inDialogue13=dialogueFreference[grep(dialogue14,dialogueFreference$content),]

dialogueNode14=dialogueNodeCombineNew %>% filter(dialogueID==471)
str(dialogueNode14)
table(dialogueNode14$choiceID)
dialogueNode14F=dialogueNode14 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode14F)

choiceID=c(0,1)
choiceContent=c("The Condenser","The Evaporator")

dialogueChoice14=data.frame(choiceID,choiceContent)

dialogueNode14FWChoiceC=left_join(dialogueNode14F,dialogueChoice14,by="choiceID")

str(dialogueNode14FWChoiceC)

# 14th embedded score: 7.4 setup the fourth piece
dialogue15=c("Which one do you think?")
inDialogue14=dialogueFreference[grep(dialogue15,dialogueFreference$content),]

dialogueNode15=dialogueNodeCombineNew %>% filter(dialogueID==473)
str(dialogueNode15)
table(dialogueNode15$choiceID)
dialogueNode15F=dialogueNode15 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode15F)

choiceID=c(0,1)
choiceContent=c("The Condenser","The Evaporator")

dialogueChoice15=data.frame(choiceID,choiceContent)

dialogueNode15FWChoiceC=left_join(dialogueNode15F,dialogueChoice15,by="choiceID")

# 15th embedded score: 1.2 Deliver Sam's supplies

## crates condition
dialogueNode22=dialogueNodeCombineNew %>% filter(dialogueID==37)
str(dialogueNode22)
table(dialogueNode22$choiceID)
dialogueNode22F=dialogueNode22 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode22F)

dialogueNode23=dialogueNodeCombineNew %>% filter(dialogueID==38)
str(dialogueNode23)
table(dialogueNode23$choiceID)
dialogueNode23F=dialogueNode23 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode23F)

relatedCol20=dialogueNode22F
relatedCol201=dialogueNode23F

twentyOneItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol20 %>% filter(playerName==playerList[i]) 
  temp1=relatedCol201 %>% filter(playerName==playerList[i])
  correctCount=nrow(temp)
  wrongCount=nrow(temp1)
  if (nrow(temp)==0){
    crateDeliveryScore=0
  }else{
    sumNumber=sum(correctCount,wrongCount)
    crateDeliveryScore=(correctCount/sumNumber * 1) + (wrongCount/sumNumber * 0)
  }
  
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,crateDeliveryScore)
  twentyOneItemScore=rbind(twentyOneItemScore,tempDataFrame)
}

# 16th embedded score: 7.1 install 4 pumps downstream of super tree
## seeds planting
unique(dialogueCombine$dialogueID)

dialogueNode24=dialogueNodeCombineNew %>% filter(dialogueID==447)
str(dialogueNode24)
dialogueNode24F=dialogueNode24 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode24F)

dialogueNode25=dialogueNodeCombineNew %>% filter(dialogueID==448)
str(dialogueNode25)
dialogueNode25F=dialogueNode25 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode25F)

dialogueNode26=dialogueNodeCombineNew %>% filter(dialogueID==449)
str(dialogueNode26)
dialogueNode26F=dialogueNode26 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode26F)

dialogueNode27=dialogueNodeCombineNew %>% filter(dialogueID==450)
str(dialogueNode27)
dialogueNode27F=dialogueNode27 %>% filter(dialogueNodeEventType == "DialogueNodeFinishEvent")
str(dialogueNode27F)

relatedCol21=dialogueNode24F
relatedCol211=dialogueNode25F
relatedCol212=dialogueNode26F
relatedCol213=dialogueNode27F

twentyTwoItemScore=c()
for(i in 1:length(playerList)){
  temp=relatedCol21 %>% filter(playerName==playerList[i]) 
  temp1=relatedCol211 %>% filter(playerName==playerList[i])
  temp2=relatedCol212 %>% filter(playerName==playerList[i])
  temp3=relatedCol213 %>% filter(playerName==playerList[i])
  correctCount=nrow(temp)
  wrongCount=sum(nrow(temp1),nrow(temp2),nrow(temp3))
  if (nrow(temp)==0){
    plantScore=0
  }else{
    sumNumber=sum(correctCount,wrongCount)
    plantScore=(correctCount/sumNumber * 1) + (wrongCount/sumNumber * (-0.5))
  }
  
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,plantScore)
  twentyTwoItemScore=rbind(twentyTwoItemScore,tempDataFrame)
}

# 17th embedded score: 1.1 find the team
FTTwoTaskEvent=filedTestTwoNew %>% filter(type=="TaskEvent")
str(FTTwoTaskEvent)
taskDetail=c()
for(i in 1:nrow(FTTwoTaskEvent)){
  temp=mhsEventDetailsById(conn,FTTwoTaskEvent$ItemID[i])
  taskDetail=rbind.fill(taskDetail,temp)
}
str(taskDetail)
taskCombine=data.frame(FTTwoTaskEvent,taskDetail)
str(taskCombine)
write.csv(taskCombine,"rawData/taskCombine.csv")

taskCombine=read.csv("rawData/taskCombine.csv",stringsAsFactors = FALSE)[,-1]
str(taskCombine)
unique(taskCombine$playerName)
unique(taskCombine$taskKey)

findTeamtask=taskCombine %>% filter(questID==20) %>% filter(taskKey=="Find the team")
unique(findTeamtask$taskKey)

playerList=unique(findTeamtask$playerName)

findTeamTimeDuration=c()
for(i in 1:length(playerList)){
  temp=findTeamtask %>% filter(playerName==playerList[i])
  taskCompleteList=temp %>% filter(taskEventType=="TaskCompleteEvent")
  taskCompleteListO=taskCompleteList[order(taskCompleteList$timestamp),]
  if(nrow(taskCompleteListO)==0){
    next
  }else{
    timeDurations=c()
    for(j in 1:nrow(taskCompleteListO)){
      endTime=taskCompleteListO$timestamp[j]
      temp1=temp %>% filter(timestamp<endTime) %>% filter(taskEventType=="TaskActiveEvent")
      if(nrow(temp1)==0){
        next
      }else{
        temp2=temp1[order(temp1$timestamp),]
        startTime=temp2$timestamp[nrow(temp2)]
        timeDuration=difftime(endTime,startTime)
        units(timeDuration)="mins"
        timeDuration1=round(timeDuration,3)
        tempDataFrame=data.frame(startTime,endTime,timeDuration1)
        timeDurations=rbind(timeDurations,tempDataFrame)
      }
      
    }
    if(is.null(timeDurations)){
      next
    }else{
      playerName=playerList[i]
      tempDataFrame1=data.frame(playerName,timeDurations)
      findTeamTimeDuration=rbind(findTeamTimeDuration,tempDataFrame1)
    }
  }
}

## hotKey detail
hotKeyEvents=filedTestTwo %>% filter(type=="HotkeyEvent")
str(hotKeyEvents)

hotKeyDetail=c()
for(i in 1:nrow(hotKeyEvents)){
  temp=mhsEventDetailsById(conn, hotKeyEvents$ItemID[i])
  hotKeyDetail=rbind.fill(hotKeyDetail,temp)
}

hotKeyCombine=cbind(hotKeyEvents,hotKeyDetail)

write.csv(hotKeyCombine,"hotKeyCombine.csv")
str(hotKeyCombine)

hotKeyCombineNew=read.csv("hotKeyCombine.csv",stringsAsFactors = FALSE)[,-1]

playerList=unique(findTeamTimeDuration$playerName)
hotKeyCombineNew$timestamp=ymd_hms(hotKeyCombineNew$timestamp)

mapUseStatus=c()
for(i in 1:length(playerList)){
  temp = hotKeyCombineNew %>% filter(playerName==playerList[i])
  temp1= findTeamTimeDuration %>% filter(playerName==playerList[i])
  mapPerTime=c()
  for(j in 1:nrow(temp1)){
    EndTime=ymd_hms(temp1[j,]$endTime) 
    StartTime=ymd_hms(temp1[j,]$startTime)
    temp2=temp %>% filter(timestamp>=StartTime) %>% filter(timestamp<=EndTime)
    mapCount=nrow(temp2 %>% filter(keyName=="M"))
    timeFreq=j
    tempDataFrame=data.frame(timeFreq,mapCount)
    mapPerTime=rbind(mapPerTime,tempDataFrame)
  }
  playerName=playerList[i]
  tempDataFrame1=data.frame(playerName,mapPerTime)
  mapUseStatus=rbind(mapUseStatus,tempDataFrame1)
} 

str(mapUseStatus)

findTheTeamStatus=cbind(findTeamTimeDuration,mapUseStatus)
findTheTeamStatus1=findTheTeamStatus[,c(1,4,7)]
write.csv(findTheTeamStatus1,"findTheTeamStatusNew.csv")

twentyThreeItemScore=c()
for(i in 1:length(playerList)){
  temp=findTheTeamStatus1 %>% filter(playerName==playerList[i])
  scoreCombine=c()
  for(j in 1:nrow(temp)){
    temp1=temp[j,]
    if(temp1$timeDuration1 <3){
      timeScore=1
    }else{
      timeScore=0
    }
    if(temp1$mapCount>0){
      mapScore=0.5
    }else{
      mapScore=0
    }
    sumScore=sum(timeScore,mapScore)
    playTime=j
    tempDaraFrame=data.frame(playTime,sumScore)
    scoreCombine=rbind(scoreCombine,tempDaraFrame)
  }
  playerName=playerList[i]
  tempDaraframe2=data.frame(playerName,scoreCombine)
  twentyThreeItemScore=rbind(twentyThreeItemScore,tempDaraframe2)
}
write.csv(twentyThreeItemScore,"findTheTeamScoreNew.csv")

findTheTeamScoreAverage=c()
for(i in 1:length(playerList)){
  temp=twentyThreeItemScore %>% filter(playerName==playerList[i])
  scoreSum=sum(temp$sumScore)
  findTeamAveScore=scoreSum/nrow(temp)
  playerName=playerList[i]
  tempDataFrame=data.frame(playerName,findTeamAveScore)
  findTheTeamScoreAverage=rbind(findTheTeamScoreAverage,tempDataFrame)
}





