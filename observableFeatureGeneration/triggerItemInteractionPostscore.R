# Load libraries
#source(file="lib/MHS-Library.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

# Unit 2 Object Interaction Correlation

# Read the post score assessment table
FT2Assessment = read.csv("MHS-FT2-Assessment/FT2Assessment.csv", stringsAsFactors = FALSE)
str(FT2Assessment)

FT2U2PostAssessment = FT2Assessment %>% select("playerName", "POSTU2")

# Read the object interaction dataset
U2ObjectInteraction = read.csv("MHSFT2U2Data/U2TriggeringFrequeS.csv", stringsAsFactors = FALSE)[,-1]
str(U2ObjectInteraction)

U2ObjectInteraction = U2ObjectInteraction %>% separate(playerName, into = c("playerName", "trial"), sep = ",")
str(U2ObjectInteraction)

U2ObjectInteractionU2PostAss = inner_join(FT2U2PostAssessment,U2ObjectInteraction,by="playerName")

str(U2ObjectInteractionU2PostAss)

U2ObjectInteractionU2PostAss = U2ObjectInteractionU2PostAss %>% mutate(playerNameTrial = paste(playerName, trial, sep = ","))

U2ObjectInteractionU2PostAss = U2ObjectInteractionU2PostAss %>% select(-c("playerName","trial"))

colnames(U2ObjectInteractionU2PostAss)

U2ObjectInteractionU2PostAssN = U2ObjectInteractionU2PostAss[,-73]

#cor_matrix = cor(U2ObjectInteractionU2PostAssN)

U2PostAssessment = U2ObjectInteractionU2PostAssN$POSTU2

cor_results <- sapply(names(U2ObjectInteractionU2PostAssN)[names(U2ObjectInteractionU2PostAssN) != "POSTU2"], function(x) {
  cor_test <- cor.test(U2ObjectInteractionU2PostAssN[[x]], U2PostAssessment, method = "spearman")
  return(c(correlation = cor_test$estimate, p_value = cor_test$p.value))
})

cor_results_df <- as.data.frame(t(cor_results))
cor_results_df$Variable <- rownames(cor_results_df)
rownames(cor_results_df) <- NULL

cor_results_df_Sig = cor_results_df %>% dplyr::filter(p_value < 0.05)
cor_results_df_Sig

cor_results_df_Sig[1,"Variable"] = "After.Argue"
cor_results_df_Sig[2,"Variable"] = "Argue"
cor_results_df_Sig[3,"Variable"] = "Basketball.Hoop"
cor_results_df_Sig[4,"Variable"] = "Watershed.DataT"
cor_results_df_Sig[5,"Variable"] = "Watershed.Data"
cor_results_df_Sig[6,"Variable"] = "Check.your.pod"
cor_results_df_Sig[7,"Variable"] = "Dialogue"
cor_results_df_Sig[8,"Variable"] = "Door"
cor_results_df_Sig[9,"Variable"] = "Get.Map"
cor_results_df_Sig[10,"Variable"] = "Collectibles.Scene"
cor_results_df_Sig[11,"Variable"] = "Quest.Scene"
cor_results_df_Sig[12,"Variable"] = "Right.Waterfall"
cor_results_df_Sig[13,"Variable"] = "Enter.Right.Direction"
cor_results_df_Sig[14,"Variable"] = "Going.Wrong.Direction"
cor_results_df_Sig[15,"Variable"] = "Taking.too.long"
cor_results_df_Sig[16,"Variable"] = "Quest.Giver"
cor_results_df_Sig[17,"Variable"] = "Sample.west.waterfall"
cor_results_df_Sig[18,"Variable"] = "Scrap.1"
cor_results_df_Sig[19,"Variable"] = "Scrap.2"
cor_results_df_Sig[20,"Variable"] = "Scrap.Pickup"
cor_results_df_Sig[21,"Variable"] = "Section.1"
cor_results_df_Sig[22,"Variable"] = "Section.2"
cor_results_df_Sig[23,"Variable"] = "Section.3"
cor_results_df_Sig[24,"Variable"] = "Toppo.is.east.of.lake"
cor_results_df_Sig[25,"Variable"] = "Toppo.is.mountain.pass"
cor_results_df_Sig[26,"Variable"] = "Toppo.is.northern.part"
cor_results_df_Sig[27,"Variable"] = "Trigger"
cor_results_df_Sig[28,"Variable"] = "Trigger.Use"
cor_results_df_Sig[29,"Variable"] = "Trigger.Wait"
cor_results_df_Sig[30,"Variable"] = "Crash.Diagnostics"
str(cor_results_df_Sig)

write.csv(cor_results_df_Sig, "InGameBehaviorLearningOUtcomeCorrelation/Unit2ObjectInteractionCorrelation.csv")

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, fill = correlation.rho > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Spearman's Correlation Coefficient") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho)) +
#   geom_point(aes(size = -log10(p_value), color = correlation.rho)) +
#   coord_flip() +  # Flip coordinates for better readability
#   scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
#                         name="Spearman's Correlation") +
#   labs(title = "Spearman's Correlation with Dependent Variable",
#        x = "Independent Variables",
#        y = "Correlation Coefficient") +
#   theme_minimal()

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, color = correlation.rho > 0)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4) +
  coord_flip() +
  labs(title = "Lollipop Plot of Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Correlation Coefficient") +
  scale_color_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# Unit 2 post score High and Low level (average standard) 

# Read the object interaction dataset
U2TriggeringPostscore = read.csv("MHSFT2U2Data/U2TriggeringPostscoreLevel.csv", stringsAsFactors = FALSE)[-1] 
str(U2TriggeringPostscore)

U2TriggeringPostscoreS = U2TriggeringPostscore %>% dplyr::select(-"playerId")
str(U2TriggeringPostscoreS)

featureNames = colnames(U2TriggeringPostscoreS)[-1]
featureNames

results <- lapply(featureNames, function(feature) {
  test_result <- wilcox.test(U2TriggeringPostscoreS[[feature]] ~ U2TriggeringPostscoreS$U2PostScoreLevel)
  return(data.frame(
    Feature = feature,
    W = test_result$statistic,
    p_value = test_result$p.value
  ))
})

resultsDataFrame <- do.call(rbind, results)
print(resultsDataFrame)

significantFeatures =  resultsDataFrame %>% dplyr::filter(p_value < 0.05)
significantFeatures

#unique(U2TriggeringPostscore$U2...Start.Jasper.Crash.Diagnostics.OnDialogueEvent)
selectFeatures = significantFeatures$Feature
colnames(U2TriggeringPostscore)

U2TriggeringPostscoreSelect =U2TriggeringPostscore %>% dplyr::select(c(selectFeatures,"U2PostScoreLevel"))
str(U2TriggeringPostscoreSelect)
colnames(U2TriggeringPostscoreSelect)

U2TriggeringPostscoreSelect = U2TriggeringPostscoreSelect %>% rowwise() %>% 
  dplyr::mutate(sum_whether_its_not_right_direction = sum(c_across(c("Player.is.going.the.right.direction.OnEnter.Entered", "Player.is.going.the.wrong.direction.OnEnter.Exited")), na.rm = TRUE)) %>% ungroup()

U2TriggeringPostscoreSelect = U2TriggeringPostscoreSelect %>% rowwise() %>% 
  dplyr::mutate(sum_scrap_interaction = sum(c_across(c("Scrap.Pickup..1..OnRaycast", "Scrap.Pickup..2..OnRaycast", "Scrap.Pickup.Toast.OnToggle.On", "Scrap.Pickup.OnRaycast")), na.rm = TRUE)) %>% ungroup()

U2TriggeringPostscoreSelect = U2TriggeringPostscoreSelect %>% rowwise() %>% 
  dplyr::mutate(sum_Toppo_location_reminder = sum(c_across(c("Toppo.is.in.a.mountain.pass.OnWait", "Toppo.is.in.the.northern.part.OnWait")), na.rm = TRUE)) %>% ungroup()

U2TriggeringPostscoreSelect = U2TriggeringPostscoreSelect %>% rowwise() %>% 
  dplyr::mutate(sum_waterfall_sampling = sum(c_across(c("Sample.the.east.waterfall.OnEnter.Entered", "Sample.the.east.waterfall.OnWait")), na.rm = TRUE)) %>% ungroup()

str(U2TriggeringPostscoreSelect)

U2TriggeringPostscoreSelectS = U2TriggeringPostscoreSelect %>% dplyr::select(U2PostScoreLevel,sum_whether_its_not_right_direction,sum_scrap_interaction,sum_Toppo_location_reminder,sum_waterfall_sampling)

featureNames = colnames(U2TriggeringPostscoreSelectS)[-1]
featureNames

results <- lapply(featureNames, function(feature) {
  test_result <- wilcox.test(U2TriggeringPostscoreSelectS[[feature]] ~ U2TriggeringPostscoreSelectS$U2PostScoreLevel)
  return(data.frame(
    Feature = feature,
    W = test_result$statistic,
    p_value = test_result$p.value
  ))
})

resultsDataFrame <- do.call(rbind, results)
print(resultsDataFrame)

significantFeatures =  resultsDataFrame %>% dplyr::filter(p_value < 0.05)
significantFeatures

selectFeatures = significantFeatures$Feature
colnames(U2TriggeringPostscoreSelectS)

U2TriggeringPostscoreFinal =U2TriggeringPostscoreSelectS %>% dplyr::select(c(selectFeatures,"U2PostScoreLevel"))
str(U2TriggeringPostscoreFinal)
colnames(U2TriggeringPostscoreFinal)

# group_medians <- U2TriggeringPostscoreSelect %>%
#   group_by(U2PostScoreLevel) %>%
#   dplyr::summarise(across(selectFeatures, median, .names = "median_{col}"))

# group_medians_wide <- group_medians %>%
#   pivot_wider(names_from = U2PostScoreLevel, values_from = starts_with("median"))

# median_differences <- group_medians_wide %>%
#   mutate(across(starts_with("median_"), ~ .[1] - .[2], .names = "diff_{col}"))

# different_features <- median_differences %>%
#   filter(across(starts_with("diff_median_"), ~ . != 0))

#str(different_features)

U2TriggeringPostscoreSelectLong = U2TriggeringPostscoreFinal %>% pivot_longer(cols = where(is.numeric), names_to = "feature", values_to = "value" )
str(U2TriggeringPostscoreSelectLong)

# ggplot(U2TriggeringPostscoreSelectLong, aes(x = feature, y = value, fill = U2PostScoreLevel)) +
#   geom_boxplot() +
#   labs(title = "Boxplots of Multiple Features by Group", x = "Feature", y = "Value") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggplot(U2TriggeringPostscoreSelectLong, aes(x = feature, fill = U2PostScoreLevel)) +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~ feature) +
#   labs(title = "Density Plot of Features by Group", x = "Value", y = "Density")

ggplot(U2TriggeringPostscoreSelectLong, aes(x = value, y = feature, fill = U2PostScoreLevel)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  labs(title = "Ridge Line Plot of Selected Features by Group", x = "Value", y = "Feature") +
  theme_ridges() +
  theme(legend.position = "right") +
  xlim(0, 10)

# Unit 3 Object Interaction Correlation

# Read the post score assessment table
FT2Assessment = read.csv("MHS-FT2-Assessment/FT2Assessment.csv", stringsAsFactors = FALSE)
str(FT2Assessment)

FT2U3PostAssessment = FT2Assessment %>% select("playerName", "POSTU3")

# Read the object interaction dataset
U3ObjectInteraction = read.csv("MHSFT2U3Data/U3TriggeringFrequeWithtrial.csv", stringsAsFactors = FALSE)[,-1]
str(U3ObjectInteraction)

# U3ObjectInteraction = U3ObjectInteraction %>% separate(playerName, into = c("playerName", "trial"), sep = ",")
# str(U2ObjectInteraction)

U3ObjectInteractionU3PostAss = inner_join(FT2U3PostAssessment,U3ObjectInteraction,by="playerName")

str(U3ObjectInteractionU3PostAss)

U3ObjectInteractionU3PostAss = U3ObjectInteractionU3PostAss %>% mutate(playerNameTrial = paste(playerName, trial, sep = ","))

U3ObjectInteractionU3PostAss = U3ObjectInteractionU3PostAss %>% select(-c("playerName","trial"))

colnames(U3ObjectInteractionU3PostAss)

U3ObjectInteractionU3PostAssN = U3ObjectInteractionU3PostAss[,-166]

#cor_matrix = cor(U2ObjectInteractionU2PostAssN)

U3PostAssessment = U3ObjectInteractionU3PostAssN$POSTU3

cor_results <- sapply(names(U3ObjectInteractionU3PostAssN)[names(U3ObjectInteractionU3PostAssN) != "POSTU3"], function(x) {
  cor_test <- cor.test(U3ObjectInteractionU3PostAssN[[x]], U3PostAssessment, method = "spearman")
  return(c(correlation = cor_test$estimate, p_value = cor_test$p.value))
})

cor_results_df <- as.data.frame(t(cor_results))
cor_results_df$Variable <- rownames(cor_results_df)
rownames(cor_results_df) <- NULL

cor_results_df_Sig = cor_results_df %>% dplyr::filter(p_value < 0.05)
cor_results_df_Sig

cor_results_df_Sig[1,"Variable"] = "Toggle.Trigger"
cor_results_df_Sig[2,"Variable"] = "Convince.Bill"
cor_results_df_Sig[3,"Variable"] = "Toggle.Trigger.2"
cor_results_df_Sig[4,"Variable"] = "Check.Sam.Garden"
cor_results_df_Sig[5,"Variable"] = "Measure.East.River"
cor_results_df_Sig[6,"Variable"] = "Measure.West.River"
cor_results_df_Sig[7,"Variable"] = "Alien.Garden.3"
cor_results_df_Sig[8,"Variable"] = "Alien.Garden.1"
cor_results_df_Sig[9,"Variable"] = "Alien.Garden.2"
cor_results_df_Sig[10,"Variable"] = "Alien.Garden.4"
cor_results_df_Sig[11,"Variable"] = "Big.Tree"
cor_results_df_Sig[12,"Variable"] = "Quest.Complete"
cor_results_df_Sig[13,"Variable"] = "Connect.Splines"
cor_results_df_Sig[14,"Variable"] = "Control.UI.off"
cor_results_df_Sig[15,"Variable"] = "Control.UI.On"
cor_results_df_Sig[16,"Variable"] = "Control.UI.Wait"
cor_results_df_Sig[17,"Variable"] = "Cube.1"
cor_results_df_Sig[18,"Variable"] = "Cube.Pad.3"
cor_results_df_Sig[19,"Variable"] = "Cube.Tutorial"
cor_results_df_Sig[20,"Variable"] = "Entrance.Wing.Pump"
cor_results_df_Sig[21,"Variable"] = "Exit.To.Garden"
cor_results_df_Sig[22,"Variable"] = "Fade.In.Start"
cor_results_df_Sig[23,"Variable"] = "Found.a.Crate"
cor_results_df_Sig[24,"Variable"] = "Go.To.Sam"
cor_results_df_Sig[25,"Variable"] = "Quest.Scene"
cor_results_df_Sig[26,"Variable"] = "Load.River"
cor_results_df_Sig[27,"Variable"] = "Pollution.Cam"
cor_results_df_Sig[28,"Variable"] = "Power.Cube"
cor_results_df_Sig[29,"Variable"] = "Pump.Wing.3"
cor_results_df_Sig[30,"Variable"] = "Quest.Giver"
cor_results_df_Sig[31,"Variable"] = "Return.Sam.After.Crates"
cor_results_df_Sig[32,"Variable"] = "River.Collider"
cor_results_df_Sig[33,"Variable"] = "Crate.1"
cor_results_df_Sig[34,"Variable"] = "Crate.2"
cor_results_df_Sig[35,"Variable"] = "Crate.4"
cor_results_df_Sig[36,"Variable"] = "Start.Level"
cor_results_df_Sig[37,"Variable"] = "Start.On"
cor_results_df_Sig[38,"Variable"] = "Teleport.U3"
cor_results_df_Sig[39,"Variable"] = "Teleport.U4"
cor_results_df_Sig[40,"Variable"] = "Teleporter.Trigger"
cor_results_df_Sig[41,"Variable"] = "Teleporter"
cor_results_df_Sig[42,"Variable"] = "Threw.All.Crates"
cor_results_df_Sig[43,"Variable"] = "Trigger.Toggle.1"
cor_results_df_Sig[44,"Variable"] = "Trigger.Toggle.2"
cor_results_df_Sig[45,"Variable"] = "Trigger.Wait.After"
cor_results_df_Sig[46,"Variable"] = "Trigger.Wait"
cor_results_df_Sig[47,"Variable"] = "U3.D.Entrance.Room"
cor_results_df_Sig[48,"Variable"] = "U3.D.Main.R.After.Wing.3"
cor_results_df_Sig[49,"Variable"] = "U3.D.Wing.2.Room.Door.Bar"
cor_results_df_Sig[50,"Variable"] = "U3.D.Wing.3.Room.Facility"
cor_results_df_Sig[51,"Variable"] = "Wing.3.Room.Waypoint"
cor_results_df_Sig[52,"Variable"] = "Wing.3.Room.Waypoint.1"
cor_results_df_Sig[53,"Variable"] = "Wrong.River"

str(cor_results_df_Sig)

write.csv(cor_results_df_Sig, "InGameBehaviorLearningOUtcomeCorrelation/Unit3ObjectInteractionCorrelation.csv")

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, fill = correlation.rho > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Spearman's Correlation Coefficient") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho)) +
#   geom_point(aes(size = -log10(p_value), color = correlation.rho)) +
#   coord_flip() +  # Flip coordinates for better readability
#   scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
#                         name="Spearman's Correlation") +
#   labs(title = "Spearman's Correlation with Dependent Variable",
#        x = "Independent Variables",
#        y = "Correlation Coefficient") +
#   theme_minimal()

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, color = correlation.rho > 0)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4) +
  coord_flip() +
  labs(title = "Lollipop Plot of Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Correlation Coefficient") +
  scale_color_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# Unit 3 post score High and Low level (average standard) 

# Read the object interaction dataset
str(U3ObjectInteractionU3PostAss)
U3ObjectInteractionU3PostAss$POSTU3D = ifelse(U3ObjectInteractionU3PostAss$POSTU3 >= 2, "High", "Low")

U3ObjectInteractionU3PostAss = U3ObjectInteractionU3PostAss %>% select(-"POSTU3")
colnames(U3ObjectInteractionU3PostAss)

U3ObjectInteractionU3PostAssS = U3ObjectInteractionU3PostAss %>% dplyr::select(-"playerNameTrial")
str(U3ObjectInteractionU3PostAssS)
colnames(U3ObjectInteractionU3PostAssS)

featureNames = colnames(U3ObjectInteractionU3PostAssS)[-165]
featureNames

results <- lapply(featureNames, function(feature) {
  test_result <- wilcox.test(U3ObjectInteractionU3PostAssS[[feature]] ~ U3ObjectInteractionU3PostAssS$POSTU3D)
  return(data.frame(
    Feature = feature,
    W = test_result$statistic,
    p_value = test_result$p.value
  ))
})

resultsDataFrame <- do.call(rbind, results)
print(resultsDataFrame)

significantFeatures =  resultsDataFrame %>% dplyr::filter(p_value < 0.05)
significantFeatures

selectFeatures = significantFeatures$Feature

U3TriggeringPostscoreSelect =U3ObjectInteractionU3PostAssS %>% dplyr::select(c(selectFeatures,"POSTU3D"))
str(U3TriggeringPostscoreSelect)
colnames(U3TriggeringPostscoreSelect)

U3TriggeringPostscoreSelectLong = U3TriggeringPostscoreSelect %>% pivot_longer(cols = where(is.numeric), names_to = "feature", values_to = "value" )
str(U3TriggeringPostscoreSelectLong)

ggplot(U3TriggeringPostscoreSelectLong, aes(x = value, y = feature, fill = POSTU3D)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  labs(title = "Ridge Line Plot of Selected Features by Group", x = "Value", y = "Feature") +
  theme_ridges() +
  theme(legend.position = "right") + 
  xlim(0, 20)

# Unit 4 Object Interaction Correlation

# Read the post score assessment table
FT2Assessment = read.csv("MHS-FT2-Assessment/FT2Assessment.csv", stringsAsFactors = FALSE)
str(FT2Assessment)

FT2U4PostAssessment = FT2Assessment %>% select("playerName", "POSTU4")

# Read the object interaction dataset
U4ObjectInteraction = read.csv("MHSFT2U4Data/U4TriggeringFreque.csv", stringsAsFactors = FALSE)[,-1]
str(U4ObjectInteraction)

U4ObjectInteractionS = U4ObjectInteraction %>% pivot_wider(names_from = objectNameWAction, values_from = frequency, values_fill = 0)
str(U4ObjectInteractionS)

U4ObjectInteractionS = U4ObjectInteractionS %>% separate(playerName, into = c("playerName", "trial"), sep = ",")
str(U4ObjectInteractionS)

U4ObjectInteractionU4PostAss = inner_join(FT2U4PostAssessment,U4ObjectInteractionS,by="playerName")

str(U4ObjectInteractionU4PostAss)

U4ObjectInteractionU4PostAss = U4ObjectInteractionU4PostAss %>% mutate(playerNameTrial = paste(playerName, trial, sep = ","))

U4ObjectInteractionU4PostAss = U4ObjectInteractionU4PostAss %>% select(-c("playerName","trial"))

colnames(U4ObjectInteractionU4PostAss)

U4ObjectInteractionU4PostAssN = U4ObjectInteractionU4PostAss[,-199]

#cor_matrix = cor(U2ObjectInteractionU2PostAssN)

U4PostAssessment = U4ObjectInteractionU4PostAssN$POSTU4

cor_results <- sapply(names(U4ObjectInteractionU4PostAssN)[names(U4ObjectInteractionU4PostAssN) != "POSTU4"], function(x) {
  cor_test <- cor.test(U4ObjectInteractionU4PostAssN[[x]], U4PostAssessment, method = "spearman")
  return(c(correlation = cor_test$estimate, p_value = cor_test$p.value))
})

cor_results_df <- as.data.frame(t(cor_results))
cor_results_df$Variable <- rownames(cor_results_df)
rownames(cor_results_df) <- NULL

cor_results_df_Sig = cor_results_df %>% dplyr::filter(p_value < 0.05)
cor_results_df_Sig

cor_results_df_Sig[1,"Variable"] = "Millitary.Load"
cor_results_df_Sig[2,"Variable"] = "Start.Load"
cor_results_df_Sig[3,"Variable"] = "Talk.Anderson"
cor_results_df_Sig[4,"Variable"] = "Making.a.Well"
cor_results_df_Sig[5,"Variable"] = "Trigger.Enter"
cor_results_df_Sig[6,"Variable"] = "Success.Pipe"
cor_results_df_Sig[7,"Variable"] = "Cube.on.Pad"
cor_results_df_Sig[8,"Variable"] = "Wires.Floor.6"
cor_results_df_Sig[9,"Variable"] = "Select.Gravel"
cor_results_df_Sig[10,"Variable"] = "Cube.Not.on.Pad"
cor_results_df_Sig[11,"Variable"] = "Cube.Pad"
cor_results_df_Sig[12,"Variable"] = "Second.Generator"
cor_results_df_Sig[13,"Variable"] = "Second.Generator.1"
cor_results_df_Sig[14,"Variable"] = "Alien.Load"
cor_results_df_Sig[15,"Variable"] = "New.Drill"
cor_results_df_Sig[16,"Variable"] = "New.Drill.1"
cor_results_df_Sig[17,"Variable"] = "Wires.to.Door.6"
cor_results_df_Sig[18,"Variable"] = "Look.at.Panel"
cor_results_df_Sig[19,"Variable"] = "B2.Trigger"
cor_results_df_Sig[20,"Variable"] = "Cinematic4"
cor_results_df_Sig[21,"Variable"] = "SoilFloor5"
cor_results_df_Sig[22,"Variable"] = "Unfinished.Dialogue"
cor_results_df_Sig[23,"Variable"] = "Unfinished.Dialogue.Wait"
cor_results_df_Sig[24,"Variable"] = "Finished.Dialogue"
cor_results_df_Sig[25,"Variable"] = "Cinematic5"
cor_results_df_Sig[26,"Variable"] = "Cinematic5.1"
cor_results_df_Sig[27,"Variable"] = "SoilFloor6"
cor_results_df_Sig[28,"Variable"] = "Exit.Scene"
cor_results_df_Sig[29,"Variable"] = "Dialogue"
cor_results_df_Sig[30,"Variable"] = "Dialogue.Waite"
cor_results_df_Sig[31,"Variable"] = "Leave.Drill.Room"
cor_results_df_Sig[32,"Variable"] = "Success.Dialogue"
cor_results_df_Sig[33,"Variable"] = "Trigger.Wait.Dialogue"
cor_results_df_Sig[34,"Variable"] = "Exit.Drill"
cor_results_df_Sig[35,"Variable"] = "Trigger.Enter.1"
cor_results_df_Sig[36,"Variable"] = "Smooth.Camera"
cor_results_df_Sig[37,"Variable"] = "At.The.Fountain"
cor_results_df_Sig[38,"Variable"] = "Direct.Irrigation"
cor_results_df_Sig[39,"Variable"] = "Head.to.Bunker"
cor_results_df_Sig[40,"Variable"] = "Bunker.Entrance"
cor_results_df_Sig[41,"Variable"] = "Trigger.Start"
cor_results_df_Sig[42,"Variable"] = "Flood.Water"
cor_results_df_Sig[43,"Variable"] = "Flood.Water.1"
cor_results_df_Sig[44,"Variable"] = "Scan.Area"
cor_results_df_Sig[45,"Variable"] = "Scan.Area.1"
cor_results_df_Sig[46,"Variable"] = "Living.Queaters"
cor_results_df_Sig[47,"Variable"] = "Mess.Hall"
cor_results_df_Sig[48,"Variable"] = "Warehouse"
cor_results_df_Sig[49,"Variable"] = "Default.Trigger"
cor_results_df_Sig[50,"Variable"] = "Default.Trigger.1"
cor_results_df_Sig[51,"Variable"] = "Leave.Bunker.Trigger"
cor_results_df_Sig[52,"Variable"] = "Fountain.Pipe"
cor_results_df_Sig[53,"Variable"] = "Report.to.Anderson"
cor_results_df_Sig[54,"Variable"] = "Scan.1"
cor_results_df_Sig[55,"Variable"] = "Scan.2"
cor_results_df_Sig[56,"Variable"] = "Scan.3"
cor_results_df_Sig[57,"Variable"] = "Report.Scans"
cor_results_df_Sig[58,"Variable"] = "Talk.to.Anderson"
cor_results_df_Sig[59,"Variable"] = "Fade.Out"
cor_results_df_Sig[60,"Variable"] = "Results.Dialogue"
cor_results_df_Sig[61,"Variable"] = "Dialogue.Toggle"
cor_results_df_Sig[62,"Variable"] = "Best.Fountain"
cor_results_df_Sig[63,"Variable"] = "Anderson.West.Supp"
cor_results_df_Sig[64,"Variable"] = "Bunker.Water.Table"
cor_results_df_Sig[65,"Variable"] = "Ruin.Water.Table"
cor_results_df_Sig[66,"Variable"] = "Teleporter"
cor_results_df_Sig[67,"Variable"] = "Teleporter.P.Base"
cor_results_df_Sig[68,"Variable"] = "Spotlights"

str(cor_results_df_Sig)

write.csv(cor_results_df_Sig, "InGameBehaviorLearningOUtcomeCorrelation/Unit4ObjectInteractionCorrelation.csv")

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, fill = correlation.rho > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Spearman's Correlation Coefficient") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho)) +
#   geom_point(aes(size = -log10(p_value), color = correlation.rho)) +
#   coord_flip() +  # Flip coordinates for better readability
#   scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
#                         name="Spearman's Correlation") +
#   labs(title = "Spearman's Correlation with Dependent Variable",
#        x = "Independent Variables",
#        y = "Correlation Coefficient") +
#   theme_minimal()

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, color = correlation.rho > 0)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4) +
  coord_flip() +
  labs(title = "Lollipop Plot of Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Correlation Coefficient") +
  scale_color_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# Unit 4 post score High and Low level (average standard) 

# Read the object interaction dataset
str(U4ObjectInteractionU4PostAss)
mean(U4ObjectInteractionU4PostAss$POSTU4)
U4ObjectInteractionU4PostAss$POSTU4D = ifelse(U4ObjectInteractionU4PostAss$POSTU4 >= 3, "High", "Low")

U4ObjectInteractionU4PostAss = U4ObjectInteractionU4PostAss %>% select(-"POSTU4")
colnames(U4ObjectInteractionU4PostAss)

U4ObjectInteractionU4PostAssS = U4ObjectInteractionU4PostAss %>% dplyr::select(-"playerNameTrial")
str(U4ObjectInteractionU4PostAssS)
colnames(U4ObjectInteractionU4PostAssS)

featureNames = colnames(U4ObjectInteractionU4PostAssS)[-198]
featureNames

results <- lapply(featureNames, function(feature) {
  test_result <- wilcox.test(U4ObjectInteractionU4PostAssS[[feature]] ~ U4ObjectInteractionU4PostAssS$POSTU4D)
  return(data.frame(
    Feature = feature,
    W = test_result$statistic,
    p_value = test_result$p.value
  ))
})

resultsDataFrame <- do.call(rbind, results)
print(resultsDataFrame)

significantFeatures =  resultsDataFrame %>% dplyr::filter(p_value < 0.01)
significantFeatures

selectFeatures = significantFeatures$Feature

U4TriggeringPostscoreSelect =U4ObjectInteractionU4PostAssS %>% dplyr::select(c(selectFeatures,"POSTU4D"))
str(U4TriggeringPostscoreSelect)
colnames(U4TriggeringPostscoreSelect)

U4TriggeringPostscoreSelectLong = U4TriggeringPostscoreSelect %>% pivot_longer(cols = where(is.numeric), names_to = "feature", values_to = "value" )
str(U4TriggeringPostscoreSelectLong)

ggplot(U4TriggeringPostscoreSelectLong, aes(x = value, y = feature, fill = POSTU4D)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  labs(title = "Ridge Line Plot of Selected Features by Group", x = "Value", y = "Feature") +
  theme_ridges() +
  theme(legend.position = "right") + 
  xlim(0, 15)

# Unit 5 Object Interaction Correlation

# Read the post score assessment table
FT2Assessment = read.csv("MHS-FT2-Assessment/FT2Assessment.csv", stringsAsFactors = FALSE)
str(FT2Assessment)

FT2U5PostAssessment = FT2Assessment %>% select("playerName", "POSTU5")

# Read the object interaction dataset
U5ObjectInteraction = read.csv("MHSFT2U5Data/U5TriggeringFrequeS.csv", stringsAsFactors = FALSE)[,-1]
str(U5ObjectInteraction)

#U5ObjectInteractionS = U4ObjectInteraction %>% pivot_wider(names_from = objectNameWAction, values_from = frequency, values_fill = 0)
#str(U4ObjectInteractionS)

U5ObjectInteraction = U5ObjectInteraction %>% separate(playerName, into = c("playerName", "trial"), sep = ",")
str(U5ObjectInteraction)

U5ObjectInteractionU5PostAss = inner_join(FT2U5PostAssessment,U5ObjectInteraction,by="playerName")

str(U5ObjectInteractionU5PostAss)

U5ObjectInteractionU5PostAss = U5ObjectInteractionU5PostAss %>% mutate(playerNameTrial = paste(playerName, trial, sep = ","))

U5ObjectInteractionU5PostAss = U5ObjectInteractionU5PostAss %>% select(-c("playerName","trial"))

colnames(U5ObjectInteractionU5PostAss)

U5ObjectInteractionU5PostAssN = U5ObjectInteractionU5PostAss[,-188]

#cor_matrix = cor(U2ObjectInteractionU2PostAssN)

U5PostAssessment = U5ObjectInteractionU5PostAssN$POSTU5

cor_results <- sapply(names(U5ObjectInteractionU5PostAssN)[names(U5ObjectInteractionU5PostAssN) != "POSTU5"], function(x) {
  cor_test <- cor.test(U5ObjectInteractionU5PostAssN[[x]], U5PostAssessment, method = "spearman")
  return(c(correlation = cor_test$estimate, p_value = cor_test$p.value))
})

cor_results_df <- as.data.frame(t(cor_results))
cor_results_df$Variable <- rownames(cor_results_df)
rownames(cor_results_df) <- NULL

cor_results_df_Sig = cor_results_df %>% dplyr::filter(p_value < 0.05)
cor_results_df_Sig

cor_results_df_Sig1 = data.frame(cor_results_df_Sig[-c(1:8),])
rownames(cor_results_df_Sig1) <- NULL
cor_results_df_Sig1

cor_results_df_Sig1[1,"Variable"] = "Atmospheric.Water"
cor_results_df_Sig1[2,"Variable"] = "After.Argumentation"
cor_results_df_Sig1[3,"Variable"] = "Evaporator.Wires.3"
cor_results_df_Sig1[4,"Variable"] = "Canteen"
cor_results_df_Sig1[5,"Variable"] = "Cave.Entrance"
cor_results_df_Sig1[6,"Variable"] = "Cave.Trigger"
cor_results_df_Sig1[7,"Variable"] = "Check.Humidity"
cor_results_df_Sig1[8,"Variable"] = "Default.Trigger"
cor_results_df_Sig1[9,"Variable"] = "No.Go.Upstairs"
cor_results_df_Sig1[10,"Variable"] = "No.Move.On.Dialogue"
cor_results_df_Sig1[11,"Variable"] = "Exit.Teleporter"
cor_results_df_Sig1[12,"Variable"] = "Factory.Humidity"
cor_results_df_Sig1[13,"Variable"] = "Fade.In"
cor_results_df_Sig1[14,"Variable"] = "Fill.Pan"
cor_results_df_Sig1[15,"Variable"] = "Gather.Water"
cor_results_df_Sig1[16,"Variable"] = "Back.Player.Base.1"
cor_results_df_Sig1[17,"Variable"] = "Hose.Pot"
cor_results_df_Sig1[18,"Variable"] = "Humidity"
cor_results_df_Sig1[19,"Variable"] = "Load.Cave.Quests"
cor_results_df_Sig1[20,"Variable"] = "Load.Quests.Start"
cor_results_df_Sig1[21,"Variable"] = "Load.Quests"
cor_results_df_Sig1[22,"Variable"] = "Near.Cave.Trigger"
cor_results_df_Sig1[23,"Variable"] = "Prefab.Dungeon.Button"
cor_results_df_Sig1[24,"Variable"] = "Prefab.Dungeon.Button.1"
cor_results_df_Sig1[25,"Variable"] = "Evaporator.Console"
cor_results_df_Sig1[26,"Variable"] = "Seaside.Humidity"
cor_results_df_Sig1[27,"Variable"] = "Soil.Puzzle.Controller"
cor_results_df_Sig1[28,"Variable"] = "Start.Quest.Line"
cor_results_df_Sig1[29,"Variable"] = "Stations.Start"
cor_results_df_Sig1[30,"Variable"] = "Stations"
cor_results_df_Sig1[31,"Variable"] = "Task.Trigger"
cor_results_df_Sig1[32,"Variable"] = "Teleporter.Base"
cor_results_df_Sig1[33,"Variable"] = "Teleporter"
cor_results_df_Sig1[34,"Variable"] = "Timeline.Enabler"
cor_results_df_Sig1[35,"Variable"] = "Trigger.Arf"
cor_results_df_Sig1[36,"Variable"] = "Dialogue.Event"
cor_results_df_Sig1[37,"Variable"] = "Trigger.On.Enter.1"
cor_results_df_Sig1[38,"Variable"] = "Trigger.On.Enter.2"
cor_results_df_Sig1[39,"Variable"] = "Trigger.On.1"
cor_results_df_Sig1[40,"Variable"] = "Trigger.On.2"
cor_results_df_Sig1[41,"Variable"] = "Wires.To.Condensor.1"

str(cor_results_df_Sig1)

write.csv(cor_results_df_Sig1, "InGameBehaviorLearningOUtcomeCorrelation/Unit5ObjectInteractionCorrelation.csv")

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, fill = correlation.rho > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Spearman's Correlation Coefficient") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho)) +
#   geom_point(aes(size = -log10(p_value), color = correlation.rho)) +
#   coord_flip() +  # Flip coordinates for better readability
#   scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
#                         name="Spearman's Correlation") +
#   labs(title = "Spearman's Correlation with Dependent Variable",
#        x = "Independent Variables",
#        y = "Correlation Coefficient") +
#   theme_minimal()

ggplot(cor_results_df_Sig, aes(x = reorder(Variable, correlation.rho), y = correlation.rho, color = correlation.rho > 0)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 4) +
  coord_flip() +
  labs(title = "Lollipop Plot of Spearman's Correlation with Dependent Variable",
       x = "Independent Variables",
       y = "Correlation Coefficient") +
  scale_color_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme_minimal()

# Unit 5 post score High and Low level (average standard) 

# Read the object interaction dataset
str(U5ObjectInteractionU5PostAss)
mean(U5ObjectInteractionU5PostAss$POSTU5)
U5ObjectInteractionU5PostAss$POSTU5D = ifelse(U5ObjectInteractionU5PostAss$POSTU5 >= 8, "High", "Low")

U5ObjectInteractionU5PostAss = U5ObjectInteractionU5PostAss %>% select(-"POSTU5")
colnames(U5ObjectInteractionU5PostAss)

U5ObjectInteractionU5PostAssS = U5ObjectInteractionU5PostAss %>% dplyr::select(-"playerNameTrial")
str(U5ObjectInteractionU5PostAssS)
colnames(U5ObjectInteractionU5PostAssS)

featureNames = colnames(U5ObjectInteractionU5PostAssS)[-187]
featureNames

results <- lapply(featureNames, function(feature) {
  test_result <- wilcox.test(U5ObjectInteractionU5PostAssS[[feature]] ~ U5ObjectInteractionU5PostAssS$POSTU5D)
  return(data.frame(
    Feature = feature,
    W = test_result$statistic,
    p_value = test_result$p.value
  ))
})

resultsDataFrame <- do.call(rbind, results)
print(resultsDataFrame)

significantFeatures =  resultsDataFrame %>% dplyr::filter(p_value < 0.05)
significantFeatures

selectFeatures = significantFeatures$Feature

U5TriggeringPostscoreSelect =U5ObjectInteractionU5PostAssS %>% dplyr::select(c(selectFeatures,"POSTU5D"))
str(U5TriggeringPostscoreSelect)
colnames(U5TriggeringPostscoreSelect)

U5TriggeringPostscoreSelectLong = U5TriggeringPostscoreSelect %>% pivot_longer(cols = where(is.numeric), names_to = "feature", values_to = "value" )
str(U5TriggeringPostscoreSelectLong)

ggplot(U5TriggeringPostscoreSelectLong, aes(x = value, y = feature, fill = POSTU5D)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  labs(title = "Ridge Line Plot of Selected Features by Group", x = "Value", y = "Feature") +
  theme_ridges() +
  theme(legend.position = "right") + 
  xlim(0, 15)


