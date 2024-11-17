# 2025-Lu-Et-Al

# Observable Feature Generation and Analytic Workflow for MHS Game

This repository contains scripts, datasets, and workflows used to generate observable features, process data into latent variables, and train machine learning models for predicting learning outcomes in the Mission HydroSci (MHS) game. Below is a detailed breakdown of the repository's contents.

---

## 📂 ObservableFeatureGeneration

This folder includes R scripts for generating observable features from raw data fetched from MongoDB.

- **`argObservableFeatureGenerator.R`**: Generates features for:
  - Argumentation-related frequency
  - Argumentation-related speed
  - Argumentation-related share

- **`behaviorTypeObservableFeatureGenerator.R`**: Generates features for:
  - Behavior type interaction frequency
  - Behavior type interaction speed
  - Behavior type interaction share

- **`dialogueReadObservableFeatureGenerator.R`**: Generates features for:
  - Dialogue interaction frequency
  - Dialogue reading speed
  - Dialogue read duration share

- **`hotkeyObservableFeatureGenerator.R`**: Generates features for:
  - Hotkey interaction frequency
  - Hotkey interaction share

- **`mapExploreObservableFeatureGenerator.R`**: Generates features for:
  - Map exploration size
  - Map exploration speed
  - Map size share

- **`taskObservableFeatureGenerator.R`**: Generates features for:
  - Task completion speed
  - Task completion duration share

- **`toolManuObservableFeatureGenerator.R`**: Generates features for:
  - Tool interaction frequency
  - Tool interaction speed
  - Tool interaction share

- **`itemInteractObservableFeatureGenerator.R`**: Generates features for:
  - In-game object interaction frequency
  - In-game object interaction duration
  - In-game object interaction share

- **`Codes_for_embedded_scores_generation.R`**: Generates features representing players’ learning progress.

### Data Folders:
- **`rawDatasetSamples`**: Contains sample raw datasets directly fetched from the server.
- **`observableFeatures`**: Contains datasets of observable features generated from the R scripts.

---

## 📂 DataProcessingWorkflow

This folder contains Python scripts for processing observable features into latent variables for machine learning model training.

### Scripts:
- **`First-Layer-Unsupervised-Learning.ipynb`**: Conducts first-layer unsupervised learning on individual observable feature datasets.
- **`Second-Layer-Unsupervised-Learning.ipynb`**: Combines first-layer latent variables by behavior type for further unsupervised learning.
- **`Third-Layer-Unsupervised-Learning.ipynb`**: Integrates latent variables across behavior types for generating interaction features.

### Data Folders:
- **`ObservableFeatures`**: Sample datasets of observable features.
- **`FirstLayerLatentVariables`**, **`SecondLayerLatentVariables`**, **`ThirdLayerLatentVariables`**: Datasets from each unsupervised learning layer.
- **`FirstSecondLayerLatentFeatures`**: Combined features from the first and second layers.
- **Outcome-Specific Files**: Feature datasets processed for specific MHS units and learning outcomes.

---

## 📂 ModelTrainingProcessOutcomes

This folder includes Python scripts and results for training ensemble learning models to predict students’ learning outcomes.

### Scripts:
- **Unit-Specific Scripts**:
  - `U2-model-training-process.ipynb`, `U3-model-training-process.ipynb`, etc.
- **Overall Game Scripts**:
  - `Overall-game-content-model-training.ipynb`
  - `Overall-game-argument-model-training.ipynb`

### Data Folders:
- **`Dataset`**: Feature sets used in model training.
- **Result Files**: CSV files containing model training results.

---

## 📂 Importance-Rate-Plots

This folder contains Python scripts and datasets for generating permutation importance rate plots.

### Scripts:
- **Unit-Specific**:
  - `Create-Importance-Rate-U2.ipynb`, etc.
- **Overall Game**:
  - `Create-Importance-Rate-All-Game-Content.ipynb`
  - `Create-Importance-Rate-All-Game-Arg.ipynb`

---

## 📂 ALEPlot

This folder includes Python scripts and datasets for generating Accumulated Local Effect (ALE) plots.

### Scripts:
- **Unit-Specific**:
  - `ALEPlot-U2.ipynb`, etc.
- **Overall Game**:
  - `ALEPlot-OverallContent.ipynb`
  - `ALEPlot-OverallArg.ipynb`

---

## 💡 How to Use

1. **Feature Generation**:
   - Navigate to `ObservableFeatureGeneration`.
   - Run R scripts to generate observable features.

2. **Data Processing**:
   - Use Python scripts in `DataProcessingWorkflow` to process features into latent variables.

3. **Model Training**:
   - Use scripts in `ModelTrainingProcessOutcomes` for training models.

4. **Visualization**:
   - Generate permutation importance rate plots and ALE plots using respective folders.

---

Feel free to explore and use the provided resources. For any questions or suggestions, please open an issue or contact the repository owner.
