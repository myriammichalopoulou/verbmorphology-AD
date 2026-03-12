# Verb Morphology Processing in Individuals with Alzheimer’s Disease

## Abstract 
Research on morphological processing in neurodegenerative populations has played a central role in the debate between rule-based and usage-based accounts of language. Dual-route models posit a symbolic rule mechanism for regular inflection and lexical storage for irregular forms, predicting selective impairments across verb types. In contrast, single-route, constructivist approaches emphasise frequency-sensitive, gradient processing based on distributional patterns in the input. Evidence from individuals with Alzheimer’s-type of dementia has often been interpreted as supporting dual-route accounts, as these individuals reportedly perform better on regular than irregular verbs. However, previous studies have not consistently controlled for token frequency, leaving open the possibility that frequency effects may drive these asymmetries. The present study investigates the past perfective verb tense in Modern Greek, in which regular verbs are marked by the aspectual morpheme -s-, while irregular and suppletive verbs involve stem allomorphy. Within this study, 29 participants (22 individuals with dementia and 7 older adult controls) completed a grammaticality judgement task with 95 experimental items. The results revealed significant token frequency effects for both regular and irregular verbs. However, the strongest predictors of performance were participant-level features, particularly educational attainment and age. The observed frequency effect poses challenges to strictly rule-based accounts and instead supports analogy-based approaches to morphological processing.

## frequency
### Description
The dataset consists of Greek verbs in their past perfective forms, focusing on both regular and irregular verbs. The data comes from two main sources: written corpora (Leipzig Corpora Collection) and spoken corpora (Corpus of Spoken Greek). Written data includes texts from Wikipedia, news websites, and general web sources, spanning the years 2011–2024, while spoken data includes recorded and transcribed spoken Greek from various speakers. For compound verbs in the past perfective sometimes there two variants included, namely one with and one without the augment. The resulting dataset contains both written and spoken occurrences of verbs, which are then aggregated to examine frequency and form variants across modalities. Each verb is annotated with:

Token: the verb form in lowercase written in the Greek alphabet

Variants: the alternative form(s) of the verb

IPA: phonetic transcription

Regularity: whether the verb is regular or irregular

Frequency: categorized as “low” or “high” depending on the count of occurrences in the corpus

Sentence: a sample sentence illustrating the verb’s use

### Data
- Written corpora CSVs: wikipedia_2021.csv, web_2011.csv, web_2015.csv, wikipedia_2016.csv, news_2019.csv, news_2020.csv, news_2021.csv, news_2022.csv, news_2023.csv, news_2024.csv

Each file contains verb tokens from a specific source and year, with counts of occurrences.

- Spoken dataset CSV: spoken_dataset.csv

Contains verb tokens from transcribed spoken Greek, with counts of occurrences.

### Data Processing
1. Reset the R environment: Clears all variables and unloads all previously loaded packages to ensure a fresh session.
2. Load necessary packages: readr for reading CSV files, dplyr for data manipulation, stringr for string processing
3. Read and combine written corpora: Each CSV is read individually and then merged using bind_rows() into a single dataset called written_dataset.
4. Select relevant verbs: Filter tokens to keep only verbs of interest (verbs_to_keep). Recode variant forms of verbs to unify augment and non-augment forms using the variants mapping.
5. Annotate written data: Convert tokens to lowercase. Summarize counts for each token.
6. Add categorical variables: Frequency, Variants, IPA, and Regularity. Check the balance of verbs across Regularity and Frequency.
7. Process spoken data: filter tokens, recode variants, summarize counts, assign Frequency and Regularity. Check balance of spoken data.
8. Combine written and spoken datasets: Merge the two datasets with bind_rows(). Aggregate counts by verb (Token). Attach annotations (Variants, IPA, Regularity) from the written dataset. Create a final categorical variable for Frequency based on total counts.
9. Add experimental sentences: Each verb is assigned a representative Greek sentence to illustrate its usage.
8. Descriptive checks: Count verbs by Regularity and Frequency to ensure a balanced dataset.

## gramjudg-task
### Description
The dataset consists of experimental results from a PCIbex study examining grammaticality judgments of Greek verbs. Participants completed a series of trials including warm-up experimental and filler items. Participant demographics (age, sex, education, bilingualism, hearing impairment, etc.) are also included. The main goal is to analyze how verb frequency, and other demographic factors can predict participants’ accuracy on grammaticality judgments.

### Data

Experimental results CSV: results_original.csv
Columns:
1. Participant.ID: unique participant identifier
2. Token: verb form
3. Condition: “grammatical” or “ungrammatical”
4. Value: participant’s response (“Σωστό” / “Λάθος”)
5. Trial.Type, PennElementName, and other technical columns from PCIbex
6. Participant demographics CSV: participants.csv
7. Age, Sex, Years.of.Education, Diagnosis, Bilingualism, Hearing.Impairment, Hand.Dominance, MMSE, DSF, DSB, etc.

### Data Processing
1. Reset R session
- Clears all variables and unloads packages to ensure a fresh session.
2. Load packages
- Packages include tidyverse, lme4, lmerTest, sjPlot, ggeffects, datawizard, car, arsenal, DHARMa, performance, pscl, and others needed for data wrangling, visualization, and modeling.
3.Read PCIbex data
- read.pcibex() function extracts column names and values from PCIbex output CSV.
- Results and demographics are read in and merged using participant IDs.
5. Clean and wrangle results
- Extract Participant.ID from PCIbex ID trials.
-Filter only experimental items (Trial.Type == "experimental item" and PennElementName == "judgement").
-Remove unnecessary PCIbex-specific columns.
- Create Accuracy variable: “correct” or “incorrect” based on participant responses and grammatical condition.
- Apply manual corrections to specific participant responses.
- Merge demographics with results to create final_data.
- Exclude trials with missing responses.
- Convert categorical variables to factors and numeric variables to numeric.
- Apply sum coding for categorical predictors.
- Compute a cognitive composite score from MMSE, DSF, and DSB, then z-score it.
- Z-score Age, Years of Education, and log-transform Count (frequency) data.
5. Descriptive checks
- Visualize density of Count, Count_log, and Count_log_z.
- Summary tables for cognitive measures, age, and education by participant group.
6. Logistic regression of cognitive measures predicting diagnosis
- Outcome: Diagnosis_binary (diagnosis vs no diagnosis)
- Predictors: MMSE, DSF, DSB individually and together
- Model diagnostics: collinearity check, correlation matrix, AIC/BIC, R² using pscl::pR2
7. Mixed-effects logistic regression predicting accuracy
- Outcome: Accuracy
- Fixed effects:
* CognitiveComposite_z × (Count_log_z × Regularity + Foreign.Languages + Hearing.Impairment + Years.of.Education_z + Sex + Age_z)
- Random effects:
* Random intercepts for Participant.ID
* Random slopes for Token (CognitiveComposite_z + Age_z)
- Diagnostics:
* Check singular fits, variance of random effects, collinearity
* Visualize random effects using dotplot
- Model summaries and visualization
* sjPlot::tab_model() provides detailed tables of coefficients, standard errors, z-values, and p-values
* Log-odds plots with 95% Wald confidence intervals
8. Key Plots
- Surface-form frequency
* ggeffects::ggeffect() used to predict accuracy across Count_log_z
* Ribbon plots show confidence intervals
- Participant-level predictions
* Example for participants with dementia
* Points and IPA labels plotted over predicted lines
- Main effects
* Age, Years of Education, Cognitive Composite Score
- Interactions
* Cognitive Composite Score × Hearing Impairment
* Cognitive Composite Score × Multilingualism
* Cognitive Composite Score × Years of Education
- Descriptive control of interactions
* Frequency × Regularity: mean and SD of log-frequency
* Number of multilinguals and monolinguals per group
* Check levels of Foreign.Languages
- Additional models
* Logistic regression: Multilingualism predicted by Years of Education
