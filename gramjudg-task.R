# Setup
## Complete reset for an R session, clearing all variables and unloading all packages

rm(list = ls(all = TRUE))
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T))

setwd("C:/Users/myria/Downloads/Thesis/Experiment")

## Load necessary packages

library(datawizard)
library(arsenal)
library(car)
library(ggeffects)
library(effects)
library(ggplot2)
library(gridExtra)
library(readr)
library(utf8)
library(LMERConvenienceFunctions)
library(lme4)
library(lmerTest)
library(magrittr)
library(NLP)
library(plotrix)
library(plyr); library(dplyr)
library(RColorBrewer)
library(RCurl)
library(sjmisc)
library(sjPlot)
library(splines)
library(tidyr)
library(vroom)
library(shiny)
library(readr)
library(stringr)
library(MuMIn)
library(performance)
library(forcats)
library(knitr)
library(kableExtra)

## Set the maximum number of items that can be printed in the R console

options(max.print=999999)

## Define function to read in PCIbex results files

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break}
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)}
        cols[index] <- value
        if (index == n.cols){
          break}}}
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))}
  else{return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))}}

# Read in files with results and participant demographics

results <- read.pcibex("C:/Users/myria/Downloads/Thesis/Experiment/results_original.csv")
demographics <- read.csv("C:/Users/myria/Downloads/Thesis/Experiment/participants.csv", sep = ";")

# Data Wrangling

## Create a new column for participant ID

results <- results %>%
  mutate(Participant.ID = ifelse(PennElementName == "ID", Value, NA)) %>%
  mutate(Participant.ID = na_if(trimws(Participant.ID), "")) %>%
  fill(Participant.ID)

## Keep only the experimental items

results <- results %>%
  filter(trimws(Trial.Type) == "experimental item",
         trimws(PennElementName) == "judgement")

## Remove the unnecessary columns

results <- results[, !names(results) %in% c("Controller.name", "Label", "Trial.Type", "PennElementType", "PennElementName", "Comments", "Oder.number.of.time", "MD5.hash.of.participant.s.IP.address", "Parameter", "Results.reception.time", "Inner.element.number", "Latin.Square.Group", "EventTime")]

## Create new column for accuracy

results <- results %>%
  mutate(Accuracy = case_when(
      Value == "Σωστό" & Condition == "grammatical"   ~ "correct",
      Value == "Σωστό" & Condition == "ungrammatical" ~ "incorrect",
      Value == "Λάθος" & Condition == "ungrammatical" ~ "correct",
      Value == "Λάθος" & Condition == "grammatical"   ~ "incorrect",
      TRUE ~ NA_character_))

## Do manual corrections

corrections <- data.frame(
  Participant.ID = c(3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
                     4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 10, 10, 11, 13, 13, 14, 14,
                     14, 14, 14, 15, 15, 17, 17, 17, 17, 17, 17, 18, 18, 19), 
  Token          = c("προκάλεσε", "κάλεσε", "καταδίωξε", "έτεινε", "θύμισε", "έφτιαξε", "δίδαξε", "άντεξε", 
                     "πλήθυνε", "σχόλασε", "έβαψε", "βγήκε", "έκλεισε", "περπάτησε", "έβηξε", "έβλαψε", "σκλήρυνε", 
                     "λείανε", "οδήγησε", "γλύκανε", "έλυσε", "εφηύρε", "ανάσανε", "έτυχε", "έπεισε", "άπλωσε", "ένιωσε", 
                     "έπλεξε", "υπέμεινε", "εισήγαγε", "θέρμανε", "έφθειρε", "συγχάρηκε", "κατέληξε", "μαγείρεψε", "έψαλε",
                     "κατέπνιξε", "μίκρυνε", "διέλυσε", "άλεσε", "πήρε", "ύφανε", "έμεινε", "βάρυνε", "ήθελε", "χρησιμοποίησε", 
                     "έσπειρε", "κάλεσε", "δημιούργησε", "πρότεινε", "ύφανε", "γνώρισε", "βγήκε", "απομάκρυνε", "προέβη", "ίδρυσε",
                     "καθάρισε", "έπλεξε", "έλυσε", "πήρε", "δημιούργησε", "επέστρεψε", "έβλαψε", "άντεξε", "μόλυνε", "έδειξε",
                     "χώρεσε", "έβηξε", "κέρδισε", "βρήκε", "εκτίμησε", "σκλήρυνε", "κατέλαβε", "έφαγε", "βοήθησε", "είπε",
                     "βάθυνε", "έβαλε", "έπεισε", "έμεινε", "κατάλαβε", "κράτησε"),
  CorrectedValue = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Σωστό", NA, NA, NA,
                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Σωστό", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Λάθος", NA, NA, NA,
                     "Σωστό", NA, NA, NA, NA, "Λάθος"),
  stringsAsFactors = FALSE)

corrections <- corrections %>%
  mutate(Participant.ID = as.character(Participant.ID),
    Token = as.character(Token))

## Combine corrections with main results

results <- results %>%
  left_join(corrections, by = c("Participant.ID", "Token")) %>%
  mutate(Value = ifelse(!is.na(CorrectedValue), CorrectedValue, Value)) %>%
  select(-CorrectedValue)

## Combine demographics to final clean data

final_data <- merge(results, demographics, by = "Participant.ID")

## Exclude trials with missing responses

final_data <- final_data %>%
  filter(!is.na(Accuracy) & Accuracy %in% c("correct", "incorrect")) %>%
  droplevels()

## Converting variables to factors and numeric variables

final_data <- final_data %>% 
  mutate(Regularity = as_factor(fct_explicit_na(Regularity, na_level = "NA")),
    Frequency = as_factor(fct_explicit_na(Frequency, na_level = "NA")),
    Condition = as_factor(fct_explicit_na(Condition, na_level = "NA")),
    Sex = as_factor(fct_explicit_na(Sex, na_level = "NA")),
    Diagnosis = as_factor(fct_explicit_na(Diagnosis, na_level = "NA")),
    Hand.Dominance = as_factor(fct_explicit_na(Hand.Dominance, na_level = "NA")),
    Bilingualism = as_factor(fct_explicit_na(Bilingualism, na_level = "NA")),
    Foreign.Languages = as_factor(fct_explicit_na(Foreign.Languages, na_level = "NA")),
    Hearing.Impairment = as_factor(fct_explicit_na(Hearing.Impairment, na_level = "NA")),
    Regularity = as_factor(fct_explicit_na(Regularity, na_level = "NA")),
    Location = as_factor(fct_explicit_na(Location, na_level = "NA")),
    Group = as_factor(fct_explicit_na(Group, na_level = "NA")),
    Count = as.numeric(as.character(Count)),
    Accuracy = factor(Accuracy, levels = c("incorrect", "correct")))

## Sum code categorical predictors

contrasts(final_data$Hearing.Impairment) <- contr.sum(nlevels(final_data$Hearing.Impairment))
contrasts(final_data$Foreign.Languages)   <- contr.sum(nlevels(final_data$Foreign.Languages))
contrasts(final_data$Hand.Dominance)      <- contr.sum(nlevels(final_data$Hand.Dominance))
contrasts(final_data$Sex)                 <- contr.sum(nlevels(final_data$Sex))
contrasts(final_data$Group)               <- contr.sum(nlevels(final_data$Group))
contrasts(final_data$Regularity)          <- contr.sum(nlevels(final_data$Regularity))

## Combine MMSE, DSF and DSB in the same variable and z-score them

final_data <- final_data %>%
  mutate(CognitiveComposite_raw = (MMSE + DSF + DSB) / 3,
         CognitiveComposite_c = CognitiveComposite_raw - mean(CognitiveComposite_raw, na.rm = TRUE),
         CognitiveComposite_z = CognitiveComposite_c / sd(CognitiveComposite_c, na.rm = TRUE))

## Z-score and sum-code variables 

final_data <- final_data %>%
  mutate(Age_c = Age - mean(Age, na.rm = TRUE),
         Age_z = Age_c / sd(Age_c, na.rm = TRUE),
         Years.of.Education_c = Years.of.Education - mean(Years.of.Education, na.rm = TRUE),
         Years.of.Education_z = Years.of.Education_c / sd(Years.of.Education_c, na.rm = TRUE))

## Check data distribution in Count

plot(density(final_data$Count),
     main = "Density of Count",
     xlab = "Count")

## Log-transform and z-score Count

final_data$Count_log <- log1p(final_data$Count)
final_data <- final_data %>%
  mutate(Count_log_c = Count_log - mean(Count_log, na.rm = TRUE),
         Count_log_z = Count_log_c / sd(Count_log_c, na.rm = TRUE))

## Compare log-transformed and z-scored distributions

plot(density(final_data$Count_log),
     main = "Density of Count",
     xlab = "Count")

plot(density(final_data$Count_log_z),
     main = "Density of Count",
     xlab = "Count")

## Check distribution after log-transform and z-score

ggplot(final_data, aes(x = Count_log_z)) +
  geom_histogram(bins = 30) +
  geom_density(color = "blue", linewidth = 1) +
  theme_minimal()

## Save final dataset 

write.csv(final_data, "final_data.csv", row.names = FALSE)

# Descriptive Statistics 

## Table with cognitive measures, age and education

descriptive_table <- final_data %>%
  group_by(Group) %>%
  summarise(
    MMSE_Mean_SD = paste0(round(mean(MMSE, na.rm = TRUE), 1), " (", round(sd(MMSE, na.rm = TRUE), 1), ")"),
    MMSE_Min = min(MMSE, na.rm = TRUE),
    MMSE_Max = max(MMSE, na.rm = TRUE),
    
    DSF_Mean_SD = paste0(round(mean(DSF, na.rm = TRUE), 1), " (", round(sd(DSF, na.rm = TRUE), 1), ")"),
    DSF_Min = min(DSF, na.rm = TRUE),
    DSF_Max = max(DSF, na.rm = TRUE),
    
    DSB_Mean_SD = paste0(round(mean(DSB, na.rm = TRUE), 1), " (", round(sd(DSB, na.rm = TRUE), 1), ")"),
    DSB_Min = min(DSB, na.rm = TRUE),
    DSB_Max = max(DSB, na.rm = TRUE),
    
    CognitiveComposite_raw_Mean_SD = paste0(round(mean(CognitiveComposite_raw, na.rm = TRUE), 1), 
                                            " (", round(sd(CognitiveComposite_raw, na.rm = TRUE), 1), ")"),
    CognitiveComposite_raw_Min = min(CognitiveComposite_raw, na.rm = TRUE),
    CognitiveComposite_raw_Max = max(CognitiveComposite_raw, na.rm = TRUE),
    Education_Mean_SD = paste0(round(mean(Years.of.Education, na.rm = TRUE), 1), " (", 
                               round(sd(Years.of.Education, na.rm = TRUE), 1), ")"),
    Education_Min = min(Years.of.Education, na.rm = TRUE),
    Education_Max = max(Years.of.Education, na.rm = TRUE),
    Age_Mean_SD = paste0(round(mean(Age, na.rm = TRUE), 1), " (", 
                         round(sd(Age, na.rm = TRUE), 1), ")"),
    Age_Min = min(Age, na.rm = TRUE),
    Age_Max = max(Age, na.rm = TRUE)) %>%
  tidyr::pivot_longer(
    cols = -Group,
    names_to = c("Measure", ".value"),
    names_pattern = "(.*)_(Mean_SD|Min|Max)") %>%
  rename(`Mean (SD)` = Mean_SD)

kbl(descriptive_table, caption = "Descriptive statistics for cognitive measures by group")

descriptive_table

# Inferential Statistics

## Model 1: Can screening test values from MMSE, DSF, and DSB predict diagnosis?

final_data <- final_data %>%
  mutate(Diagnosis_binary = ifelse(Diagnosis == "no", "no_diagnosis", "diagnosis")) %>%
  mutate(Diagnosis_binary = factor(Diagnosis_binary, levels = c("no_diagnosis", "diagnosis")))

contrasts(final_data$Diagnosis_binary)


model_1 <- glm(Diagnosis_binary ~ MMSE + DSF + DSB,
                  data = final_data,
                  family = binomial)

summary(model_1)

check_collinearity(model_1)

## Compute correlation matrix

cor(final_data[, c("MMSE", "DSF", "DSB")], use = "pairwise.complete.obs")

## Run three separate models for each of the measurements

model_MMSE <- glm(Diagnosis_binary ~ MMSE, 
                  data=final_data, 
                  family=binomial)

summary(model_MMSE)

model_DSF  <- glm(Diagnosis_binary ~ DSF,  
                  data=final_data, 
                  family=binomial)

summary(model_DSF)

model_DSB  <- glm(Diagnosis_binary ~ DSB,  
                  data=final_data, 
                  family=binomial)

summary(model_DSB)

## Model-comparison criteria AIC() and BIC().

BIC(model_MMSE, model_DSB, model_DSF)
AIC(model_MMSE, model_DSB, model_DSF)

## Bonferroni Correction

p.adjust(c(summary(model_MMSE)$coefficients[,4],
           summary(model_DSB)$coefficients[,4]),
         method = "bonferroni")

## Compare models using R²

library(pscl)

pR2(model_1)

pR2(model_MMSE)
pR2(model_DSF)
pR2(model_DSB)

## Logistic generalized linear mixed-effects model

model_main <- glmer(Accuracy ~ CognitiveComposite_z *
                     (Count_log_z * Regularity +
                        Foreign.Languages +
                        Hearing.Impairment +
                        Years.of.Education_z +
                        Sex + Age_z) +
                     (1 | Participant.ID) + 
                     (1 + CognitiveComposite_z + Age_z | Token),
                   data = final_data,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))

## Find optimiser 

#allFit(model_main)

## Check variance of random effects

summary(model_main)$varcor

## Check for singular fits

lme4::isSingular(model_main)

## Check levels in Accuracy and Sex

levels(final_data$Sex)
levels(final_data$Accuracy)
contrasts(final_data$Accuracy)
levels(final_data$Regularity)
levels(final_data$Hearing.Impairment)

## Show model summary

summary(model_main)

## Check Model Assumptions 

library(DHARMa)
library(see)
library(lattice)
library(influence.ME)
library(Matrix)
library(stats)

## Collinearity
check_collinearity(model_main)

## Random effects structure
library(lme4)
library(lattice)
ranef_vals <- ranef(model_main, condVar = TRUE)
dotplot(ranef_vals)  

# Visualise model output

## Table with model summary

tab_model(model_main,
          show.est  = TRUE,
          show.se   = TRUE,
          show.stat = TRUE,
          show.p    = TRUE,
          show.ci   = FALSE,
          show.obs  = FALSE,   
          show.r2   = FALSE, 
          show.re.var = FALSE,
          pred.labels = c("(Intercept)" = "Intercept",
                          "Years.of.Education_z" = "Years of Education",
                          "CognitiveComposite_z:Foreign.Languages1" = "Cognitive Composite Score:Multilingualism",
                          "CognitiveComposite_z:Years.of.Education_z" = "Cognitive Composite Score:Years of Education",
                          "Count_log_z" = "Surface-form Frequency",
                          "CognitiveComposite_z:Hearing.Impairment1" = "Cognitive Composite Score:Hearing Impairment",
                          "Regularity1" = "Regularity",
                          "CognitiveComposite_z:Count_log_z" = "Cognitive Composite Score:Surface-form Frequency",
                          "CognitiveComposite_z:Regularity1" = "Cognitive Composite Score:Regularity",
                          "Sex1" = "Sex",
                          "CognitiveComposite_z" = "Cognitive Composite Score",
                          "Foreign.Languages1" = "Multilingualism",
                          "Hearing.Impairment1" = "Hearing Impairment",
                          "CognitiveComposite_z:Sex1" = "Cognitive Composite Score:Sex",
                          "CognitiveComposite_z:Age_z" = "Cognitive Composite Score:Age",
                          "Age_z" = "Age"),
          string.est  = "Est.",
          string.se   = "SE",
          string.stat = "z value",
          string.p    = "p value")

summ <- summary(model_main)
coefs <- as.data.frame(summ$coefficients)

## Wald CIs (log-odds)

coefs$Lower <- coefs$Estimate - 1.96 * coefs$`Std. Error`
coefs$Upper <- coefs$Estimate + 1.96 * coefs$`Std. Error`
coefs$Term  <- rownames(coefs)

## Plot log-odds

table_logodds <- ggplot(coefs, aes(x = Estimate, y = reorder(Term, Estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
  geom_point(size = 3) +
  labs(x = "Log-odds",
       y = NULL) +
  scale_y_discrete(labels = c(
    `(Intercept)` = "Intercept",
    `Years.of.Education_z` = "Years of Education",
    `CognitiveComposite_z:Foreign.Languages1` = "Cognitive Composite Score:Multilingualism",
    `CognitiveComposite_z:Years.of.Education_z` = "Cognitive Composite Score:Years of Education",
    `Count_log_z` = "Surface-form Frequency",
    `CognitiveComposite_z:Hearing.Impairment1` = "Cognitive Composite Score:Hearing Impairment",
    `Regularity1` = "Regularity",
    `CognitiveComposite_z:Count_log_z` = "Cognitive Composite Score:Surface-form Frequency",
    `CognitiveComposite_z:Regularity1` = "Cognitive Composite Score:Regularity",
    `Sex1` = "Sex",
    `Count_log_z:Regularity1` = "Surface-form Frequency:Regularity",
    `CognitiveComposite_z` = "Cognitive Composite Score",
    `Foreign.Languages1` = "Multilingualism",
    `CognitiveComposite_z:Count_log_z:Regularity1` = "Cognitive Composite Score:Surface-form Frequency:Regularity",
    `Hearing.Impairment1` = "Hearing Impairment",
    `CognitiveComposite_z:Sex1` = "Cognitive Composite Score:Sex",
    `CognitiveComposite_z:Age_z` = "Cognitive Composite Score:Age",
    `Age_z` = "Age"))
theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major.y = element_blank())

table_logodds

# Visualise significant main effects and interactions

## Main Effects

### Get predicted values for both groups

label_data <- final_data %>%
  group_by(IPA, Regularity, Token) %>% 
  summarise(Count_log_z = mean(Count_log_z, na.rm = TRUE),
            CognitiveComposite_z = mean(CognitiveComposite_z, na.rm = TRUE),
            Years.of.Education_z = mean(Years.of.Education_z, na.rm = TRUE),
            Age_z = mean(Age_z, na.rm = TRUE),
            Foreign.Languages = first(Foreign.Languages),
            Hearing.Impairment = first(Hearing.Impairment),
            Sex = first(Sex),
            Participant.ID = first(Participant.ID),
            .groups = "drop") %>%
  mutate(
    predicted = predict(
      model_main,
      newdata = .,
      type = "response",
      re.form = NA))

### Frequency predictions for both groups

frequency <- ggeffect(model_main, terms = "Count_log_z", bias_correction = TRUE)
ggplot(frequency, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.2) +
  geom_line(color = "black", size = 1) +  
  labs(x = "Surface-form Frequency", y = "Accuracy", title = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

### Get predicted values for group with dementia

selected_ids <- c(1,2,3,4,5,7,8,11,12,13,14,16,17,18,19,22,23,24,25)
label_data_dementia <- final_data %>%
  filter(Participant.ID %in% selected_ids) %>%  
  group_by(IPA, Regularity, Token) %>% 
  summarise(Count_log_z = mean(Count_log_z, na.rm = TRUE),
            CognitiveComposite_z = mean(CognitiveComposite_z, na.rm = TRUE),
            Years.of.Education_z = mean(Years.of.Education_z, na.rm = TRUE),
            Age_z = mean(Age_z, na.rm = TRUE),
            Foreign.Languages = first(Foreign.Languages),
            Hearing.Impairment = first(Hearing.Impairment),
            Sex = first(Sex),
            Participant.ID = first(Participant.ID),
            .groups = "drop") %>%
  mutate(predicted = predict(
    model_main,
    newdata = .,
    type = "response",
    re.form = NA))

### Frequency for individuals with Alzheimer's disease 

frequency_dementia <- ggeffect(model_main, terms = "Count_log_z", bias_correction = TRUE)
ggplot(frequency_dementia, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.2) +
  geom_line(color = "black", size = 1) +  
  geom_point(data = label_data_dementia,
             aes(x = Count_log_z, y = predicted, colour = Regularity),
             size = 2) +
  geom_text(data = label_data_dementia,
            aes(x = Count_log_z, y = predicted, label = IPA, colour = Regularity),
            size = 3,
            vjust = -0.5,
            inherit.aes = FALSE,
            show.legend = FALSE) + 
  scale_color_manual(values = c("regular" = "#56B4E9", "irregular" = "#D55E00"), breaks = c("regular", "irregular")) + 
  labs(x = "Surface-form Frequency", y = "Accuracy", title = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

### Age

age <- ggeffect(model_main, terms = "Age_z", bias_correction = TRUE)
ggplot(age, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.2) +
  geom_line(color = "black", size = 1) +  
  labs(x = "Age", y = "Accuracy", title = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

### Years of Education
education <- ggeffect(model_main, terms = "Years.of.Education_z", bias_correction = TRUE)
ggplot(education, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.2) +
  geom_line(color = "black", size = 1) +  
  labs(x = "Years of Education", y = "Accuracy", title = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

## Interactions

### CCS x Hearing Impairment
plot(ggeffect(model_main, c("CognitiveComposite_z", "Hearing.Impairment"), bias_correction = T))

### CCS x Multilingualism
CC_Multilingualism <- ggeffect(model_main, terms = c("CognitiveComposite_z", "Foreign.Languages"), bias_correction = TRUE)
ggplot(CC_Multilingualism, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  labs(x = "Cognitive Composite Score", y = "Accuracy", color = "Multilingualism", fill = "Multilingualism") +
  scale_color_manual(values = c("yes" = "#56B4E9", "no" = "#D55E00"),
                     breaks = c("yes", "no")) +
  scale_fill_manual(values = c("yes" = "#56B4E9", "no" = "#D55E00"),
                    breaks = c("yes", "no")) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())

### CCS x Years of Education

CCS_Education <- ggeffect(model_main, terms = c("CognitiveComposite_z", "Years.of.Education_z"), bias_correction = TRUE)
ggplot(CC_Education, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  labs(x = "Cognitive Composite Score", y = "Accuracy", color = "Years of Education", fill = "Years of Education") +
  scale_color_manual(values = c("1" = "#56B4E9", "0" = "#F0E442", "-1" = "#D55E00"),
                     breaks = c("1", "0", "-1")) +
  scale_fill_manual(values = c("1" = "#56B4E9", "0" = "#F0E442", "-1" = "#D55E00"),
                    breaks = c("1", "0", "-1")) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())

# Further analysis and descriptive control of interactions from the main model

## FrequencyxRegularity

final_data %>%
  group_by(Regularity) %>%
  summarise(mean_frequency = mean(Count_log_z, na.rm = TRUE),
            sd_frequency   = sd(Count_log_z, na.rm = TRUE),
            n    = n_distinct(IPA))

## CCSxmultilingualism

### Number of multilinguals and monolinguals per participant group

final_data %>%
  group_by(Group, Foreign.Languages) %>%
  summarise(
    n = n_distinct(Participant.ID),
    .groups = "drop")

### Descriptive Table

final_data %>%
  group_by(Foreign.Languages) %>%
  summarise(mean_education = mean(Years.of.Education, na.rm = TRUE),
            sd_education   = sd(Years.of.Education, na.rm = TRUE),
            n    = n_distinct(Participant.ID))

### Check levels of Foreign Languages

levels(final_data$Foreign.Languages)

### Binomial Generalized Linear Model 

model_education_multilingualism <- glm(Foreign.Languages ~ Years.of.Education_z,
                                       data = final_data,
                                       family = binomial)

summary(model_education_multilingualism)

# Table with model summary

tab_model(model_education_multilingualism,
          show.est  = TRUE,
          show.se   = TRUE,
          show.stat = TRUE,
          show.p    = TRUE,
          show.ci   = FALSE,
          show.obs  = FALSE,   
          show.r2   = FALSE, 
          pred.labels = c("(Intercept)" = "Intercept",
                          "Years.of.Education_z" = "Years of Education"),
          dv.labels = "Multilingualism (yes/no)",
          string.est  = "Est.",
          string.se   = "SE",
          string.stat = "z value",
          string.p    = "p value")