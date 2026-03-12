# Complete reset for an R session, clearing all variables and unloading all packages

rm(list = ls(all = TRUE))
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T))

# Load necessary packages

library("readr")
library("dplyr")
library("stringr")
library("ggplot2")

# Read in csv files from the Leipzig Corpora Collection and merge them into a single dataset

dataset1 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/wikipedia_2021.csv", delim = ";")
dataset2 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/web_2011.csv", delim = ";")
dataset3 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/web_2015.csv", delim = ";")
dataset4 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/wikipedia_2016.csv", delim = ";")
dataset5 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/news_2019.csv", delim = ";")
dataset6 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/news_2022.csv", delim = ";")
dataset7 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/news_2021.csv", delim = ";")
dataset8 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/news_2023.csv", delim = ";")
dataset9 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/news_2024.csv", delim = ";")
dataset10 <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/news_2020.csv", delim = ";")
written_dataset <- bind_rows(dataset1, dataset2, dataset3, dataset4, dataset5, dataset6, dataset7, dataset8, dataset9, dataset10)

# Select both realisations of compound verbs in past perfective (with and without an augment)

variants <- c("επίστρεψε" = "επέστρεψε",
  "εγκατάλειψε" = "εγκατέλειψε",
  "επίτρεψε" = "επέτρεψε",
  "δούλευσε" = "δούλεψε",
  "διάλυσε" = "διέλυσε",
  "κατάστρεψε" = "κατέστρεψε",
  "ανάτειλε" = "ανέτειλε",
  "σύλλεξε" = "συνέλεξε",
  "σύνδεσε" = "συνέδεσε",
  "κατάληξε" = "κατέληξε",
  "πρόβλεψε" = "προέβλεψε",
  "κατεδίωξε" = "καταδίωξε",
  "συνέβησε" = "συνέβη",
  "συνέβηκε" = "συνέβη",
  "παρενέβηκε" = "παρενέβη",
  "επενέβησε" = "επενέβη",
  "επενέβηκε" = "επενέβη",
  "εφήυρε" = "εφηύρε",
  "προέβηκε" = "προέβη",
  "προέβησε" = "προέβη",
  "κατάπνιξε" = "κατέπνιξε")

# Select tokens from the corpora

verbs_to_keep <- c("έλυσε", "έδεσε", "άπλωσε", "έχασε", "έπεισε", "διέσχυσε", "διάσχυσε", "κέρδισε",
  "ένιωσε", "καθάρισε", "διάλυσε", "θύμισε", "άκουσε", "ίδρυσε", "ξέσπασε", "διέλυσε",
  "σύνδεσε", "συνέδεσε", "έπλεξε", "καταδίωξε", "κατεδίωξε", "έμπλεξε", "κατέπνιξε", "κατάπνιξε",
  "γνώρισε", "άνοιξε", "κατάληξε", "κατέληξε", "σύλλεξε", "συνέλεξε", "έβρεξε", "έτρεξε",
  "πρόσεξε", "έβηξε", "έσφιξε", "έφτιαξε", "έδειξε", "δίδαξε", "πρόβλεψε", "προέβλεψε",
  "εγκατέλειψε", "εγκατάλειψε", "επίτρεψε", "επέτρεψε", "δούλεψε", "δούλευσε", 
  "μαγείρευσε", "μαγείρεψε", "έκοψε", "έτριψε", "έγραψε", "επίστρεψε", "επέστρεψε",
  "κατάστρεψε", "κατέστρεψε", "έβαψε", "άλειψε", "απομάκρυνε", "έλαμψε",
  "έβλαψε", "αποκάλυψε", "ανέβηκε", "βγήκε", "βρήκε", "γλύκανε", "ανάσανε",
  "ύφανε", "λείανε", "ακρίβυνε", "σκλήρυνε", "έμεινε", "ανάτειλε", "ανέτειλε", "έμαθε",
  "πήγε", "κατάλαβε", "έτυχε", "κατέλαβε", "συνέβη", "συνέβησε", "συνέβηκε", "παρενέβη",
  "παρενέβηκε", "παρενέβησε", "προέβη", "προέβησε", "προέβηκε", "επενέβη", "επενέβησε",
  "επενέβηκε", "έφερε", "έγειρε", "έγδαρε", "πήρε", "έσυρε", "έστειλε", "έψαλε", 
  "κατάπιε", "έβγαλε", "έβαλε", "πρότεινε", "μόλυνε", "έσφαλε",
  "έτεινε", "έφθειρε", "έκανε", "ήξερε", "έφαγε", "είπε", "είδε", "ήρθε",
  "έγινε", "εφηύρε", "εφήυρε", "έπαθε", "άλεσε", "έπλυνε", "συμπέρανε", "μίκρυνε", "έκλεισε", "Έκλεισε", 
  "έριξε", "Έριξε", "Μίκρυνε", "Έπλυνε", "Συμπέρανε", "Άλεσε", "Έπαθε", "Έλυσε", "Άκουσε", "Ίδρυσε",
  "Ξέσπασε", "Έδεσε", "Άπλωσε", "Έχασε", "Έπεισε", "Διέσχυσε", "Διάσχυσε", "Κέρδισε",
  "Ένιωσε", "Καθάρισε", "Διάλυσε", "Διέλυσε", "Σύνδεσε", "Συνέδεσε", "Έπλεξε",
  "Καταδίωξε", "Κατεδίωξε", "Έμπλεξε", "Κατέπνιξε", "Κατάπνιξε", "Γνώρισε", "Άνοιξε", "Κατάληξε",
  "Κατέληξε", "Σύλλεξε", "Συνέλεξε", "Έβρεξε", "Έτρεξε", "Πρόσεξε", "Έβηξε", "Έσφιξε",
  "Έφτιαξε", "Έδειξε", "Δίδαξε", "Πρόβλεψε", "Προέβλεψε", "Εγκατέλειψε", "Εγκατάλειψε",
  "Επίτρεψε", "Επέτρεψε", "Δούλεψε", "Θύμισε", "Δούλευσε", "Μαγείρευσε", "Μαγείρεψε",
  "Έκοψε", "Έτριψε", "Έγραψε", "Επίστρεψε", "Επέστρεψε", "Κατάστρεψε", "Κατέστρεψε",
  "Έβαψε", "Άλειψε", "Παράπεμψε", "Απομάκρυνε", "Έλαμψε", "Έβλαψε", "Αποκάλυψε",
  "Ανέβηκε", "Βγήκε", "Βρήκε", "Γλύκανε", "Ανάσανε", "Ύφανε", "Λείανε",
  "Ακρίβυνε", "Σκλήρυνε", "Έμεινε", "Ανάτειλε", "Ανέτειλε", "Έμαθε", "Πήγε", "Κατάλαβε",
  "Έτυχε", "Κατέλαβε", "Συνέβη", "Συνέβησε", "Συνέβηκε", "Παρενέβη", "Παρενέβηκε",
  "Παρενέβησε", "Επενέβη", "Επενέβησε", "Επενέβηκε", "Έφερε", "Έγειρε", "Έγδαρε",
  "Πήρε", "Έσυρε", "Έστειλε", "Έψαλε", "Κατάπιε", "Έβγαλε",
  "Έβαλε", "Πρότεινε", "Μόλυνε", "Έσφαλε", "Έτεινε", "Έφθειρε", "Έκανε",
  "Ήξερε", "Έφαγε", "Είπε", "Είδε", "Ήρθε", "Έγινε", "Εφήυρε", "Εφηύρε")

# Prepare and annotate the dataset with the written data

written_dataset <- written_dataset %>%
  select(-ID) %>%
  filter(Token %in% verbs_to_keep) %>%
  mutate(Token = str_to_lower(Token),
         Token = recode(Token, !!!variants)) %>%
  group_by(Token) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  mutate(Frequency = if_else(Count <= 2000, "low frequency", "high frequency")) %>%
  mutate(Variants = case_when(
    Token == "επέστρεψε" ~ "επίστρεψε",
    Token == "εγκατέλειψε" ~ "εγκατάλειψε",
    Token == "επέτρεψε" ~ "επίτρεψε",
    Token == "δούλεψε" ~ "δούλευσε",
    Token == "διέλυσε" ~ "διάλυσε",
    Token == "κατέστρεψε" ~ "κατάστρεψε",
    Token == "ανέτειλε" ~ "ανάτειλε",
    Token == "συνέλεξε" ~ "σύλλεξε",
    Token == "συνέδεσε" ~ "σύνδεσε",
    Token == "κατέληξε" ~ "κατάληξε",
    Token == "προέβλεψε" ~ "πρόβλεψε",
    Token == "καταδίωξε" ~ "κατεδίωξε",
    Token == "συνέβη" ~ "συνέβηκε, συνέβησε",
    Token == "παρενέβη" ~ "παρενέβηκε",
    Token == "επενέβη" ~ "επενέβηκε, επενέβησε",
    Token == "προέβη" ~ "προέβηκε, προέβησε",
    TRUE ~ NA_character_),
    IPA = case_when(Token == "είπε" ~ "/ípe/",
    Token == "έγινε" ~ "/éʝine/",
    Token == "έκανε" ~ "/ékane/",
    Token == "πήρε" ~ "/píre/",
    Token == "ήρθε" ~ "/írθe/",
    Token == "έγραψε" ~ "/éɣrapse/",
    Token == "κέρδισε" ~ "/kérðise/",
    Token == "προέβη" ~ "/proévi/, /proévike/, /proévise/",
    Token == "έχασε" ~ "/éxase/",
    Token == "έπαθε" ~ "/épaθe/",
    Token == "θύμισε" ~ "/θímise/",
    Token == "άκουσε" ~ "/ákuse/",
    Token == "ίδρυσε" ~ "/íðrise/",
    Token == "ξέσπασε" ~ "/kséspase/",
    Token == "πήγε" ~ "/píʝe/",
    Token == "επέστρεψε" ~ "/epéstrepse/, /epístrepse/",
    Token == "βρήκε" ~ "/vríce/",
    Token == "έμεινε" ~ "/émine/",
    Token == "έφερε" ~ "/éfere/",
    Token == "συνέβη" ~ "/sinévi/, /sinévike/, /sinévise/",
    Token == "κατέληξε" ~ "/katélikse/, /katálikse/",
    Token == "έστειλε" ~ "/éstile/",
    Token == "έδειξε" ~ "/éðikse/",
    Token == "είδε" ~ "/íðe/",
    Token == "άνοιξε" ~ "/ánikse/",
    Token == "αποκάλυψε" ~ "/apokálipse/",
    Token == "βγήκε" ~ "/vʝíce/",
    Token == "ανέβηκε" ~ "/anévike/",
    Token == "έβγαλε" ~ "/évɣale/",
    Token == "πρότεινε" ~ "/prótine/",
    Token == "κατέλαβε" ~ "/katélave/",
    Token == "έμαθε" ~ "/émaθe/",
    Token == "ήξερε" ~ "/íksere/",
    Token == "εγκατέλειψε" ~ "/egkatélipse/, /egkatálipse/",
    Token == "επέτρεψε" ~ "/epétrepse/, /epítrepse/",
    Token == "κατάλαβε" ~ "/katálave/",
    Token == "δίδαξε" ~ "/ðíðakse/",
    Token == "έκοψε" ~ "/ékopse/",
    Token == "έτυχε" ~ "/étiçe/",
    Token == "κατέστρεψε" ~ "/katéstrepse/, /katástrepse/",
    Token == "έφτιαξε" ~ "/éftiakse/",
    Token == "έπεισε" ~ "/épise/",
    Token == "έτρεξε" ~ "/étrekse/",
    Token == "δούλεψε" ~ "/ðúlepse/, /ðúlevse/",
    Token == "ένιωσε" ~ "/éɲose/",
    Token == "διέλυσε" ~ "/ðiélise/, /ðʝálise/",
    Token == "βγήκε" ~ "/vʝíce/",
    Token == "έβαλε" ~ "/évale/",
    Token == "έφαγε" ~ "/éfaʝe/",
    Token == "προέβλεψε" ~ "/proévlepse/, /próvlepse/",
    Token == "έλυσε" ~ "/élise/",
    Token == "παρενέβη" ~ "/parenévi/, /parenévike/",
    Token == "πρόσεξε" ~ "/prósekse/",
    Token == "καθάρισε" ~ "/kaθárise/",
    Token == "συνέδεσε" ~ "/sinéðese/, /sínðese/",
    Token == "έδεσε" ~ "/éðese/",
    Token == "επενέβη" ~ "/epenévi/, /epenévik-e/, /epenévise/",
    Token == "συνέλεξε" ~ "/sinélekse/, /sílekse/",
    Token == "κατέπνιξε" ~ "/katépnikse/, /katápnikse/",
    Token == "έλαμψε" ~ "/élampse/",
    Token == "κατάπιε" ~ "/katápʝe/",
    Token == "άπλωσε" ~ "/áplose/",
    Token == "καταδίωξε" ~ "/kateðíokse/, /kataðíokse/",
    Token == "έσφιξε" ~ "/ésfikse/",
    Token == "έγειρε" ~ "/éʝire/",
    Token == "έπλεξε" ~ "/éplekse/",
    Token == "έβλαψε" ~ "/évlapse/",
    Token == "έβαψε" ~ "/évapse/",
    Token == "έμπλεξε" ~ "/éblekse/",
    Token == "έτεινε" ~ "/étine/",
    Token == "έσυρε" ~ "/ésire/",
    Token == "μαγείρεψε" ~ "/maʝírepse/",
    Token == "έβρεξε" ~ "/évrekse/",
    Token == "έψαλε" ~ "/épsale/",
    Token == "ανέτειλε" ~ "/anétile/, /anátile/",
    Token == "μόλυνε" ~ "/móline/",
    Token == "άλειψε" ~ "/álipse/",
    Token == "έσφαλε" ~ "/ésfale/",
    Token == "ανάσανε" ~ "/anásane/",
    Token == "έφθειρε" ~ "/éfθire/",
    Token == "έτριψε" ~ "/étripse/",
    Token == "έγδαρε" ~ "/éɣðare/",
    Token == "ακρίβυνε" ~ "/akrívine/",
    Token == "ύφανε" ~ "/ífane/",
    Token == "σκλήρυνε" ~ "/sklírine/",
    Token == "Γνώρισε" ~ "/ébikse/",
    Token == "γλύκανε" ~ "/ɣlíkane/",
    Token == "έβηξε" ~ "/évikse/",
    Token == "εφηύρε" ~ "/efívre/",
    Token == "λείανε" ~ "/líane/",
    Token == "άλεσε" ~ "/álese/",
    Token == "έπλυνε" ~ "/épline/",
    Token == "συμπέρανε" ~ "/sibérane/",
    Token == "μίκρυνε" ~ "/míkrine/",
    Token == "έριξε" ~ "/érikse/",
    Token == "έκλεισε" ~ "/éklise/",
    Token == "γνώρισε" ~ "/ɣnórise/",
    TRUE ~ NA_character_),
    Regularity = case_when(
    Token == "είπε" ~ "irregular",
    Token == "συμπέρανε" ~ "irregular",
    Token == "έγινε" ~ "irregular",
    Token == "έκανε" ~ "irregular",
    Token == "πήρε" ~ "irregular",
    Token == "ήρθε" ~ "irregular",
    Token == "έγραψε" ~ "regular",
    Token == "κέρδισε" ~ "regular",
    Token == "έχασε" ~ "regular",
    Token == "πήγε" ~ "irregular",
    Token == "προέβη" ~ "irregular",
    Token == "έπαθε" ~ "irregular",
    Token == "θύμισε" ~ "regular",
    Token == "άκουσε" ~ "regular",
    Token == "ίδρυσε" ~ "regular",
    Token == "ξέσπασε" ~ "regular",
    Token == "επέστρεψε" ~ "regular",
    Token == "βρήκε" ~ "irregular",
    Token == "έμεινε" ~ "irregular",
    Token == "έφερε" ~ "irregular",
    Token == "συνέβη" ~ "irregular",
    Token == "κατέληξε" ~ "regular",
    Token == "έστειλε" ~ "irregular",
    Token == "έδειξε" ~ "regular",
    Token == "είδε" ~ "irregular",
    Token == "άνοιξε" ~ "regular",
    Token == "αποκάλυψε" ~ "regular",
    Token == "βγήκε" ~ "irregular",
    Token == "ανέβηκε" ~ "irregular",
    Token == "έβγαλε" ~ "irregular",
    Token == "πρότεινε" ~ "irregular",
    Token == "κατέλαβε" ~ "irregular",
    Token == "έμαθε" ~ "irregular",
    Token == "ήξερε" ~ "irregular",
    Token == "εγκατέλειψε" ~ "regular",
    Token == "επέτρεψε" ~ "regular",
    Token == "κατάλαβε" ~ "irregular",
    Token == "δίδαξε" ~ "regular",
    Token == "έκοψε" ~ "regular",
    Token == "έτυχε" ~ "irregular",
    Token == "κατέστρεψε" ~ "regular",
    Token == "έφτιαξε" ~ "regular",
    Token == "έπεισε" ~ "regular",
    Token == "έτρεξε" ~ "regular",
    Token == "δούλεψε" ~ "regular",
    Token == "ένιωσε" ~ "regular",
    Token == "διέλυσε" ~ "regular",
    Token == "βγήκε" ~ "irregular",
    Token == "έβαλε" ~ "irregular",
    Token == "έφαγε" ~ "irregular",
    Token == "προέβλεψε" ~ "regular",
    Token == "έλυσε" ~ "regular",
    Token == "παρενέβη" ~ "irregular",
    Token == "πρόσεξε" ~ "regular",
    Token == "καθάρισε" ~ "regular",
    Token == "απομάκρυνε" ~ "irregular",
    Token == "συνέδεσε" ~ "regular",
    Token == "έδεσε" ~ "regular",
    Token == "επενέβη" ~ "irregular",
    Token == "συνέλεξε" ~ "regular",
    Token == "κατέπνιξε" ~ "regular",
    Token == "έλαμψε" ~ "regular",
    Token == "κατάπιε" ~ "irregular",
    Token == "άπλωσε" ~ "regular",
    Token == "καταδίωξε" ~ "regular",
    Token == "έσφιξε" ~ "regular",
    Token == "έγειρε" ~ "irregular",
    Token == "έπλεξε" ~ "regular",
    Token == "έβλαψε" ~ "regular",
    Token == "έβαψε" ~ "regular",
    Token == "έμπλεξε" ~ "regular",
    Token == "έτεινε" ~ "irregular",
    Token == "έσυρε" ~ "irregular",
    Token == "μαγείρεψε" ~ "regular",
    Token == "έβρεξε" ~ "regular",
    Token == "έψαλε" ~ "irregular",
    Token == "ανέτειλε" ~ "irregular",
    Token == "μόλυνε" ~ "irregular",
    Token == "άλειψε" ~ "regular",
    Token == "έσφαλε" ~ "irregular",
    Token == "ανάσανε" ~ "irregular",
    Token == "έφθειρε" ~ "irregular",
    Token == "έτριψε" ~ "regular",
    Token == "έγδαρε" ~ "irregular",
    Token == "ακρίβυνε" ~ "irregular",
    Token == "ύφανε" ~ "irregular",
    Token == "σκλήρυνε" ~ "irregular",
    Token == "γνώρισε" ~ "regular",
    Token == "γλύκανε" ~ "irregular",
    Token == "έβηξε" ~ "regular",
    Token == "εφηύρε" ~ "irregular",
    Token == "λείανε" ~ "irregular",
    Token == "άλεσε" ~ "regular",
    Token == "έπλυνε" ~ "irregular",
    Token == "κατένειμε" ~ "irregular",
    Token == "μίκρυνε" ~ "irregular",
    Token == "έριξε" ~ "regular",
    Token == "έκλεισε" ~ "regular",
    TRUE ~ NA_character_))

# Check descriptively if the dataset with the written data is balanced 

written_dataset %>%
  count(Regularity, Frequency)

# Read in csv files from the Corpus of Spoken Greek 

spoken_dataset <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/spoken_dataset.csv", delim = ";")

# Prepare and annotate the dataset with the spoken data

spoken_dataset <- spoken_dataset %>%
  select(-ID) %>%
  filter(Token %in% verbs_to_keep) %>%
  mutate(Token = str_to_lower(Token),
         Token = recode(Token, !!!variants)) %>%
  group_by(Token) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  mutate(Frequency = if_else(Count <= 900, "low frequency", "high frequency")) %>%
  mutate(Regularity = case_when(Token == "είπε" ~ "irregular",
      Token == "έγινε" ~ "irregular",
      Token == "έκανε" ~ "irregular",
      Token == "πήρε" ~ "irregular",
      Token == "ήρθε" ~ "irregular",
      Token == "έγραψε" ~ "regular",
      Token == "κέρδισε" ~ "regular",
      Token == "έχασε" ~ "regular",
      Token == "πήγε" ~ "irregular",
      Token == "προέβη" ~ "irregular",
      Token == "συμπέρανε" ~ "irregular",
      Token == "έπαθε" ~ "irregular",
      Token == "θύμισε" ~ "regular",
      Token == "άκουσε" ~ "regular",
      Token == "ίδρυσε" ~ "regular",
      Token == "ξέσπασε" ~ "regular",
      Token == "επέστρεψε" ~ "regular",
      Token == "βρήκε" ~ "irregular",
      Token == "έμεινε" ~ "irregular",
      Token == "έφερε" ~ "irregular",
      Token == "συνέβη" ~ "irregular",
      Token == "κατέληξε" ~ "regular",
      Token == "έστειλε" ~ "irregular",
      Token == "έδειξε" ~ "regular",
      Token == "είδε" ~ "irregular",
      Token == "άνοιξε" ~ "regular",
      Token == "αποκάλυψε" ~ "regular",
      Token == "βγήκε" ~ "irregular",
      Token == "ανέβηκε" ~ "irregular",
      Token == "έβγαλε" ~ "irregular",
      Token == "πρότεινε" ~ "irregular",
      Token == "κατέλαβε" ~ "irregular",
      Token == "έμαθε" ~ "irregular",
      Token == "ήξερε" ~ "irregular",
      Token == "εγκατέλειψε" ~ "regular",
      Token == "επέτρεψε" ~ "regular",
      Token == "κατάλαβε" ~ "irregular",
      Token == "δίδαξε" ~ "regular",
      Token == "έκοψε" ~ "regular",
      Token == "έτυχε" ~ "irregular",
      Token == "κατέστρεψε" ~ "regular",
      Token == "έφτιαξε" ~ "regular",
      Token == "έπεισε" ~ "regular",
      Token == "έτρεξε" ~ "regular",
      Token == "δούλεψε" ~ "regular",
      Token == "ένιωσε" ~ "regular",
      Token == "διέλυσε" ~ "regular",
      Token == "βγήκε" ~ "irregular",
      Token == "έβαλε" ~ "irregular",
      Token == "έφαγε" ~ "irregular",
      Token == "προέβλεψε" ~ "regular",
      Token == "έλυσε" ~ "regular",
      Token == "παρενέβη" ~ "irregular",
      Token == "πρόσεξε" ~ "regular",
      Token == "καθάρισε" ~ "regular",
      Token == "απομάκρυνε" ~ "irregular",
      Token == "συνέδεσε" ~ "regular",
      Token == "έδεσε" ~ "regular",
      Token == "επενέβη" ~ "irregular",
      Token == "συνέλεξε" ~ "regular",
      Token == "κατέπνιξε" ~ "regular",
      Token == "έλαμψε" ~ "regular",
      Token == "κατάπιε" ~ "irregular",
      Token == "άπλωσε" ~ "regular",
      Token == "καταδίωξε" ~ "regular",
      Token == "έσφιξε" ~ "regular",
      Token == "έγειρε" ~ "irregular",
      Token == "έπλεξε" ~ "regular",
      Token == "έβλαψε" ~ "regular",
      Token == "έβαψε" ~ "regular",
      Token == "έμπλεξε" ~ "regular",
      Token == "έτεινε" ~ "irregular",
      Token == "έσυρε" ~ "irregular",
      Token == "μαγείρεψε" ~ "regular",
      Token == "έβρεξε" ~ "regular",
      Token == "έψαλε" ~ "irregular",
      Token == "ανέτειλε" ~ "irregular",
      Token == "μόλυνε" ~ "irregular",
      Token == "άλειψε" ~ "regular",
      Token == "έσφαλε" ~ "irregular",
      Token == "ανάσανε" ~ "irregular",
      Token == "έφθειρε" ~ "irregular",
      Token == "έτριψε" ~ "regular",
      Token == "έγδαρε" ~ "irregular",
      Token == "ακρίβυνε" ~ "irregular",
      Token == "ύφανε" ~ "irregular",
      Token == "σκλήρυνε" ~ "irregular",
      Token == "γνώρισε" ~ "regular",
      Token == "γλύκανε" ~ "irregular",
      Token == "έβηξε" ~ "regular",
      Token == "εφηύρε" ~ "irregular",
      Token == "λείανε" ~ "irregular",
      Token == "άλεσε" ~ "regular",
      Token == "έπλυνε" ~ "irregular",
      Token == "κατένειμε" ~ "irregular",
      Token == "μίκρυνε" ~ "irregular",
      Token == "έριξε" ~ "regular",
      Token == "έκλεισε" ~ "regular",
      TRUE ~ NA_character_))

# Check descriptively if dataset with the spoken data is balanced 

spoken_dataset %>%
  count(Regularity, Frequency)

# Combine the written with the spoken dataset

dataset <- bind_rows(written_dataset, spoken_dataset)

# Group datasets according to the verb form

dataset <- dataset %>%
  group_by(Token) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Attach columns 'Variants', 'IPA' and 'Regularity' in the dataset and create the categorical condition of 'Frequency'

dataset <- left_join(dataset, written_dataset %>% 
           select(Token, Variants, IPA, Regularity), by = "Token") %>% 
           mutate(Frequency = if_else(Count <= 1600, "low frequency", "high frequency"))

# Check descriptively if the dataset is balanced 

dataset %>%
  count(Regularity, Frequency) 

# Create column 'Sentence' containing the experimental items

dataset <- dataset %>% 
  mutate(Sentence = case_when(Token == "είπε" ~ "Η Μαρία μου είπε την αλήθεια χθες.",
    Token == "έγινε" ~ "Το ατύχημα έγινε στον αυτοκινητόδρομο.",
    Token == "έκανε" ~ "Ο Νίκος έκανε το καθήκον του με υπευθυνότητα.",
    Token == "πήρε" ~ "Η Άννα πήρε το λεωφορείο από την πλατεία.",
    Token == "ήρθε" ~ "Ο παππούς ήρθε στο σπίτι το απόγευμα.",
    Token == "έγραψε" ~ "Ο μαθητής έγραψε την έκθεση με προσοχή.",
    Token == "κέρδισε" ~ "Η ομάδα μας κέρδισε τον αγώνα με διαφορά.",
    Token == "έχασε" ~ "Ο Πέτρος έχασε το πορτοφόλι του στο λεωφορείο.",
    Token == "πήγε" ~ "Η Ελένη πήγε στον γιατρό το μεσημέρι.",
    Token == "προέβη" ~ "Ο υπουργός προέβη σε δηλώσεις μετά τη συνάντηση.",
    Token == "έπαθε" ~ "Η Μαρία έπαθε σοκ όταν είδε το αποτέλεσμα.",
    Token == "θύμισε" ~ "Η μουσική θύμισε στον Γιώργο τα παιδικά του χρόνια.",
    Token == "άκουσε" ~ "Ο φοιτητής άκουσε τη διάλεξη με προσοχή.",
    Token == "ίδρυσε" ~ "Η εταιρεία ίδρυσε νέο υποκατάστημα στη Θεσσαλονίκη.",
    Token == "ξέσπασε" ~ "Η φωτιά ξέσπασε στο δάσος σήμερα το βράδυ.",
    Token == "βρήκε" ~ "Ο αστυνομικός βρήκε τα στοιχεία κάτω από το τραπέζι.",
    Token == "έμεινε" ~ "Η Λένα έμεινε στο ξενοδοχείο για δύο βράδια",
    Token == "έφερε" ~ "Ο ταχυδρόμος έφερε το γράμμα νωρίς το πρωί.",
    Token == "επέστρεψε" ~ "Ο μαθητής επέστρεψε στην τάξη μετά το διάλειμμα.",
    Token == "συνέβη" ~ "Το περιστατικό συνέβη στην κεντρική πλατεία χθες.",
    Token == "κατέληξε" ~ "Ο ασθενής κατέληξε στο νοσοκομείο μετά από επιπλοκές.",
    Token == "έστειλε" ~ "Η δασκάλα έστειλε μήνυμα στους μαθητές της.",
    Token == "έδειξε" ~ "Ο οδηγός έδειξε την άδεια κυκλοφορίας στον αστυνομικό.",
    Token == "είδε" ~ "Η Ελένη είδε την ταινία στο σινεμά με την παρέα της.",
    Token == "άνοιξε" ~ "Ο Μανώλης άνοιξε το παράθυρο της κουζίνας.",
    Token == "αποκάλυψε" ~ "Ο δημοσιογράφος αποκάλυψε το σκάνδαλο σε συνέντευξη.",
    Token == "βγήκε" ~ "Μια κοπέλα βγήκε από το κατάστημα αμέσως.",
    Token == "ανέβηκε" ~ "Ένας τουρίστας ανέβηκε το βουνό με κόπο.",
    Token == "έβγαλε" ~ "Ο Γιάννης έβγαλε φωτογραφίες στο πάρκο.",
    Token == "πρότεινε" ~ "Ο δάσκαλος πρότεινε ένα νέο σχέδιο για την τάξη.",
    Token == "κατέλαβε" ~ "Ο στρατός κατέλαβε την πόλη μετά από μάχη.",
    Token == "έμαθε" ~ "Η Μαρία έμαθε τα νέα από την τηλεόραση.",
    Token == "ήξερε" ~ "Ο Αντώνης ήξερε την απάντηση από πριν.",
    Token == "εγκατέλειψε" ~ "Η οικογένεια εγκατέλειψε το σπίτι λόγω κινδύνου.",
    Token == "επέτρεψε" ~ "Ο καθηγητής επέτρεψε την είσοδο στους φοιτητές.",
    Token == "κατάλαβε" ~ "Η Σοφία κατάλαβε το πρόβλημα αμέσως.",
    Token == "δίδαξε" ~ "Η δασκάλα δίδαξε τα παιδιά με ενθουσιασμό.",
    Token == "έκοψε" ~ "Ο μάγειρας έκοψε τα λαχανικά με μαχαίρι.",
    Token == "έτυχε" ~ "Ο Δημήτρης έτυχε το μπλε μπαλόνι στη μοιρασιά.",
    Token == "κατέστρεψε" ~ "Η καταιγίδα κατέστρεψε την καλλιέργεια τη νύχτα.",
    Token == "έφτιαξε" ~ "Η Μαίρη έφτιαξε γλυκό με κεράσι.",
    Token == "έπεισε" ~ "Ο δικηγόρος έπεισε τον δικαστή με τα επιχειρήματά του.",
    Token == "έτρεξε" ~ "Ο αθλητής έτρεξε τον αγώνα σε χρόνο ρεκόρ.",
    Token == "δούλεψε" ~ "Ο πατέρας δούλεψε σε αυτό το εργοστάσιο για χρόνια.",
    Token == "ένιωσε" ~ "Ο μαθητής ένιωσε χαρά μετά την επιτυχία.",
    Token == "διέλυσε" ~ "Η ζέστη διέλυσε τον πάγο στο ποτήρι.",
    Token == "βγήκε" ~ "Η κοπέλα βγήκε από το κατάστημα με χαμόγελο.",
    Token == "έβαλε" ~ "Η Μαρία έβαλε το βιβλίο στο ράφι.",
    Token == "έφαγε" ~ "Ο Νίκος έφαγε το γλυκό με όρεξη.",
    Token == "προέβλεψε" ~ "Ο μετεωρολόγος προέβλεψε την χθεσινή καταιγίδα.",
    Token == "έλυσε" ~ "Ο μαθητής έλυσε την άσκηση σε λίγα λεπτά.",
    Token == "παρενέβη" ~ "Ο πρόεδρος παρενέβη στη συζήτηση για τον πόλεμο.",
    Token == "πρόσεξε" ~ "Η Μαρία πρόσεξε το παιδί σε όλο το ταξίδι.",
    Token == "καθάρισε" ~ "Ο Πέτρος καθάρισε το δωμάτιο προσεκτικά.",
    Token == "απομάκρυνε" ~ "Ο αστυνομικός απομάκρυνε το πλήθος από τον δρόμο.",
    Token == "συνέδεσε" ~ "Ο τεχνικός συνέδεσε τον εκτυπωτή με το δίκτυο.",
    Token == "έδεσε" ~ "Ο ναύτης έδεσε το πλοίο στο λιμάνι.",
    Token == "επενέβη" ~ "Ο υπουργός επενέβη στο ζήτημα άμεσα.",
    Token == "συνέλεξε" ~ "Η Ελένη συνέλεξε τα δεδομένα για την έρευνά της.",
    Token == "κατέπνιξε" ~ "Ο αστυνομικός κατέπνιξε την εξέγερση πάραυτα.",
    Token == "έλαμψε" ~ "Ο ήλιος έλαμψε μετά από μέρες βροχής.",
    Token == "κατάπιε" ~ "Το παιδί κατάπιε το φαγητό πολύ γρήγορα.",
    Token == "άπλωσε" ~ "Η γιαγιά άπλωσε τα ρούχα στο μπαλκόνι.",
    Token == "καταδίωξε" ~ "Η αστυνομία καταδίωξε τον δράστη μέσα στην πόλη.",
    Token == "έσφιξε" ~ "Ο άντρας έσφιξε το χέρι του φίλου του με δύναμη.",
    Token == "έγειρε" ~ "Η γέφυρα έγειρε από το βάρος των φορτηγών.",
    Token == "έπλεξε" ~ "Η γιαγιά έπλεξε το κασκόλ για την εγγονή της.",
    Token == "έβλαψε" ~ "Η ξηρασία έβλαψε τις καλλιέργειες της περιοχής.",
    Token == "έβαψε" ~ "Ο ζωγράφος έβαψε τον φράχτη πράσινο.",
    Token == "έμπλεξε" ~ "Ο μαθητής έμπλεξε σε φασαρία στο διάλειμμα.",
    Token == "έτεινε" ~ "Η ερευνήτρια έτεινε προς τη θεωρία της εξέλιξης.",
    Token == "έσυρε" ~ "Ο άντρας έσυρε την καρέκλα κοντά στο τζάκι.",
    Token == "μαγείρεψε" ~ "Η μαμά μαγείρεψε σούπα για βράδυ.",
    Token == "έβρεξε" ~ "Χθες το βράδυ έβρεξε για λίγη ώρα.",
    Token == "έψαλε" ~ "Ο μοναχός έψαλε τον εσπερινό δυνατά.",
    Token == "ανέτειλε" ~ "Ο ήλιος ανέτειλε πίσω από το βουνό.",
    Token == "μόλυνε" ~ "Το εργοστάσιο μόλυνε το ποτάμι με τα απόβλητα.",
    Token == "άλειψε" ~ "Η μαμά άλειψε το ψωμί με μέλι.",
    Token == "έσφαλε" ~ "Ο δικαστής έσφαλε στην ερμηνεία του νόμου.",
    Token == "ανάσανε" ~ "Ο ορειβάτης ανάσανε βαθιά μετά την ανάβαση.",
    Token == "έφθειρε" ~ "Ο ήλιος έφθειρε το ύφασμα με τον χρόνο.",
    Token == "έτριψε" ~ "Η μαμά έτριψε την κατσαρόλα μέχρι να καθαρίσει.",
    Token == "έγδαρε" ~ "Ο κυνηγός έγδαρε το ζώο με μαχαίρι.",
    Token == "ακρίβυνε" ~ "Το ενοίκιο ακρίβυνε λόγω του πληθωρισμού.",
    Token == "ύφανε" ~ "Η γιαγιά ύφανε το χαλί στον αργαλειό.",
    Token == "σκλήρυνε" ~ "Το κρύο σκλήρυνε το χώμα στην αυλή.",
    Token == "γνώρισε" ~ "Ο Κώστας γνώρισε τον συγγραφέα στην έκθεση βιβλίου.",
    Token == "γλύκανε" ~ "Η καραμέλα γλύκανε τον καφέ ελαφρά.",
    Token == "έβηξε" ~ "Ο ασθενής έβηξε δυνατά μετά την εξέταση.",
    Token == "εφηύρε" ~ "Ο επιστήμονας εφηύρε νέα συσκευή για την εξοικονόμηση ενέργειας.",
    Token == "λείανε" ~ "Ο τεχνίτης λείανε την επιφάνεια με γυαλόχαρτο.",
    Token == "άλεσε" ~ "Ο φούρναρης άλεσε το σιτάρι για το ψωμί.",
    Token == "έπλυνε" ~ "Η Άννα έπλυνε τα πιάτα με ζεστό νερό.",
    Token == "κατένειμε" ~ "Η επιτροπή κατένειμε τα κονδύλια ισότιμα σε όλα τα τμήματα.",
    Token == "μίκρυνε" ~ "Το ρούχο μίκρυνε στο πλύσιμο από το ζεστό νερό.",
    Token == "έριξε" ~ "Ο Πέτρος έριξε το μπαλάκι στο καλάθι.",
    Token == "έκλεισε" ~ "Η δασκάλα έκλεισε το βιβλίο μετά το μάθημα.",
    Token == "συμπέρανε" ~ "Η μελέτη συμπέρανε τη σημασία της σωστής μεθοδολογίας.",
    TRUE ~ NA_character_))

dataset <- dataset %>%
  mutate(`Trial Type` = "experimental item")

# Read in csv file containing the filler sentences

fillers <- read_delim("C:/Users/myria/Downloads/Thesis/Experiment/fillers.csv", delim = ";")

# Combine filler sentences with the dataset

full_dataset <- bind_rows(dataset, fillers)

# Save the final dataset

write_csv(full_dataset, "C:/Users/myria/Downloads/Thesis/Experiment/dataset.csv")

