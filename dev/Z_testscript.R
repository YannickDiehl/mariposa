################################################
# Test der bereinigten oneway_anova_test Funktion
# Ohne repeated measures Parameter
################################################

# Bibliotheken laden
library(tidyverse)
library(sjmisc)
library(sjlabelled)

# Lade Daten
allbus <- haven::read_sav("allbus.sav")
rm_data <- haven::read_sav("rm_data_wide.sav")

source("oneway_anova_test.R")
source("rm_anova_test.R") 
source("levene_test.R")
source("tukey_test.R")
source("t_test.R")
source("rm_t_test.R")




