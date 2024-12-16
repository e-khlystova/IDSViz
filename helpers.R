library(tidyverse)
library(readxl)
library(tibble)
library(forcats)
library(tidytext)
library(ggpol)

`%notin%` <- Negate(`%in%`)

hz <- function(x) {
  
  # The adjustment for very low or very high measurements
  x[x < 2 & !is.na(x)] <- (x[x < 2 & !is.na(x)] - 0.3) / 0.85
  x[x > 20.1 & !is.na(x)] <- (x[x > 20.1 & !is.na(x)] + 4.422) / 1.22
  
  # The main function
  hz <- (1960 * (x + 0.53)) / (26.28 - x)
  
  return(hz)
}

englishVowels <- read_csv("eng_ids_bark.csv") 

sd_engvow <- sd(englishVowels$duration)
mn_engvow <- mean(englishVowels$duration)

englishVowels <- englishVowels %>% 
  filter(duration > mn_engvow - 2*sd_engvow) %>% 
  filter(duration < mn_engvow + 2*sd_engvow) %>% 
  mutate(f1 = hz(f1)) %>% 
  mutate(f2 = hz(f2)) %>% 
  mutate(f3 = hz(f3)) %>% 
  mutate(language = "eng") %>% 
  mutate(spanish_speaker = NA) %>% 
  mutate(speaker = english_speaker) %>% 
  select(vowel, f1, f2, f3, duration, speaker)




#TO DO: UPDATE AND SMOOTH WITH SPANISH VOWEL CSV FULL DATA SET 
spanishVowels <- read_csv("sp_ids.csv")



sd_spvow <- sd(spanishVowels$duration_ms)
mn_spvow <- mean(spanishVowels$duration_ms)

spanishVowels <- spanishVowels %>% 
  filter(duration_ms > mn_spvow - 2*sd_spvow) %>% 
  filter(duration_ms < mn_spvow + 2*sd_spvow) %>% 
  mutate(duration = duration_ms) %>% 
  mutate(language = "span") %>% 
  mutate(english_speaker = NA) %>% 
  mutate(speaker = spk) %>% 
  select(vowel, f1, f2, f3, duration, speaker)




x_sampa_vowels <- c(
  "i0", "I2", "i1", "@0", "A1", "eI1", "aI1", "oU1", "aU1", "OI1", "oU0",
  "O2", "I", "OU1", "aI2", "oU2", "eI2", "E1", "aI1", "E2", "oU", "@`1",
  "aU", "aI", "e1I", "E'", "INS", "I1", "O0", "{0", ">1", "}1", "aI0", "E0", "I0", "U1", "O1", "E`1", 
  "@`0", "3`1", "@", "A2", "a1", "eI", "EI1", "A0", "i", "{", "AU1", "{2", "u2", "O", "<1", ">0", "A",
  "@1", "U", "@I1", "E'1", "{U1", "{u1", "e2", "@'", "aU2", "@'1", "<2", "o2", "A'1", ">'1", "U2", "E'2",
  "u'1", "E1'", "O'1", "U0", "e0", ">@", "II", "o", "E", "{1", "u1", "u", "u0", "@'0", "@`", ">2", "OU0", 
  "oI1", "i2", "AI0", "EH1", "E`", "AI1", "OW0", "E`0", "E'0", "oW1", "3`", "o0", "o1", "e1", "eI0", "@2", 
  "u1}", "aII", ">", "V1"
)

englishIDS <- readRDS("EnglishIDSdata.rds") %>% 
  filter(position %in% c("I", "M", "F")) %>% 
  #filter(coding != "vow") %>% 
  filter(ur %notin% x_sampa_vowels) %>% 
  group_by(ur) %>% 
  filter(n()>=5) %>% 
  ungroup()


englishIDS$preceding <- gsub("}", "", englishIDS$preceding)
englishIDS$following <- gsub("}", "", englishIDS$following)
englishIDS$preceding <- gsub("?", "", englishIDS$preceding)
englishIDS$following <- gsub("}", "", englishIDS$following)

englishIDS$pre_vowel <- str_detect(englishIDS$preceding, "V")
englishIDS$pre_stress <- str_detect(englishIDS$preceding, "'V") | str_detect(englishIDS$following, '"V')

englishIDS$post_vowel <- str_detect(englishIDS$following, "V")
englishIDS$post_stress <- str_detect(englishIDS$following, "'V") | str_detect(englishIDS$following, '"V')


englishIDS <- englishIDS %>% 
  mutate(before = case_when(
    (pre_vowel == T) & (pre_stress == T) ~ "Stressed vowel",
    (pre_vowel == T) & (pre_stress == F) ~ "Unstressed vowel",
    preceding == "#" ~ "Utterance boundary",
    T ~ preceding)) %>% 
  mutate(after = case_when(
    (post_vowel == T) & (post_stress == T) ~ "Stressed vowel",
    (post_vowel == T) & (post_stress == F) ~ "Unstressed vowel",
    following == "#" ~ "Utterance boundary",
    T ~ following)
    )

