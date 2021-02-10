# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

library(data.table)
library(stringr)
library(car)

################################################################################
### UPDATE LANGUAGE AND OUTPUT FOR N
################################################################################

#LANGUAGE <- "English"
LANGUAGE <- "French"
DATASET_VERSION <- "2020-04-16"

filtered <- fread(paste0("data/", DATASET_VERSION, "/", LANGUAGE, "_filtered.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
filtered[filtered == ""] <- NA  # treat empty strings as NA
filtered$AtLeastOneOpenResponse <- as.integer(filtered$AtLeastOneOpenResponse) # recode flag for open responses to 0 and 1
n <- nrow(filtered)
n
# 83

# save timestamps (column name may be language-specific)
timestamps <- filtered$Horodateur
filtered$Horodateur <- NULL

# function definitions (no need to change)
match_str <- function(search_str) {
  as.logical(unlist(sapply(filtered[,1], function(input) {
    !is.na(str_match(input, search_str))
  })))
}
recode_column <- function(mapping, asCharacter=FALSE) {
  if (asCharacter == TRUE) {
    as.character(car::recode(unlist(filtered[,1]), mapping)) 
  } else {
    as.integer(car::recode(unlist(filtered[,1]), mapping)) 
  }
}
remove_column <- function() {
  subset(filtered, select=2:ncol(filtered))
}
keep_column <- function() {
  unlist(filtered[,1])
}
print_column_name <- function() {
  names(filtered)[1]
}
print_unique_responses <- function() {
  unique(filtered[,1])
}
print_remaining_responses <- function(filter) {
  unique(filtered[filter,1])
}

################################################################################
### UPDATE THE FOLLOWING MAPPINGS INCLUDING QUESTION 3
################################################################################

# Please translate the following strings:
# The function match_str matches substrings, so it's enough to provide unique substrings to match the different cases.
map_agreement_helpful <- function() {
  result <- as.character(rep(NA, n))
  #true <- match_str('true')
  true <- match_str('applique')
  #helpful <- match_str('helpful')
  helpful <- match_str('utile')
  result[true & helpful] <- "both"
  result[true & !helpful] <- "yes"
  result[!true & helpful] <- "helpful"
  result
}

# Please adapt/translate the following regular expressions to match developer end engineering roles:
# You can use print_unique_responses() and print_remaining_responses(filter) to derive this regex.
is_developer <- function() {
  developer <- match_str(".*[Dd]eveloper.*")
  engineer <- match_str(".*[Ee]ngineer.*")
  result <- developer | engineer
  result
}

# Please adapt/translate the following regular expressions to match full time workers:
# You can use print_unique_responses() and print_remaining_responses(filter) to derive this regex.
is_fulltime <- function() {
  #result <- match_str(".*[Ff]ull time.*")
  result <- match_str(".*[Tt]emps plein.*")
  result
}

# Please translate the following mappings:
# You have to escape parentheses when using match_str, because it expects a regular expression
# You have to escape single and double quotes when using recode_column
# covid_response_mapping <- "'I am not leaving home at all, for any reason.'=3; 'I am leaving home only for essential tasks (e.g. groceries, medical care, exercise)'=2; 'I am leaving home regularly for work or pleasure. I am keeping my distance from others.'=1; 'I am leaving home regularly. I am NOT keeping my distance from others.'=0; else=NA"
covid_response_mapping <- "'Je ne quitte pas du tout la maison, quelque soit la raison.'=3; 'Je ne quitte la maison que pour des tâches essentielles (par exemple, faire les courses, aller chez le médecin/pharmacien, faire de l\\'exercice)'=2; 'Je quitte la maison régulièrement pour le travail ou les loisirs. Je maintiens mes distances avec les autres.'=1; 'Je quitte la maison régulièrement. Je ne maintiens PAS mes distances avec les autres.'=0; else=NA"
# yes_no_maybe_mapping <- "'Yes'=1; 'Maybe/Unsure'=0; 'No'=-1; else=NA"
yes_no_maybe_mapping <- "'Oui'=1; 'Peut-être/Incertain'=0; 'Non'=-1; else=NA"
# likert_scale_mapping_1 <- "'Agree completely'=4; 'Agree quite a bit'=3; 'Agree somewhat'=2; 'Agree a little bit'=1; 'Do not agree at all'=0; 'Don\\'t know / no opinion'=-99; else=NA"
likert_scale_mapping_1 <- "'Complètement d\\'accord'=4; 'Plutôt d\\'accord'=3; 'D\\'accord dans une certaine mesure'=2; 'Quelque peu d\\'accord'=1; 'Pas du tout d\\'accord'=0; 'Ne sait pas / sans opinion'=-99; else=NA"
# likert_scale_mapping_2 <- "'Strongly Agree'=3; 'Agree'=2; 'Somewhat Agree'=1; 'Somewhat Disagree'=-1; 'Disagree'=-2; 'Strongly Disagree'=-3; else=NA"
likert_scale_mapping_2 <- "'Tout à fait d\\'accord'=3; 'D\\'accord'=2; 'Plutôt d\\'accord'=1; 'Plutôt pas d\\'accord'=-1; 'Pas d\\'accord'=-2; 'Pas du tout d\\'accord'=-3; else=NA"
# frequency_mapping_1 <- "'All of the time'=5; 'Most of the time'=4; 'More than half of the time'=3; 'Less than half of the time'=2; 'Some of the time'=1; 'At no time'=0; else=NA"
frequency_mapping_1 <- "'Tout le temps'=5; 'La plupart du temps'=4; 'Plus de la moitié du temps'=3; 'Moins de la moitié du temps'=2; 'Parfois'=1; 'Jamais'=0; else=NA"
# frequency_mapping_2 <- "'All of the time'=4; 'Most of the time'=3; 'Some of the time'=2; 'A little of the time'=1; 'None of the time'=0; else=NA"
frequency_mapping_2 <- "'Tout le temps'=4; 'La plupart du temps'=3; 'Parfois'=2; 'Peu souvent'=1; 'Jamais'=0; else=NA"
# performance_mapping <- "'You were a lot better than other workers'=3; 'You were somewhat better than other workers'=2; 'You were a little better than other workers'=1; 'You were about average'=0; 'You were a little worse than other workers'=-1; 'You were somewhat worse than other workers'=-2; 'You were a lot worse than other worker'=-3; else=NA"
performance_mapping <- "'Vous étiez bien meilleurs que les autres travailleurs'=3; 'Vous étiez dans une certaine mesure meilleur que les autres travailleurs'=2; 'Vous étiez un peu mieux que les autres travailleurs'=1; 'Vous étiez dans la moyenne'=0; 'Vous étiez un peu moins bon que les autres travailleurs'=-1; 'Vous étiez dans une certaine mesure moins bon que les autres travailleurs'=-2; 'Vous étiez bien moins bon que les autres travailleurs'=-3; else=NA"
# performance_mapping_2 <- "'You were a lot better than other workers'=3; 'You were somewhat better than other workers'=2; 'You were a little better than other workers'=1; 'You were about average'=0; 'You were a little worse than other workers'=-1; 'You were somewhat worse than other workers'=-2; 'You were a lot worse than other worker'=-3; else=NA"
performance_mapping_2 <- "'Vous êtes bien meilleurs que les autres travailleurs'=3; 'Vous êtes dans une certaine mesure meilleur que les autres travailleurs'=2; 'Vous êtes un peu mieux que les autres travailleurs'=1; 'Vous êtes dans la moyenne'=0; 'Vous êtes un peu moins bon que les autres travailleurs'=-1; 'Vous êtes dans une certaine mesure moins bon que les autres travailleurs'=-2; 'Vous êtes bien moins bon que les autres travailleurs'=-3; else=NA"
# age_mapping <- "'Less than 20'=1; '20-24'=2; '25-29'=3; '30-34'=4; '35-39'=5; '40-44'=6; '45-49'=7; '50-54'=8; '55-59'=9; '60-64'=10; '65+'=11; else=NA"
age_mapping <- "'Moins de 20 ans'=1; '20-24'=2; '25-29'=3; '30-34'=4; '35-39'=5; '40-44'=6; '45-49'=7; '50-54'=8; '55-59'=9; '60-64'=10; '65+'=11; else=NA"
# gender_mapping <- "'Female'='Female'; 'Male'='Male'; 'Non-binary / third gender'='NB'; 'Other'='Other'; else=NA"
gender_mapping <- "'Féminin'='Female'; 'Masculin'='Male'; 'Non-binaire / Troisième genre'='NB'; 'Other'='Other'; else=NA"
# education_mapping <- "'PhD or equivalent'=4; 'Masters degree or equivalent'=3; 'Undergraduate degree or equivalent'=2; 'Some post-secondary education'=1; 'No post-secondary education'=0; else=NA"
education_mapping <- "'Doctorat ou équivalent'=4; 'Maîtrise/Master ou équivalent'=3; 'License/Bachelor/Bachelier ou équivalent'=2; 'Une formation post-bac/post-secondaire'=1; 'Aucune formation post-bac/post-secondaire'=0; else=NA"
# organization_size_mapping <- "'0 to 9'=1; '10 to 99'=2; '100 to 999'=3; '1000 to 9999'=4; '10,000 to 99,999'=5; '100,000 or more'=6; else=NA"
organization_size_mapping <- "'entre 0 et 9'=1; 'entre 10 et 99'=2; 'entre 100 et 999'=3; 'entre 1000 et 9999'=4; 'entre 10,000 et 99,999'=5; 'plus de 100,000'=6; else=NA"
# disability_mapping <- "'Yes'=2; 'Maybe'=1; 'No'=0; else=NA"
disability_mapping <- "'Oui'=2; 'Peut-être'=1; 'Non'=0; else=NA"



# Please translate the following strings:
################################################################################
# Question 3 (COVIDStatus)
################################################################################
print_column_name()
# "Please select all that apply."
# filtered$COVIDStatus5 <- as.integer(match_str("I have tested positive for COVID-19"))
filtered$COVIDStatus5 <- as.integer(match_str("J'ai été testé positif au COVID-19"))
# filtered$COVIDStatus4 <- as.integer(match_str("Someone else in my home has tested positive for COVID-19"))
filtered$COVIDStatus4 <- as.integer(match_str("Quelqu'un d'autre au sein de mon foyer a été testé positif au COVID-19"))
# filtered$COVIDStatus3 <- as.integer(match_str("A member of my family has tested positive for COVID-19"))
filtered$COVIDStatus3 <- as.integer(match_str("Un membre de ma famille a été testé positif au COVID-19"))
# filtered$COVIDStatus2 <- as.integer(match_str("A close friend of mine has tested positive for COVID-19"))
filtered$COVIDStatus2 <- as.integer(match_str("Un ami proche a été testé positif au COVID-19"))
# filtered$COVIDStatus1 <- as.integer(match_str("I am currently or was recently quarantined \\(ordered not to leave home for any reason\\)"))
filtered$COVIDStatus1 <- as.integer(match_str("Je suis en ce moment ou j'ai été jusqu'à récemment mis en quarantaine \\(reçu ordre de ne pas quitter la maison quelque soit la raison\\)"))
# filtered$COVIDStatus0 <- as.integer(match_str("None of the above"))
filtered$COVIDStatus0 <- as.integer(match_str("Aucun des éléments précédents"))
filtered <- remove_column()

# You do not need to translate anything below, but, depending on your character set,
# you may also have to update the mapping for:
#   * Questions 8, 18, 19, 20, 22, 23, 25, 26 (should yield ASCII-compatible numbers)
#   * Question 29 (should yield English country names)

################################################################################
# Question 4 (Isolation)
################################################################################
print_column_name()
# "Choose the option that best describes you."
filtered$Isolation <- recode_column(covid_response_mapping)
filtered <- remove_column()

################################################################################
# Question 5 (FR)
################################################################################
print_column_name()
# "Are you fearful that… [...a close family member will die from COVID-19]"
filtered$FR3 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

print_column_name()
# "Are you fearful that… [...you will die from COVID-19]"
filtered$FR2 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

# Question 5_1
print_column_name()
# "Are you fearful that… [...you are infected with COVID-19]"
filtered$FR1 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

################################################################################
# Question 6 (FR)
################################################################################
print_column_name()
# "Right now do you feel... [...helpless]"
filtered$FR5 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

print_column_name()
# "Right now do you feel... [...horrified]"
filtered$FR6 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

print_column_name()
# "Right now do you feel... [...fearful]"
filtered$FR4 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

################################################################################
# Question 7 (FR)
################################################################################
print_column_name()
# "Please answer the following questions. [Do you usually recover quickly after an illness?]"
filtered$FR8 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

print_column_name()
# "Please answer the following questions. [Do you have any friends you can contact by phone/email/etc. so that you do not feel alone?]"
filtered$FR9 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

print_column_name()
# "Please answer the following questions. [Are you fearful that you will run out of money if you cannot work for the next 2-3 months?]"
filtered$FR7 <- recode_column(yes_no_maybe_mapping)
filtered <- remove_column()

################################################################################
# Question 8 (FR)
################################################################################
print_column_name()
# "How many different prescription medications are you on?"
filtered$FR10 <- as.integer(keep_column())
filtered <- remove_column()

################################################################################
# Question 9 (DP)
################################################################################
print_column_name()
# "Please react to the following statements. [I have an emergency supply kit.]"
filtered$DP2 = recode_column(likert_scale_mapping_1)
filtered <- remove_column()

print_column_name()
# "Please react to the following statements. [I discuss with others the information I get on the COVID-19 pandemic.]"
filtered$DP1 = recode_column(likert_scale_mapping_1)
filtered <- remove_column()

print_column_name()
# "Please react to the following statements. [I know people who will help me if I have to self-isolate.]"
filtered$DP5 = recode_column(likert_scale_mapping_1)
filtered <- remove_column()

print_column_name()
# "Please react to the following statements. [I am complying with government recommendations regarding the COVID-19 pandemic.]"
filtered$DP4= recode_column(likert_scale_mapping_1)
filtered <- remove_column()

print_column_name()
# "Please react to the following statements. [I have a plan if I get symptoms of COVID-19.]"
filtered$DP3 = recode_column(likert_scale_mapping_1)
filtered <- remove_column()

################################################################################
# Question 10 (Erg)
################################################################################
print_column_name()
# "Please indicate the extent to which you agree with each of the following questions about your workspace at home.
# [The noise level of my home workspace is acceptable]"
filtered$Erg2 = recode_column(likert_scale_mapping_2)
filtered <- remove_column()

print_column_name()
# "Please indicate the extent to which you agree with each of the following questions about your workspace at home.
# [My home workspace has good ergonomics]"
filtered$Erg6 = recode_column(likert_scale_mapping_2)
filtered <- remove_column()

print_column_name()
# "Please indicate the extent to which you agree with each of the following questions about your workspace at home.
# [The lighting in my home workspace is acceptable]"
filtered$Erg4 = recode_column(likert_scale_mapping_2)
filtered <- remove_column()

print_column_name()
# "Please indicate the extent to which you agree with each of the following questions about your workspace at home.
# [My chair is comfortable]"
filtered$Erg5 = recode_column(likert_scale_mapping_2)
filtered <- remove_column()

print_column_name()
# "Please indicate the extent to which you agree with each of the following questions about your workspace at home.
# [The temperature of my home workspace environment is acceptable]"
filtered$Erg3 = recode_column(likert_scale_mapping_2)
filtered <- remove_column()

print_column_name()
# "Please indicate the extent to which you agree with each of the following questions about your workspace at home.
# [My home working environment is free of distractions]"
filtered$Erg1 = recode_column(likert_scale_mapping_2)
filtered <- remove_column()

################################################################################
# Question 11 (OS)
################################################################################
print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank. 
# [My organization is improving documentation of its processes (e.g. how code changes are approved)]"
filtered$OS21 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization will pay for software we need to work from home]"
filtered$OS3 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [Someone is keeping high priority work ready and our backlog organized]"
filtered$OS22 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization is encouraging staff to use this time for professional training]"
filtered$OS17 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization has reassured me that I will keep my job]"
filtered$OS5 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My team is having virtual social events (e.g. via video chat)]"
filtered$OS12 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization is providing at-home exercise programs]"
filtered$OS16 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization has reassured me that I will continue to be paid]"
filtered$OS6 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization is sending food to staff working from home]"
filtered$OS14 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization is providing activities to occupy staff member's children]"
filtered$OS15 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization is encouraging staff to touch base regularly with each other]"
filtered$OS11 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization has reassured me that they understand if my work performance suffers]"
filtered$OS8 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization will buy new equipment we need to work from home]"
filtered$OS2 <- map_agreement_helpful()
filtered <- remove_column()

# Question 11_14
print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization will pay for some or all of my internet charges]"
filtered$OS4 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [For most of the day, I work with an open video or audio call to some or all of my team.]"
filtered$OS13 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My team has good work-from-home infrastructure (e.g. source control, VPN, remote desktop, file sharing)]"
filtered$OS18 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My team uses a build system to automate compilation and testing]"
filtered$OS20 <- map_agreement_helpful()
filtered <- remove_column()

# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My team is peer reviewing commits, change requests or pull requests (peer code review)]"
filtered$OS19 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [I can (or could) take equipment (e.g. monitors) home from my workplace]"
filtered$OS1 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My team is continuing to have regular meetings (e.g. via video chat)]"
filtered$OS9 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My organization has reassured me that I can take time off if I'm sick or need to care for dependents]"
filtered$OS7 <- map_agreement_helpful()
filtered <- remove_column()

print_column_name()
# "Below is a series of statements about how your organization might support you while you are working from home. For each item, first, if your company is doing it, please check the \"\"\"\"true\"\"\"\" box; if not, leave it blank. (Please also select \"\"\"\"true\"\"\"\" if your organization doesn't have to reassure you of something because it is obvious.) Second, if you think it is helping or would help, check the \"\"\"\"helpful\"\"\"\" box; if not leave it blank.
# [My team is avoiding synchronous communication (e.g. video chat)]"
filtered$OS10 <- map_agreement_helpful()
filtered <- remove_column()

################################################################################
# Question 12 (WHO5B)
################################################################################
print_column_name()
# "Thinking of the 4 weeks (28 days) prior to when you began working from home...
# [...I felt active and vigorous]"
filtered$WHO5B3 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Thinking of the 4 weeks (28 days) prior to when you began working from home...
# [...I woke up feeling fresh and rested]"
filtered$WHO5B4 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Thinking of the 4 weeks (28 days) prior to when you began working from home...
# [...My daily life was filled with things that interest me]"
filtered$WHO5B5 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Thinking of the 4 weeks (28 days) prior to when you began working from home...
# [...I felt calm and relaxed]"
filtered$WHO5B2 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Thinking of the 4 weeks (28 days) prior to when you began working from home...
# [...I felt cheerful and in good spirits]"
filtered$WHO5B1 = recode_column(frequency_mapping_1)
filtered <- remove_column()

################################################################################
# Question 13 (WHO5S)
################################################################################
print_column_name()
# "Since I began working at home due to the COVID-19 pandemic...
# [...I have felt calm and relaxed]"
filtered$WHO5S2 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Since I began working at home due to the COVID-19 pandemic...
# [...I have felt active and vigorous]"
filtered$WHO5S3 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Since I began working at home due to the COVID-19 pandemic...
# [...My daily life has been filled with things that interest me]"
filtered$WHO5S5 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Since I began working at home due to the COVID-19 pandemic...
# [...I have felt cheerful and in good spirits]"
filtered$WHO5S1 = recode_column(frequency_mapping_1)
filtered <- remove_column()

print_column_name()
# "Since I began working at home due to the COVID-19 pandemic...
# [...I wake up feeling fresh and rested]"
filtered$WHO5S4 = recode_column(frequency_mapping_1)
filtered <- remove_column()

################################################################################
# Question 14 (HPQB)
################################################################################
print_column_name()
# "In the 4 weeks before you began working from home...
# [How often was your performance higher than most workers on your job?]"
filtered$HPQB1 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "In the 4 weeks before you began working from home...
# [How often did concern about the COVID-19 pandemic limit the kind or amount of work you could do?]"
filtered$HPQB7 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "In the 4 weeks before you began working from home...
# [How often did you do no work at times when you were supposed to be working?]"
filtered$HPQB3 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "In the 4 weeks before you began working from home...
# [How often did you not concentrate enough on your work?]"
filtered$HPQB6 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "In the 4 weeks before you began working from home...
# [How often was the quality of your work lower than it should have been?]"
filtered$HPQB5 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "In the 4 weeks before you began working from home...
# [How often did you find yourself not working as carefully as you should?]"
filtered$HPQB4 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "In the 4 weeks before you began working from home...
# [How often was your performance lower than most workers on your job?]"
filtered$HPQB2 = recode_column(frequency_mapping_2)
filtered <- remove_column()

################################################################################
# Question 15 (HPQB)
################################################################################
print_column_name()
# "How would you compare your overall job performance with the performance of most other workers who have a similar type of job during the 4 weeks (28 days) prior to when you began working from home?"
filtered$HPQB8 = recode_column(performance_mapping)
filtered <- remove_column()

################################################################################
# Question 16 (HPQS)
################################################################################
print_column_name()
# "Since you began working from home... [How often was your performance higher than most workers on your job?]"
filtered$HPQS1 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "Since you began working from home... [How often did you find yourself not working as carefully as you should?]"
filtered$HPQS4 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "Since you began working from home... [How often did you not concentrate enough on your work?]"
filtered$HPQS6 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "Since you began working from home... [How often did you do no work at times when you were supposed to be working?]"
filtered$HPQS3 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "Since you began working from home... [How often was the quality of your work lower than it should have been?]"
filtered$HPQS5 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "Since you began working from home... [How often was your performance lower than most workers on your job?]"
filtered$HPQS2 = recode_column(frequency_mapping_2)
filtered <- remove_column()

print_column_name()
# "Since you began working from home... [How often did concern about the COVID-19 pandemic limit the kind or amount of work you could do?]"
filtered$HPQS7 = recode_column(frequency_mapping_2)
filtered <- remove_column()

################################################################################
# Question 17 (HPQS)
################################################################################
print_column_name()
# "How would you compare your overall job performance,  since you began working from home, with the performance of most other workers who have a similar type of job?"
#filtered$HPQS8 = recode_column(performance_mapping)
filtered$HPQS8 = recode_column(performance_mapping_2)
filtered <- remove_column()

################################################################################
# Question 18 (PerformanceOthers)
################################################################################
print_column_name()
# "How would you rate the usual performance of most workers in a job similar to yours?"
filtered$PerformanceOthers <- as.integer(keep_column())
filtered <- remove_column()

################################################################################
# Question 19 (PerB1item)
################################################################################
print_column_name()
# "How would you rate your usual job performance over the past year or two?"
filtered$PerB1item <- as.integer(keep_column())
filtered <- remove_column()

################################################################################
# Question 20 (PerS1item)
################################################################################
print_column_name()
# "How would you rate your overall job performance on the days you worked since you began working at home due to the COVID-19 pandemic?"
filtered$PerS1item <- as.integer(keep_column())
filtered <- remove_column()

################################################################################
# Question 21 (RolesIncludeDeveloper)
################################################################################
print_column_name()
# "Which of the following best describes your role?"
filtered$RolesIncludeDeveloper <- as.integer(is_developer())
filtered <- remove_column()

################################################################################
# Question 22 (YearsOfExperience)
################################################################################
print_column_name()
"How many years of professional experience do you have working in software development?"
filtered$YearsOfExperience <- as.numeric(keep_column())
filtered <- remove_column()

################################################################################
# Question 23 (YearsOfWorkFromHomeExperience)
################################################################################
print_column_name()
# "How many years of professional experience do you have working from home?"
filtered$YearsOfWorkFromHomeExperience <- as.numeric(keep_column())
filtered <- remove_column()

################################################################################
# Question 24 (Fulltime)
################################################################################
print_column_name()
# "My primary job is:"
filtered$Fulltime <- as.integer(is_fulltime())
filtered <- remove_column()

################################################################################
# Question 25 (Coinhabitants)
################################################################################
print_column_name()
# "How many people live in your home, including you?"
filtered$Coinhabitants <- as.integer(keep_column())
filtered <- remove_column()

################################################################################
# Question 26 (Children)
################################################################################
print_column_name()
# "How many of the people who live with you are children under the age of 12?"
filtered$Children <- as.integer(keep_column())
filtered <- remove_column()

################################################################################
# Question 27 (Age)
################################################################################
print_column_name()
# "How old are you (in years)?"
filtered$Age = recode_column(age_mapping)
filtered <- remove_column()

################################################################################
# Question 28 (Gender)
################################################################################
print_column_name()
# "What is your gender?"
filtered$Gender = recode_column(gender_mapping, asCharacter=TRUE)
filtered <- remove_column()

################################################################################
# Question 29 (Country)
################################################################################
print_column_name()
# "In what country do you live?"
filtered$Country <- as.character(keep_column())
filtered <- remove_column()

################################################################################
# Question 30 (Education)
################################################################################
print_column_name()
# "What is the highest level of education you have completed?"
filtered$Education <- recode_column(education_mapping)
filtered <- remove_column()

################################################################################
# Question 31 (OrganizationSize)
################################################################################
print_column_name()
# "How many employees does your whole organization (not your team) have?"
filtered$OrganizationSize <- recode_column(organization_size_mapping)
filtered <- remove_column()

################################################################################
# Question 32 (Disabilities)
################################################################################
print_column_name()
# "Do you have a disability (or disabilities) that affects your work?"
filtered$Disabilities <- recode_column(disability_mapping)
filtered <- remove_column()

################################################################################
# Add ID and Language
################################################################################
ID <- 1:n
Language <- rep(LANGUAGE, n)
filtered <- data.frame(cbind(Language, filtered))
filtered <- data.frame(cbind(ID, filtered))

# add timestamps
filtered$Timestamp <- timestamps

# write recoded results
write.table(filtered, file=paste0("data/", DATASET_VERSION, "/", LANGUAGE, "_recoded.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
