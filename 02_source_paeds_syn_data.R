#Creating synthetic paediatric vital sign data

# dataset specifications (based on POPS and nPEWS criteria): 
# age: 0-1, 1-2, 2-4, 5-12, 13-16
# gender 
# temperature
# sats
# breathing / respiratory distress
# respiratory support device
# AVPU
# gut feeling
# other
# pulse
# blood pressure 
# resp rate

#demographic details
#create synthetic data
#set number of patients
n_patients <- 1000
id <- paste0("patient_", 1:n_patients)
#assign gender randomly using binomial distribution,use if else statements to assign sex category
set.seed(888)
sex <- sample(x = c('male', 'female'), n_patients, replace = T)
#generate ages with uniform distribution 
set.seed(888)
age <- round(runif(n_patients, 0, 16))

#non age dependent variables
sats <- round(rbeta(n_patients, shape1 = 97, shape2 = 5)*100,0)
resp_distress <- sample(c("No distress", 
                          "Audible grunt or wheeze", 
                          "Mild or moderate recession", 
                          "Stridor", 
                          "Severe recession"),
                        n_patients,
                        replace = TRUE)
avpu <- sample(c("alert", "voice", "pain"),
               n_patients,
               replace = TRUE)
gestalt <- sample(c("well", "low level concern", "child looks unwell"),
                  n_patients,
                  replace = TRUE)
other <- sample(c("well", "significant PMH", "Oncology patient", "Congenital heart disease"),
                n_patients,
                replace = TRUE)
resp_support_base <- round(rbeta(n_patients, shape1 = 21, shape2 = 70)*100,0)
#resp_support <- resp_support_base %>%
#  mutate(resp_distress_binary = ifelse())

#merge into single synthetic data set
syn_paeds_data <- as.data.frame(cbind(id, age, sex, 
                                      sats, resp_distress, avpu, gestalt, other))

#age dependent variables
syn_paeds_data <- syn_paeds_data %>%
  mutate(pulse = case_when(age < 2 ~ round(rnorm(n_patients, mean = 135, sd = 15),0),
                           age >=1 | age <= 2 ~ round(rnorm(n_patients, mean = 125, sd = 20),0),
                           age >=2 | age  <5 ~ round(rnorm(n_patients, mean = 115, sd = 15),0),
                           age >=5 | age <= 12 ~round(rnorm(n_patients, mean = 100, sd = 20),0),
                           age >= 13 ~ round(rnorm(n_patients, mean = 80, sd = 13),0)),
         resp_rate = case_when(age < 2 ~ round(rnorm(n_patients, mean = 35, sd = 6),0),
                               age >=1 | age <= 2 ~ round(rnorm(n_patients, mean = 30, sd = 6),0),
                               age >=2 | age  <5 ~ round(rnorm(n_patients, mean = 27, sd = 5),0),
                               age >=5 | age <= 12 ~round(rnorm(n_patients, mean = 24, sd = 5),0),
                               age >= 13 ~ round(rnorm(n_patients, mean = 18, sd = 3),0)),
         temperature = case_when(age < 2 ~ round(rnorm(n_patients, mean = 36.7, sd = 1),1),
                                 age >=1 | age <= 2 ~ round(rnorm(n_patients, mean = 37, sd = 1),1),
                                 age >=2 ~ round(rnorm(n_patients, mean = 37, sd = 1),1)))

#test <- round(rbeta(n_patients, shape1 = 21, shape2 = 70)*100,0)
#hist(test, breaks = 50, xlim = c(0, 100))

