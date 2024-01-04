#POPS calculator
#Paediatric Observation Priority Score (POPS)

test <- syn_paeds_data %>%
  mutate(pops_sats = case_when(sats >=95 ~ 0,
                      sats %in% c(90:95) ~1,
                      sats <90 ~2),
         pops_breathing = case_when(resp_distress == "No distress" ~ 0,
                                    resp_distress == "Mild or moderate recession" ~ 1,
                                    resp_distress == "Audible grunt or wheeze" ~ 1,
                                    resp_distress == "Severe recession" ~ 2,
                                    resp_distress == "Stridor" ~ 2))

