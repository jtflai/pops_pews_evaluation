#POPS calculator
#Paediatric Observation Priority Score (POPS)

test <- syn_paeds_data %>%
  mutate(pops_sats = case_when(sats >=95 ~ 0,
                      sats %in% c(90:95) ~1,
                      sats <90 ~2),
         pops_breathing = case_when(resp_distress == "No distress" ~ 0,
                                    resp_distress == "Mild recession" | resp_distress == "Moderate recession" ~ 1,
                                    resp_distress == "Audible grunt"  | resp_distress =="Audible wheeze" ~ 1,
                                    resp_distress == "Severe recession" ~ 2,
                                    resp_distress == "Stridor" ~ 2),
         pops_gestalt = case_when(gestalt == "Well" ~0,
                                  gestalt == "Low level concern" ~ 1,
                                  gestalt == "High level concern" ~ 2,
                                  gestalt == "Child looks unwell" ~ 1),
         pops_other = case_when(other == "Well" ~ 0,
                                other == "Significant PMH" ~ 1,
                                other == "Oncology patient" ~ 2,
                                other == "Congenital heart disease" ~ 2),
         pops_pulse = case_when(age <1 & pulse %in% c(110:160) ~ 0,
                                age <1 & pulse %in% c(90:109) ~1,
                                age <1 & pulse %in% c(161:180) ~ 1,
                                age <1 & pulse <90 ~ 2,
                                age <1 & pulse >180 ~ 2, 
                                age %in% c(1:2) & pulse %in% c(100:150) ~0,
                                age %in% c(1:2) & pulse %in% c(90:99) ~1,
                                age %in% c(1:2) & pulse %in% c(151:170) ~1,
                                age %in% c(1:2) & pulse <90 ~ 2,
                                age %in% c(1:2) & pulse >170 ~2,
                                age %in% c(2:4) & pulse %in% c(95:140) ~0,
                                age %in% c(2:4) & pulse %in% c(80:94) ~1, 
                                age %in% c(2:4) & pulse %in% c(141:160) ~1,
                                age %in% c(2:4) & pulse <80 ~2,
                                age %in% c(2:4) & pulse >160 ~ 2,
                                age %in% c(5:12) & pulse %in% c(80:110) ~0,
                                age %in% c(5:12) & pulse %in% c(70:79) ~1,
                                age %in% c(5:12) & pulse %in% c(111:150) ~1,
                                age %in% c(5:12) & pulse <70 ~2,
                                age %in% c(5:12) & pulse >150 ~2,
                                age >13 & pulse %in% c(60:100) ~0,
                                age >13 & pulse %in% c(50:59) ~1,
                                age >13 & pulse %in% c(101:110) ~1,
                                age >13 & pulse < 50 ~ 2,
                                age >13 & pulse > 110 ~ 2))
