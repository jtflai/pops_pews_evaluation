#POPS calculator
#Paediatric Observation Priority Score (POPS)

syn_paeds_data %>%
  pops_sats = case_when(sats >=95 ~ 0,
                        sats %in% c(90:95) ~1,
                        sats <90 ~2)
  