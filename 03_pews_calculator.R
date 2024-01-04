#nPEWS calculator
#National Paediatric Early Warning System (nPEWS) 

test <- syn_paeds_data %>%
  mutate(pews_sats = case_when(sats >=95 ~ 0,
                               sats %in% c(90:95) ~1,
                               sats <90 ~2),
         pews_breathing = case_when(resp_distress == "No distress" ~ 0,
                                    resp_distress == "Mild" ~ 1,
                                    resp_distress == "Moderate" ~ 2,
                                    resp_distress == "Severe recession" ~ 4,
                                    resp_distress == "Stridor" ~ 4))