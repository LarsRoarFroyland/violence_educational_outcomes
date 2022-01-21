##### Variable generating

## Study line
uvcompl <- uvcompl %>% 
  mutate(studretn = case_when(
    studretn_2007 == "Allmene og okonomiske fag"  ~ 1,
    studretn_2007 == "Musikk, dans og drama"  ~ 1,
    studretn_2007 == "Idrettsfag"  ~ 1,
    studretn_2007 == "Media og kommunikasjon"  ~ 1,
    !is.na(studretn_2007) ~ 0))
    
### Variables for comparing register sample with complete sample

## Migration background
uvcompl <- uvcompl %>% 
  mutate(innv = case_when(
    io_fode == "I Norge"  ~ 0,
    io_fode != "I Norge" ~ 1))

### Physical violence

## Physical violence from mother

# At least one instance of less severe physical violence from the mother before 13
uvreg <- uvreg %>% 
  mutate(viomoth_less1 = case_when(
    morv6_2007 == "Ja" | morv7_2007 == "Ja" | morv8_2007 == "Ja" ~ "Yes",
    morv6_2007 == "Nei" | morv7_2007 == "Nei" | morv8_2007 == "Nei" ~ "No"),
    viomoth_less1 = factor(viomoth_less1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_less1)

# At least one instance of less severe physical violence from the mother after 13
uvreg <- uvreg %>% 
  mutate(viomoth_less2 = case_when(
    morv18_2007 == "Ja" | morv19_2007 == "Ja" | morv20_2007 == "Ja" ~ "Yes",
    morv18_2007 == "Nei" | morv19_2007 == "Nei" | morv20_2007 == "Nei" ~ "No"),
    viomoth_less2 = factor(viomoth_less2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_less2)

# At least one instance of less severe physical violence from the mother ever
uvreg <- uvreg %>% 
  mutate(viomoth_less = case_when(
    viomoth_less1 == "Yes" | viomoth_less2 == "Yes" ~ "Yes",
    viomoth_less1 == "No" | viomoth_less2 == "No" ~ "No"),
    viomoth_less = factor(viomoth_less, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_less)

# At least one instance of more severe physical violence from the mother before 13
uvreg <- uvreg %>% 
  mutate(viomoth_sev1 = case_when(
    morv9_2007 == "Ja" | morv10_2007 == "Ja" | morv11_2007 == "Ja" | morv12_2007 == "Ja" ~ "Yes",
    morv9_2007 == "Nei" | morv10_2007 == "Nei" | morv11_2007 == "Nei" | morv12_2007 == "Nei" ~ "No"),
    viomoth_sev1 = factor(viomoth_sev1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_sev1)

# At least one instance of more severe physical violence from the mother after 13
uvreg <- uvreg %>% 
  mutate(viomoth_sev2 = case_when(
    morv21_2007 == "Ja" | morv22_2007 == "Ja" | morv23_2007 == "Ja" | morv24_2007 == "Ja" ~ "Yes",
    morv21_2007 == "Nei" | morv22_2007 == "Nei" | morv23_2007 == "Nei" | morv24_2007 == "Nei" ~ "No"),
    viomoth_sev2 = factor(viomoth_sev2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_sev2)

# At least one instance of more severe physical violence from the mother ever
uvreg <- uvreg %>% 
  mutate(viomoth_sev = case_when(
    viomoth_sev1 == "Yes" | viomoth_sev2 == "Yes" ~ "Yes",
    viomoth_sev1 == "No" | viomoth_sev2 == "No" ~ "No"),
    viomoth_sev = factor(viomoth_sev, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_sev)

# At least one instance of any type of physical violence from the mother before 13
uvreg <- uvreg %>% 
  mutate(viomoth_phys1 = case_when(
    viomoth_less1 == "Yes" | viomoth_sev1 == "Yes" ~ "Yes",
    viomoth_less1 == "No" | viomoth_sev1 == "No" ~ "No"),
    viomoth_phys1 = factor(viomoth_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_phys1)

# At least one instance of any type of physical violence from the mother after 13
uvreg <- uvreg %>% 
  mutate(viomoth_phys2 = case_when(
    viomoth_less2 == "Yes" | viomoth_sev2 == "Yes" ~ "Yes",
    viomoth_less2 == "No" | viomoth_sev2 == "No" ~ "No"),
    viomoth_phys2 = factor(viomoth_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_phys2)

# At least one instance of any type of physical violence from the mother ever
uvreg <- uvreg %>% 
  mutate(viomoth_phys = case_when(
    viomoth_less == "Yes" | viomoth_sev == "Yes" | mioskad1_2007 == "Ja, en gang" | mioskad1_2007 == "Ja, flere ganger" | mioskad2_2007 == "Ja, en gang" | mioskad2_2007 == "Ja, flere ganger" | mioskad3_2007 == "Ja, en gang" | mioskad3_2007 == "Ja, flere ganger" ~ "Yes",
    viomoth_less == "No" | viomoth_sev == "No" | mioskad1_2007 == "Nei, aldri" | mioskad2_2007 == "Nei, aldri" | mioskad3_2007 == "Nei, aldri" ~ "No"),
    viomoth_phys = factor(viomoth_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_phys)

## Physical violence from father

# At least one instance of less severe physical violence from the father before 13
uvreg <- uvreg %>% 
  mutate(viofath_less1 = case_when(
    farv6_2007 == "Ja" | farv7_2007 == "Ja" | farv8_2007 == "Ja" ~ "Yes",
    farv6_2007 == "Nei" | farv7_2007 == "Nei" | farv8_2007 == "Nei" ~ "No"),
    viofath_less1 = factor(viofath_less1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_less1)

# At least one instance of less severe physical violence from the father after 13
uvreg <- uvreg %>% 
  mutate(viofath_less2 = case_when(
    farv18_2007 == "Ja" | farv19_2007 == "Ja" | farv20_2007 == "Ja" ~ "Yes",
    farv18_2007 == "Nei" | farv19_2007 == "Nei" | farv20_2007 == "Nei" ~ "No"),
    viofath_less2 = factor(viofath_less2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_less2)

# At least one instance of less severe physical violence from the father ever
uvreg <- uvreg %>% 
  mutate(viofath_less = case_when(
    viofath_less1 == "Yes" | viofath_less2 == "Yes" ~ "Yes",
    viofath_less1 == "No" | viofath_less2 == "No" ~ "No"),
    viofath_less = factor(viofath_less, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_less)

# At least one instance of more severe physical violence from the father before 13
uvreg <- uvreg %>% 
  mutate(viofath_sev1 = case_when(
    farv9_2007 == "Ja" | farv10_2007 == "Ja" | farv11_2007 == "Ja" | farv12_2007 == "Ja" ~ "Yes",
    farv9_2007 == "Nei" | farv10_2007 == "Nei" | farv11_2007 == "Nei" | farv12_2007 == "Nei" ~ "No"),
    viofath_sev1 = factor(viofath_sev1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_sev1)

# At least one instance of more severe physical violence from the father after 13
uvreg <- uvreg %>% 
  mutate(viofath_sev2 = case_when(
    farv21_2007 == "Ja" | farv22_2007 == "Ja" | farv23_2007 == "Ja" | farv24_2007 == "Ja" ~ "Yes",
    farv21_2007 == "Nei" | farv22_2007 == "Nei" | farv23_2007 == "Nei" | farv24_2007 == "Nei" ~ "No"),
    viofath_sev2 = factor(viofath_sev2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_sev2)

# At least one instance of more severe physical violence from the father ever
uvreg <- uvreg %>% 
  mutate(viofath_sev = case_when(
    viofath_sev1 == "Yes" | viofath_sev2 == "Yes" ~ "Yes",
    viofath_sev1 == "No" | viofath_sev2 == "No" ~ "No"),
    viofath_sev = factor(viofath_sev, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_sev)

# At least one instance of any type of physical violence from the father before 13
uvreg <- uvreg %>% 
  mutate(viofath_phys1 = case_when(
    viofath_less1 == "Yes" | viofath_sev1 == "Yes" ~ "Yes",
    viofath_less1 == "No" | viofath_sev1 == "No" ~ "No"),
    viofath_phys1 = factor(viofath_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_phys1)

# At least one instance of any type of physical violence from the father after 13
uvreg <- uvreg %>% 
  mutate(viofath_phys2 = case_when(
    viofath_less2 == "Yes" | viofath_sev2 == "Yes" ~ "Yes",
    viofath_less2 == "No" | viofath_sev2 == "No" ~ "No"),
    viofath_phys2 = factor(viofath_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_phys2)

# At least one instance of any type of physical violencefrom the father ever
uvreg <- uvreg %>% 
  mutate(viofath_phys = case_when(
    viofath_less == "Yes" | viofath_sev == "Yes" | fioskad1_2007 == "Ja, en gang" | fioskad1_2007 == "Ja, flere ganger" | fioskad2_2007 == "Ja, en gang" | fioskad2_2007 == "Ja, flere ganger" | fioskad3_2007 == "Ja, en gang" | fioskad3_2007 == "Ja, flere ganger" ~ "Yes",
    viofath_less == "No" | viofath_sev == "No" | fioskad1_2007 == "Nei, aldri" | fioskad2_2007 == "Nei, aldri" | fioskad3_2007 == "Nei, aldri" ~ "No"),
    viofath_phys = factor(viofath_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_phys)

## Physical violence from any parent

# At least one instance of less severe physical violence from any parent before 13
uvreg <- uvreg %>% 
  mutate(viopar_less1 = case_when(
    viomoth_less1 == "Yes" | viofath_less1 == "Yes" ~ "Yes",
    viomoth_less1 == "No" | viofath_less1 == "No" ~ "No"),
    viopar_less1 = factor(viopar_less1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_less1)

# At least one instance of less severe physical violence from any parent after 13
uvreg <- uvreg %>% 
  mutate(viopar_less2 = case_when(
    viomoth_less2 == "Yes" | viofath_less2 == "Yes" ~ "Yes",
    viomoth_less2 == "No" | viofath_less2 == "No" ~ "No"),
    viopar_less2 = factor(viopar_less2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_less2)

# At least one instance of more severe physical violence from any parent before 13
uvreg <- uvreg %>% 
  mutate(viopar_sev1 = case_when(
    viomoth_sev1 == "Yes" | viofath_sev1 == "Yes" ~ "Yes",
    viomoth_sev1 == "No" | viofath_sev1 == "No" ~ "No"),
    viopar_sev1 = factor(viopar_sev1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_sev1)

# At least one instance of more severe physical violence from any parent after 13
uvreg <- uvreg %>% 
  mutate(viopar_sev2 = case_when(
    viomoth_sev2 == "Yes" | viofath_sev2 == "Yes" ~ "Yes",
    viomoth_sev2 == "No" | viofath_sev2 == "No" ~ "No"),
    viopar_sev2 = factor(viopar_sev2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_sev2)

# At least one instance of any type of physical violence from any parent before 13
uvreg <- uvreg %>% 
  mutate(viopar_phys1 = case_when(
    viomoth_phys1 == "Yes" | viofath_phys1 == "Yes" ~ "Yes",
    viomoth_phys1 == "No" | viofath_phys1 == "No" ~ "No"),
    viopar_phys1 = factor(viopar_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_phys1)

# At least one instance of any type of physical violence from any parent after 13
uvreg <- uvreg %>% 
  mutate(viopar_phys2 = case_when(
    viomoth_phys2 == "Yes" | viofath_phys2 == "Yes" ~ "Yes",
    viomoth_phys2 == "No" | viofath_phys2 == "No" ~ "No"),
    viopar_phys2 = factor(viopar_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_phys2)

# At least one instance of less severe physical violence from any parent ever
uvreg <- uvreg %>% 
  mutate(viopar_less = case_when(
    viomoth_less == "Yes" | viofath_less == "Yes" ~ "Yes",
    viomoth_less == "No" | viofath_less == "No" ~ "No"),
    viopar_less = factor(viopar_less, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_less)

# At least one instance of more severe physical violence from any parent ever
uvreg <- uvreg %>% 
  mutate(viopar_sev = case_when(
    viomoth_sev == "Yes" | viofath_sev == "Yes" ~ "Yes",
    viomoth_sev == "No" | viofath_sev == "No" ~ "No"),
    viopar_sev = factor(viopar_sev, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_sev)

# At least one instance of any type of physical violence from any parent ever
uvreg <- uvreg %>% 
  mutate(viopar_phys = case_when(
    viomoth_phys == "Yes" | viofath_phys == "Yes" ~ "Yes",
    viomoth_phys == "No" | viofath_phys == "No" ~ "No"),
    viopar_phys = factor(viopar_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_phys)

## Physical violence from peers

# At least one instance of physical violence from peers the previous 12 months
uvreg <- uvreg %>% 
  mutate(viopeer_phys1 = case_when(
    (skadet1 == "Avkrysset" | skadet2 == "Avkrysset" | skadet3 == "Avkrysset" | skadet4 == "Avkrysset" | skadet5 == "Avkrysset") & (volda5_2007 == "Ja" | volda6_2007 == "Ja" | volda7_2007 == "Ja" ) ~ "Yes",
    volda5_2007 == "Nei" | volda6_2007 == "Nei" | volda7_2007 == "Nei" ~ "No"),
    viopeer_phys1 = factor(viopeer_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopeer_phys1)

# At least one instance of physical violence from peers before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viopeer_phys2 = case_when(
    (skadet1 == "Avkrysset" | skadet2 == "Avkrysset" | skadet3 == "Avkrysset" | skadet4 == "Avkrysset" | skadet5 == "Avkrysset") & (voldb5_2007 == "Ja" | voldb6_2007 == "Ja" | voldb7_2007 == "Ja") ~ "Yes",
    voldb5_2007 == "Nei" | voldb6_2007 == "Nei" | voldb7_2007 == "Nei" ~ "No"),
    viopeer_phys2 = factor(viopeer_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopeer_phys2)

# At least one instance of any type of physical violence from peers ever
uvreg <- uvreg %>% 
  mutate(viopeer_phys = case_when(
    viopeer_phys1 == "Yes" | viopeer_phys2 == "Yes" ~ "Yes",
    viopeer_phys2 == "No" | viopeer_phys2 == "No" ~ "No"),
    viopeer_phys = factor(viopeer_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopeer_phys)

### Witnessing violence

## Violence against mother

# At least one instance of verbal violence the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_verb1 = case_when(
    mor2_2007 == "Ja" | mor3_2007 == "Ja" ~ "Yes",
    mor2_2007 == "Nei" | mor3_2007 == "Nei" ~ "No"),
    viowitmoth_verb1 = factor(viowitmoth_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_verb1)

# At least one instance of verbal violence before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_verb2 = case_when(
    mor12_2007 == "Ja" | mor13_2007 == "Ja" ~ "Yes",
    mor12_2007 == "Nei" | mor13_2007 == "Nei" ~ "No"),
    viowitmoth_verb2 = factor(viowitmoth_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_verb2)

# At least one instance of verbal violence ever
uvreg <- uvreg %>% 
  mutate(viowitmoth_verb = case_when(
    viowitmoth_verb1 == "Yes" | viowitmoth_verb2 == "Yes" ~ "Yes",
    viowitmoth_verb1 == "No" | viowitmoth_verb2 == "No" ~ "No"),
    viowitmoth_verb = factor(viowitmoth_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_verb)

# At least one instance of less severe physical violence the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_less1 = case_when(
    mor4_2007 == "Ja" | mor5_2007 == "Ja" | mor6_2007 == "Ja" ~ "Yes",
    mor4_2007 == "Nei" | mor5_2007 == "Nei" | mor6_2007 == "Nei" ~ "No"),
    viowitmoth_less1 = factor(viowitmoth_less1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_less1)

# At least one instance of less severe physical violence before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_less2 = case_when(
    mor14_2007 == "Ja" | mor15_2007 == "Ja" | mor16_2007 == "Ja" ~ "Yes",
    mor14_2007 == "Nei" | mor15_2007 == "Nei" | mor16_2007 == "Nei" ~ "No"),
    viowitmoth_less2 = factor(viowitmoth_less2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_less2)

# At least one instance of less severe physical violence ever
uvreg <- uvreg %>% 
  mutate(viowitmoth_less = case_when(
    viowitmoth_less1 == "Yes" | viowitmoth_less2 == "Yes" ~ "Yes",
    viowitmoth_less1 == "No" | viowitmoth_less2 == "No" ~ "No"),
    viowitmoth_less = factor(viowitmoth_less, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_less)

# At least one instance of more severe physical violence the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_sev1 = case_when(
    mor7_2007 == "Ja" | mor8_2007 == "Ja" | mor9_2007 == "Ja" | mor10_2007 == "Ja" ~ "Yes",
    mor7_2007 == "Nei" | mor8_2007 == "Nei" | mor9_2007 == "Nei" | mor10_2007 == "Nei" ~ "No"),
    viowitmoth_sev1 = factor(viowitmoth_sev1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_sev1)

# At least one instance of more severe physical violence before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_sev2 = case_when(
    mor17_2007 == "Ja" | mor18_2007 == "Ja" | mor19_2007 == "Ja" | mor20_2007 == "Ja" ~ "Yes",
    mor17_2007 == "Nei" | mor18_2007 == "Nei" | mor19_2007 == "Nei" | mor20_2007 == "Nei" ~ "No"),
    viowitmoth_sev2 = factor(viowitmoth_sev2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_sev2)

# At least one instance of more severe physical violence ever
uvreg <- uvreg %>% 
  mutate(viowitmoth_sev = case_when(
    viowitmoth_sev1 == "Yes" | viowitmoth_sev2 == "Yes" ~ "Yes",
    viowitmoth_sev1 == "No" | viowitmoth_sev2 == "No" ~ "No"),
    viowitmoth_sev = factor(viowitmoth_sev, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_sev)

# At least one instance of any type of physical violence the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_phys1 = case_when(
    viowitmoth_less1 == "Yes" | viowitmoth_sev1 == "Yes" ~ "Yes",
    viowitmoth_less1 == "No" | viowitmoth_sev1 == "No" ~ "No"),
    viowitmoth_phys1 = factor(viowitmoth_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_phys1)

# At least one instance of any type of physical violence before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitmoth_phys2 = case_when(
    viowitmoth_less2 == "Yes" | viowitmoth_sev2 == "Yes" ~ "Yes",
    viowitmoth_less2 == "No" | viowitmoth_sev2 == "No" ~ "No"),
    viowitmoth_phys2 = factor(viowitmoth_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_phys2)

# At least one instance of any type of physical violence ever
uvreg <- uvreg %>% 
  mutate(viowitmoth_phys = case_when(
    viowitmoth_less == "Yes" | viowitmoth_sev == "Yes" | morskad1_2007 == "Ja, en gang" | morskad1_2007 == "Ja, flere ganger" | morskad2_2007 == "Ja, en gang" | morskad2_2007 == "Ja, flere ganger" ~ "Yes",
    viowitmoth_less == "No" | viowitmoth_sev == "No" | morskad1_2007 == "Nei, aldri" | morskad2_2007 == "Nei, aldri" ~ "No"),
    viowitmoth_phys = factor(viowitmoth_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitmoth_phys)

## Violence against father

# At least one instance of verbal violence against the father the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_verb1 = case_when(
    far2_2007 == "Ja" | far3_2007 == "Ja" ~ "Yes",
    far2_2007 == "Nei" | far3_2007 == "Nei" ~ "No"),
    viowitfath_verb1 = factor(viowitfath_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_verb1)

# At least one instance of verbal violence against the father before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_verb2 = case_when(
    far12_2007 == "Ja" | far13_2007 == "Ja" ~ "Yes",
    far12_2007 == "Nei" | far13_2007 == "Nei" ~ "No"),
    viowitfath_verb2 = factor(viowitfath_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_verb2)

# At least one instance of verbal violence against the father ever
uvreg <- uvreg %>% 
  mutate(viowitfath_verb = case_when(
    viowitfath_verb1 == "Yes" | viowitfath_verb2 == "Yes" ~ "Yes",
    viowitfath_verb1 == "No" | viowitfath_verb2 == "No" ~ "No"),
    viowitfath_verb = factor(viowitfath_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_verb)

# At least one instance of less severe physical violence against the father the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_less1 = case_when(
    far4_2007 == "Ja" | far5_2007 == "Ja" | far6_2007 == "Ja" ~ "Yes",
    far4_2007 == "Nei" | far5_2007 == "Nei" | far6_2007 == "Nei" ~ "No"),
    viowitfath_less1 = factor(viowitfath_less1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_less1)

# At least one instance of less severe physical violence against the father before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_less2 = case_when(
    far14_2007 == "Ja" | far15_2007 == "Ja" | far16_2007 == "Ja" ~ "Yes",
    far14_2007 == "Nei" | far15_2007 == "Nei" | far16_2007 == "Nei" ~ "No"),
    viowitfath_less2 = factor(viowitfath_less2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_less2)

# At least one instance of less severe physical violence against the father ever
uvreg <- uvreg %>% 
  mutate(viowitfath_less = case_when(
    viowitfath_less1 == "Yes" | viowitfath_less2 == "Yes" ~ "Yes",
    viowitfath_less1 == "No" | viowitfath_less2 == "No" ~ "No"),
    viowitfath_less = factor(viowitfath_less, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_less)

# At least one instance of more severe physical violence against the father the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_sev1 = case_when(
    far7_2007 == "Ja" | far8_2007 == "Ja" | far9_2007 == "Ja" | far10_2007 == "Ja" ~ "Yes",
    far7_2007 == "Nei" | far8_2007 == "Nei" | far9_2007 == "Nei" | far10_2007 == "Nei" ~ "No"),
    viowitfath_sev1 = factor(viowitfath_sev1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_sev1)

# At least one instance of more severe physical violence against the father before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_sev2 = case_when(
    far17_2007 == "Ja" | far18_2007 == "Ja" | far19_2007 == "Ja" | far20_2007 == "Ja" ~ "Yes",
    far17_2007 == "Nei" | far18_2007 == "Nei" | far19_2007 == "Nei" | far20_2007 == "Nei" ~ "No"),
    viowitfath_sev2 = factor(viowitfath_sev2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_sev2)

# At least one instance of more severe physical violence against the father ever
uvreg <- uvreg %>% 
  mutate(viowitfath_sev = case_when(
    viowitfath_sev1 == "Yes" | viowitfath_sev2 == "Yes" ~ "Yes",
    viowitfath_sev1 == "No" | viowitfath_sev2 == "No" ~ "No"),
    viowitfath_sev = factor(viowitfath_sev, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_sev)

# At least one instance of any type of physical violence against the father the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_phys1 = case_when(
    viowitfath_less1 == "Yes" | viowitfath_sev1 == "Yes" ~ "Yes",
    viowitfath_less1 == "No" | viowitfath_sev1 == "No" ~ "No"),
    viowitfath_phys1 = factor(viowitfath_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_phys1)

# At least one instance of any type of physical violence against the father before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitfath_phys2 = case_when(
    viowitfath_less2 == "Yes" | viowitfath_sev2 == "Yes" ~ "Yes",
    viowitfath_less2 == "No" | viowitfath_sev2 == "No" ~ "No"),
    viowitfath_phys2 = factor(viowitfath_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_phys2)

# At least one instance of any type of physical violence against the father ever
uvreg <- uvreg %>% 
  mutate(viowitfath_phys = case_when(
    viowitfath_less == "Yes" | viowitfath_sev == "Yes" | farskad1_2007 == "Ja, en gang" | farskad1_2007 == "Ja, flere ganger" | farskad2_2007 == "Ja, en gang" | farskad2_2007 == "Ja, flere ganger" ~ "Yes",
    viowitfath_less == "No" | viowitfath_sev == "No" | farskad1_2007 == "Nei, aldri" | farskad2_2007 == "Nei, aldri" ~ "No"),
    viowitfath_phys = factor(viowitfath_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitfath_phys)

## Witnessing violence against any parent

# At least one instance of verbal violence against any parent the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitpar_verb1 = case_when(
    viowitmoth_verb1 == "Yes" | viowitfath_verb1 == "Yes" ~ "Yes",
    viowitmoth_verb1 == "No" | viowitfath_verb1 == "No" ~ "No"),
    viowitpar_verb1 = factor(viowitpar_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitpar_verb1)

# At least one instance of verbal violence against any parent before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitpar_verb2 = case_when(
    viowitmoth_verb2 == "Yes" | viowitfath_verb2 == "Yes" ~ "Yes",
    viowitmoth_verb2 == "No" | viowitfath_verb2 == "No" ~ "No"),
    viowitpar_verb2 = factor(viowitpar_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitpar_verb2)

# At least one instance of verbal violence against any parent ever
uvreg <- uvreg %>% 
  mutate(viowitpar_verb = case_when(
    viowitmoth_verb == "Yes" | viowitfath_verb == "Yes" ~ "Yes",
    viowitmoth_verb == "No" | viowitfath_verb == "No" ~ "No"),
    viowitpar_verb = factor(viowitpar_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitpar_verb)

# At least one instance of physical violence against any parent the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitpar_phys1 = case_when(
    viowitmoth_phys1 == "Yes" | viowitfath_phys1 == "Yes" ~ "Yes",
    viowitmoth_phys1 == "No" | viowitfath_phys1 == "No" ~ "No"),
    viowitpar_phys1 = factor(viowitpar_phys1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitpar_phys1)

# At least one instance of physical violence against any parent before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viowitpar_phys2 = case_when(
    viowitmoth_phys2 == "Yes" | viowitfath_phys2 == "Yes" ~ "Yes",
    viowitmoth_phys2 == "No" | viowitfath_phys2 == "No" ~ "No"),
    viowitpar_phys2 = factor(viowitpar_phys2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitpar_phys2)

# At least one instance of physical violence against any parent ever
uvreg <- uvreg %>% 
  mutate(viowitpar_phys = case_when(
    viowitmoth_phys == "Yes" | viowitfath_phys == "Yes" ~ "Yes",
    viowitmoth_phys == "No" | viowitfath_phys == "No" ~ "No"),
    viowitpar_phys = factor(viowitpar_phys, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowitpar_phys)

## Witnessing violence against siblings

# At least one instance of witnessing violence against siblings
uvreg <- uvreg %>% 
  mutate(viowit_sib = case_when(
    vitnevm_2007 == "Ja, en gang" | vitnevm_2007 == "Ja, flere ganger" | vitnevf_2007 == "Ja, en gang" | vitnevf_2007 == "Ja, flere ganger" ~ "Yes",
    vitnevm_2007 == "Nei, aldri" | vitnevf_2007 == "Nei, aldri" ~ "No"),
    viowit_sib = factor(viowit_sib, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viowit_sib)

### Sexual violence

# At least one instance of sexual violence before 13
uvreg <- uvreg %>% 
  mutate(sexviob13 = case_when(
    sexf13b_2007 == "Ja" | sexf13c_2007 == "Ja" | sexf13d_2007 == "Ja" | sexf13e_2007 == "Ja" | sexf13f_2007 == "Ja" | sexf13g_2007 == "Ja" | sexf13h_2007 == "Ja" | sexf13i_2007 == "Ja" | sexf13j_2007 == "Ja" | sexf13k_2007 == "Ja" ~ "Yes",
    sexf13b_2007 == "Nei" | sexf13c_2007 == "Nei" | sexf13d_2007 == "Nei" | sexf13e_2007 == "Nei" | sexf13f_2007 == "Nei" | sexf13g_2007 == "Nei" | sexf13h_2007 == "Nei" | sexf13i_2007 == "Nei" | sexf13j_2007 == "Nei" | sexf13k_2007 == "Nei" ~ "No"),
    sexviob13 = factor(sexviob13, levels = c("No","Yes"))
  )

uvreg %>% 
  count(sexviob13)

# At least one instance of sexual violence after 13
uvreg <- uvreg %>% 
  mutate(sexvioa13 = case_when(
    sexe13b_2007 == "Ja" | sexe13c_2007 == "Ja" | sexe13d_2007 == "Ja" | sexe13e_2007 == "Ja" | sexe13f_2007 == "Ja" | sexe13g_2007 == "Ja" | sexe13h_2007 == "Ja" | sexe13i_2007 == "Ja" | sexe13j_2007 == "Ja" | sexe13k_2007 == "Ja" ~ "Yes",
    sexe13b_2007 == "Nei" | sexe13c_2007 == "Nei" | sexe13d_2007 == "Nei" | sexe13e_2007 == "Nei" | sexe13f_2007 == "Nei" | sexe13g_2007 == "Nei" | sexe13h_2007 == "Nei" | sexe13i_2007 == "Nei" | sexe13j_2007 == "Nei" | sexe13k_2007 == "Nei" ~ "No"),
    sexvioa13 = factor(sexvioa13, levels = c("No","Yes"))
  )

uvreg %>% 
  count(sexvioa13)

### Verbal violence

## From mother

# At least one instance of verbal violence from the mother before 13
uvreg <- uvreg %>% 
  mutate(viomoth_verb1 = case_when(
    morv3_2007 == "Ja" | morv4_2007 == "Ja" | morv5_2007 == "Ja" ~ "Yes",
    morv3_2007 == "Nei" | morv4_2007 == "Nei" | morv5_2007 == "Nei" ~ "No"),
    viomoth_verb1 = factor(viomoth_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_verb1)

# At least one instance of verbal violence from the mother after 13
uvreg <- uvreg %>% 
  mutate(viomoth_verb2 = case_when(
    morv15_2007 == "Ja" | morv16_2007 == "Ja" | morv17_2007 == "Ja" ~ "Yes",
    morv15_2007 == "Nei" | morv16_2007 == "Nei" | morv17_2007 == "Nei" ~ "No"),
    viomoth_verb2 = factor(viomoth_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_verb2)

# At least one instance of verbal violence from the mother ever
uvreg <- uvreg %>% 
  mutate(viomoth_verb = case_when(
    viomoth_verb1 == "Yes" | viomoth_verb2 == "Yes" ~ "Yes",
    viomoth_verb1 == "No" | viomoth_verb2 == "No" ~ "No"),
    viomoth_verb = factor(viomoth_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viomoth_verb)

## From father

# At least one instance of verbal violence from the father before 13
uvreg <- uvreg %>% 
  mutate(viofath_verb1 = case_when(
    farv3_2007 == "Ja" | farv4_2007 == "Ja" | farv5_2007 == "Ja" ~ "Yes",
    farv3_2007 == "Nei" | farv4_2007 == "Nei" | farv5_2007 == "Nei" ~ "No"),
    viofath_verb1 = factor(viofath_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_verb1)

# At least one instance of verbal violence from the father after 13
uvreg <- uvreg %>% 
  mutate(viofath_verb2 = case_when(
    farv15_2007 == "Ja" | farv16_2007 == "Ja" | farv17_2007 == "Ja" ~ "Yes",
    farv15_2007 == "Nei" | farv16_2007 == "Nei" | farv17_2007 == "Nei" ~ "No"),
    viofath_verb2 = factor(viofath_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_verb2)

# At least one instance of verbal violence from the father ever
uvreg <- uvreg %>% 
  mutate(viofath_verb = case_when(
    viofath_verb1 == "Yes" | viofath_verb2 == "Yes" ~ "Yes",
    viofath_verb1 == "No" | viofath_verb2 == "No" ~ "No"),
    viofath_verb = factor(viofath_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viofath_verb)

## From any parent

# At least one instance of verbal violence from any parent before 13
uvreg <- uvreg %>% 
  mutate(viopar_verb1 = case_when(
    viomoth_verb1 == "Yes" | viofath_verb1 == "Yes" ~ "Yes",
    viomoth_verb1 == "No" | viofath_verb1 == "No" ~ "No"),
    viopar_verb1 = factor(viopar_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_verb1)

# At least one instance of verbal violence from any parent after 13
uvreg <- uvreg %>% 
  mutate(viopar_verb2 = case_when(
    viomoth_verb2 == "Yes" | viofath_verb2 == "Yes" ~ "Yes",
    viomoth_verb2 == "No" | viofath_verb2 == "No" ~ "No"),
    viopar_verb2 = factor(viopar_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_verb2)

# At least one instance of verbal violence from any parent ever
uvreg <- uvreg %>% 
  mutate(viopar_verb = case_when(
    viomoth_verb == "Yes" | viofath_verb == "Yes" ~ "Yes",
    viomoth_verb == "No" | viofath_verb == "No" ~ "No"),
    viopar_verb = factor(viopar_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopar_verb)

## From peers

# At least one instance of verbal violence from peers the previous 12 months
uvreg <- uvreg %>% 
  mutate(viopeer_verb1 = case_when(
    volda2_2007 == "Ja" | volda3_2007 == "Ja" | volda4_2007 == "Ja" ~ "Yes",
    volda2_2007 == "Nei" | volda3_2007 == "Nei" | volda4_2007 == "Nei" ~ "No"),
    viopeer_verb1 = factor(viopeer_verb1, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopeer_verb1)

# At least one instance of verbal violence from peers before the previous 12 months
uvreg <- uvreg %>% 
  mutate(viopeer_verb2 = case_when(
    voldb2_2007 == "Ja" | voldb3_2007 == "Ja" | voldb4_2007 == "Ja" ~ "Yes",
    voldb2_2007 == "Nei" | voldb3_2007 == "Nei" | voldb4_2007 == "Nei" ~ "No"),
    viopeer_verb2 = factor(viopeer_verb2, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopeer_verb2)

# At least one instance of verbal violence from peers ever
uvreg <- uvreg %>% 
  mutate(viopeer_verb = case_when(
    viopeer_verb1 == "Yes" | viopeer_verb2 == "Yes" ~ "Yes",
    viopeer_verb1 == "No" | viopeer_verb2 == "No" ~ "No"),
    viopeer_verb = factor(viopeer_verb, levels = c("No","Yes"))
  )

uvreg %>% 
  count(viopeer_verb)

### Educational attainment in 2016
uvreg$eduF2016_rec <- fct_recode(uvreg$eduF2016, Ungdomsskole = "Ungdomsskoleutdanning (8.-10. klassetrinn)",
                                 Videregaende = "Videregående, avsluttende utdanning (13. klassetrinn +)",
                                 Videregaende = "Påbygging til videregående utdanning (14. klassetrinn +)",
                                 Universitet_lavere = "Universitets- og høgskoleutdanning, lavere nivå (14.-17. klassetrinn)",
                                 Universitet_hoyere = "Universitets- og høgskoleutdanning, høyere nivå (18.-19. klassetrinn)",
                                 Universitet_hoyere = "Forskerutdanning (20. klassetrinn +)")

uvreg$eduF2016_rec <- fct_drop(uvreg$eduF2016_rec) # drops empty factor levels

uvreg %>% 
  count(eduF2016_rec)

### Confounders

## Gender
uvreg <- uvreg %>% 
  mutate(female = case_when(
    kjoenn == "Kvinne" ~ 1,
    kjoenn == "Mann" ~ 0)
  )

uvreg %>% 
  count(female)

## Parental education
uvreg <- uvreg %>% 
  mutate(paredu = case_when(
    SOSBAK == "Grunnskole. Mor eller far eller begge har utdanning på nivå 0, 1 eller 2" ~ 0,
    SOSBAK == "Videregående. Mor eller far eller begge har utdanning på nivå 3, 4 eller 5" ~ 1,
    SOSBAK == "Kort høyere. Mor eller far eller begge har utdanning på nivå 6" ~ 2,
    SOSBAK == "Lang høyere. Mor eller far eller begge har utdanning på nivå 7 el. 8" ~ 3)
  )

uvreg %>% 
  count(paredu)

## Migration background
uvreg <- uvreg %>% 
  mutate(invkat_dik = case_when(
    invkat == "Innvandrere" ~ 1,
    invkat == "Norskfødte med innvandrerforeldre" ~ 1,
    TRUE ~ 0)
  )

uvreg %>% 
  count(invkat_dik)

## Single-parent families at age 16
uvreg <- uvreg %>% 
  mutate(singpar = case_when(
    fodeaar == 1987 & (famtype_2003 == "Mor med barn" | famtype_2003 == "Far med barn") ~ 1,
    fodeaar == 1988 & (famtype_2004 == "Mor med barn" | famtype_2004 == "Far med barn") ~ 1,
    fodeaar == 1989 & (famtype_2005 == "Mor med barn" | famtype_2005 == "Far med barn") ~ 1,
    TRUE ~ 0)
    )

uvreg %>% 
  count(singpar)