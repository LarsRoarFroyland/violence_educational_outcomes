### Getting results from LCA
all_output <- readModels(here("output", "LCA", "model_selection"), quiet = TRUE)
all_output2 <- readModels(here("output", "LCA", "sensitivity", "Parents_sep"), quiet = TRUE)
all_output3 <- readModels(here("output", "LCA", "sensitivity", "Severity"), quiet = TRUE)
all_output4 <- readModels(here("output", "LCA", "sensitivity", "Count"), quiet = TRUE)
model_step1 <- readModels(here("output", "LCA", "BCH", "step1_2step_bch.out"), quiet = TRUE)
model_step2 <- readModels(here("output", "LCA", "BCH", "step2_2step_bch.out"), quiet = TRUE)

### Numbers in text

## Comparing register file to complete file
glm.kjonn <- glm(reg ~ kjonn, data = uvcompl, family = binomial)
coef.kjonn <- cbind(format(round(exp(tidy(glm.kjonn)$estimate[2]), 2), nsmall = 2), 
      p_format(tidy(glm.kjonn)$p.value[2], digits = 2, accuracy = 0.01, leading.zero = FALSE, space = TRUE))

glm.innv <- glm(reg ~ innv, data = uvcompl, family = binomial)
coef.innv <- cbind(format(round(exp(tidy(glm.innv)$estimate[2]), 2), nsmall = 2), 
                    p_format(tidy(glm.innv)$p.value[2], digits = 2, accuracy = 0.01, leading.zero = FALSE, space = TRUE))

## Posterior probabilities
plot1 <- as.data.frame(all_output[["c5_lca_vio.out"]]  #
                       [["gh5"]][["means_and_variances_data"]]  #       
                       [["estimated_probs"]][["values"]]  # 
                       [seq(2, 18, 2), ])  #seq('from','to','by')   

### Table 1: descriptives

# select violence instruments
viotab <- uvreg %>% 
  select(viopeer_verb, viopeer_phys, viopar_verb, viopar_phys, viowitpar_verb, viowitpar_phys, viowit_sib, sexviob13, sexvioa13)

# function for descriptives for dichotomous variables 
viotab <- lapply(viotab, function(k) {
  a <- round(sum(k == "Yes", na.rm = TRUE) / sum(!is.na(k)) * 100, digits = 1)
  b <- as.numeric(sum(k == "Yes", na.rm = TRUE))
  str_glue("{a} ({b})")
})

# create table on violence instrucments
viotab <- 
  as.data.frame(viotab) %>% 
  mutate(vio = "", .before = viopeer_verb) %>%                         # add empty header column
  rename(c(                                                            # add correct labels to instruments
    "Type of violence" = vio,
    "Verbal violence from peers" = viopeer_verb,
    "Physical violence from peers" = viopeer_phys,
    "Verbal violence from parents" = viopar_verb,
    "Physical violence from parents" = viopar_phys,
    "Witnessing verbal violence toward a parent" = viowitpar_verb,
    "Witnessing physical violence toward a parent" = viowitpar_phys,
    "Witnessing physical violence toward a sibling" = viowit_sib,
    "Sexual violence before 13" = sexviob13,
    "Sexual violence after 13" = sexvioa13)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
  delete_part(part = "header")                                         # to prepare for merging with other parts of the table

# gender table
kjoenntab <- uvreg %>% 
  summarize(mean_1 = round(sum(kjoenn == "Mann", na.rm = TRUE) / sum(!is.na(kjoenn)) * 100, digits = 1),
            n_1 = comma(as.numeric(sum(kjoenn == "Mann", na.rm = TRUE))),
            mean_2 = round(sum(kjoenn == "Kvinne", na.rm = TRUE) / sum(!is.na(kjoenn)) * 100, digits = 1),
            n_2 = comma(as.numeric(sum(kjoenn == "Kvinne", na.rm = TRUE)))) %>% 
  transmute(Sex = "",                                                 
            Boys = str_glue("{mean_1} ({n_1})"),
            Girls = str_glue("{mean_2} ({n_2})")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
  delete_part(part = "header")                                        

# migration background table
invtab <- uvreg %>% 
  summarize(mean_1 = round(sum(invkat_dik == 0, na.rm = TRUE) / sum(!is.na(invkat_dik)) * 100, digits = 1),
            n_1 = comma(as.numeric(sum(invkat_dik == 0, na.rm = TRUE))),
            mean_2 = round(sum(invkat_dik == 1, na.rm = TRUE) / sum(!is.na(invkat_dik)) * 100, digits = 1),
            n_2 = comma(as.numeric(sum(invkat_dik == 1, na.rm = TRUE)))) %>% 
  transmute(`Migration background` = "",
            No = str_glue("{mean_1} ({n_1})"),
            Yes = str_glue("{mean_2} ({n_2})")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
  delete_part(part = "header")

# table on living with two parents
partab <- uvreg %>% 
  summarize(mean_1 = round(sum(singpar == 0, na.rm = TRUE) / sum(!is.na(singpar)) * 100, digits = 1),
            n_1 = comma(as.numeric(sum(singpar == 0, na.rm = TRUE))),
            mean_2 = round(sum(singpar == 1, na.rm = TRUE) / sum(!is.na(singpar)) * 100, digits = 1),
            n_2 = comma(as.numeric(sum(singpar == 1, na.rm = TRUE)))) %>% 
  transmute(`Single parent family at age 16` = "",
            No = str_glue("{mean_1} ({n_1})"),
            Yes = str_glue("{mean_2} ({n_2})")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
  delete_part(part = "header")

# table on social background
sosbaktab <- uvreg %>% 
  summarize(mean = format(round(mean(paredu, na.rm = TRUE), digits = 2), nsmall = 2),
            sd = format(round(sd(paredu, na.rm = TRUE), digits = 2), nsmall = 2)) %>% 
  transmute(`Parental education at age 16` = str_glue("{mean} ({sd})")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
  delete_part(part = "header")

# table on educational attainment in 2016
edutab <- uvreg %>% 
  summarize(mean = format(round(mean(as.numeric(eduF2016_rec, na.rm = TRUE)-1), digits = 2), nsmall = 2),
            sd = format(round(sd(as.numeric(eduF2016_rec, na.rm = TRUE)), digits = 2), nsmall = 2)) %>% 
  transmute(`Educational attainment in 2016` = str_glue("{mean} ({sd})")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable() %>% 
  delete_part(part = "header")

# combine tables
desctab <-  
  rbind(viotab$body$dataset, 
        edutab$body$dataset,
        kjoenntab$body$dataset, 
        invtab$body$dataset, 
        partab$body$dataset, 
        sosbaktab$body$dataset)

### Table 2: summary table of fit indices (Garber, 2021)

# extract fit indices from LCA output
enum_extract <- LatexSummaryTable(all_output, keepCols=c("Observations", "Title", "Parameters", "LL",
                                                         "BIC", "aBIC", "BLRT_PValue", "T11_VLMR_PValue"),
                                  sortBy = "Title")

# generate appropriate indices
allFit <- enum_extract %>% 
  mutate(Title = str_sub(Title, start = 8, end = 8)) %>%                      # class number
  mutate(aBIC = -2*LL+Parameters*log((Observations+2)/24)) %>%                # sample-size adjusted BIC
  mutate(CAIC = -2*LL+Parameters*(log(Observations)+1)) %>%                   # consistent Akaike information criterion
  mutate(AWE = -2*LL+2*Parameters*(log(Observations)+1.5)) %>%                # approximate weight of evidence criterion
  mutate(BLRT_PValue = case_when(                                             # bootstrapped likelihood ratio test (p-value format)
    !is.na(BLRT_PValue) ~ p_format(BLRT_PValue, 
                                   digits = 3,
                                   accuracy = 0.001,
                                   leading.zero = FALSE,
                                   space = TRUE))) %>% 
  mutate(T11_VLMR_PValue = case_when(                                         # Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test (p-value format)
    !is.na(T11_VLMR_PValue) ~ 
      p_round(T11_VLMR_PValue, digits = 3) %>%
      p_format(T11_VLMR_PValue, 
               digits = 3,
               accuracy = 0.001,
               leading.zero = FALSE,
               space = TRUE))) %>% 
  mutate(SIC = -.5*BIC) %>%                                                   # Schwartz information criterion
  mutate(expSIC = exp(SIC - max(SIC))) %>% 
  mutate(expSUM = sum(expSIC)) %>% 
  mutate(BF_1 = round(exp(SIC-lead(SIC)), digits = 2)) %>%                    # Bayes Factor
  mutate(BF = case_when(
    BF_1 > 10 ~ "> 10",
    BF_1 < 0.1 ~ "< 0.10",
    TRUE ~ as.character(BF_1))) %>% 
  mutate(cmPk_1 = format(round(expSIC/expSUM, digits = 2), nsmall = 2)) %>%   # correct model probability
  mutate(cmPk = case_when(
    cmPk_1 < 0.01 ~ "< .01",
    TRUE ~ as.character(cmPk_1))) %>% 
  select(2,4,3,9,5:6,10,8,7,15,17)

### Table 3: model classification diagnostics table

# names of classes (from analyses)
classes <- c("Witnesses of domestic violence", "Polyvictims", "Peer victims", "Victims of parental violence", "Non-victims")

# order of classes for tables
classord <- c("Non-victims", "Peer victims", "Polyvictims", "Victims of parental violence", "Witnesses of domestic violence")

# model estimated class proportions
cprop <- format(round(model_step1[["class_counts"]][["modelEstimated"]][["proportion"]], digits = 3), nsmall = 3)
cprop <- str_replace(cprop, "0.", ".")

# class counts
ccounts <- model_step1[["class_counts"]][["mostLikely"]][["count"]]

# 95% bootstrapped ci of class proportions
lowCI <- round(model_step1[["parameters"]][["ci.unstandardized"]][["low2.5"]], digits = 3)
lowCI <- lowCI[50:54]
lowCI <- format(lowCI, nsmall = 3)

hiCI <- round(model_step1[["parameters"]][["ci.unstandardized"]][["up2.5"]], digits = 3)
hiCI <- hiCI[50:54]
hiCI <- format(hiCI, nsmall = 3)

CI <- str_glue("[{lowCI}, {hiCI}]")
CI <- str_replace_all(CI, fixed("0."), ".")

# modal class assignment proportion (mcapk)
mcap <- format(round(model_step1[["class_counts"]][["mostLikely"]][["proportion"]], digits = 3), nsmall = 3)
mcap <- str_replace(mcap, "0.", ".")

# average classification probabilities
avepp <- format(round(diag(model_step1[["class_counts"]][["avgProbs.mostLikely"]]), digits = 3), nsmall = 3)
avepp <- str_replace(avepp, "0.", ".")

# occ
cprop_n <- model_step1[["class_counts"]][["modelEstimated"]][["proportion"]]
avepp_n <- diag(model_step1[["class_counts"]][["avgProbs.mostLikely"]])

occ <- round((avepp_n/(1-avepp_n))/(cprop_n/(1-cprop_n)), digits = 2)

# entropy
entropy <- format(round(model_step1[["summaries"]][["Entropy"]], digits = 2), nsmall = 2)
entropy <- str_replace(entropy, "0.", ".")
entropy <- as.data.frame(entropy) %>%            # add empty rows to put entropy in the top row of the final table
  add_row(.before = 1) %>%  
  add_row(.before = 1) %>%  
  add_row(.before = 1) %>% 
  add_row(.before = 1)

# merge numbers into final diagnostics table
diagnostics <- cbind.data.frame(classes, cprop, CI, mcap, avepp, occ, entropy) %>% 
  slice(match(classord, classes))             # changes the order of the classes to match the rest of the paper

### Figure 1 - Conditional item probability plot (Nylund-Gibson et al., 2021)

# class proportions for header
c_size <- as.data.frame(model_step1[["class_counts"]][["modelEstimated"]][["proportion"]])

# function for creating conditional item probability plot
plot_lca_function <- function(model_name,item_num,class_num,item_labels,
                              class_labels,class_legend_order,plot_title){
  mplus_model <- as.data.frame(model_name$gh5$means_and_variances_data$estimated_probs$values)
  plot_data <- mplus_model[seq(2, 2*item_num, 2),]
  c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion)
  colnames(c_size) <- paste0("cs")
  c_size <- c_size %>% mutate(cs = round(cs*100, 1))
  colnames(plot_data) <- paste0(class_labels, glue(" ({c_size[1:class_num,]}%)"))
  plot_data <- plot_data %>% relocate(all_of(class_legend_order))
  plot_data <- cbind(Var = paste0("U", 1:item_num), plot_data)
  plot_data$Var <- factor(plot_data$Var,
                          labels = item_labels)
  plot_data$Var <- fct_inorder(plot_data$Var)
  pd_long_data <- melt(plot_data, id.vars = "Var")

    # This syntax uses the date-frame created above to produce the plot with `ggplot()`
  p <- pd_long_data %>%
    ggplot(aes(x = as.integer(Var), y = value,
               shape = variable, colour = variable, lty = variable)) +
    geom_point(size = 4) + geom_line() +
    scale_x_continuous("", breaks = 1:9, labels = plot_data$Var) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_colour_grey(start = 0, end = 0.6) +
    labs(title = plot_title, y = "Conditional Item Probabilities") +
    theme_cowplot() +
    theme(legend.title = element_blank(),
          legend.position = "top",
          legend.key.width = unit(.5, "line"),
          legend.text = element_text(family="sans", size=18),
          legend.justification = "center",
          axis.text.x = element_text(angle = 90, hjust = .95, vjust = 0.4, size=18),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE))
  p
  return(p)
}

### Figure 2 - socioeconomic attainment in latent classes (Nylund-Gibson et al., 2021)

# extract unstandardized parameters
model_step2_un <- data.frame(model_step2$parameters$unstandardized)

# creates table to plot
distal_data <- model_step2_un %>%
  filter(paramHeader == "Intercepts" & LatentClass != "Categorical.Latent.Variables") %>% 
  mutate(param = case_when(
    param == "EDUF2016" ~ "Obtained education")) %>% 
  mutate(LatentClass = factor(LatentClass,
                              labels = classes)) %>% 
  mutate(LatentClass = fct_relevel(LatentClass, "Non-victims", "Peer victims", "Polyvictims", "Victims of parental violence", "Witnesses of domestic violence")) %>% 
  mutate(value_labels = round(est, digits = 2))

#### Supplementary material

### Supplementary Table 1: Items Used in Instruments of Violent Victimization

# Verbal violence from peers
verbpeer_1 <- "Verbal violence from peers"
verbpeer_2 <- "Have you experienced any of this? I have been..."
verbpeer_3 <- c("...exposed to severe bullying by peers", 
                "...exposed to sexual harrassment", 
                "...threatened with violence")
verbpeer <- cbind(verbpeer_1, verbpeer_2, verbpeer_3)

# Physical violence from peers
physpeer_1 <- "Physical violence from peers"
physpeer_2 <- "Have you experienced any of this? I have..."
physpeer_3 <- c("...been hit without getting bruises",
                "...gotten bruises or injuries as a result of violence but did not need medical assistance",
                "...been injured so badly due to violence that I needed medical assistance")
physpeer <- cbind(physpeer_1, physpeer_2, physpeer_3)

# Verbal violence from parents
verbpar_1 <- "Verbal violence from parents"
verbpar_2 <- "Has your mother/father done any of the following towards you when you have argued?"
verbpar_3 <- c("Insulted or humuliated you",
               "Threw, hit, or kicked something", 
               "Threatened you with violence")
verbpar <- cbind(verbpar_1, verbpar_2, verbpar_3)

# Physical violence from parents
physpar_1 <- "Physical violence from parents"
physpar_2 <- c("Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father done any of the following towards you when you have argued? ",
               "Has your mother/father ever used violence against you that caused...",
               "Has your mother/father ever used violence against you that caused...",
               "Has your mother/father ever used violence against you that caused...") 
physpar_3 <- c("Pushed or shook you violently", 
               "Pulled your hair or pinched you", 
               "Slapped you", 
               "Hit you with the fist", 
               "Hit you with an object", 
               "Beaten you up", 
               "Done any other violent act towards you",
               "...bruises/physical injuries?",
               "...you to be in pain the next day?",
               "...you to need medical assistance?")
physpar <- cbind(physpar_1, physpar_2, physpar_3)

# Witnessing verbal domestic violence
viowitverb_1 <- "Witnessing verbal domestic violence"
viowitverb_2 <- "Have you heard or seen your mother/father experience any of this at home?"
viowitverb_3 <- c("He/she has been insulted or humiliated",
                  "He/she has been threatened with violence")
viowitverb <- cbind(viowitverb_1, viowitverb_2, viowitverb_3)

# Witnessing physical domestic violence
viowitphys_1 <- "Witnessing physical domestic violence"
viowitphys_2 <- c("Have you heard or seen your mother/father experience any of this at home? ",
                  "Have you heard or seen your mother/father experience any of this at home? ",
                  "Have you heard or seen your mother/father experience any of this at home? ",
                  "Have you heard or seen your mother/father experience any of this at home? ",
                  "Have you heard or seen your mother/father experience any of this at home? ",
                  "Have you heard or seen your mother/father experience any of this at home? ",
                  "Have you heard or seen your mother/father experience any of this at home? ",
                  "Has your mother/father been...",
                  "Has your mother/father been...")
viowitphys_3 <- c("She/he was shoved or shaken violently",
                  "She/he was pulled by the hair or pinched",
                  "She/he was slapped",
                  "She/he was hit with the fist",
                  "She/he was hit with an object",
                  "She/he got beaten up",
                  "She/he was exposed to another violent act",
                  "...injured/had bruises caused by violence that she/he was exposed to at home but did not need medical care?",
                  "...injured by violence that she/he was exposed to at home and needed medical care?")
viowitphys <- cbind(viowitphys_1, viowitphys_2, viowitphys_3)

# Witnessing violence towards a sibling
viowitsib_1 <- "Witnessing violence towards a sibling"
viowitsib_2 <- ""
viowitsib_3 <- "Have you ever seen or heard your mother/father using physical violence against a sibling?"
viowitsib <- cbind(viowitsib_1, viowitsib_2, viowitsib_3)

# Sexual violence
sexvio_1 <- "Sexual violence before/after the age of 13"
sexvio_2 <- "Have you been exposed to any of the following against your will?"
sexvio_3 <- c("Someone has groped you",
              "You have touched yourself in front of somebody",
              "You have touched someone else",
              "You have masturbated in front of someone",
              "You have had sexual intercourse",
              "You have had oral sex",
              "You have had anal sex",
              "You have had some other form of sex",
              "You have been exposed to rape attempt(s)",
              "You have been raped")
sexvio <- cbind(sexvio_1, sexvio_2, sexvio_3)

# combine items into final table
qitems <- as.data.frame(rbind(verbpeer, physpeer, verbpar, physpar, viowitverb, viowitphys, viowitsib, sexvio))

### Supplementary Table 2: summary table of fit indices - separate measures for mother and father
enum_extract2 <- LatexSummaryTable(all_output2, keepCols=c("Observations", "Title", "Parameters", "LL",
                                                           "BIC", "aBIC", "BLRT_PValue", "T11_VLMR_PValue"),
                                   sortBy = "Title")

allFit2 <- enum_extract2 %>% 
  mutate(Title = str_sub(Title, start = 8, end = 8)) %>% 
  mutate(aBIC = -2*LL+Parameters*log((Observations+2)/24)) %>% 
  mutate(CAIC = -2*LL+Parameters*(log(Observations)+1)) %>% 
  mutate(AWE = -2*LL+2*Parameters*(log(Observations)+1.5)) %>%
  mutate(BLRT_PValue = case_when(
    !is.na(BLRT_PValue) ~ p_format(BLRT_PValue, 
                                   digits = 3,
                                   accuracy = 0.001,
                                   leading.zero = FALSE,
                                   space = TRUE))) %>% 
  mutate(T11_VLMR_PValue = case_when(
    !is.na(T11_VLMR_PValue) ~ 
      p_round(T11_VLMR_PValue, digits = 3) %>%
      p_format(T11_VLMR_PValue, 
               digits = 3,
               accuracy = 0.001,
               leading.zero = FALSE,
               space = TRUE))) %>% 
  mutate(SIC = -.5*BIC) %>% 
  mutate(expSIC = exp(SIC - max(SIC))) %>% 
  mutate(expSUM = sum(expSIC)) %>% 
  mutate(BF_1 = round(exp(SIC-lead(SIC)), digits = 2)) %>%
  mutate(BF = case_when(
    BF_1 > 10 ~ "> 10",
    BF_1 < 0.1 ~ "< 0.10",
    TRUE ~ as.character(BF_1))) %>% 
  mutate(cmPk_1 = format(round(expSIC/expSUM, digits = 2), nsmall = 2)) %>% 
  mutate(cmPk = case_when(
    cmPk_1 < 0.01 ~ "< .01",
    TRUE ~ as.character(cmPk_1))) %>% 
  select(2,4,3,9,5:6,10,8,7,15,17)

## Supplementary Table 3: summary table of fit indices - separate measures for before/after 13, before/after previous 12 months
enum_extract3 <- LatexSummaryTable(all_output3, keepCols=c("Observations", "Title", "Parameters", "LL",
                                                           "BIC", "aBIC", "BLRT_PValue", "T11_VLMR_PValue"),
                                   sortBy = "Title")

allFit3 <- enum_extract3 %>% 
  mutate(Title = str_sub(Title, start = 8, end = 8)) %>% 
  mutate(aBIC = -2*LL+Parameters*log((Observations+2)/24)) %>% 
  mutate(CAIC = -2*LL+Parameters*(log(Observations)+1)) %>% 
  mutate(AWE = -2*LL+2*Parameters*(log(Observations)+1.5)) %>%
  mutate(BLRT_PValue = case_when(
    !is.na(BLRT_PValue) ~ p_format(BLRT_PValue, 
                                   digits = 3,
                                   accuracy = 0.001,
                                   leading.zero = FALSE,
                                   space = TRUE))) %>% 
  mutate(T11_VLMR_PValue = case_when(
    !is.na(T11_VLMR_PValue) ~ 
      p_round(T11_VLMR_PValue, digits = 3) %>%
      p_format(T11_VLMR_PValue, 
               digits = 3,
               accuracy = 0.001,
               leading.zero = FALSE,
               space = TRUE))) %>% 
  mutate(SIC = -.5*BIC) %>% 
  mutate(expSIC = exp(SIC - max(SIC))) %>% 
  mutate(expSUM = sum(expSIC)) %>% 
  mutate(BF_1 = round(exp(SIC-lead(SIC)), digits = 2)) %>%
  mutate(BF = case_when(
    BF_1 > 10 ~ "> 10",
    BF_1 < 0.1 ~ "< 0.10",
    TRUE ~ as.character(BF_1))) %>% 
  mutate(cmPk_1 = format(round(expSIC/expSUM, digits = 2), nsmall = 2)) %>% 
  mutate(cmPk = case_when(
    cmPk_1 < 0.01 ~ "< .01",
    TRUE ~ as.character(cmPk_1))) %>% 
  select(2,4,3,9,5:6,10,8,7,15,17)

## Supplementary Table 4: summary table of fit indices - counts
enum_extract4 <- LatexSummaryTable(all_output4, keepCols=c("Observations", "Title", "Parameters", "LL",
                                                           "BIC", "aBIC", "BLRT_PValue", "T11_VLMR_PValue"),
                                   sortBy = "Title")

allFit4 <- enum_extract4 %>% 
  mutate(Title = str_sub(Title, start = 8, end = 8)) %>% 
  mutate(aBIC = -2*LL+Parameters*log((Observations+2)/24)) %>% 
  mutate(CAIC = -2*LL+Parameters*(log(Observations)+1)) %>% 
  mutate(AWE = -2*LL+2*Parameters*(log(Observations)+1.5)) %>%
  mutate(BLRT_PValue = case_when(
    !is.na(BLRT_PValue) ~ p_format(BLRT_PValue, 
                                   digits = 3,
                                   accuracy = 0.001,
                                   leading.zero = FALSE,
                                   space = TRUE))) %>% 
  mutate(T11_VLMR_PValue = case_when(
    !is.na(T11_VLMR_PValue) ~ 
      p_round(T11_VLMR_PValue, digits = 3) %>%
      p_format(T11_VLMR_PValue, 
               digits = 3,
               accuracy = 0.001,
               leading.zero = FALSE,
               space = TRUE))) %>% 
  mutate(SIC = -.5*BIC) %>% 
  mutate(expSIC = exp(SIC - max(SIC))) %>% 
  mutate(expSUM = sum(expSIC)) %>% 
  mutate(BF_1 = round(exp(SIC-lead(SIC)), digits = 2)) %>%
  mutate(BF = case_when(
    BF_1 > 10 ~ "> 10",
    BF_1 < 0.1 ~ "< 0.10",
    TRUE ~ as.character(BF_1))) %>% 
  mutate(cmPk_1 = format(round(expSIC/expSUM, digits = 2), nsmall = 2)) %>% 
  mutate(cmPk = case_when(
    cmPk_1 < 0.01 ~ "< .01",
    TRUE ~ as.character(cmPk_1))) %>% 
  select(2,4,3,9,5:6,10,8,7,15,17)

## Supplementary Figure 1: Elbow plot (Nylund-Gibson et al., 2021)
elbow <- allFit %>% 
  select(2:7) %>%
  rowid_to_column() %>%
  pivot_longer(`CAIC`:`AWE`,
               names_to = "Index",
               values_to = "ic_value") %>%
  mutate(Index = factor(Index,
                        levels = c("AWE","CAIC","BIC","aBIC")))

## Supplementary Figure 2: Conditional Item Probability Plots (Nylund-Gibson et al., 2021)
model_results <- data.frame()

for (i in 1:length(all_output)) {
  temp <- all_output[[i]]$parameters$unstandardized
  temp <- data.frame(unclass(temp)) %>%
    mutate(model = paste0(i, "-Class Model"))
  model_results <- rbind(model_results, temp)
}

model_results <- model_results %>%
  mutate(param = ordered(param, levels = unique(param))) %>% 
  filter(paramHeader == "Thresholds") %>%
  select(est, model, LatentClass, param) %>%  
  mutate(prob = (1 / (1 + exp(est))))