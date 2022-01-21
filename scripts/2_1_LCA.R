### Latent Class Analyses

## Prepare data file
lca_vio <- uvreg %>%
  select(viopeer_verb, viopeer_phys, viopar_verb, viopar_phys, viowitpar_verb, viowitpar_phys, viowit_sib, sexviob13, sexvioa13, 
         female, paredu, invkat_dik, singpar, eduF2016_rec) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate(eduF2016_rec = eduF2016_rec-1)

# write a CSV datafile (preferable format for reading into R, without labels)
write_csv(lca_vio, here("data", "recoded", "lca_vio.csv"))

# read the unlabeled data back into R
nolabel_data <- read_csv(here("data", "recoded", "lca_vio.csv"))

# write a DAT datafile (this function removes header row & converts missing values to non-string)
prepareMplusData(nolabel_data, here("data", "recoded", "lca_vio.dat")) # for permanent storage
prepareMplusData(nolabel_data, "M:/p497-larsrf/lca_vio.dat") # correct path for analyses

# renaming variables to mplus standard of less than 8 characters
new_names <- nolabel_data %>% 
  rename( verbpeer  = viopeer_verb ,
          physpeer = viopeer_phys ,
          verbpar = viopar_verb ,
          physpar  = viopar_phys ,
          witparve = viowitpar_verb ,
          witparph = viowitpar_phys ,
          vwit_si = viowit_sib ,
          sexvb13 = sexviob13 ,
          sexva13 = sexvioa13 ,
          female = female ,
          paredu = paredu ,
          invkat = invkat_dik ,
          singpar = singpar ,
          eduF2016 = eduF2016_rec) 

## Running 1-8 latent class analyses in Mplus
lca_k1_8  <- lapply(1:8, function(k) {
  lca_enum  <- mplusObject(
    
    TITLE = glue("Class {k};"), 
    
    VARIABLE = glue(
      "categorical = verbpeer-sexva13; 
      usevar = verbpeer-sexva13;
      auxiliary = female paredu invkat singpar eduF2016;
      classes = c({k}); "),
    
    ANALYSIS = 
    "estimator = mlr; 
    type = mixture;
    stseed = 5212020;
    starts = 1000 250;
    lrtstarts = 0 0 200 50;
    k-1starts = 750 6;
    processors = 10;",

    OUTPUT = 
    "cinterval svalues entropy residual tech11 tech14;",
    
    PLOT = 
    "type = plot3; 
    series = verbpeer-sexva13(*);",
    
    usevariables = colnames(new_names),
    rdata = new_names)
  
  lca_enum_fit <- mplusModeler(lca_enum, 
                               dataout=glue("Z:/home/p497-larsrf/lca_vio.dat"),
                               modelout=glue("Z:/home/p497-larsrf/c{k}_lca_vio.inp"),
                               check=TRUE, run = TRUE, hashfilename = FALSE)
})

## Step 1 of BCH auxiliary variable analysis
BCH_step1  <- mplusObject(
  
  TITLE = "Five-class model;", 
  
  VARIABLE = 
    "categorical = verbpeer-sexva13; 
    usevar = verbpeer-sexva13;
    auxiliary = female paredu invkat singpar eduF2016;
    classes = c(5); ",
  
  ANALYSIS = 
    "estimator = mlr; 
    type = mixture;
    stseed = 5212020;
    starts = 1000 250;
    lrtstarts = 0 0 200 50;
    k-1starts = 750 6;
    processors = 10;",
  
  MODEL =
    "%overall%
    [c#1*] (c1);
    [c#2*] (c2);
    [c#3*] (c3);
    [c#4*] (c4);",
  
  MODELCONSTRAINT =
    "NEW(PROB1 PROB2 PROB3 PROB4 PROB5);
    PROB1 = EXP(C1) / (1+EXP(C1)+EXP(C2)+EXP(C3)+EXP(C4));
    PROB2 = EXP(C2) / (1+EXP(C1)+EXP(C2)+EXP(C3)+EXP(C4));
    PROB3 = EXP(C3) / (1+EXP(C1)+EXP(C2)+EXP(C3)+EXP(C4));
    PROB4 = EXP(C4) / (1+EXP(C1)+EXP(C2)+EXP(C3)+EXP(C4));
    PROB5 = 1 / (1+EXP(C1)+EXP(C2)+EXP(C3)+EXP(C4));",
  
  OUTPUT =
    "cinterval svalues entropy residual tech11 tech14;",
  
  PLOT = 
    "type = plot3; 
    series = verbpeer-sexva13(*);",
  
  SAVEDATA =
    "File=2step_BCH_savedata.dat;
    Save=bchweights;
    Missflag= .;",
  
  usevariables = colnames(new_names),
  rdata = new_names)

BCH_step1_fit <- mplusModeler(BCH_step1, 
                              dataout=glue("Z:/home/p497-larsrf/Step1_2step_BCH.dat"),
                              modelout=glue("Z:/home/p497-larsrf/Step1_2step_BCH.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)

## Step 2 of BCH auxiliary variable analysis
savedata <- as.data.frame(BCH_step1_fit[["results"]]
                          [["savedata"]])

BCH_step2 <- mplusObject(
  TITLE = "Step2_3step BCH Violent victimization;",
  VARIABLE =
    "usevar = female paredu invkat singpar eduF2016 bchW1-bchW5;
    classes = c(5);
    training = bchW1-bchW5(bch);",
  DEFINE =
    "Center female (Grandmean);
    Center paredu (Grandmean);
    Center singpar (Grandmean);
    Center invkat (Grandmean);",
  ANALYSIS =
    "estimator = mlr;
    type = mixture;
    stseed = 5212020;
    starts = 0;",
  MODEL =
    "!DISTAL = eduF2016
    !MEDIATORS = female paredu invkat singpar

    %OVERALL%
    C on female paredu invkat singpar;
    eduF2016 on female paredu invkat singpar;
    eduF2016;

    %C#1%
    [eduF2016] (m11);
    eduF2016;
    eduF2016 on female paredu invkat singpar (s11_1 s11_2 s11_3 s11_4);

    %C#2%
    [eduF2016] (m21);
    eduF2016;
    eduF2016 on female paredu invkat singpar (s21_1 s21_2 s21_3 s21_4);

    %C#3%
    [eduF2016] (m31);
    eduF2016;
    eduF2016 on female paredu invkat singpar (s31_1 s31_2 s31_3 s31_4);

    %C#4%
    [eduF2016] (m41);
    eduF2016;
    eduF2016 on female paredu invkat singpar (s41_1 s41_2 s41_3 s41_4);

    %C#5%
    [eduF2016] (m51);
    eduF2016;
    eduF2016 on female paredu invkat singpar (s51_1 s51_2 s51_3 s51_4);",
  
  usevariables = colnames(savedata),
  rdata = savedata)

BCH_step2_fit <- mplusModeler(BCH_step2,
                              dataout="Z:/home/p497-larsrf/Step2_2step_BCH.dat",
                              modelout="Z:/home/p497-larsrf/Step2_2step_BCH.inp",
                              check=TRUE, run = TRUE, hashfilename = FALSE)

#### Supplementary latent class analyses

### Separate instruments for violence from mother/father

## Prepare data file
lca_vio2 <- uvreg %>%
  select(viopeer_verb, viopeer_phys, viomoth_verb, viofath_verb, viomoth_phys, viofath_phys, viowitmoth_verb,
         viowitfath_verb, viowitmoth_phys, viowitfath_phys, viowit_sib, sexviob13, sexvioa13) %>% 
  mutate_if(is.factor, as.numeric)

# write a CSV datafile (preferable format for reading into R, without labels)
write_csv(lca_vio2, here("data", "recoded", "lca_vio2.csv"))

# read the unlabeled data back into R
nolabel_data2 <- read_csv(here("data", "recoded", "lca_vio2.csv"))

# write a DAT datafile (this function removes header row & converts missing values to non-string)
prepareMplusData(nolabel_data2, here("data", "recoded", "lca_vio2.dat")) # for permanent storage
prepareMplusData(nolabel_data2, "Z:/home/p497-larsrf/lca_vio2.dat") # correct path for analyses

# renaming variables to mplus standard of less than 8 characters
new_names2 <- nolabel_data2 %>% 
  rename( verbpeer  = viopeer_verb ,
          physpeer = viopeer_phys ,
          verbmoth = viomoth_verb ,
          verbfath = viofath_verb ,
          physmoth  = viomoth_phys ,
          physfath  = viofath_phys ,
          witmothv = viowitmoth_verb ,
          witfathv = viowitfath_verb ,
          witmothp = viowitmoth_phys ,
          witfathp = viowitfath_phys ,
          vwit_si = viowit_sib ,
          sexvb13 = sexviob13 ,
          sexva13 = sexvioa13 )

# running 1-8 latent class analyses in Mplus
lca_k1_8b  <- lapply(1:8, function(k) {
  lca_enum  <- mplusObject(
    
    TITLE = glue("Class {k};"), 
    
    VARIABLE = glue(
      "categorical = verbpeer-sexva13; 
      usevar = verbpeer-sexva13;
      classes = c({k}); "),
    
    ANALYSIS = 
      "estimator = mlr; 
    type = mixture;
    stseed = 5212020;
    starts = 1000 250;
    lrtstarts = 0 0 200 50;
    k-1starts = 750 6;
    processors = 10;",
    
    OUTPUT = "entropy residual tech11 tech14;",
    
    PLOT = 
      "type = plot3; 
    series = verbpeer-sexva13(*);",
    
    usevariables = colnames(new_names3),
    rdata = new_names3)
  
  lca_enum_fit <- mplusModeler(lca_enum, 
                               dataout=glue("Z:/home/p497-larsrf/c_lca_vio2.dat"),
                               modelout=glue("Z:/home/p497-larsrf/c{k}_lca_vio2.inp"),
                               check=TRUE, run = TRUE, hashfilename = FALSE)
})

### Separate instruments for violence before/after 13, before/after previous 12 months

## Prepare data file
lca_vio3 <- uvreg %>%
  select(viopeer_verb1, viopeer_verb2, viopeer_phys1, viopeer_phys2, viopar_verb1, viopar_verb2, viopar_phys1, viopar_phys2,  
         viowitpar_verb1, viowitpar_verb2, viowitpar_phys1, viowitpar_phys2, viowit_sib, sexviob13, sexvioa13) %>% 
  mutate_if(is.factor, as.numeric)

# write a CSV datafile (preferable format for reading into R, without labels)
write_csv(lca_vio3, here("data", "recoded", "lca_vio3.csv"))

# read the unlabeled data back into R
nolabel_data2 <- read_csv(here("data", "recoded", "lca_vio3.csv"))

# write a DAT datafile (this function removes header row & converts missing values to non-string)
prepareMplusData(nolabel_data2, here("data", "recoded", "lca_vio3.dat")) # for permanent storage
prepareMplusData(nolabel_data2, "Z:/home/p497-larsrf/lca_vio3.dat") # correct path for analyses

# renaming variables to mplus standard of less than 8 characters
new_names3 <- nolabel_data3 %>% 
  rename( verbpee1  = viopeer_verb1 ,
          verbpee2  = viopeer_verb2 ,
          physpee1 = viopeer_phys1 ,
          physpee2 = viopeer_phys2 ,
          verbpa1 = viopar_verb1 ,
          verbpa2 = viopar_verb2 ,
          physpar1  = viopar_phys1 ,
          physpar2  = viopar_phys2 ,
          witparv1 = viowitpar_verb1 ,
          witparv2 = viowitpar_verb2 ,
          witparp1 = viowitpar_phys1 ,
          witparp2 = viowitpar_phys2 ,
          vwit_si = viowit_sib ,
          sexvb13 = sexviob13 ,
          sexva13 = sexvioa13 )

# running 1-8 latent class analyses in Mplus
lca_k1_8c  <- lapply(1:8, function(k) {
  lca_enum  <- mplusObject(
    
    TITLE = glue("Class {k};"), 
    
    VARIABLE = glue(
      "categorical = verbpee1-sexva13; 
      usevar = verbpee1-sexva13;
      classes = c({k}); "),
    
    ANALYSIS = 
      "estimator = mlr; 
    type = mixture;
    stseed = 5212020;
    starts = 1000 250;
    lrtstarts = 0 0 200 50;
    k-1starts = 750 6;
    processors = 10;",
    
    OUTPUT = 
      "cinterval svalues entropy residual tech11 tech14;",
    
    PLOT = 
      "type = plot3; 
    series = verbpee1-sexva13(*);",
    
    usevariables = colnames(new_names2),
    rdata = new_names2)
  
  lca_enum_fit <- mplusModeler(lca_enum, 
                               dataout=glue("Z:/home/p497-larsrf/lca_vio3.dat"),
                               modelout=glue("Z:/home/p497-larsrf/c{k}_lca_vio3.inp"),
                               check=TRUE, run = TRUE, hashfilename = FALSE)
})