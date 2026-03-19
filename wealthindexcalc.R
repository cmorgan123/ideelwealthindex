# Overview-------------------------
# This is a script to guide categorization of household and individual demographic variables as inputs for a principle components analysis to calculate a wealth index of survey participants.
# Recommend reading : Vyas et al (2006) and Howe et al (2008)
# This method of estimating wealth/socioeconomic status is imperfect. It will not provide indicators of absolute wealth, but rather relative differences between groups of participants in a given study

# key points: it is important there is variability in data inputs. if 1 out of 500 participants owns a car, this is not a very useful variable to include in the PCA
# data inputs should not be missing

##House attributes----------------------
# House structure: roof, window, flooring, walls

### Roof------------
#roof type - create indicator for modern or not, following https://gh.bmj.com/content/5/6/e002316.long, see supplement
# reflective of divide by expensive vs cheaper for wealth index
# Modern roof: metal, zinc/cement, tiles/slate, or cement (options=7, 9, 10, 11, 12 )
# 1, 01 = Chaume/palme/feuilles | 2, 02 = Mottes de terre | 3, 03 = Nattes | 4, 04 = Palmes/bambou | 5, 05 = Planches en bois | 6, 06 = Carton | 7, 07 = Tôle | 8, 08 = Bois | 9, 09 = Zinc/fibre de ciment | 10, 10 = Tuiles | 11, 11 = Béton (ciment) | 12, 12 = Shingles | 97, 97 = Autre | 98, 98 = Ne sait pas | 99, 99 = Refusé(e)

hhdata1 = hhdata1 %>%
  mutate(modernroof = case_when(
    h1_roof_type___7 == 1  ~ 1, #sheet metal
    h1_roof_type___9 >= 1  ~ 1, # zinc fiber
    h1_roof_type___10 == 1  ~ 1, # tiles
    h1_roof_type___11 == 1  ~ 1, #ciment
    h1_roof_type___12 == 1  ~ 1, # shingles
    h1_roof_type___7 == 1  ~ 1,
    h1_roof_type___1 == 1  ~ 0, # palm/leaves
    h1_roof_type___2 == 1  ~ 0, # clods of earth
    h1_roof_type___3 == 1  ~ 0, # Mats
    h1_roof_type___4 == 1  ~ 0, # bamboo
    h1_roof_type___5 == 1  ~ 0, # wooden planks
    h1_roof_type___6 == 1  ~ 0, # cardboard/plywood
    h1_roof_type___8 == 1  ~ 0, # Wood
    # no 97, 98, 99, and other
    TRUE ~ NA_real_) %>% as.numeric())

addmargins(table(hhdata1$modernroof,useNA = "always"))

# select out missing obs to check
# do these truly not have rooves?
missingroof <- hhdata1 %>% 
  dplyr::select( "hrhhid","h10_hbv_rdt", "hdov", starts_with("h1_roof")) %>% 
  filter(is.na(hhdata1$modernroof))

### Walls----------------------------------------
# Modern wall: cement, stone, bricks, or covered adobe (31, 32, 33, 34, 35)

hhdata1 = hhdata1 %>%
  mutate(modernwalls = case_when(
    h1_walls_type___1 == 1  ~ 0, #earth
    h1_walls_type___2 == 1  ~ 0, # bamboo/palms/trunks
    h1_walls_type___3 == 1  ~ 0, # bamboo w mud
    h1_walls_type___4 == 1  ~ 1, #stones with mud #****check this
    h1_walls_type___5 == 1  ~ 0, # uncovered adobe
    h1_walls_type___6 == 1  ~ 0, # plywood
    h1_walls_type___7 == 1  ~ 0, # cardboard
    h1_walls_type___8 == 1  ~ 0, # reclaimed wood
    h1_walls_type___9 == 1  ~ 1, # cement
    h1_walls_type___10 == 1  ~ 1, # stones with cement
    h1_walls_type___11 == 1  ~ 1, # bricks
    h1_walls_type___12 == 1  ~ 1, # cement blocks
    h1_walls_type___13 == 1  ~ 1, # covered adobe
    h1_walls_type___14 == 1  ~ 1, # wood planks/shingles
    h1_walls_otherlist == "Tol"  ~ 0, # tol/tole = sheet metal - not modern
    h1_walls_otherlist == "Tole"  ~ 0, # tol/tole = sheet metal
    h1_walls == 0 ~ 0, 
    # no 98, 99, and other
    TRUE ~ NA_real_) %>% as.numeric())
addmargins(table(hhdata1$modernwalls,useNA = "always"))
# no missing

### Flooring----------------------------------------
# Modern floor: vinyl, asphalt, ceramic tiles, cement, or carpet
hhdata1 = hhdata1 %>%
  mutate(modernfloor = case_when(
    h1_floor_type___1 == 1  ~ 0, #earth
    h1_floor_type___2 == 1  ~ 0, # dung, none
    h1_floor_type___3 == 1  ~ 0, # wooden planks
    h1_floor_type___4 == 1  ~ 0, # bamboo/palm leaves
    h1_floor_type___5 == 1  ~ 1, # parquet/polished wood
    h1_floor_type___6 == 1  ~ 1, # vinyl/asphalt
    h1_floor_type___7 == 1  ~ 1, # tiles
    h1_floor_type___8 == 1  ~ 1, # cement
    h1_floor_type___9 == 1  ~ 1, # carpet
    # no 97, 98, 99, and other
    TRUE ~ NA_real_) %>% as.numeric())
addmargins(table(hhdata1$modernfloor,useNA = "always"))
# no missing

### Windows----------------------------------------
# Modern windows: glass or screens
table(hhdata1$h1_windows_otherlist, hhdata1$h1_windows_type___98, useNA = "always")

hhdata1 = hhdata1 %>%
  mutate(modernwindow = case_when(
    h1_windows == 0 ~ 0, # no windows
    is.na(h1_windows) ~ 0, # no windows
    h1_windows_type___1 == 1  ~ 1, #glass
    h1_windows_type___2 == 1  ~ 1, # screen
    h1_windows_type___3 == 1  ~ 0, # open
    h1_windows_type___4 == 1  ~ 0, # plastic/paper/carton
    h1_windows_type___5 == 1  ~ 0, # planks
    h1_windows_type___97 == 1 ~ 0, # all 'other' are wood or metal
    h1_windows_type___98 == 1 ~ 0, # don't know: count as don't know since screen/glass only one
    TRUE ~ NA_real_) %>% as.numeric())
addmargins(table(hhdata1$modernwindow,useNA = "always"))
table(hhdata1$h1_windows, hhdata1$modernwindow,useNA = "always")

# no missing

### Holes in wall/house------------
addmargins(table(hhdata1$h2_walls_holes, hhdata1$modernwalls, useNA = "always"))

# Modern housing: modernroof==1, modernwalls==1, modernfloor==1, modernwindows==1, h2_walls_holes==0
hhdata1 = hhdata1 %>%
  mutate(modernhousing = case_when(
    modernroof == 1 & modernwalls == 1  & modernfloor == 1  & modernwindow == 1  & h2_walls_holes == 0 ~ 1,
    TRUE ~ 0) %>% as.numeric())
addmargins(table(hhdata1$modernhousing, useNA = "always"))
addmargins(table(hhdata1$modernhousing, hhdata1$modernfloor ,useNA = "always")) 


## Household objects-----------------------------
table(hhdata1$h4_hh_member_wealth___1)       

hhdata1 <- hhdata1 %>% 
  rename(h4_hh_member_wealth_watch = h4_hh_member_wealth___1,
         h4_hh_member_wealth_cellph = h4_hh_member_wealth___2,
         h4_hh_member_wealth_canoe = h4_hh_member_wealth___3,
         h4_hh_member_wealth_moto = h4_hh_member_wealth___4,
         h4_hh_member_wealth_car = h4_hh_member_wealth___5,
         h4_hh_member_wealth_animalcart = h4_hh_member_wealth___6,         
         h4_hh_member_wealth_motorboat = h4_hh_member_wealth___7,
         h4_hh_member_wealth_bike = h4_hh_member_wealth___8,
         h4_hh_member_wealth_comp = h4_hh_member_wealth___9,
         h4_hh_member_wealth_houserent = h4_hh_member_wealth___10)

table(hhdata1$h4_hh_member_wealth___98) # two reports don't know      
table(hhdata1$h4_hh_member_wealth___99) # none refuse      

## Land owned--------------------------------------------------
addmargins(table(hhdata1$h5_cultivable_land, useNA = "always")) # one refusal, one missing
addmargins(table(hhdata1$h5a_cultivable_land_hect, useNA = "always"))
table(hhdata1$h5_cultivable_land, hhdata1$h5a_cultivable_land_hect ,useNA = "always") # assign missing hectaires to 0 if fam reported not ownership

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5_cultivable_land_f=factor(
    hhdata1$h5_cultivable_land, 
    levels = c(0, 1, 99),
    labels = c("No", "Yes", "Refused")))

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5_cultivable_land_wi = case_when(
    h5_cultivable_land == 0 ~ 0,
    h5_cultivable_land == 1 ~ 1,
    h5_cultivable_land == 99 ~ 1, # assigning single refusal to yes
    #h5_cultivable_land == 99 ~ NA_real_, # other option to assign refused (n=1) to missing (but not included in wealth index)
    is.na(h5_cultivable_land) ~ 0, # assign missing (n=1) to 0
    TRUE ~ h5_cultivable_land))
addmargins(table(hhdata1$h5_cultivable_land_wi, useNA = "always"))

hhdata1$h5a_cultivable_land_hect_num <- as.numeric(hhdata1$h5a_cultivable_land_hect)
hhdata1$h5a_cultivable_land_hect_num <- ifelse(hhdata1$h5_cultivable_land==0,0,hhdata1$h5a_cultivable_land_hect_num) # can give all 0 if missing based above analysis above
table(hhdata1$h5_cultivable_land, hhdata1$h5a_cultivable_land_hect_num ,useNA = "always")

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5a_cultivable_land_hect_char = case_when(
    h5_cultivable_land == 0 ~ 0,
    h5a_cultivable_land_hect_num == 1 ~ 1,
    h5a_cultivable_land_hect_num == 2 ~ 2,
    h5a_cultivable_land_hect_num == 3 ~ 3,
    h5a_cultivable_land_hect_num == 4 ~ 4,
    h5a_cultivable_land_hect_num == 20 ~ 20,
    h5a_cultivable_land_hect_num == 200 ~ 200,
    h5a_cultivable_land_hect_num > 95 & h5a_cultivable_land_hect_num <= 98 ~ 98, # collapsing don't knows
    h5a_cultivable_land_hect_num == 99  ~ 99, # refusal
    is.na(h5a_cultivable_land_hect_num) ~ 99, # refusal
    TRUE ~ NA_real_))

hhdata1 <- hhdata1 %>% 
  dplyr::mutate(h5a_cultivable_land_hect_char_f=factor(
    hhdata1$h5a_cultivable_land_hect_char, 
    levels = c(0, 1, 2, 3, 4, 20, 200, 98,99),
    labels = c("No land", "1 hectare", "2 hectares", "3 hectares", "4 hectares", "20 hectares", "200 hectares", "Exact quantity unknown","Refuse to say")))
addmargins(table(hhdata1$h5a_cultivable_land_hect_char_f, useNA = "always"))

missingland <- hhdata1 %>% filter(is.na(h5_cultivable_land) | h5_cultivable_land==99)

## Water source------------------------------------------------------
# explore frequency of different water sources
table(hhdata1$h6_water_access___3, hhdata1$h6_water_access___1)
table(hhdata1$h6_water_access___6, hhdata1$h6_water_access___3) 
table(hhdata1$h6_water_access___99) #97 = other, 98 = don't know, 99 = refused
table(hhdata1$h6_water_access_otherlist, hhdata1$h6_water_access___4) #97 = other, 98 = don't know, 99 = refused
# forage, Forage, Forrage = borehole

# not uncommon to have multiple sources - could reflect wealth to have access to multiple. 
# if only or at least one source is piped to house, code as 3 (best)
# if piped to neighbor, code as 2 (next best)
# if communal tap/protected borehole, code as 1 (next best to have some distinction in wealth index)
# if only spring or no source, 0.

hhdata1 = hhdata1 %>%
  mutate(privatewater = case_when(
    h6_water_access___1 == 1 ~ 3, #private piped water
    h6_water_access___2 == 1 ~ 2, # piped water from neighbor
    h6_water_access___3 == 1 ~ 1, # communal tap
    h6_water_access___4 == 1 ~ 1, # protected well
    h6_water_access___6 == 1 ~ 0, #  spring water
    h6_water_access_otherlist == "forage" ~ 1,
    h6_water_access_otherlist == "Forage" ~ 1,
    h6_water_access_otherlist == "Forrage" ~ 1,
    TRUE ~ NA_real_) %>% as.numeric())

table(hhdata1$privatewater, useNA = "always")
table(hhdata1$privatewater, hhdata1$h6_water_access___6)

## Cooking source------------------------------------------------------
addmargins(table(hhdata1$h7_cooking_fuel___1,hhdata1$h7_cooking_fuel___2,hhdata1$h7_cooking_fuel___3  ))

hhdata1 = hhdata1 %>%
  mutate(cookfuel = case_when(
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 0 ~ 0, # charcoal only
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 0 ~ 0, # charcoal only
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 0 ~ 1, # charcoal plus gas 
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 1 ~ 1, # charcoal plus electric 
    h7_cooking_fuel___1 == 1 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 1 ~ 2, # all 3
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 0 ~ 3, # gas only, none with this
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 0 & h7_cooking_fuel___3 == 1 ~ 3, # electric only
    h7_cooking_fuel___1 == 0 & h7_cooking_fuel___2 == 1 & h7_cooking_fuel___3 == 1 ~ 3, # electric or gas 
    TRUE ~ NA_real_) %>% as.numeric())
#Summary of cooking categorizations
# having electric with/without gas backup but no charcoal highest standard of living
# using charcoal lower standard
# if electric/gas with charcoal this lower than electric/gas alone
# charcoal plus one of the other below having all 3
# charcoal only lowest

table(hhdata1$cookfuel, useNA = "always")

## People in household per bed--------------------------------------------------
table(hhdata1$n, useNA = "always")
table(hhdata1$h3_hh_wealth_beds, useNA = "always") # yes/no to owning beds, not number

# Make wealth index------------------------------------------------------
## subset dataset for prcomp()-----------------
wealthpca_hoverhh <- hhdata1[, c("hrhhid", "n",
                                 #"cookfuel", 
                                 "privatewater", "modernwalls", "modernfloor", 
                                 #"modernroof", 
                                 "modernwindow",
                                 "h5_cultivable_land_wi",
                                 "h3_hh_wealth_electr" ,
                                 "h3_hh_wealth_toilet",
                                 "h3_hh_wealth_radio",
                                 "h3_hh_wealth_tv" ,
                                 "h3_hh_wealth_fridge" ,
                                 "h3_hh_wealth_cooktop",
                                 "h3_hh_wealth_generator", 
                                 "h3_hh_wealth_beds" ,
                                 "h3_hh_wealth_lamps" ,
                                 "h3_hh_wealth_over",
                                 "h3_hh_wealth_hoes" ,
                                 "h3_hh_wealth_sewing", 
                                 "h4_hh_member_wealth_watch", "h4_hh_member_wealth_cellph",  "h4_hh_member_wealth_canoe",   
                                 "h4_hh_member_wealth_moto", "h4_hh_member_wealth_car", "h4_hh_member_wealth_animalcart","h4_hh_member_wealth_motorboat", "h4_hh_member_wealth_bike", "h4_hh_member_wealth_comp", "h4_hh_member_wealth_houserent")]

##Look at overall distribut of data and remove vars with low variability-----------------
summary(wealthpca_hoverhh) # look at the distributions of all possible variables to include
##observations##
# few have animal cart h4_hh_member_wealth_animalcart
# few have motorboat h4_hh_member_wealth_motorboat
# no one has a canoe
# is radio necessary - remove?

# drop these variables
wealthpca_hoverhh <- wealthpca_hoverhh %>% dplyr::select(!c(h4_hh_member_wealth_animalcart,h4_hh_member_wealth_motorboat, h4_hh_member_wealth_canoe,h3_hh_wealth_radio))

## complete case----------

wealthpca_hoverhh_nomiss <- wealthpca_hoverhh[complete.cases(wealthpca_hoverhh), ]
view(wealthpca_hoverhh_nomiss)
# which IDs dropped
notcompcase <- subset(wealthpca_hoverhh, !(wealthpca_hoverhh$hrhhid %in% wealthpca_hoverhh_nomiss$hrhhid))
# none dropped 

## Run PCA--------------
pca_hover <- prcomp(as.matrix(wealthpca_hoverhh_nomiss[, 3:25]), scale=TRUE)#change to 28
summary(pca_hover) # assess % of variance that first component explains
# this feels low (Odum Institute, talking with other students, and the internet suggest that you could include multiple components, to reach closer to 80%, but Marcel's group (see papers cited in AVERT supplement) only used the first component, so we followed that precedent).
pca_R_output <- as.data.frame(pca_hover$x)
# pca_R_output$subject_id <- rownames(pca_R_output)

pca_R_output <- cbind(pca_R_output, wealthpca_hoverhh_nomiss$hrhhid)
#Create percentiles and wealth index
summary(pca_R_output$PC1)

## look at distribution------------
ggplot(pca_R_output)+
  geom_histogram( aes(PC1), binwidth = 0.5)+
  ggtitle("Distribution of values for Principal Component 1")+
  xlab("Principal component 1")+
  ylab("Count of participants")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)
  )

hist(pca_R_output$PC1, breaks=seq(-6,6,0.5))+title("Value of Principal Component 1")
quantile(pca_R_output$PC1, probs = c(0.25, 0.5,0.75, 1), na.rm = T)

## Scree plot for supplementary material-----------
var_explained_df <- data.frame(PC= paste0("PC", 1:23), #or 26
                               var_explained=(pca_hover$sdev)^2/sum((pca_hover$sdev)^2))

head(var_explained_df)
var_explained_df$PC <- factor(var_explained_df$PC, levels = var_explained_df$PC[order(-var_explained_df$var_explained)])

var_explained_df %>%
  ggplot(aes(x=PC,y=var_explained, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: PCA on scaled data")+
  ylab("Variance explained by each component")+
  xlab("Principal component")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
view(var_explained_df)

## Determine cutoffs for percentiles (quartiles vs quintiles vs other)----------------
quantile(pca_R_output$PC1, probs = c(0.25, 0.5,0.75, 1), na.rm = T)

# Use the above cutoffs as values for wealth_R
pca_R_output$wealth_R <- as.numeric(0)
pca_R_output = pca_R_output %>%
  mutate(wealth_R = case_when(
    PC1 <= -1.3286 ~ 0,
    PC1 > -1.3286 & PC1 <= -0.3221 ~ 1,
    PC1 > -0.3221 & PC1 <= 1.1651 ~ 2,
    PC1 > 1.1651  ~ 3,
    TRUE ~ wealth_R
  ) %>% as.numeric())

table(pca_R_output$wealth_R, useNA = "always")

#Merge new wealth index onto main dataset---------------------------
# rename PID variable so it matches enrollment and merge will work in next step
pca_R_output <- pca_R_output %>% rename(hrhhid = `wealthpca_hoverhh_nomiss$hrhhid`)
# add new wealth variable back onto main enrollment dataset

hhdata1 <- inner_join(hhdata1, pca_R_output[, c("hrhhid", "wealth_R")], by = c("hrhhid"))
