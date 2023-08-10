library(janitor)
library(tidyverse)

# Load baseline individual data
baseline_individual <- read.csv("L4H_individual_baseline_sample.csv") %>%
  clean_names()

# Load baseline mother data
baseline_mother <- read.csv("L4H_mother_baseline_sample.csv") %>%
  clean_names()

# Load baseline household data and filter eligible households
baseline_household <- read.csv("L4H_household_baseline_sample.csv") %>%
  clean_names() %>%
  filter(hh_eligible == 1)

# Merge datasets
merged_data <- baseline_individual %>%
  full_join(baseline_mother, by = c("number" = "number_0"))

merged_data <- baseline_household %>%
  full_join(merged_data, by = "household_id")


merged_data1 <- merged_data %>%
  mutate(reason_for_ineligibility = recode(reason_for_ineligibility, "1" = "No adult occupier >16 years", "2" = "Withdrawal", "3" = "Other reason"),
         rspntgndr = recode(rspntgndr, "1" = "male", "2" = "female"),
         h_hfrml_eductn = recode(h_hfrml_eductn, "1" = "Not completed Primary school", "2" = "Primary school", "3" = "Secondary school", "4" = "College-graduate", "5" = "Madrassa", "6" = "Other"),
         rspndtmarital = recode(rspndtmarital, "1" = "Single", "2" = "Married monogamous", "3" = "Married polygamous", "4" = "Divorced", "5" = "Separated", "6" = "Widow(er)"),
         rspndt_eductn = recode(rspndt_eductn, "1" = "No formal education", "2" = "Primary School", "3" = "Secondary school", "4" = "College-graduate", "5" = "Madrassa", "6" = "Other"),
         maincme = recode(maincme, "1" = "Sale of livestock & livestock products", "2" = "Sale of crops", "3" = "Trading/business", "4" = "Employment (salaried income)", "5" = "Sale of personal assets", "6" = "Remittance", "7" = "Other"))


merged_data1 <- merged_data1 %>%
  separate(lvstckown, into = paste0("lvstckown_", 1:15), sep = " ", extra = "merge") %>%
  separate(herdynamics, into = paste0("herdynamics_", 1:9), sep = " ", extra = "merge")

# Create study_arm column
Study_arm_1 = c("Lependera", "Gobb Arbelle", "Nahgan-ngusa", "Sulate", "Saale-Sambakah", 
                "Namarei", "Manyatta Lengima", "Lokoshula", "TubchaDakhane", "Rengumo-Gargule")
Study_arm_2 = c("Galthelian-Torrder", "Uyam village", "Galthelan Elemo", "Nebey", "Rongumo_kurkum", 
                "Urawen_Kurkum", "Eisimatacho", "Manyatta K.A.G", "Ltepes Ooodo", "Lorokushu", 
                "Marti", "Manyatta Juu West/East", "Lbaarok1")

merged_data1 <- merged_data1 %>%
  mutate(study_arm = case_when(
    village.x %in% Study_arm_1 ~ "Study arm 1",
    village.x %in% Study_arm_2 ~ "Study arm 2",
    TRUE ~ "Study arm 3"
  ))

herd_dynamics <- merged_data1 %>%
  select(interview_date.x, household_id, study_arm, 
         ends_with("brth"),
         ends_with("death"),
         ends_with("gft"),
         ends_with("gfts"),
         ends_with("gvnout")) %>%
  mutate(monthyear = substr(interview_date.x, 1, 7))

# Convert columns to numeric
herd_dynamics <- herd_dynamics %>%
  group_by(study_arm, monthyear) %>%
  mutate(cow_births = sum(as.numeric(cwsbrth), na.rm = TRUE)) %>%
  mutate(shp_births = sum(as.numeric(shpbrth), na.rm = TRUE)) %>%
  mutate(gt_births = sum(as.numeric(goatsbrth), na.rm = TRUE)) %>%
  mutate(cml_births = sum(as.numeric(cmlsbrth), na.rm = TRUE)) %>%
  mutate(cow_deaths = sum(as.numeric(calves_death), as.numeric(bulls_death), as.numeric(cows_death), na.rm = TRUE)) %>%
  mutate(shp_deaths = sum(as.numeric(sheep_death), as.numeric(msheep_death), as.numeric(fsheep_death), na.rm = TRUE)) %>%
  mutate(gt_deaths = sum(as.numeric(goats_death), as.numeric(mgoats_death), as.numeric(fgoats_death), na.rm = TRUE)) %>%
  mutate(cml_deaths = sum(as.numeric(camels_death), as.numeric(mcamels_death), as.numeric(fcamels_death), na.rm = TRUE)) %>%
  mutate(cow_gifts = sum(as.numeric(cowsgft), na.rm = TRUE)) %>%
  mutate(shp_gifts = sum(as.numeric(sheepgfts), na.rm = TRUE)) %>%
  mutate(gt_gifts = sum(as.numeric(goatsgft), na.rm = TRUE)) %>%
  mutate(cml_gifts = sum(as.numeric(cmlsgft), na.rm = TRUE)) %>%
  mutate(cow_givenout = sum(as.numeric(cowsgvnout), na.rm = TRUE)) %>%
  mutate(shp_givenout = sum(as.numeric(sheepgvnout), na.rm = TRUE)) %>%
  mutate(gt_givenout = sum(as.numeric(goatsgvnout), na.rm = TRUE)) %>%
  mutate(cml_givenout = sum(as.numeric(cmlsgvnout), na.rm = TRUE)) %>%
  ungroup()


#subset
herd_dynamics <- herd_dynamics%>%
  select(study_arm, monthyear, cow_births, shp_births, gt_births, cml_births, cow_deaths, shp_deaths, gt_deaths, cml_deaths, cow_gifts, shp_gifts, gt_gifts, cml_gifts, cow_givenout, shp_givenout, gt_givenout, cml_givenout)%>%
  distinct(study_arm, monthyear, .keep_all=T)

# Pivot the data to long format
herd_dynamics_long <- herd_dynamics %>%
  pivot_longer(cols = contains(c("birth", "death", "gifts", "givenout")),
               names_to = c("species", "action"),
               names_sep = "_",
               values_to = "count")

# Create the plot
animal_plot <- ggplot(herd_dynamics_long, aes(x = monthyear, y = count, fill = species)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(action), scales = "free_y") +
  labs(title = "Animal Births, Deaths, Gifts In, and Given Out Over Time",
       x = "Month-Year",
       y = "Count") +
  scale_fill_discrete(name = "Animal Species")

animal_plot
