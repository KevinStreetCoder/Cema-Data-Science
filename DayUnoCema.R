#creating an object and finding statistical data

object1 <- c(1:100)

mean(object1)

median(object1)

summary(object1)

x <- rnorm(100)

print(x)

#installing packages in R
install.packages("tidyverse")
search()
help(package = "dplyr")

#loading packages
library(tidyverse)

dat1 <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#getting top and last part of the dataset
head(dat1)
tail(dat1)
str(dat1)
class(dat1$admit)
?atomic

# recode admit (o - deny, 1 - accept)
dat2 <- dat1 %>%
  mutate(admit = recode(admit, "0" = "deny", "1"= "accept"))

# rename columns (rank - position)
dat3 <- dat1 %>%
  rename(position= rank)
dat3

ideal1 <- read.csv("https://raw.githubusercontent.com/cema-uonbi/R_course/main/ideal1.csv")
head(ideal1)
str(ideal1)

# recode Calfsex column (1- male, 2 - female)

ideal1a <- ideal1 %>%
  mutate(CalfSex = recode(CalfSex, "1"="male", "2"= "female"))

ideal1a

summary(ideal1)
#check col names
colnames(ideal1b)

ideal1b <- ideal1 %>%
  rename(Calf_date_of_birth = CADOB)
ideal1b

# install.packages("janitor")

library(janitor)
ideal1c <- ideal1 %>%
  clean_names()

# install.packages("lubridate")
library(lubridate)
ideal1d <- ideal1 %>%
  mutate(CADOB = as_date(CADOB, format ="%d/%m/%Y")) %>%
  arrange(CADOB)

#subsetting
## if we want to create a subset from one location _ kidera
table(ideal1$sublocation)
idealkidera1 <- subset(ideal1, ideal1$sublocation == "Kidera")

## using filter
idealkidera <- ideal1 %>%
  filter(sublocation == "Kidera")

ideal2 <- read.csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal2.csv")
ideal2a <- ideal2 %>%
  mutate(VisitDate = as.Date(VisitDate, format = "%d/%m/%Y"))

# left join
ideal3 <- ideal1 %>%
  left_join(ideal2, by = "CalfID")
ideal4 <- ideal2 %>%
  full_join(ideal1, by= "CalfID")

# subsetting data using columns using select
ideal3a <- ideal3 %>%
  select(CalfID,VisitID,VisitDate,Theileria.spp.,ELISA_mutans,ELISA_parva)
ideal3b <- ideal3a %>%
  pivot_longer(cols = c(Theileria.spp.,ELISA_mutans,ELISA_parva), names_to = "tests", values_to = "outcome")
# grouping data (group_by)
ideal3c <- ideal3 %>%
  group_by(CalfID) %>%
  mutate(Avg_weight = mean(Weight, na.rm = TRUE))
  
# more data load
dogdemography <- read.csv("https://raw.githubusercontent.com/cema-uonbi/R_course/main/DogcohortDemographics.csv")

# data wrangling steps
## rename coln names
## format interview date
## recode categorical variable
## get average number of household member per village
## get the average dog per village

dogdemography1 <- dogdemography %>% 
  rename( InterviewDate = IntDate, householdMember = HhMmbrs, numDogsOwned = DgsOwnd, adultDogsOwned = AdltDgs, puppiesOwned = Puppies, dogDiedPastMonth = DogDied, numDogsDiedPastMonth = NumDd, dogBitesPastMonth = DogBite) %>%
  mutate(InterviewDate = as.Date(InterviewDate, format= "%m/%d/%y"))

dogdemography1 <- dogdemography1 %>%
  mutate(VillageID = as.character(VillageID)) %>%
  mutate ( OwnDogs = recode(OwnDogs, "1" = "Yes", "0" = "No")) %>%
  mutate ( dogBitesPastMonth  = recode(dogDiedPastMonth , "1" = "Yes", "0" = "No")) 

dogdemography1 <- dogdemography1 %>%
  group_by(VillageID)%>%
  mutate(Avg_household = round (mean( householdMember)))

dogdemography1 <- dogdemography1 %>%
  group_by(VillageID)%>%
  mutate(Avg_Dogs_village = round(mean( numDogsOwned)))

dogdemography2 <- dogdemography1 %>%
  group_by(VillageID)%>%
  summarize(dogdemography1, Avg_Dogs_village = mean( numDogsOwned) )%>%
  arrange(InterviewDate)

# loading more data
ideal <- read.csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal3a.csv")
#summerize reason death
table(ideal$ReasonsLoss1)
library(tidyverse)
library(ggplot2)

#plotting reason loss
ggplot(data = ideal, aes(x = ReasonsLoss1)) + 
  geom_bar() + 
  theme_bw() + #change background theme
  labs(x = "Reasons for loss of calves", 
       y = "Number of calves", 
       title = "Bar graph plot of calves loss")

# get frequency (count)
calves_sublocation <- ideal %>%
  select(sublocation) %>%
  group_by(sublocation) %>%
  count() %>%
  ungroup()

#OR

calves_sublocation <- ideal %>%
  select(sublocation) %>%
  group_by(sublocation) %>%
  summarise(freq = n())%>%
  ungroup()

#plot frequency
ggplot(calves_sublocation, aes(x = reorder(sublocation, freq), y = freq)) +
  geom_col() +
  theme_classic() +
  labs(x = "Sublocation", y = "Frequency") +
  coord_flip()

# Custom color palette for the bars
custom_colors <- c( "#C2C2C2", "#8A92D1", "#FF9896")

# Create the plot
ggplot(calves_sublocation, aes(x = reorder(sublocation, -freq), y = freq, fill = sublocation)) +
  geom_col() +
  scale_fill_manual(values = custom_colors) +  # Apply the custom color palette
  labs(x = "Sublocation", y = "Frequency") +
  coord_flip() +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(
    legend.position = "none",  # Hide the legend as it's not needed in this case
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    plot.title = element_text(size = 20, face = "bold"),  # Increase title font size and make it bold
    axis.title = element_text(size = 14),  # Increase axis label font size
    axis.text = element_text(size = 12),  # Increase axis tick label font size
    panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  ) +
  labs(title = "Calves Sublocation Frequency", subtitle = "Sorted by frequency in descending order")

calves_sublocation2 <- ideal %>%
  select(sublocation, CalfID) %>%
  distinct() %>%
  group_by(sublocation) %>%
  summarise(freq = n())%>%
  ungroup()

library(viridis)

# Assuming calves_sublocation is your data frame and you have loaded the ggplot2 and viridis libraries.

# Create the plot with dynamic color scale
ggplot(calves_sublocation2, aes(x = reorder(sublocation, -freq), y = freq, fill = sublocation)) +
  geom_col() +
  scale_fill_viridis_d(option = "C") +  # Use a dynamic color scale from viridis
  labs(x = "Sublocation", y = "Frequency") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 360, hjust = 1),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Calves Sublocation Frequency", subtitle = "Sorted by frequency in descending order")

calves_gender <- ideal%>%
  select(sublocation, CalfSex) %>%
  mutate(CalfSex=recode(CalfSex, "1" = "male", "2" = "female")) %>%
  group_by(sublocation, CalfSex) %>%
  summarise(freq = n()) %>%
  ungroup()

ggplot(calves_gender, aes( x = reorder(sublocation, freq), y = freq, fill = CalfSex))+
  geom_col()+
  theme_classic()+
  scale_fill_manual(values= custom_colors)+
  labs(x = "Sublocation" , y = "Frequency")+
  coord_flip()

calves_gender1 <- ideal%>%
  select(sublocation, CalfSex) %>%
  mutate(CalfSex=recode(CalfSex, "1" = "male", "2" = "female")) %>%
  group_by(sublocation, CalfSex) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(sublocation) %>%
  mutate(proportion = freq/sum(freq) * 100)

ggplot(calves_gender1, aes(x = sublocation, y = proportion, fill = CalfSex)) +
  geom_col(position = "stack", width = 0.7) +
  labs(x = "Sublocation", y = "Proportion (%)", fill = "Calf Gender") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("male" = "#1f78b4", "female" = "#33a02c")) +
  geom_text(aes(label = paste(round(proportion, 1), "%")), position = position_stack(vjust = 0.5))

# facetting graphs
## have several graphs on same panel
## facet_grid(rows=vars(columnname))
ggplot(calves_gender1, aes(x = sublocation, y = proportion, fill = CalfSex)) +
  geom_col(position = "stack", width = 0.7) +
  facet_grid(cols = vars(CalfSex))+
  labs(x = "Sublocation", y = "Proportion (%)", fill = "Calf Gender") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("male" = "#1f78b4", "female" = "#33a02c")) +
  geom_text(aes(label = paste(round(proportion, 1), "%")), position = position_stack(vjust = 0.5))+
  coord_flip()

#one colors for all grid
## geom_col(fill = "#1f78b4")

# facet wrap
ggplot(calves_gender1, aes(x = sublocation, y = proportion)) +
  geom_col(position = "stack", width = 0.7, fill = "#1f78b4") +
  facet_wrap(~CalfSex)+
  labs(x = "Sublocation", y = "Proportion (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = paste(round(proportion, 1), "%")), position = position_stack(vjust = 0.5))+
  coord_flip()

#seperate one column into several columns (seperate)
dogdemography3 <- dogdemography1 %>%
  separate(InterviewDate, into = c("year", "month","day"), sep = "-")

# combine different columns into one columns (paste0)
dogdemography4 <- dogdemography3 %>%
  mutate(InterviewDate = paste0(year, "/", month, "/", day))


