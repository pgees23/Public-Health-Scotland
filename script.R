
library(tidyverse)

# Load data
hb_2014_2019 <- read_csv("Data/hb14_hb19.csv")
ca_2011_2019 <- read_csv("Data/ca11_ca19.csv")
ui_admissions_2023 <- read_csv("Data/ui_admissions_2023.csv")
ui_deaths_2023 <- read_csv("Data/ui_deaths_-2023-.csv")

# Check health board labels structure
hb_2014_2019

# Check council area labels structure
ca_2011_2019

# Question 1 -------------------------------------------------------------------

# Scotland label in health boards
scotland <- unique(hb_2014_2019$Country)

# Filter dataset for total number of admissions for Scotland in 2022/23
q1a_dat <- ui_admissions_2023 %>% 
  filter(FinancialYear == "2022/23", 
         CA == scotland,
         InjuryType == "All Diagnoses",
         InjuryLocation == "All",
         Sex == "All",
         AgeGroup == "All")

# Total number of admissions in Scotland = 54,709
total_admissions <- q1a_dat$NumberOfAdmissions
total_admissions

# To see injury types
table(ui_admissions_2023$InjuryType)
unique(ui_admissions_2023$InjuryType)

# Get total number of fall admissions for Scotland in 2022/23
q1b_dat <- ui_admissions_2023 %>% 
  filter(FinancialYear == "2022/23", 
         CA == scotland,
         InjuryType == "Falls",
         InjuryLocation == "All",
         Sex == "All",
         AgeGroup == "All")

# Total number of fall admissions in Scotland - 36,050
total_falls <- q1b_dat$NumberOfAdmissions
total_falls

# Calculate falls as percentage of total admissions for all health boards
# 65.9%
falls_percent <- total_falls/total_admissions*100
falls_percent

# Question 2 -------------------------------------------------------------------

# Filter dataset for total admissions for health boards in 2020/21 + 2021/22
# Get total admissions per health board in each financial year
# Spread dataset so each year value is in a separate column
# Calculate percentage change
# Join the health board names
q2_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         FinancialYear == "2020/21" | FinancialYear == "2021/22",
         InjuryType == "All Diagnoses",
         InjuryLocation == "All",
         Sex == "All",
         AgeGroup == "All") %>% 
  group_by(HBR, FinancialYear) %>% 
  summarise(total_admissions = sum(NumberOfAdmissions)) %>% 
  pivot_wider(names_from = FinancialYear, values_from = total_admissions) %>%
  mutate(change = round((`2021/22` - `2020/21`)/`2020/21` * 100, 1)) %>% 
  left_join(hb_2014_2019 %>% select(HBR = HB, HBName)) %>% 
  ungroup()

# Health board with largest percentage change
q2_dat %>% slice_max(change)

# Question 3 -------------------------------------------------------------------

# Filter dataset for total number of road traffic accidents for 
# health boards in 2019/20
q3a_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         FinancialYear == "2019/20",
         InjuryType == "RTA",
         InjuryLocation == "All",
         Sex == "All",
         AgeGroup == "All") %>% 
  group_by(HBR) %>% 
  summarise(total_RTAs = sum(NumberOfAdmissions)) %>% 
  left_join(hb_2014_2019 %>% select(HBR = HB, HBName)) %>% 
  ungroup()

# Health board with highest number of RTAs
q3a_ans <- q3a_dat %>% slice_max(total_RTAs)

# Filter dataset for number of RTAs by sex for health boards in 2019/20 
q3b_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         FinancialYear == "2019/20",
         InjuryType == "RTA",
         InjuryLocation == "All",
         Sex == "Male",
         AgeGroup == "All") %>% 
  group_by(HBR, Sex) %>% 
  summarise(male_RTAs = sum(NumberOfAdmissions)) %>% 
  left_join(hb_2014_2019 %>% select(HBR = HB, HBName)) %>% 
  ungroup()

# Name of health board
q3b_hb <- q3a_dat %>% slice_max(total_RTAs) %>% select(HBName)
q3b_hb <- as.vector(q3b_hb$HBName)

# Percentage of male RTAs - 66.4%
q3b_dat %>% 
  filter(HBName == q3b_hb) %>% 
  left_join(q3a_dat %>% filter(HBName == q3b_hb)) %>% 
  mutate(pcnt = male_RTAs/q3a_ans$total_RTAs*100)

# Question 4 -------------------------------------------------------------------

# Filter dataset for home scald admissions for Lothian in 2019/20 + 2020/21 by
# joining health board labels and filtering for Lothian
# Spread dataset so each year value is in a separate column
# Calculate percentage change
q4_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         FinancialYear == "2019/20" | FinancialYear == "2020/21",
         InjuryType == "Scalds",
         InjuryLocation == "Home",
         Sex == "All",
         AgeGroup == "All") %>% 
  inner_join(hb_2014_2019 %>% 
               filter(grepl("Lothian", HBName)) %>% 
               select(HBR = HB, HBName)) %>% 
  group_by(HBR, FinancialYear) %>% 
  summarise(total_admissions = sum(NumberOfAdmissions)) %>% 
  pivot_wider(names_from = FinancialYear, values_from = total_admissions) %>% 
  ungroup()

# Percentage change - 23.1%
q4_ans <- (q4_dat$`2020/21` - q4_dat$`2019/20`)/q4_dat$`2019/20` * 100
q4_ans

# Question 5 -------------------------------------------------------------------

unique(ui_admissions_2023$FinancialYear)

# Years not to include
not_yrs <- c("2013/14", "2014/15", "2015/16", "2016/17")

# Get total admissions for North Ayrshire council in 2017/18 to 2022/23 by
# joining council area labels and filtering for North Ayrshire
q5_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         !(FinancialYear %in% not_yrs),
         InjuryType == "All Diagnoses",
         InjuryLocation == "All",
         Sex == "All",
         AgeGroup == "All") %>% 
  inner_join(ca_2011_2019 %>% 
               filter(grepl("North Ayrshire", CAName)) %>% 
               select(CA, CAName))

# Yearly average - 1548.17
mean(q5_dat$NumberOfAdmissions)

# Question 6 -------------------------------------------------------------------

# Get home falls in 2022/23 for Perth and Kinross
q6_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         FinancialYear == "2022/23",
         InjuryType == "Falls",
         InjuryLocation != "All",
         Sex == "All",
         AgeGroup == "All") %>% 
  inner_join(ca_2011_2019 %>% 
               filter(grepl("Perth", CAName)) %>% 
               select(CA, CAName))

q6_home <- q6_dat$NumberOfAdmissions[q6_dat$InjuryLocation == "Home"]
q6_total <- sum(q6_dat$NumberOfAdmissions)

# Home falls as percentage of total - 51.9%
q6_ans <- q6_home/q6_total * 100
q6_ans

# Question 7 -------------------------------------------------------------------

# Get admissions for females >=75 in Scotland in 2022/23
q7_dat <- ui_admissions_2023 %>% 
  filter(HBR == scotland,
         FinancialYear == "2022/23",
         Sex == "Female",
         AgeGroup == "75plus years",
         InjuryType == "All Diagnoses",
         InjuryLocation != "All")

unique(ui_admissions_2023$AgeGroup)

# Total admissions for females >=75 - 13830
q7_ans <- sum(q7_dat$NumberOfAdmissions)
q7_ans

# Question 8 -------------------------------------------------------------------

# Get admissions for males in 2022/23 with location type and health board
q8_dat <- ui_admissions_2023 %>% 
  filter(HBR != scotland,
         FinancialYear == "2022/23",
         Sex == "Male",
         AgeGroup == "All",
         InjuryType == "All Diagnoses",
         InjuryLocation != "All") %>% 
  inner_join(hb_2014_2019 %>% 
               select(HBR = HB, HBName)) %>% 
  group_by(HBName, InjuryLocation) %>%
  summarise(total_admissions = sum(NumberOfAdmissions)) %>% 
  ungroup() %>% 
  mutate(InjuryLocation = factor(InjuryLocation,
                                 levels = c(
                                   "Home", "Other", "Undisclosed",
                                   "Not Applicable"
                                 )))

# Plot as bar chart
plot <- q8_dat %>% 
  ggplot(aes(x = fct_rev(HBName), 
             y = total_admissions, 
             fill = fct_rev(InjuryLocation))) +
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  labs(x = NULL, 
       y = "Total Number of Admissions for Unintentional Injuries",
       fill = "Injury Location") +
  scale_fill_discrete(breaks = c("Home", "Other", "Undisclosed",
                                 "Not Applicable")) +
  theme_bw() +
  theme(
    axis.title.x = 
      element_text(
        size = 10,
        face = "bold",
        colour = "black"
      ),
    axis.text.x =
      element_text(
        size = 10,
        colour = "black"
      ),
    axis.text.y =
      element_text(
        size = 10,
        face = "bold",
        colour = "black"
      ),
    legend.title =
      element_text(
        size = 10,
        face = "bold",
        colour = "black"
      ),
    legend.text =
      element_text(
        size = 10,
        colour = "black"
      )
  ) +
  coord_flip()

ggsave("q8_plot.png", plot, dpi = 300, height = 7, width = 10)
