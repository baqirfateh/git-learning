library(readr)
library(readxl)
library(tidyverse)
library(here)

#Define the top level project directory here


here::i_am("code/Rcode_Inputs_table.R")

##Uploading the data file from the Teams folder 

dat <- read_excel("inputs_table.xlsx",
                  sheet = "Clean Tables")

names(dat) <- tolower(names(dat))

#Removing the unecessary columns 
dat <- dat%>%select(category,estimate,variable, country, age)

#Number of data points by category 
dat%>%filter(variable%in%c("prob.by.age.x","prob.by.age.x.RF"))%>%
  count(category)%>%arrange(n)


#Distribution of sample data points by category of eye conditions - all data
dat%>%filter(variable%in%c("prob.by.age.x","prob.by.age.x.RF"))%>%
ggplot(aes(reorder(category,log(estimate), FUN = median), log(estimate), fill = category))+
  geom_boxplot()+xlab("Category of Eye Conditions")+ylab("Log(estimate)")+
  scale_fill_discrete(name="Category")+
  theme(legend.position = "none")+
  ggtitle("Distribution by Categories - all data")
ggsave(here("graphs","Distribution - all data.png"), width = 23, height = 9, units = "cm")

#Descriptive statistics 

stat_all <-dat%>%filter(variable%in%c("prob.by.age.x","prob.by.age.x.RF"))%>%
  group_by(category)%>%summarise(min = min(estimate), max = max(estimate),
                                 median = median(estimate), mean = mean(estimate))
stat_all

### Only US data
dat_us <- dat%>%filter(country=="USA"&variable%in%c("prob.by.age.x","prob.by.age.x.RF"))

# Number of data points for each category 
dat_us%>%
  count(category)%>%arrange(n)

#Removing categories (Keratoconus, Retinal Dystrophies, Corneal Dystrophies) with two or less than two data points:
dat_us <- dat_us%>%filter(category%in%c("Dry Eye", "Glaucoma","RE", "VDTBI"))

#Distribution of sample data points by category of eye conditions - US data
dat_us%>%
  ggplot(aes(reorder(category,log(estimate), FUN = median), log(estimate), fill = category))+
  geom_boxplot()+xlab("Category of Eye Conditions")+ylab("Log(estimate)")+
  scale_fill_discrete(name="Category")+
  theme(legend.position = "none")+
  ggtitle("Distribution by Categories - US data")
ggsave(here("graphs","Distribution - US data.png"),width = 15, height = 9, units = "cm")

#Descriptive statistics 

stat_us <- dat_us%>%
  group_by(category)%>%summarise(min = min(estimate), max = max(estimate),
                                 median = median(estimate),
                                 mean = mean(estimate))
stat_us

#Combing the stat tables - us and all the sample
stats_prob.age <- stat_all%>%filter(!category%in%c("Dry Eye", "Glaucoma","RE", "VDTBI"))%>%
  rbind(stat_us)%>%
  mutate(variable = ifelse(category=="VDTBI","prob.by.age.x.RF", "prob.by.age.x"),
         country = ifelse(!category%in%c("Dry Eye", "Glaucoma","RE", "VDTBI"),
                      "all sample", "Only US"))


#Number of data points by category - sensitivity 
dat%>%filter(variable=="sensitivity")%>%
  count(category)%>%arrange(n)

dat%>%filter(variable=="sensitivity")%>%
  ggplot(aes(reorder(category,log(estimate), FUN = median), log(estimate), fill = category))+
  geom_boxplot()+xlab("Category of Eye Conditions")+ylab("Log(estimate)")+
  scale_fill_discrete(name="Category")+
  theme(legend.position = "none")+
  ggtitle("Sensitivity - all data")
ggsave(here("graphs","sensitivity - all data.png"),width = 23, height = 9, units = "cm")

#Descriptive statistics - sensitivity 

stat_sensi <- dat%>%filter(variable=="sensitivity")%>%
  group_by(category)%>%summarise(min = min(estimate), max = max(estimate),
                                 median = median(estimate),
                                 mean = mean(estimate))%>%
  mutate(variable = "sensitivity")
stat_sensi

#Number of data points by category - specificity 
dat%>%filter(variable=="specificity")%>%
  count(category)%>%arrange(n)

## Distribution of specificity
dat%>%filter(variable=="specificity")%>%
  ggplot(aes(reorder(category,log(estimate), FUN = median), log(estimate), fill = category))+
  geom_boxplot()+xlab("Category of Eye Conditions")+ylab("Log(estimate)")+
  scale_fill_discrete(name="Category")+
  theme(legend.position = "none")+
  ggtitle("specificity - all data")
ggsave(here("graphs","specificity - all data.png"),width = 23, height = 9, units = "cm")

#Descriptive statistics - sensitivity 
stat_speci <- dat%>%filter(variable=="specificity")%>%
  group_by(category)%>%summarise(min = min(estimate), max = max(estimate),
                                 median = median(estimate),
                                 mean = mean(estimate))%>%
  mutate(variable = "specificity")

stat_speci

#Number of data points by category - treatment efficacy 
dat%>%filter(variable=="tx.eff")%>%
  count(category)%>%arrange(n)

## Distribution of treatment efficacy 
dat%>%filter(variable=="tx.eff")%>%
  ggplot(aes(reorder(category,log(estimate), FUN = median), log(estimate), fill = category))+
  geom_boxplot()+xlab("Category of Eye Conditions")+ylab("Log(estimate)")+
  scale_fill_discrete(name="Category")+
  theme(legend.position = "none")+
  ggtitle("specificity - all data")
ggsave(here("graphs","treatment efficacy - all data.png"),width = 23, height = 9, units = "cm")

#Descriptive statistics - treatment efficacy 
stat_efficacy1 <- dat%>%filter(variable=="tx.eff")%>%
  mutate(variable = case_when(age>35~"post.35.tx.eff",
                              age<35~"pre.35.tx.eff"))%>%
  group_by(category, variable)%>%summarise(min = min(estimate), max = max(estimate),
                                 median = median(estimate),
                                 mean = mean(estimate))%>%
  relocate(variable, .after = mean)
stat_efficacy1
  
stat_efficacy2 <- dat%>%filter(variable=="tx.eff")%>%
group_by(category)%>%summarise(min = min(estimate), max = max(estimate),
                               median = median(estimate),
                               mean = mean(estimate))%>%

  mutate(variable = "tx.eff")

stat_efficacy2

stat_efficacy <- rbind(stat_efficacy1, stat_efficacy2)
stat_efficacy
## combining the tables 
stats_sens_spec_eff <- rbind(stat_sensi,stat_speci,stat_efficacy)%>%
  mutate(country = "all sample")

stats <- rbind(stats_prob.age,stats_sens_spec_eff)

#Saving the final table
write_csv(stats,here("final table","final_input_table.csv"))


###############################################################################
## Treatment cost

# Uploading the cost table from the Teams folder 
cost <- read_excel(here("treatment cost.xlsx"))

# Removing NAs from the cost column 
cost <- cost%>%filter(estimated_costs != "NA")%>%
  mutate(estimated_costs = estimated_costs/10)%>%
  select(category, treatment_method, estimated_costs)




# Generating descriptive statistics 
stats <- cost%>%group_by(category)%>%
  summarise(min = min(estimated_costs), max = max(estimated_costs),
            mean = mean(estimated_costs))
stats

# creating age table
year <- data.frame(age = rep(18:55,7), category = rep(stats$category,38))

# Merging the stats and age tables 
cost_final <- year%>%left_join(stats,by = "category")%>%
  mutate(variable = "tx_cost")%>%relocate(variable, .after = category)


# Saving the file  

write_csv(cost_final, here("final table","avg_treatment_costs.csv"))



