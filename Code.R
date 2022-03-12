
library(tidyverse)
library(dplyr)
library(ggplot2)



# Read data sets
IPClaims <- read.csv("IPClaims.csv", header = TRUE)
head(IPClaims)

library(readr)
IPClaims <- IPClaims %>% mutate(Paid.Amount = parse_number(IPClaims$Paid.Amount))
head(IPClaims)

#as.numeric(IPClaims$Paid.Amount)

ACORoster <- read.csv("ACORoster.csv", header = TRUE)
head(ACORoster)


# Check dimensions 
dim(IPClaims)
dim(ACORoster)

# look into the data types 
sapply(IPClaims, typeof)
sapply(ACORoster, typeof)


# Check summary stats
summary(IPClaims)
summary(ACORoster)


# Or we can use describe 
library(psych)
describe(IPClaims)
describe(ACORoster)


summary(numeric(IPClaims$Paid.Amount))
describe(ACORoster)


# Col names of the data sets
names(IPClaims)
names(ACORoster)


# Count the number of members by groups

# data IP Claims
IPClaims_Member_Count <- IPClaims  %>%
  count(Member.ID) %>% arrange(desc(n))
dim(IPClaims_Member_Count)
head(IPClaims_Member_Count)


IPClaims_Discharge.facility_Count <- IPClaims  %>%
  count(Discharge.facility) %>% arrange(desc(n))

dim(IPClaims_Discharge.facility_Count)
head(IPClaims_Discharge.facility_Count)




IPClaims_Discharge.facility.type_Count <- IPClaims  %>%
  count(Discharge.facility.type) %>% arrange(desc(n))

dim(IPClaims_Discharge.facility.type_Count)
head(IPClaims_Discharge.facility.type_Count)



IPClaims_Admission.Diagnosis.Group_Count <- IPClaims  %>%
  count(Admission.Diagnosis.Group) %>% arrange(desc(n))

dim(IPClaims_Admission.Diagnosis.Group_Count)
head(IPClaims_Admission.Diagnosis.Group_Count)





IPClaims_Admission.Date_Count <- IPClaims  %>%
  count(Admission.Date) %>% arrange(desc(n))

dim(IPClaims_Admission.Date_Count)
head(IPClaims_Admission.Date_Count)



IPClaims_Discharge.Date_Count <- IPClaims  %>%
  count(Discharge.Date) %>% arrange(desc(n))

dim(IPClaims_Discharge.Date_Count)
head(IPClaims_Discharge.Date_Count)



IPClaims_Paid.Amount_Count <- IPClaims  %>%
  count(Paid.Amount) %>% arrange(desc(n))

dim(IPClaims_Paid.Amount_Count)
head(IPClaims_Paid.Amount_Count)




# data ACO Roster
names(ACORoster)
ACORoster_Member_Count <- IPClaims  %>%
  count(Member.ID) %>% arrange(desc(n))
dim(ACORoster_Member_Count)
head(ACORoster_Member_Count)

ACORoster_Primary.Care.Site_Count <- ACORoster  %>%
  count(Primary.Care.Site) %>% arrange(desc(n))
dim(ACORoster_Primary.Care.Site_Count)
head(ACORoster_Primary.Care.Site_Count)


ACORoster_Sex_Count <- ACORoster  %>%
  count(Sex) %>% arrange(desc(n))
dim(ACORoster_Sex_Count)
head(ACORoster_Sex_Count)


ACORoster_Race_Count <- ACORoster  %>%
  count(Race) %>% arrange(desc(n))
dim(ACORoster_Race_Count)
head(ACORoster_Race_Count)


ACORoster_Disabled_Count <- ACORoster  %>%
  count(Disabled) %>% arrange(desc(n))
dim(ACORoster_Disabled_Count)
head(ACORoster_Disabled_Count)

ACORoster_Age_Count <- ACORoster  %>%
  count(Age) %>% arrange(desc(n))
dim(ACORoster_Age_Count)
head(ACORoster_Age_Count)

describe(ACORoster$Age)





# data Claim_Roster
#  we left join IP Claims to ACORoster
Claim_Roster<- IPClaims %>% left_join(ACORoster, by = "Member.ID")
dim(Claim_Roster)
names(Claim_Roster)



names(ACORoster)
ACORoster_Member_Count <- Claim_Roster  %>%
  count(Member.ID) %>% arrange(desc(n))
dim(ACORoster_Member_Count)
head(ACORoster_Member_Count)

ACORoster_Primary.Care.Site_Count <- Claim_Roster  %>%
  count(Primary.Care.Site) %>% arrange(desc(n))
dim(ACORoster_Primary.Care.Site_Count)
head(ACORoster_Primary.Care.Site_Count)


ACORoster_Sex_Count <- Claim_Roster %>%
  count(Sex) %>% arrange(desc(n))
dim(ACORoster_Sex_Count)
head(ACORoster_Sex_Count)



ACORoster_Race_Count <- Claim_Roster  %>%
  count(Race) %>% arrange(desc(n))
dim(ACORoster_Race_Count)
head(ACORoster_Race_Count)


ACORoster_Disabled_Count <- Claim_Roster  %>%
  count(Disabled) %>% arrange(desc(n))
dim(ACORoster_Disabled_Count)
head(ACORoster_Disabled_Count)

ACORoster_Age_Count <- Claim_Roster  %>%
  count(Age) %>% arrange(desc(n))
dim(ACORoster_Age_Count)
head(ACORoster_Age_Count)

describe(ACORoster$Age)










# Find out descriptive statistics for each group
# We explore the payment amount by demographics 
names(IPClaims)
names(ACORoster)

# we first explore the continuous variables 
boxplot(IPClaims$Paid.Amount)
summary(IPClaims$Paid.Amount)


# inpatient stays 
IPClaims_inpatient <- IPClaims %>%
  group_by(Member.ID) %>% 
  mutate(Admission.Date=as.Date(Admission.Date, "%m/%d/%y"),
         Discharge.Date=as.Date(Discharge.Date, "%m/%d/%y")) %>% 
  mutate(inpatientstay = as.numeric(difftime(Discharge.Date, Admission.Date, units = "days"))) %>%
  filter(inpatientstay >= 0)

#names(IPClaims_inpatient)
# we first explore the continuous variables 
summary(IPClaims_inpatient$inpatientstay)

boxplot(IPClaims_inpatient$inpatientstay)




# Histogram of payment 
# hist(log(IPClaims$Paid.Amount), # histogram
#      col="blue", # column color
#      border="black",
#      prob = TRUE, # show densities instead of frequencies
#      xlab = "cost",
#      main = "Cost Histogram")
# lines(density(IPClaims$Paid.Amount), # density plot
#       lwd = 2, # thickness of line
#       col = "chocolate3")



library(ggplot2)
ggplot(data = IPClaims, aes(y=Paid.Amount)) +
  geom_boxplot(fill="orange") +
  labs(title="Payment Boxplot", x="Payment", y="Amount")




ggplot(data = IPClaims, aes(x=Discharge.facility.type, y=Paid.Amount)) +
  geom_boxplot(fill="orange") +
  labs(title="Payment Boxplot by facility type", x="Payment", y="Amount")


# now we left join IP Claims to ACORoster
Claim_Roster<- IPClaims %>% left_join(ACORoster, by = "Member.ID")
dim(Claim_Roster)
names(Claim_Roster)
#write.csv(Claim_Roster, "Claim_Roster.csv")


ggplot(data = Claim_Roster, aes(x=Sex, y=Paid.Amount)) +
  geom_boxplot(fill="orange") +
  labs(title="Payment Boxplot by Sex", x="Payment", y="Amount")



ggplot(data = Claim_Roster, aes(x=Primary.Care.Site , y=Paid.Amount)) +
  geom_boxplot(fill="orange") +
  labs(title="Payment Boxplot by Primary.Care.Site ", x="Payment", y="Amount")


ggplot(data = Claim_Roster, aes(x=Race, y=Paid.Amount)) +
  geom_boxplot(fill="orange") +
  labs(title="Payment Boxplot by Race", x="Payment", y="Amount")



ggplot(data = Claim_Roster, aes(x=Disabled, y=Paid.Amount)) +
  geom_boxplot(fill="orange") +
  labs(title="Payment Boxplot by Disabled or Not", x="Payment", y="Amount")


#===========================================
# We calculate the group descriptive statistics 
#==========================================
names(IPClaims)
#library(formattable)  # The library for percentage format 

#==========================================
# The mean cost rank by Discharge.facility
#==========================================

sum_Discharge.facility_1 <- IPClaims %>%
  group_by(Discharge.facility) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE), ) %>% arrange(desc(mean))

#==========================================
# The total cost rank by Discharge.facility
#==========================================

sum_Discharge.facility_2 <- IPClaims %>%
  group_by(Discharge.facility) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost))

#==========================================
# by Discharge.facility type
#==========================================

head(sum_Discharge.facility_2)
#sum_Discharge.facility_2 %>% na.omit()


sum_Discharge.facility.type_1 <- IPClaims %>%
  group_by(Discharge.facility.type) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean))



sum_Discharge.facility.type_2 <- IPClaims %>%
  group_by(Discharge.facility.type) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost))


#==========================================
# by Admission.Diagnosis.Group
#==========================================

sum_Admission.Diagnosis.Group_1 <- IPClaims %>%
  group_by(Admission.Diagnosis.Group) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean))


sum_Admission.Diagnosis.Group_2 <- IPClaims %>%
  group_by(Admission.Diagnosis.Group) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost))


#==========================================
# by Primary Care Site
#==========================================


sum_Primary.Care.Site_1 <- Claim_Roster %>%
  group_by(Primary.Care.Site ) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean)) %>% na.omit()

head(sum_Primary.Care.Site)


sum_Primary.Care.Site_2 <- Claim_Roster %>%
  group_by(Primary.Care.Site ) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost))  %>% na.omit()

na.omit(sum_Primary.Care.Site_2)

names(Claim_Roster)
#================

#==========================================
# by Sex
#==========================================

sum_Sex_1 <- Claim_Roster %>%
  group_by(Sex) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean)) %>% na.omit()
head(sum_Sex_1)


sum_Sex_2 <- Claim_Roster %>%
  group_by(Sex) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost)) %>% na.omit()


# =====
#==========================================
# by Race
#==========================================

sum_Race_1 <- Claim_Roster %>%
  group_by(Race) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean)) %>% na.omit()

sum_Race_2 <- Claim_Roster %>%
  group_by(Race) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost)) %>% na.omit()

#===

#==========================================
# by Disabled
#==========================================


sum_disable_1 <- Claim_Roster %>%
  group_by(Disabled) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean)) %>% na.omit()

sum_disable_2 <- Claim_Roster %>%
  group_by(Disabled) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost)) %>% na.omit()

#=======

#==========================================
# by Age
#==========================================

sum_Age_1 <- Claim_Roster %>%
  group_by(Age) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(mean)) %>% na.omit()

sum_Age_2 <- Claim_Roster %>%
  group_by(Age) %>%
  summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% arrange(desc(cost)) %>% na.omit()


#=================================
# calculate the re-admission rate
#==================================
names(Claim_Roster)

IPClaims_v1 <- IPClaims %>%
  group_by(Member.ID) %>% 
  mutate(Admission.Date=as.Date(Admission.Date, "%m/%d/%y"),
         Discharge.Date=as.Date(Discharge.Date, "%m/%d/%y")) %>%
  mutate(count_member= n(), Discharge.Date_lead = lead(Discharge.Date)) %>%
  filter(!is.na(Discharge.Date_lead)) %>%
  mutate(Discharge.Date_min = min(Discharge.Date_lead)) %>%
  mutate(readmission = ifelse(Admission.Date >= Discharge.Date_min+30,"Y","N")) 

View(IPClaims_v1)
names(IPClaims_v1)



#====================================
#  readmission rate in difference cases 
#====================================



# =================================
# Individual level readmission rate
# =================================
library(formattable)
re_ad_ind <- IPClaims %>%
  group_by(Member.ID) %>% 
  mutate(Admission.Date=as.Date(Admission.Date, "%m/%d/%y"),
         Discharge.Date=as.Date(Discharge.Date, "%m/%d/%y")) %>%
  mutate(count_member= n(), Discharge.Date_lead = lead(Discharge.Date)) %>%
  filter(!is.na(Discharge.Date_lead)) %>%
  mutate(Discharge.Date_min = min(Discharge.Date_lead)) %>%
  mutate(readmission = ifelse(Admission.Date >= Discharge.Date_min+30,"Y","N")) %>% 
  summarize(count =count_member, mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% mutate(re_ad_rate = percent(re_ad_count/count)) 
re_ad_ind_v1 <- re_ad_ind %>% arrange(desc(re_ad_rate))



# re_ad_ind <- IPClaims_v1 %>%
#   group_by(Member.ID) %>%
#   summarize(count = n(), mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
#             cost=sum(Paid.Amount,na.rm=TRUE)) %>% mutate(re_ad_rate = percent(re_ad_count/count)) %>%
#   arrange(desc(re_ad_rate))
# names(re_ad_ind)

qplot(re_ad_rate,cost, data=re_ad_ind)

qplot(re_ad_rate,mean, data=re_ad_ind)


# =================================
# readmission rate Discharge.facility
# =================================
re_ad_Discharge.facility  <- IPClaims_v1 %>%  group_by(Discharge.facility) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(IPClaims[,c("Member.ID", "Discharge.facility")], by = "Discharge.facility") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()
  

# =================================
# readmission rate Discharge.facility type
# =================================
names(IPClaims_v1)
re_ad_Discharge.facility.type  <- IPClaims_v1 %>%  group_by(Discharge.facility.type) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(IPClaims[,c("Member.ID", "Discharge.facility.type")], by = "Discharge.facility.type") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()

# =================================
# readmission rate Admission.Diagnosis.Group
# =================================
names(IPClaims_v1)
re_ad_Admission.Diagnosis.Group  <- IPClaims_v1 %>%  group_by(Admission.Diagnosis.Group) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(IPClaims[,c("Member.ID", "Admission.Diagnosis.Group")], by = "Admission.Diagnosis.Group") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()


# =================================
# readmission rate Primary.Care.Site
# =================================
names(ACORoster)
re_ad_Admission.Diagnosis.Group  <- IPClaims_v1 %>% left_join(ACORoster, by = "Member.ID")%>%
  group_by(Primary.Care.Site) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(Claim_Roster[,c("Member.ID", "Primary.Care.Site")], by = "Primary.Care.Site") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()


# =================================
# readmission rate Sex 
# =================================
names(ACORoster)
re_ad_Sex  <- IPClaims_v1 %>% left_join(ACORoster, by = "Member.ID")%>%
  group_by(Sex) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(Claim_Roster[,c("Member.ID", "Sex")], by = "Sex") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()

# =================================
# readmission rate Race 
# =================================
names(ACORoster)
re_ad_Race  <- IPClaims_v1 %>% left_join(ACORoster, by = "Member.ID")%>%
  group_by(Race) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(Claim_Roster[,c("Member.ID", "Race")], by = "Race") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()


# =================================
# readmission rate Disabled
# =================================
names(ACORoster)
re_ad_Disabled  <- IPClaims_v1 %>% left_join(ACORoster, by = "Member.ID")%>%
  group_by(Disabled) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(Claim_Roster[,c("Member.ID", "Disabled")], by = "Disabled") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()

# =================================
# readmission rate Age
# =================================
names(ACORoster)
re_ad_Age  <- IPClaims_v1 %>% left_join(ACORoster, by = "Member.ID")%>%
  group_by(Age) %>% 
  summarize(mean=mean(Paid.Amount,na.rm=TRUE), re_ad_count=sum(readmission == "Y"),
            cost=sum(Paid.Amount,na.rm=TRUE)) %>% 
  inner_join(Claim_Roster[,c("Member.ID", "Age")], by = "Age") %>% 
  mutate(count_member= n(),re_ad_rate = percent(re_ad_count/count_member)) %>% select(-Member.ID) %>% 
  arrange(desc(re_ad_rate)) %>% distinct()







