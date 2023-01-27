library(dplyr)
library(tidyr)
library(ggplot2)
getwd()
d <- read.csv("./BWH/eink/studyData/EInkdemographics.csv")
#make sex a number
d <- d %>% mutate(sn = ifelse(Sex == "Male",0,1))
hist(d$Age.at.Enrollment..AUTOCALCULATED.)
#split into 2 groups
control <- d %>% filter(Cohort == 2)
vw <- d %>% filter(Cohort == 1)

#age means and SD
wilcox.test(control$Age.at.Enrollment..AUTOCALCULATED.,vw$Age.at.Enrollment..AUTOCALCULATED.)
sd(control$Age.at.Enrollment..AUTOCALCULATED.)
sd(vw$Age.at.Enrollment..AUTOCALCULATED.)

#sex
t.test(control$sn, vw$sn)

##RACE
chisq.test(table(d$Cohort, d$Race))

##ETHNICITY
chisq.test(table(d$Cohort, d$Ethnicity))





chisq.test(control$Race, vw$Race)


#MTUAS data
m <- read.csv("./BWH/eink/studyData/MTUAS.csv")
eink <- m %>% filter(Study == 1)
eink <- eink %>% filter(record_id != 15 & record_id != 16 & record_id != 186 & record_id != 227 & record_id != 240 & record_id != 279 & record_id != 310 & record_id != 406)

#table(m$call_ave)
#hist

#phone ownership
#control
c <- eink %>% filter(studyarm == 2)
#experiment
e <- eink %>% filter(studyarm == 1)

#smartphone ownership
mean(as.numeric(c$a1_smartphone_own), na.rm = TRUE)
c %>% count(a1_smartphone_own)
mean(as.numeric(e$a1_smartphone_own), na.rm = TRUE)
e %>% count(a1_smartphone_own)
wilcox.test(as.numeric(c$a1_smartphone_own),as.numeric(e$a1_smartphone_own))

#computer ownership
mean(as.numeric(c$a2_computer), na.rm = TRUE)
c %>% count(a2_computer)
mean(as.numeric(e$a2_computer), na.rm = TRUE)
e %>% count(a2_computer)
wilcox.test(as.numeric(c$a2_computer),as.numeric(e$a2_computer))

#texting
mean(as.numeric(c$text_ave), na.rm = TRUE)
mean(as.numeric(e$text_ave), na.rm = TRUE)

sd(as.numeric(c$text_ave), na.rm = T)
sd(as.numeric(e$text_ave), na.rm = T)

wilcox.test(as.numeric(c$text_ave),as.numeric(e$text_ave))

#calling

mean(as.numeric(c$call_ave), na.rm = TRUE)
mean(as.numeric(e$call_ave), na.rm = TRUE)

sd(as.numeric(c$call_ave), na.rm = T)
sd(as.numeric(e$call_ave), na.rm = T)

wilcox.test(as.numeric(c$call_ave),as.numeric(e$call_ave))

#smartphone

mean(as.numeric(c$smartphone_ave), na.rm = TRUE)
mean(as.numeric(e$smartphone_ave), na.rm = TRUE)

sd(as.numeric(c$smartphone_ave), na.rm = T)
sd(as.numeric(e$smartphone_ave), na.rm = T)

wilcox.test(as.numeric(c$smartphone_ave),as.numeric(e$smartphone_ave))

#email

mean(as.numeric(c$email_ave), na.rm = TRUE)
mean(as.numeric(e$email_ave), na.rm = TRUE)

sd(as.numeric(c$email_ave), na.rm = T)
sd(as.numeric(e$email_ave), na.rm = T)

wilcox.test(as.numeric(c$email_ave),as.numeric(e$email_ave))

#attitude

mean(as.numeric(c$positive_att_ave), na.rm = TRUE)
mean(as.numeric(e$positive_att_ave), na.rm = TRUE)

sd(as.numeric(c$positive_att_ave), na.rm = T)
sd(as.numeric(e$positive_att_ave), na.rm = T)

wilcox.test(as.numeric(c$positive_att_ave),as.numeric(e$positive_att_ave))

#anxiety without technology

mean(as.numeric(c$anxiety_ave), na.rm = TRUE)
mean(as.numeric(e$anxiety_ave), na.rm = TRUE)

sd(as.numeric(c$anxiety_ave), na.rm = T)
sd(as.numeric(e$anxiety_ave), na.rm = T)

wilcox.test(as.numeric(c$anxiety_ave),as.numeric(e$anxiety_ave))

