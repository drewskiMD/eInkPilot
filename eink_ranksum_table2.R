library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

getwd()
d <- read.csv("./BWH/eink/studyData/fullData.csv")
#remove pts that did not participate
d <- d %>% filter(Complete. == "Complete")

d$Study.Status...Study.lead.to.update.throughout.the.study.
#capture dropouts
dropouts <- d %>% filter(Study.Status...Study.lead.to.update.throughout.the.study. !="Enrolled")


dropouts <- dropouts %>% mutate(sn = ifelse(Sex == "Male",0,1))
mean(dropouts$Age.at.Enrollment..AUTOCALCULATED.)
mean(dropouts$sn)

#remove dropouts
#223 lost to followup
#354 withdrew consent
d <- d %>% filter(Record.ID != 15 & Record.ID != 16 & Record.ID != 186 & Record.ID != 227 & Record.ID != 240 & Record.ID != 279 & Record.ID != 310 & Record.ID != 406 & Record.ID != 223 & Record.ID != 354)

#convert to numerical scales
#Q1 Were.you.seen.by.a.care.provider.in.a.timely.manner.
#d <- d %>% mutate(q1b = case_when(Were.you.seen.by.a.care.provider.in.a.timely.manner. == "No" ~ 0, Were.you.seen.by.a.care.provider.in.a.timely.manner. == "Yes, somewhat" ~ 1,  Were.you.seen.by.a.care.provider.in.a.timely.manner. == "Yes, mostly" ~ 2, Were.you.seen.by.a.care.provider.in.a.timely.manner. == "Yes, definitely" ~ 3))

#baseline
d <- d %>% mutate(q1b = recode(Were.you.seen.by.a.care.provider.in.a.timely.manner., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q1e = recode(Were.you.seen.by.a.care.provider.in.a.timely.manner..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))

#Q2 At.the.time.of.your.arrival..did.the.registration.staff.treat.you.with.courtesy.and.respect.

#baseline
d <- d %>% mutate(q2b = recode(At.the.time.of.your.arrival..did.the.registration.staff.treat.you.with.courtesy.and.respect., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q2e = recode(At.the.time.of.your.arrival..did.the.registration.staff.treat.you.with.courtesy.and.respect..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))

#Q3 Were.you.kept.informed.about.any.delays.

#baseline
d <- d %>% mutate(q3b = recode(Were.you.kept.informed.about.any.delays., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q3e = recode(Were.you.kept.informed.about.any.delays..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))

#Q4 Did.the.care.providers.listen.carefully.to.you.
#baseline
d <- d %>% mutate(q4b = recode(Did.the.care.providers.listen.carefully.to.you., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q4e = recode(Did.the.care.providers.listen.carefully.to.you..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q5 Did.the.care.providers.explain.things.in.a.way.you.could.understand.
#baseline
d <- d %>% mutate(q5b = recode(Did.the.care.providers.explain.things.in.a.way.you.could.understand., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q5e = recode(Did.the.care.providers.explain.things.in.a.way.you.could.understand..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q6 Did.you.know.what.to.do.if.you.had.questions.concerns.after.discharge. 
#baseline
d <- d %>% mutate(q6b = recode(Did.you.know.what.to.do.if.you.had.questions.concerns.after.discharge., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q6e = recode(Did.you.know.what.to.do.if.you.had.questions.concerns.after.discharge..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q7 Did.nurses.treat.you.with.courtesy.and.respect.
#baseline
d <- d %>% mutate(q7b = recode(Did.nurses.treat.you.with.courtesy.and.respect., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q7e = recode(Did.nurses.treat.you.with.courtesy.and.respect..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q8 Did.nurses.listen.carefully.to.you. 
#baseline
d <- d %>% mutate(q8b = recode(Did.nurses.listen.carefully.to.you., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q8e = recode(Did.nurses.listen.carefully.to.you..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q9 Did.nurses.explain.things.in.a.way.you.could.understand.
#baseline
d <- d %>% mutate(q9b = recode(Did.nurses.explain.things.in.a.way.you.could.understand., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q9e = recode(Did.nurses.explain.things.in.a.way.you.could.understand..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q10 Was.there.good.communication.between.the.different.doctors.and.nurses. 
#baseline
d <- d %>% mutate(q10b = recode(Was.there.good.communication.between.the.different.doctors.and.nurses., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q10e = recode(Was.there.good.communication.between.the.different.doctors.and.nurses..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q11 Were.you.comfortable.talking.with.nurses.about.your.worries.or.concerns.
#baseline
d <- d %>% mutate(q11b = recode(Were.you.comfortable.talking.with.nurses.about.your.worries.or.concerns., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q11e = recode(Were.you.comfortable.talking.with.nurses.about.your.worries.or.concerns..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#Q12 Did.you.have.enough.input.or.say.in.your.care.
#baseline
d <- d %>% mutate(q12b = recode(Did.you.have.enough.input.or.say.in.your.care., "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))
#exit
d <- d %>% mutate(q12e = recode(Did.you.have.enough.input.or.say.in.your.care..1, "No" = 0, "Yes, somewhat" = 1, "Yes, mostly" = 2, "Yes, definitely" = 3))

#How.likely.would.you.be.to.recommend.this.facility.to.your.family.and.friends.
#baseline
d <- d %>% mutate(q13b = recode(How.likely.would.you.be.to.recommend.this.facility.to.your.family.and.friends., "10- most likely" = 10,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9))
#exit
d <- d %>% mutate(q13e = recode(How.likely.would.you.be.to.recommend.this.facility.to.your.family.and.friends..1, "10- most likely" = 10,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9))

#split into control vs experimental

ctl <- d %>% filter(Cohort1..Virtual.Whiteboard.2..Control. == 2)
exp <- d %>% filter(Cohort1..Virtual.Whiteboard.2..Control. == 1)



#Both groups after ER stay
#timely manner
mean(ctl$q1e, na.rm = T)
mean(exp$q1e, na.rm = T)

#breakout by number in each category
table(ctl$q1e)
table(exp$q1e)

graphDataControl <- data.frame("Q" = c('Were you seen by a provider in a timely manner?'),table(ctl$q1e))
graphDataExperimental <- data.frame("Q" = c('Were you seen by a provider in a timely manner?'),table(exp$q1e))

sd(ctl$q1e, na.rm = T)
sd(exp$q1e, na.rm = T)

wilcox.test(ctl$q1e,exp$q1e)
#arrival/reg
mean(ctl$q2e, na.rm = T)
mean(exp$q2e, na.rm = T)

table(ctl$q2e)
table(exp$q2e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did the registration staff treat you with courtesy and respect?'),table(ctl$q2e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did the registration staff treat you with courtesy and respect?'),table(exp$q2e)))

#graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('q02'),table(ctl$q2e)))
sd(ctl$q2e, na.rm = T)
sd(exp$q2e, na.rm = T)

wilcox.test(ctl$q2e,exp$q2e)
#delays
mean(ctl$q3e, na.rm = T)
mean(exp$q3e, na.rm = T)

#breakout by number in each category
table(ctl$q3e)
table(exp$q3e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Were you kept informed of any delays?'),table(ctl$q3e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Were you kept informed of any delays?'),table(exp$q3e)))


sd(ctl$q3e, na.rm = T)
sd(exp$q3e, na.rm = T)

wilcox.test(ctl$q3e,exp$q3e)
#providers listen
mean(ctl$q4e, na.rm = T)
mean(exp$q4e, na.rm = T)

#breakout by number in each category
table(ctl$q4e)
table(exp$q4e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did the providers listen carefully to you?'),table(ctl$q4e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did the providers listen carefully to you?'),table(exp$q4e)))

sd(ctl$q4e, na.rm = T)
sd(exp$q4e, na.rm = T)

wilcox.test(ctl$q4e,exp$q4e)
#providers explain
mean(ctl$q5e, na.rm = T)
mean(exp$q5e, na.rm = T)

#breakout by number in each category
table(ctl$q5e)
table(exp$q5e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did the providers explain things to you in a way you could understand?'),table(ctl$q5e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did the providers explain things to you in a way you could understand?'),table(exp$q5e)))
sd(ctl$q5e, na.rm = T)
sd(exp$q5e, na.rm = T)

wilcox.test(ctl$q5e,exp$q5e)
#questions after discharge
mean(ctl$q6e, na.rm = T)
mean(exp$q6e, na.rm = T)

#breakout by number in each category
table(ctl$q6e)
table(exp$q6e)

sd(ctl$q6e, na.rm = T)
sd(exp$q6e, na.rm = T)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did you know what to do if you had questions after discharge?'),table(ctl$q6e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did you know what to do if you had questions after discharge?'),table(exp$q6e)))

wilcox.test(ctl$q6e,exp$q6e)
#nurses respect
mean(ctl$q7e, na.rm = T)
mean(exp$q7e, na.rm = T)

#breakout by number in each category
table(ctl$q7e)
table(exp$q7e)

sd(ctl$q7e, na.rm = T)
sd(exp$q7e, na.rm = T)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did the nurses treat you with courtesy and respect?'),table(ctl$q7e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did the nurses treat you with courtesy and respect?'),table(exp$q7e)))

wilcox.test(ctl$q7e,exp$q7e)
#nurses listen carefully
mean(ctl$q8e, na.rm = T)
mean(exp$q8e, na.rm = T)

#breakout by number in each category
table(ctl$q8e)
table(exp$q8e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did the nurses listen carefully?'),table(ctl$q8e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did the nurses listen carefully?'),table(exp$q8e)))
sd(ctl$q8e, na.rm = T)
sd(exp$q8e, na.rm = T)

wilcox.test(ctl$q8e,exp$q8e)
#nurses explain
mean(ctl$q9e, na.rm = T)
mean(exp$q9e, na.rm = T)

#breakout by number in each category
table(ctl$q9e)
table(exp$q9e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did the nurses explain things in a way you could understand?'),table(ctl$q9e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did the nurses explain things in a way you could understand?'),table(exp$q9e)))
sd(ctl$q9e, na.rm = T)
sd(exp$q9e, na.rm = T)

wilcox.test(ctl$q9e,exp$q9e)
#good communication
mean(ctl$q10e, na.rm = T)
mean(exp$q10e, na.rm = T)

#breakout by number in each category
table(ctl$q10e)
table(exp$q10e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Was there good communication between the different doctors and nurses?'),table(ctl$q10e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Was there good communication between the different doctors and nurses?'),table(exp$q10e)))

sd(ctl$q10e, na.rm = T)
sd(exp$q10e, na.rm = T)


wilcox.test(ctl$q10e,exp$q10e)
#comfortable talking with nurses
mean(ctl$q11e, na.rm = T)
mean(exp$q11e, na.rm = T)

sd(ctl$q11e, na.rm = T)
sd(exp$q11e, na.rm = T)

#breakout by number in each category
table(ctl$q11e)
table(exp$q11e)


graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Were you comfortable talking with nurses about your worries or concerns?'),table(ctl$q11e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Were you comfortable talking with nurses about your worries or concerns?'),table(exp$q11e)))
wilcox.test(ctl$q11e,exp$q11e)
#input into care
mean(ctl$q12e, na.rm = T)
mean(exp$q12e, na.rm = T)

#breakout by number in each category
table(ctl$q12e)
table(exp$q12e)

graphDataControl <- rbind(graphDataControl, data.frame("Q" = c('Did you have enough input or say in your care?'),table(ctl$q12e)))
graphDataExperimental <- rbind(graphDataExperimental, data.frame("Q" = c('Did you have enough input or say in your care?'),table(exp$q12e)))
sd(ctl$q12e, na.rm = T)
sd(exp$q12e, na.rm = T)

wilcox.test(ctl$q12e,exp$q12e)

graphDataControl <- graphDataControl %>% rename("Score" = "Var1")
graphDataExperimental <- graphDataExperimental %>% rename("Score" = "Var1")
#p <- ggplot(data = subset(graphDataControl, Score == 3), aes(x = Q, y = Freq)) + geom_col(aes(fill = Score), width = 0.7) +scale_fill_brewer(palette="Greys") +coord_flip()

#p

graphDataControl$Score = factor(graphDataControl$Score, levels = c(0,1,2,3))

plot <- ggplot(graphDataControl, aes(x = Q, y=Freq)) +
  geom_col(data = subset(graphDataControl, Score == 3), 
           aes(y = Freq, fill = factor(Score))) +
  geom_text(aes(label = Freq), hjust = 0, color = '#555555') +
  geom_col(data = subset(graphDataControl, Score != 3), 
           aes(y = -Freq, fill = factor(Score))) +
  coord_flip() + scale_fill_brewer(palette="Greys", name="Response", breaks=c(0,1,2,3), labels=c("No", "Yes, somewhat", "Yes, mostly","Yes, definitely"))+
  geom_hline(aes(yintercept = 0, linetype = "test"), colour = "black", size = 0.75, show.legend = F)+
  labs(title = "Control Group", x =  NULL, y= 'n')+
  theme_gray(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = label_wrap(30)) +
  scale_y_continuous(breaks = seq(-20,50, by = 5),
                     labels = (c(seq(20, 0, by = -5), seq(5,50,by=5))))
#control
plot

graphDataExperimental$Score = factor(graphDataExperimental$Score, levels = c(0,1,2,3))

plot2 <- ggplot(graphDataExperimental, aes(x = Q,y=Freq)) +
  geom_col(data = subset(graphDataExperimental, Score == 3), 
           aes(y = Freq, fill = factor(Score))) +
  geom_text(aes(label = Freq), hjust = 0, color = '#555555') +
  geom_col(data = subset(graphDataExperimental, Score != 3), 
           aes(y = -Freq, fill = factor(Score))) +
  coord_flip() + scale_fill_brewer(palette="Greys", name="Response", breaks=c(0,1,2,3), labels=c("No", "Yes, somewhat", "Yes, mostly","Yes, definitely"))+
  geom_hline(aes(yintercept = 0, linetype = "test"), colour = "black", size = 0.75, show.legend = F)+
  labs(title = "Experimental Group", x =  NULL, y= 'n')+
  theme_gray(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = label_wrap(30)) +
  scale_y_continuous(breaks = seq(-20,50, by = 5),
                     labels = (c(seq(20, 0, by = -5), seq(5,50,by=5))))
#experimental
plot2
