


	#PACKAGES NEEDED
#install.packages("PairedData")
#install.packages("ggplot2")
#install.packages("ggfortify")
#install.packages("agricolae")
#install.packages("DescTools")
#install.packages("ggpubr")
#install.packages("ggsignif")
#install.packages("DescTools")
#install.packages("lindia")
#install.packages("PMCMR")
#install.packages("emmeans")
#install.packages("conover.test")
#install.packages("RColorBrewer")
#install.packages("lmerTest")
#install.packages("rcompanion")
#install.packages("tidyverse")
#install.packages("interactions")

library(beepr)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(ggsignif)
library(agricolae)
library(DescTools)
library(rcompanion)
library(RColorBrewer)
library(svglite)
library(dplyr)
library(MASS)
library(lindia)
library(car)
library(multcomp)
library(emmeans)
library(conover.test)
library(broom)
library(rstatix)
library(PMCMR)
library(interactions)
library(lme4)
library(lmerTest)
library(lattice)
library(emmeans)
library(tidyverse)
library(psych)
library(psycho)


# CLEAN SCREEN AND MEMORY

rm(list = ls())		#Remove all objects
cat("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n") # I like clean spaces when trying the same analysis with slight changes or differents datasets



# SETTINGS:


# 1) INPUT FILE(S)



# DIRECTORY

getwd() #check
#Change working directory if needed
#setwd("D:/Dropbox/R/habituacija muralis")


measurespod = read.table("Data/habituacija controla.csv", sep = ';', header =TRUE)	  			#read the data into a table, read headers
str(measurespod)
tail(measurespod)

measurespod

#Set up the number of initial columns with individual or non-relevant/usaable data
otherstuff = 0
#Set up the number of columns with Trialing variable
groupvar = 4
randomvar = 0



################################## Below this don't touch anythink unless you know what you are doing

coltags <- names(measurespod)
maxcol <- ncol (measurespod)								#number of columns
randomgrup = randomvar
lastgrup = otherstuff + groupvar + randomvar				#column with the last Trialing variable

firstvar = lastgrup + 1										# column with the first variable
firstgrup = otherstuff+1
numvar = maxcol - lastgrup
varinfo <- measurespod[c(firstvar:maxcol)]									#take a subset including the data of the measured variables
groupinfo <- measurespod[c(firstgrup:lastgrup)]									#take a subset with the grouping variables to test
groupnames <- names (groupinfo)
colgroup = c(firstgrup:lastgrup)
colgrouprand = c(randomgrup)
colvariab = c(firstvar:maxcol)
varnames <- names(varinfo)

#CHECK BEFORE PROCEEDING
cat ("Check that this is alright:\n\t", maxcol, "columns in TOTAL\n\t", otherstuff, "with individual/non-relevant data\n\t", groupvar, " columns with grouping variables:", groupnames, "\n\t", numvar, "numeric variables:", varnames, "\n")
measurespod$Trial <-  as.factor(measurespod$Trial)
##for plots


groupname <- unique(measurespod$group)
numgroups <- length(groupnames)
groupname

#za grafove za značajnosti
minimum <- min(measurespod$ea)
maximum <- max(measurespod$ea)
rangeval = maximum - minimum
dotsize = (rangeval/100)*2
unit <- rangeval/100
Qart <- quantile(measurespod$ea)
Quartiles <- as.data.frame(Qart)
			
q1 <- Quartiles[1,] - unit
q5 <- Quartiles[5,] + unit


#####   Subseting   ##############    ^2
measurespod_sub <- filter(measurespod, group == c("1")| Trial == c("2")| Trial == c("3"))
"2","3"))
measurespod_sub
measurespod_sub <- filter(measurespod, Experiment == "Control")
measurespod_sub <- filter(measurespod_sub, !Subject == "F17")
measurespod_sub
t_measurespod <- measurespod [-c(1,),]
t_measurespod

m.lm <- lmer( (Distance_moved)~ Test + (1|Id) ,data=measurespod_sub)
#if data needs to be transformed use his line - put it in the script
t_measurespod <- measurespod %>% mutate_at(.vars = vars(firstvar:maxcol), list (~(.+1)))  # ako nesto ne radi - umjesto list je prije bilo funs (.+1))
str(t_measurespod)

#if data needs to be transformed use his line - put it in the script - square root
t_measurespod <- measurespod %>% mutate_at(.vars = vars(firstvar:maxcol), list (~sqrt(.+1)))
(t_measurespod)


#if data is normal use fixed or mixed model
######################             fixed model      #####################################
m.lm <- lmer( sqrt(Distance_moved) ~ Trial + (1|Subject) ,data=measurespod_sub)

str(measurespod)
# 1st plot -  if residuals have non-linear patterns.  If you find equally spread residuals around a 
#horizontal line without distinct patterns, that is a good indication you don’t have non-linear relationships.
# 2nd plot - normality
#3rd plot homoscedasticity- This plot shows if residuals are spread equally along the ranges of predictors
# It’s good if you see a horizontal line with equally (randomly) spread points.
#4th plot-  find influential cases - Not all outliers are influential in linear regression analysis

qqnorm(resid(m.lm))
## standardized residuals versus fitted values by gender or species (what you specify after | )
plot(m.lm, resid(., scaled=TRUE) ~ fitted(.) | Trial, abline = 0)
## standardized residuals versus fitted values by gender or species (what you specify after | )
plot(m.lm, resid(., scaled=TRUE) ~ fitted(.) | Trial, abline = 0)
## standardized residuals versus fitted values by gender or species (what you specify after | )
plot(m.lm, resid(., scaled=TRUE) ~ fitted(.) | Sex, abline = 0)
## box-plots of residuals by Id
plot(m.lm, Subject ~ resid(., scaled=TRUE))
## observed versus fitted values by Id
plot(m.lm,   ea  ~ fitted(.) | Subject, abline = c(0,1))

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
par(mfrow=c(1,1)) # Change back to 1 x 1
par(mfrow=c(1,2))
p1+p2
ggarrange(p1, p2,  
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
qqmath(m.lm, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)
plot(m.lm) ###residuals plot
plot_ranef(m.lm)

plot(resid(m.lm))
#Calculate leverage
lev<-hat(model.matrix(m.lm))
lev
#Plot leverage against standardised residuals
plot(resid(m.lm,type="pearson")~lev,las=1,ylab="Standardised residuals",xlab="Leverage" )
text(resid(m.lm,type="pearson")~lev, labels=1:200, cex=0.8,  pos=1)
measurespod[126,]


shapiro.test(residuals(m.lm)) # needs to be non significant
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
bartlett.test(   (ea ) ~Trial, data=measurespod_sub) #by Sex
bartlett.test(   (ea )  ~Sex, data=measurespod) #by species
bartlett.test(   (ea ) ~ interaction(Trial,Trial), data=measurespod)
#levene test - needs to be non significant
leveneTest( (ea ) ~ Day, center = mean, data = measurespod)
leveneTest( ea  ~ Sex, center = mean, data = measurespod)
leveneTest( (ea ) ~ interaction(Day,Sex), center = mean, data = measurespod)

interact_plot(m.lm, pred = ea, modx = Trial)
### to save run everthing in order to get proper save
anova(m.lm)
m = anova(m.lm)
linear_model = as.matrix(m)
linear_model
###save results
print(m)
library(xlsx)
write.xlsx(linear_model, "linear_model_Distance_moved_Control.xlsx")


##### novi save
install.packages("devtools")
devtools::install_github("kassambara/r2excel")
library(r2excel)
#create workbook only once 
wb <- createWorkbook(type="xlsx")
sheet <- createSheet(wb, sheetName = "example1")


#add results to workbook
xlsx.addHeader(wb, sheet, 
     value="ea ")
xlsx.addLineBreak(sheet, 1)
xlsx.addTable(wb, sheet, m)
xlsx.addLineBreak(sheet, 2)


#save when done
saveWorkbook(wb, "linear_model_results.xlsx")


Anova(m.lm, white.adjust =TRUE)##welch F test

#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm, lsm(pairwise~Trial), test = adjusted("BH")))
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "posthoc_Distance_moved_Control.xlsx")  ####if p value is less than 0.001 it gives 0 in save. just replace in excel to <0.001

##calculate means for line plot
by_species <- measurespod_sub %>% group_by(Trial)
by_species
by_Trial <- measurespod_sub %>% group_by(Trial)

by_species_numeric <- by_species %>% summarise_if(is.numeric,  tibble::lst(mean,sd,min,max))   ### probaj ovo dodat u funs (n())
by_species_numeric

#plot line plot by trial and sex
p <- ggplot(by_species_numeric, aes(x=Trial, y=Distance_moved_mean, group = Sex)) +
    geom_line(aes(color= Sex), position=position_dodge(0.15), alpha = 1.5)	+
	geom_point(aes(color= Sex, shape=Sex) ,position=position_dodge(0.15), size=2.5)+
	scale_color_brewer(palette="Paired") +
	geom_errorbar( aes(x=Trial, ymin=Distance_moved_mean - Distance_moved_sd, ymax=Distance_moved_mean+ Distance_moved_sd), width=0.2, position=position_dodge(0.15), colour="black", alpha=0.7, size=0.1) 
	
p

   #stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75), shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
   #stat_pvalue_manual(stat.test.dunn,  bracket.shorten = 0.1 , y.position = maximum, step.increase = - 0.05, label = "p.adj.signif", tip.length = 0.00)
p+labs(y = "Distance moved")+theme_classic()


print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
tiff (filename="graph_line_Distance_moved Trial_sex .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_line_Distance_moved Trial_sex .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()


#plot line plot by trial
by_species_numeric$Trial <- factor(by_species_numeric$Trial, levels = paste0("T", 1:10))
p <- ggplot(by_species_numeric, aes(x=Trial, y=Distance_moved_mean, group=1 )) +
    geom_line()+	
	geom_point() +
	geom_errorbar( aes(x=Trial, ymin=Distance_moved_mean - Distance_moved_sd,	ymax=Distance_moved_mean + Distance_moved_sd), width=0.2,	colour="black", alpha=0.7, size=0.1)
	
p
   #stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75), shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
   #stat_pvalue_manual(stat.test.dunn,  bracket.shorten = 0.1 , y.position = maximum, step.increase = - 0.05, label = "p.adj.signif", tip.length = 0.00)
p+labs(y = "Distance moved")+theme_classic()


print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
tiff (filename="graph_line_Distance_moved Trial_Control .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_line_Distance_moved Trial_Control .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()


 #plot nicer line plot by Trial
p <- ggplot(measurespod, aes(x=Trial, y=ea)) + #,label =Subject
	   geom_boxplot(aes (x=Trial)) +	
		#geom_text()
   scale_fill_brewer(palette="Paired") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75),  shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background


tiff (filename="graph_Distance_moved box plot Trial  .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved box plot Trial .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()




# plot nicer line plot by Trial
p <- ggplot(measurespod, aes(x=Trial, y=ea,group=Trial)) +
   geom_boxplot() +
   scale_fill_brewer(palette="Paired") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75),  shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background


tiff (filename="graph_Distance_moved Test .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved test .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()

#####  save interaction plot 
int_plot <- interact_plot(m.lm, pred = Distance moved, modx = test)
int_plot
print(int_plot+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background

tiff (filename="graph_Distance moved interaction plot .tiff", units="cm", width=11, height=8, res=600)	#Save to file
print(int_plot+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved interaction plot .svg", width=4.33, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(int_plot+labs(y = "Distance moved")+theme_classic())
dev.off()

measurespod

str(measurespod)
##subset data for outliers                        #######################################
t_measurespod_corr <- filter(measurespod, !(Subject == "F16" & Experiment == "Test"))##select with multiple conditions
t_measurespod_corr <- filter(t_measurespod_corr, !(Subject == "M12" & Experiment == "Test")) 
 t_measurespod_corr <- filter(t_measurespod_corr, !(Subject == "M20" & Experiment == "Test"))
 t_measurespod_corr <- filter(measurespod$experiment %in% !subject ==  "F17")
t_measurespod_corr <- filter(measurespod, !Subject ==  "F17") %>% filter(measurespod, !Subject ==  "F17")
t_measurespod_corr = filter(t_measurespod_corr, !Subject ==  "F1")
t_measurespod_corr = filter(t_measurespod_corr, !Subject ==  "F10")
t_measurespod_corr = filter(t_measurespod_corr, !Subject ==  "F6")
t_measurespod_corr

t_measurespod_corr <- select(t_measurespod_corr, contains("F17"))
t_measurespod_corr

res <- t.test(ea~ Trial, data = t_measurespod_corr, paired = FALSE)## var.equal = TRUE bez korekcije, obični t test
res
######################             fixed model without outliers             ##################
m.lm <- lmer(  ea ~ Trial*Sex + (1|Subject)  ,data=t_measurespod_corr)
#plot dispersion of errors - 3rd plot Homogenost varijanci
plot(m.lm)
qqPlot(m.lm)
qqmath(m.lm, id=0.05)
gg_reshist(m.lm)#look for gaus distribution
shapiro.test(residuals(m.lm)) # needs to be non significant
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
bartlett.test(   (Distance_moved) ~Trial, data=t_measurespod_corr) #by Sex
bartlett.test(   (Distance_moved)  ~Sex, data=t_measurespod_corr) #by species
bartlett.test(   (Distance_moved) ~ interaction(Trial,Sex), data=t_measurespod_corr)
#levene test - needs to be non significant
leveneTest( (Distance_moved) ~ Trial, center = mean, data = t_measurespod_corr)
leveneTest( ea ~ Sex, center = mean, data = t_measurespod_corr)
leveneTest( (Distance_moved) ~ interaction(Trial,Sex), center = mean, data = t_measurespod_corr)
### if significant scroll to end of script and do welch or brown forsythe F test
#type 3 for unbalanced design :
#Anova(m.lm, contrasts=list(Trial=contr.sum, Sex=contr.sum), type=3)
### to save run everthing in order to get proper save
anova(m.lm)
m = anova(m.lm)
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/habituacija/linear_model_Distance_moved_bez outliera_controla .xlsx")
####if bartlet is significant use anova with adjustment -> Anova(m.lm, white.adjust =TRUE)


m = Anova(m.lm, white.adjust =TRUE)
   
vif(m.lm)

#posthoc test
lsm <- lsmeans(m.lm, list(pairwise ~ Trial*Sex), adjust = "tukey")
lsm
lsm <- lsmeans(m.lm, list(pairwise ~ Trial*Sex))
lsm.glht <- as.glht(lsm)
summary(lsm.glht, lsm(pairwise~Species*Sex), test = adjusted("BH"))

#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm, lsm(pairwise~Trial), test = adjusted("BH")))
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/habituacija/posthoc_Distance_moved_bez outliera controla.xlsx")

### prepare means for line plot
by_species <- t_measurespod_corr %>% group_by(Trial,Experiment)
by_species
by_Trial <- t_measurespod_corr %>% group_by(Trial)

by_species_numeric <- by_species %>% summarise_if(is.numeric,  tibble::lst(mean,sd,min,max))   ### probaj ovo dodat u funs (n())
by_species_numeric

####plot line plot
by_species_numeric$Trial <- factor(by_species_numeric$Trial, levels = paste0("T", 1:10))
p1 <- ggplot(by_species_numeric, aes(x=Trial, y=Distance_moved_mean, group = Experiment)) +
    geom_line(aes(color= Experiment), position=position_dodge(0.15), alpha = 1.5)	+
	geom_point(aes(color= Experiment, shape=Experiment) ,position=position_dodge(0.15), size=2.5)+
	scale_color_brewer(palette="Set1") +
	geom_errorbar( aes(x=Trial, ymin=Distance_moved_mean - Distance_moved_sd, ymax=Distance_moved_mean+ Distance_moved_sd), width=0.2, position=position_dodge(0.15), colour="black", alpha=0.7, size=0.1) 
	
p1
   #stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75), shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
   #stat_pvalue_manual(stat.test.dunn,  bracket.shorten = 0.1 , y.position = maximum, step.increase = - 0.05, label = "p.adj.signif", tip.length = 0.00)
p1+labs(y = "Distance moved")+theme_classic()


print(p1+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
tiff (filename="graph_line_Distance_moved Trial_experiment bez_outliera.tiff", units="cm", width=11, height=10, res=600)	#Save to file
print(p1+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_line_Distance_moved Trial_experiment bez_outliera.svg", width=4.33, height=3.93)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p1+labs(y = "Distance moved")+theme_classic())
dev.off()


####plot line plot by trial
p2 <- ggplot(by_species_numeric, aes(x=Trial, y=Distance_moved_mean, group = 1)) +
    geom_line()	+
	geom_point()+
	scale_color_brewer(palette="Paired") +
	geom_errorbar( aes(x=Trial, ymin=Distance_moved_mean - Distance_moved_sd, ymax=Distance_moved_mean+ Distance_moved_sd), width=0.2, position=position_dodge(0.15), colour="black", alpha=0.7, size=0.1) 
	
p2
   #stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75), shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
   #stat_pvalue_manual(stat.test.dunn,  bracket.shorten = 0.1 , y.position = maximum, step.increase = - 0.05, label = "p.adj.signif", tip.length = 0.00)
p2+labs(y = "Distance moved")+theme_classic()


print(p2+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
tiff (filename="graph_line_Distance_moved Trial bez outliera .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p2+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_line_Distance_moved Trial bez outliera .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p2+labs(y = "Distance moved")+theme_classic())
dev.off()



# plot nicer line plot by Trial
p <- ggplot(t_measurespod_corr, aes(x=Trial, y=ea,fill=Sex)) +
   geom_boxplot() +
   scale_fill_brewer(palette="Paired") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75),  shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly   

	
p

print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background

tiff (filename="graph_Distance_moved Trial_sex bez outliera .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved Trial_sex bez outliera .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()


############################### 	transf.arcsin	^2			trasnformed variable 			###################################################
#install.packages("metafor")

library(metafor )

m.lm2 <- lmer( sqrt(Distance_moved)~ Trial*Sex + (1|Subject) , data=t_measurespod_corr) #**(-2) is transformation
qqnorm(resid(m.lm2))
qqmath(m.lm2, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)
plot(m.lm2) ###residuals plot

plot(resid(m.lm))
shapiro.test(residuals(m.lm2))

summary(m.lm2)
anova(m.lm2)
m=anova(m.lm2)
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/habituacija/linear_model_Distance_moved_bez outliera .xlsx")

coefficients(m.lm2)
####scroll down to save everthing
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
bartlett.test(  sqrt(Distance_moved) ~Trial, data=t_measurespod_corr) #by Sex
bartlett.test( log10(Distance_moved) ~Trial, data=t_measurespod_corr) #by species
bartlett.test( log10(Distance_moved)~ interaction(Trial,Sex), data=t_measurespod_corr)

m=Anova(m.lm2, white.adjust =TRUE)
m
#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm, lsm(pairwise~Trial), test = adjusted("BH")))
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/habituacija/posthoc_Distance_moved_trial_bez_outliera.xlsx")



p <- ggplot(t_measurespod_corr, aes(x=Trial, y=ea, fill=Sex)) +
   geom_boxplot() +
   scale_fill_brewer(palette="Paired")  + #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75), shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly
   # stat_pvalue_manual(stat.test.dunn,  bracket.shorten = 0.1 , y.position = maximum + 0.5, step.increase = - 0.05, label = "p.adj.signif", tip.length = 0.00, )
p
print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
tiff (filename="graph_Distance_moved Trial_sex .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved Trial_sex  .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()


# plot nicer line plot by Trial
p <- ggplot(t_measurespod_corr, aes(x=Trial, y=ea )) +
   geom_boxplot() +
   scale_fill_brewer(palette="Paired") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75),  shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly   

	
print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background

tiff (filename="graph_Distance_moved Trial  .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved Trial  .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()





#find valid transformation
library(MASS)
library(VGAM)
#install.packages("VGAM")
boxcox(m.lm, lambda = seq(-0.3, 0.3, length = 10))
b <- boxcox(m.lm, lambda = seq(-0.3, 0.3, length = 10))
b 
lambda <- b$x # lambda values
lik <- b$y # log likelihood values for SSE
bc <- cbind(lambda, lik) # combine lambda and lik
sorted_bc <- bc[order(-lik),] # values are sorted to identify the lambda value for the maximum log likelihood for obtaining minimum SSE
head(sorted_bc, n = 10)

#Table 1: Common Box-Cox Transformations
# l			Y’
# -2	Y-2 = 1/Y2
# -1	Y-1 = 1/Y1
# -0.5	Y-0.5 = 1/(Sqrt(Y))
# 0		log(Y)
# 0.5	Y0.5 = Sqrt(Y)
# 1		Y1 = Y
# 2			Y2


shapiro.test(residuals(m.lm))

###########################  analyse trasnformed variable  ^3    #############################
m.lm2 <- lmer( sqrt(Distance_moved) ~ Trial*Sex + (1|Subject)  , data=measurespod) #**(-2) is transformation
measurespod
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
par(mfrow=c(1,1)) # Change back to 1 x 1

plot(m.lm2)
qqnorm(resid(m.lm2))
qqmath(m.lm2, id=0.05)
shapiro.test(residuals(m.lm2))
summary(m.lm2)
anova(m.lm2)
coefficients(m.lm2)
#Calculate leverage
lev<-hat(model.matrix(m.lm2))
#Plot leverage against standardised residuals
plot(resid(m.lm2,type="pearson")~lev,las=1,ylab="Standardised residuals",xlab="Leverage" )
text(resid(m.lm2,type="pearson")~lev, labels=1:200, cex=0.8,  pos=1)
measurespod[103,]

### to save run everthing in order to get proper save
anova(m.lm2)
m = anova(m.lm2)
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/habituacija/linear_model_Distance_moved_controla.xlsx")
####if bartlet is significant use anova with adjustment -> Anova(m.lm, white.adjust =TRUE)
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
bartlett.test(  sqrt(Distance_moved) ~Trial, data=measurespod) #by Sex
bartlett.test( sqrt(Distance_moved) ~Sex, data=measurespod) #by species
bartlett.test( log10(Distance_moved)~ interaction(Trial,Trial), data=measurespod)

Anova(m.lm2, white.adjust =TRUE)

#posthoc test
lsm <- lsmeans(m.lm2, list(pairwise ~ Trial*Sex), adjust = "tukey")
lsm
lsm <- lsmeans(m.lm2, list(pairwise ~ Trial*Sex))
lsm.glht <- as.glht(lsm)
summary(lsm.glht, lsm(pairwise~Species*Sex), test = adjusted("BH"))

#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm2, lsm(pairwise~Trial), test = adjusted("BH")))
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/habituacija/posthoc_Distance_moved_controla.xlsx")







# plot nicer line plot by Trial and Sex
stat.test.dunn <- measurespod %>% dunn_Control(ea~ Trial, p.adjust.method = "BH") 
stat.test.dunn <- stat.test.dunn %>% add_x_position(dodge = 1)
stat.test.dunn

p <- ggplot(measurespod, aes(x=Trial, y=ea,)) +
   geom_boxplot() +
   scale_fill_brewer(palette="Paired") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75), shape=2, size=2) + ## position_dodge(width = 0.75) play with number to position mean value correctly
    stat_pvalue_manual(stat.test.dunn,  bracket.shorten = 0.1 , y.position = maximum + 0.5, step.increase = - 0.05, label = "p.adj.signif", tip.length = 0.00, )

print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
tiff (filename="graph_Distance_moved Trial_sex .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved Trial_sex .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()


# plot nicer line plot by Trial
p <- ggplot(measurespod, aes(x=Trial, y=ea)) +
   geom_boxplot() +
   scale_fill_brewer(palette="Paired") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75),  shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly   
print(p+labs(y = "Distance moved")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background

tiff (filename="graph_Distance_moved controla .tiff", units="cm", width=9, height=8, res=600)	#Save to file
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()
## save as svg file for editing options
svg ("graph_Distance_moved controla .svg", width=3.54, height=3.15)	#Save to file, size is in inches - divide cm with 2.54 to get inch
print(p+labs(y = "Distance moved")+theme_classic())
dev.off()


############### end ###########################






#posthoc test
emm <- emmeans(m.lm2, list(pairwise ~ Species*Sex), adjust = "tukey")
emm
summary(emm, infer = TRUE, null = log(35), type = "response")   ?????
emm2 <- emmeans(m.lm2, "Species", "Sex",  transform = "response") #ok
emm2
lsm <- lsmeans(m.lm2, list(pairwise ~ Species*Sex))
lsm.glht <- as.glht(lsm)
summary(lsm.glht, lsm(pairwise~Species*Sex), test = adjusted("holm"))

#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm2, lsm(pairwise~Species*Sex), test = adjusted("BH")))

#save posthoc test to output
posthoc_out <- capture.output(emm) #capture output
cat("\n\n", file = "posthoc.txt", append = TRUE)
cat("\t##  ea  ##\n\n", file="posthoc.txt", append = TRUE)  #Title to print in the file
cat("\n\n", file = "posthoc.txt", append = TRUE)
cat(posthoc_out, file="posthoc.txt", sep="\n", append=TRUE) #Output to print in the file

#save posthoc test backtransformation to output
posthoc_out <- capture.output(emm2) #capture output
cat("\n\n", file = "posthoc.txt", append = TRUE)
cat("\t##  ea  ##\n\n", file="posthoc.txt", append = TRUE)  #Title to print in the file
cat("\n\n", file = "posthoc.txt", append = TRUE)
cat(posthoc_out, file="posthoc.txt", sep="\n", append=TRUE) #Output to print in the file








######################             fixed model without outliers  with covariate           ##################
m.lm <- lm( ea + Species*Sex + Batch ,data=t_measurespod_corr)
#plot dispersion of errors - 3rd plot Homogenost varijanci
plot(m.lm)
gg_reshist(m.lm)
shapiro.test(residuals(m.lm)) # needs to be non significant
anova(m.lm)
Anova(m.lm)
summary(m.lm)
vif(m.lm)

###save results
LMM_out <- capture.output(anova(m.lm)) #capture output
cat("\n\n", file = "LMM.txt", append = TRUE)
cat("\t##  eabez outliera  ##\n\n", file="LMM.txt", append = TRUE)  #Title to print in the file
cat("\n\n", file = "LMM.txt", append = TRUE)
cat(LMM_out, file="LMM.txt", sep="\n", append=TRUE) #Output to print in the file



kwtest = kruskal.test(ea~Trial, data=measurespod)
print(kwtest)
ConoverTest(  ea~Trial ,data=measurespod, method = "holm")

wmwtest = wilcox.test(ea~Species, data=measurespod)
print(wmwtest)


#######################   ANOVA or Welch F test when homogeneity of variances has been violated ##################### 
#install.packages("onewaytests")
library(onewaytests)
out <- welch.test( ea ~  Sex ,data=measurespod)

paircomp(out)
out <- welch.test( ea ~  interaction(Trial,Sex) ,data=measurespod)

interaction(Trial,Sex)

############Anova with heteroscedasticity-corrected coefficient covariance matrix

Anova(m.lm, white.adjust =TRUE)

summary(m.lm)
m = Anova(m.lm, white.adjust =TRUE)
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/duje pijavica/linear_model_hetero_corr ea.xlsx")


#for reference save also like this
posthoc_out <- capture.output(lsm) #capture output
cat("\n\n", file = "posthoc.txt", append = TRUE)
cat("\t##  ea  ##\n\n", file="posthoc.txt", append = TRUE)  #Title to print in the file
cat("\n\n", file = "posthoc.txt", append = TRUE)
cat(posthoc_out, file="posthoc.txt", sep="\n", append=TRUE) #Output to print in the file








