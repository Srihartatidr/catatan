library(tidyverse) # data wrangling and visualization
library(reshape2)  # data wrangling
library(lattice)   # for plotting
library(sjPlot)    # to visualizing random effects
library(ggeffects) # for plotting predictions of MEM
library(knitr)     # beautifying tables
library(lme4)      # "golden standard" for mixed-effects modelling in R (no p-values)
library(lmerTest)  # p-values for MEMs based on the Satterthwaite approximation
library(report)    # to report the model results
library(broom)     # for tidy results
library(data.table) # extracting data

#mengambil data
stroke <- fread("http://www.statsci.org/data/oz/stroke.txt")
View (stroke)
str(stroke)

#transformasi data wide to long
library(tidyverse)
stroke_long = stroke %>% select(c(1:6,39:46)) %>% 
  pivot_longer(cols=Bart1:Bart8,
               names_to= "time",
               names_prefix = "Bart",
               values_to = "ability")
names(stroke_long)
glimpse(stroke_long)
View (stroke_long)

#NOMOR 1
#Menganalisis perbedaan dari Functional Ability pekan pertama (Bart1) berdasarkan grup intervensi (Group) dgn visualisasi boxplot
#Membuat grafik boxplot dan menginterpretasi dan menyimpulkan hasilnya.
boxplot(Bart1 ~ Group, data = stroke)

#NOMOR 2
#Memeriksa normalitas data Bart1
#Menggunakan shapiro-wilk test untuk ukuran sampel kecil
shapiro.test(stroke$Bart1)

#NOMOR 3
#Membuat variabel Bart_diff
stroke$Bart_diff <- stroke$Bart8 - stroke$Bart1
str(stroke$Bart_diff)

#NOMOR 4
#Memeriksa normalitas data Bart_diff
#Menggunakan shapiro-wilk test untuk ukuran sampel kecil
shapiro.test(stroke$Bart_diff)

#NOMOR 5
#Memeriksa variance
#visualisasi dengan boxplot
library(car)
boxplot(Bart_diff ~ Group, xlab='Group', ylab='Bart_diff', data=stroke)
bartlett.test(Bart_diff ~ Group, data=stroke)

#NOMOR 6
#Membuat plot mean dan 95% CI dari Bart_diff berdasarkan Group
#Langkah 1: Menghitung mean dan 95% CI
library(rcompanion)
Sum = groupwiseMean(Bart_diff ~ Group,
                    data   = stroke,
                    conf   = 0.95,
                    digits = 3)

Sum

#Langkah 2: Membuat interval plot
library(ggplot2)
qplot(x = Group , y = Mean, data = Sum) +
  geom_errorbar(aes(ymin = Trad.lower, ymax  = Trad.upper, width = 0.15))
theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Mean of Bart_diff")

#NOMOR 7
#Melakukan uji anova
res.aov <- aov(Bart_diff ~ Group, data = stroke)
summary(res.aov)

#NOMOR 8
#membuat model linear regresi dengan Bartlet sebagai outcome
#dan explanatory variables: waktu(time/week), grup intervensi (group), dan interaksi waktu dan grup intervensi.

#fit model regresi linear dengan interaksi
stroke_long$Group <- as.factor(stroke_long$Group)
mod_lm1 <- lm(ability ~ time + Group + time*Group, data = stroke_long)
summary(mod_lm1)

#NOMOR 9
#melakukan ulang langkah no 8 tanpa variabel interaksi di dalam model
mod_lm2 <- lm(ability ~ time + Group, data = stroke_long)
summary(mod_lm2)

#NOMOr 10
#Menghitung AIC dari model no 8 dan 9, serta menginterpretasikan perbandingan nilai AIC-nya
library(AICcmodavg)

#membuat daftar model
models <- list(mod_lm1, mod_lm2)

#memberi nama model
mod.names <- c('mod1_interaksi', 'mod2_tanpa interaksi')

#menghitung AIC dari setiap model
aictab(cand.set = models, modnames = mod.names)

#NOMOR 12
#Melakukan analisis mixed model (random intercept) menggunakan package nlme.
#Functional Ability (Bartlet) sebagai outcome(y)
#Explanatory variables meliputi: waktu(time/week), grup intervensi (group), dan Random intercept.
library(nlme)
library(lme4)
library(lmerTest)

mm1 <- lmer(ability ~ time + Group + (1 | Subject) , data = stroke_long)
summary(mm1)

#NOMOR 13
#Melakukan ulang analisis dengan Functional Ability (Bartlet) sebagai outcome
#dan explanatory variables meliputi: waktu(time/week), grup intervensi (group)
#dengan General Estimating Equation (GEE) dengan correlation structure:
#exchangeable, auto regressive, unstructured

library(geepack)
#exchangeable
gee.exc<-geeglm(ability ~ as.factor(Group) + as.numeric(time), family=gaussian,
                data = stroke_long, id = as.factor(Subject), wave = as.numeric(time), corst="exchangeable")
summary(gee.exc)

#unstructured
gee.un<-geeglm(ability ~ as.factor(Group) + as.numeric(time), family=gaussian,
               data = stroke_long, id = as.factor(Subject), wave = as.numeric(time), corst="unstructured")
summary(gee.un)

#auto regressive
gee.ar1<-geeglm(ability ~ as.factor(Group) + as.numeric(time), family=gaussian,
                data=stroke_long, id=as.factor(Subject), wave=as.numeric(time), corst="ar1")
summary(gee.ar1)

library(nlme)
rndeff<-lme(ability~as.factor(Group) + as.numeric(time), data=stroke_long,
            random=~1|Subject)
summary(rndeff)

#NOMOR 14
#Mengingat GEE tidak dapat mengeluarkan AIC
#dengan menggunakan statement gls
#menghitung AIC dari model GLS dengan ketiga struktur korelasi di atas.

exch<-corCompSymm(form = ~ 1 | Subject)
gls.exch<-gls(ability~as.factor(Group)+as.numeric(time), data=stroke_long,
              correlation=exch)

ar1<-corAR1(form = ~ 1 | Subject)
gls.ar1<-gls(ability~as.factor(Group)+as.numeric(time), data=stroke_long,
             correlation=ar1)

un<-corSymm(form = ~ 1 | Subject)
gls.un<-gls(ability~as.factor(Group)+as.numeric(time), data=stroke_long,
            correlation=un)

#NOMOR 15
#Membuat tabel untuk Membandingkan AIC dari
#model korelasi struktur Exchangeable, Auto regressive, dan Unstructured, dengan AIC linear regresi model
#Interpretasikan dan simpulkan.

aic = AIC(gls.exch, gls.ar1, gls.un, mod_lm2)
write.csv(aic,"aic.csv")
View(aic)