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

#NOMOR 1
#mengambil data
stroke <- fread("http://www.statsci.org/data/oz/stroke.txt")
View (stroke)
str(stroke)

#subsetting: memilih subjek berdasarkan variables Subject, Group, Bart1 to Bart8
stroke_filter <- stroke[,c(1:6,39:46)] # column selecting
View(stroke_filter)
colnames(stroke_filter) <- c('Subject', 'Group', 'Sex', 'Side', 'Age', 'Lapse', '1', '2', '3', '4', '5', '6', '7', '8')
str(stroke_filter)

#transform ke format long data
library(reshape2)
stroke_long <- melt(stroke_filter, id.vars=c("Subject", "Group", "Sex", "Side", "Age", "Lapse"),
                    measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8"),
                    variable.name="Time",
                    value.name="Bart_Score")
View (stroke_long)
str(stroke_long)

#NOMOR 2
#membuat grafik individual
#memakai fungsi ggplot
library(ggplot2)
ggplot(stroke_long, aes(x = Time, y = Bart_Score, group = Subject))
ggplot(stroke_long, aes(x = Time, y = Bart_Score, group = Subject)) +
  geom_line(aes(color = Subject, group = Subject), size = 1) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '5', '6', '7', '8')) +
  labs(title = "Functional Ability", y = "Barthel Index Score", x = "Time (Week)")

#menampilkan grafik individu per group
ggplot(stroke_long, aes(x = Time, y = Bart_Score, group = Subject)) + geom_line(aes(color = Subject, group = Subject), size = 1) + facet_wrap( ~ Group, labeller = label_both)

#interpretasi:
#setiap subjek memiliki nilai baseline Barthel score yang bervariasi (dilihat dari nilai di axis y)
#secara umum, semua subjek mengalami kenaikan nilai Barthel score
#subjek VI mengalami kenaikan paling besar (melihat nilai awal-nilai akhir)

#NOMOR 3
#membuat grafik berdasarkan nilai mean per treatment group
ggplot(stroke_long, aes(x = Time, y = Bart_Score, group = Subject)) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '5', '6', '7', '8')) +
  labs(title = "Functional Ability", y = "Barthel Index Score", x = "Time (Week)") +
  stat_summary(aes(group = Group, color = Group),
  geom = "line", fun.y = mean, size = 2)

#interpretasi:
#secara umum, semua grup menunjukkan adanya peningkatan Barthel Index selama periode 8 minggu penilaian
#grup E yaitu grup yang diberikan experimental program
#grup E menunjukkan peningkatan nilai Barthel score yang paling besar (8 grid)
#dengan nilai akhir yang paling besar

#NOMOR 4
#subsetting: select subject berdasarkan variables Bart1 to Bart8
stroke_bart <- stroke[,c(39:46)]
View(stroke_bart)

#merubah nama kolom
colnames(stroke_bart) <- c('Week1', 'Week2', 'Week3', 'Week4', 'Week5', 'Week6', 'Week7', 'Week8')

#membuat scatter plot matrix
plot(stroke_bart, pch=20, cex=2, col='steelblue')

#interpretasi: 
#scatter plot matrix membantu untuk secara kasar menentukan apakah ada linear correlation
#di antara Barthel score di setiap minggunya
#sumbu x menunjukkan skor di minggu-1, sumbu y menunjukkan skor di minggu-2
#kemungkinan ada linear correlation dari Barthel score di minggu-minggu yang berdekatan
#minggu-1 dan minggu-2, minggu-2 dan minggu-3, dst
#semakin jauh jarak minggunya, semakin tidak tercermin linear correlation dari Barthel score

#NOMOR 5
round(cor(stroke_bart), digits = 2) # rounded to 2 decimals
#interpretasi: 
#Jika nilainya berada di antara  0 and +1/-1, berarti ada hubungan,
#tetapi tidak semua titik berada tepat di atas garis
#semua nilainya positif berarti menunjukkan korelasi yang positif
#jika nilai di minggu-1 meningkat, maka nilai di minggu-2 juga meningkat
#nilai correlation coefficient paling besar terdapat di variabel minggu yang berdekatan (0.92 - 0.98)
#nilai correlation coefficient semakin berkurang jika jarak antar minggu semakin bertambah

#NOMOR 7
#membuat model dengan random intercept dan random slopes
library(lme4)

#membuat model
stroke_mm <- lmer (Bart_Score ~ Time + (1 + Time |Subject), data = stroke_long)

#meng-ekstrak koefisien dari mixed-model stroke
coef(stroke_mm)
beta <- coef(stroke_mm)$Subject
colnames(beta) <- c("Intercept", "Slope")
View(beta)