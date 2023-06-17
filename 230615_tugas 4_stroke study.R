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
#mengekstraksi nilai koefisien dari model
ml <- lmList(Bart_Score ~ Time | Subject, stroke_long)
summary(ml)
intercepts <- sapply(ml,coef)[1,]
summary(intercepts)
slopes <- sapply(ml,coef)[2,]

Hasil:
      Intercept    Std. Error      Slope     Std. Error
I     30.0000000    4.03722       7.5000000  0.7994887 
II    15.5357143    4.03722       3.2142857  0.7994887  
III   39.8214286    4.03722       6.4285714  0.7994887 
IV    11.6071429    4.03722       8.3928571  0.7994887
V    100.0000000        NaN       0.0000000        NaN 
VI     0.8928571    4.03722      11.1904762  0.7994887 
VII   15.3571429    4.03722       7.9761905  0.7994887 
VIII  25.3571429    4.03722       5.8928571  0.7994887 
1     38.5714286    4.03722       7.2619048  0.7994887 
2     61.9642857    4.03722       2.6190476  0.7994887  
3     14.4642857    4.03722       9.7023810  0.7994887 
4     26.0714286    4.03722       2.6785714  0.7994887 
5     48.7500000    4.03722       5.0000000  0.7994887 
6     10.1785714    4.03722       1.0714286  0.7994887 
7     31.2500000    4.03722       2.5000000  0.7994887 
8     34.1071429    4.03722       3.8095238  0.7994887
A     21.0714286    4.03722       1.4285714  0.7994887 
B     34.1071429    4.03722       0.8928571  0.7994887  
C     32.1428571    4.03722       1.6071429  0.7994887 
D     42.3214286    4.03722       7.2619048  0.7994887 
E     48.5714286    4.03722       7.2619048  0.7994887 
F     24.8214286    4.03722       2.2619048  0.7994887 
G     22.3214286    4.03722       1.8452381  0.7994887 
H     13.0357143    4.03722       6.5476190  0.7994887
