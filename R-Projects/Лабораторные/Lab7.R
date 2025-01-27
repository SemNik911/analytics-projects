library(readxl)
setwd("D:/FIIT20Semenov")
getwd()
data = read_excel("Lab7data.xlsx")
data
n = data$n
a = data$A
b = data$B
c = data$C

sa=sum(a)
sb=sum(b)
sc=sum(c)

S=sa+sb+sc

SS_fact=(sa^2+sb^2+sc^2)/8 - S^2/24
SS_fact
a=a^2
b=b^2
c=c^2
SS_obsh=-(sum(a)+sum(b)+sum(c)-S^2)/24
SS_obsh
SS_sluch=SS_obsh-SS_fact
SS_sluch

k_fact=2
k_obsh=23
k_sluch=k_obsh-k_fact

MS_fact=SS_fact/k_fact
MS_fact
MS_sluch=SS_sluch/k_sluch
MS_sluch

F_emp=MS_fact/MS_sluch
F_emp

#F_krit = 3,40
#F_emp<F_krit, следовательно принемается нулевая гипотеза, т.е. особой значимости между
#заводами нет