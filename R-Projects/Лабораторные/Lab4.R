library(readxl)
setwd("D:/FIIT20Semenov")
getwd()
data  =  read_excel("lab4data.xlsx", sheet = "Вариант 15")
data
y = data$y
t = data$t

#1a

sry <- mean(y)
srt <- mean(t)
s = sum((t - srt)*(y - sry))
st = sum((t - srt)^2)

b = s/st
a1 = sry - b*srt
a1
b

y_new <- a1 + b*t
SST = sum((y - sry)^2)
SSE = sum((y - y_new)^2)

R = 1 - SSE/SST
R

#1б

srt2 <- mean(t^2)
s1 = sum((t^2 - srt2)*(y-sry))
st2 = sum((t^2 - srt2)^2)

c = s1/st2
a2 = sry - b*srt - c*srt*srt
a2
b
c

y2_new <- a2 + b*t + c*t^2
SSE2 = sum((y - y2_new)^2)
R2 = 1 - SSE2/SST
R2

#2

t_new = c(23,24,25,26)
y_predicted = a1 + b*t_new
y_predicted
write.csv(y_predicted, "new_file.csv")
