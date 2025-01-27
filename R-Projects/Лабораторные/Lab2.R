library(readxl)
setwd("D:/FIIT20Semenov")
getwd()
data = read_excel("lab2data.xls", sheet="Вариант 15")
data
y = data$y
x1= data$x1
x2= data$x2

#1

sry <- mean(y)
srx1 <- mean(x1)
srx2 <- mean(x2)
Sum <- matrix(seq(0,0), nrow = 3, ncol = 3)
for (i in 1:10){
  t1=x1[i]-srx1
  t2=x2[i]-srx2
  t3=y[i]-sry
  Sum[1,1]=Sum[1,1]+t1*t3
  Sum[2,1]=Sum[2,1]+t2*t3
  Sum[3,1]=Sum[3,1]+t1*t2
  Sum[1,2]=Sum[1,2]+t1*t1
  Sum[1,3]=Sum[1,3]+t3*t3
  Sum[2,2]=Sum[2,2]+t2*t2
  Sum[2,3]=Sum[2,3]+t3*t3
  Sum[3,2]=Sum[1,2]+t1*t1
  Sum[3,3]=Sum[1,3]+t2*t2
}
rx1y=Sum[1,1]/sqrt(Sum[1,2]*Sum[1,3])
rx2y=Sum[2,1]/sqrt(Sum[2,2]*Sum[2,3])
rx1x2=Sum[3,1]/sqrt(Sum[3,2]*Sum[3,3])
Rx1x2y=sqrt((rx1y^2+rx2y^2-2*rx1y*rx2y*rx1x2)/(1-rx1x2^2))
Rx1x2y

#2

ryx1_x2=(rx1y-rx2y*rx1x2)/sqrt((1-rx2y^2)*(1-rx1x2^2))
ryx2_x1=(rx2y-rx1y*rx1x2)/sqrt((1-rx1y^2)*(1-rx1x2^2))
rx1x2_y=(rx1x2-rx1y*rx1x2)/sqrt((1-rx1y^2)*(1-rx2y^2))
ryx1_x2
ryx2_x1
rx1x2_y

df <- c(Rx1x2y, ryx1_x2, ryx2_x1, rx1x2_y)

write.csv(df, "new_file.csv")
