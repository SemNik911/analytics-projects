library(readxl)
setwd("D:/FIIT20Semenov")
getwd()
data = read_excel("lab5data.xls", sheet="Вариант 16")
data
x = data$x
y = data$y

ymin = min(y)
xsr = sum(x)/length(x)
ysr = sum(y)/length(y)
ymax = max(y)

r <- matrix(seq(0,0), nrow = 3, ncol = 36)
area <- matrix(seq(0,0), nrow = 1, ncol = 36)

f=0
while(f<1){
  k1=0
  k2=0
  k3=0
  x1=0
  y1=0
  x2=0
  y2=0
  x3=0
  y3=0
  for (i in 1:36){
    r[1, i]=sqrt((y[i]-ymax)^2+(x[i]-xsr)^2)
    r[2, i]=sqrt((y[i]-ysr)^2+(x[i]-xsr)^2)
    r[3, i]=sqrt((y[i]-ymin)^2+(x[i]-xsr)^2)
    if(r[1,i]<r[2,i] && r[1,i]<r[3,i]){
      area[i]=1
      k1=k1+1
      x1=x1+x[i]
      y1=y1+y[i]
    }
    if(r[2,i]<r[1,i] && r[2,i]<r[3,i]){
      area[i]=2
      k2=k2+1
      x2=x2+x[i]
      y2=y2+y[i]
    }
    if(r[3,i]<r[1,i] && r[3,i]<r[2,i]){
      area[i]=3
      k3=k3+1
      x3=x3+x[i]
      y3=y3+y[i]
    }
  }
  ymin2 = y3/k3
  ysr2 = y2/k2
  ymax2 = y1/k1
  xsr2 = x2/k2
  if(ymin2 == ymin &&
     xsr2 == xsr &&
     ysr2 == ysr &&
     ymax2 == ymax) f=1
  else{
    ymin = ymin2
    xsr = xsr2
    ysr = ysr2
    ymax = ymax2
  }
}
area
vect <- c(area = area)
write.csv(vect, file = "result5.csv")
getwd()
