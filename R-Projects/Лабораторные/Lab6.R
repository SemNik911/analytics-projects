library(readxl)
setwd("D:\Учебные материалы\Прикладные методы анализа и виз данных")
getwd()
data = read_excel("lab6data.xls", sheet="??????? 15")
data
x = data$x
y = data$y
z = data$z
xmin = min(x)
ymin = min(y)
zmin = min(z)
xsr = sum(x)/length(x)
ysr = sum(y)/length(y)
zsr = sum(z)/length(z)
xmax = max(x)
ymax = max(y)
zmax = max(z)
r <- matrix(seq(0,0), nrow = 3, ncol = 36)
area <- matrix(seq(0,0), nrow = 1, ncol = 36)

f=0
while(f<1){
  k1=0
  k2=0
  k3=0
  x1=0
  y1=0
  z1=0
  x2=0
  y2=0
  z2=0
  x3=0
  y3=0
  z3=0
  for (i in 1:36){
    r[1, i]=sqrt((x[i]-xmax)*(x[i]-xmax)+(y[i]-ymax)*(y[i]-ymax)+(z[i]-zmax)*(z[i]-zmax))
    r[2, i]=sqrt((x[i]-xsr)*(x[i]-xsr)+(y[i]-ysr)*(y[i]-ysr)+(z[i]-zsr)*(z[i]-zsr))
    r[3, i]=sqrt((x[i]-xmin)*(x[i]-xmin)+(y[i]-ymin)*(y[i]-ymin)+(z[i]-zmin)*(z[i]-zmin))
    if(r[1,i]<r[2,i] && r[1,i]<r[3,i]){
      area[i]=1
      k1=k1+1
      x1=x1+x[i]
      y1=y1+y[i]
      z1=z1+z[i]
    }
    if(r[2,i]<r[1,i] && r[2,i]<r[3,i]){
      area[i]=2
      k2=k2+1
      x2=x2+x[i]
      y2=y2+y[i]
      z2=z2+z[i]
    }
    if(r[3,i]<r[1,i] && r[3,i]<r[2,i]){
      area[i]=3
      k3=k3+1
      x3=x3+x[i]
      y3=y3+y[i]
      z3=z3+z[i]
    }
  }
  xmin2 = x3/k3
  ymin2 = y3/k3
  zmin2 = z3/k3
  xsr2 = x2/k2
  ysr2 = y2/k2
  zsr2 = z2/k2
  xmax2 = x1/k1
  ymax2 = y1/k1
  zmax2 = z1/k1
  if(xmin2 == xmin &&
     ymin2 == ymin &&
     zmin2 == zmin &&
     xsr2 == xsr &&
     ysr2 == ysr &&
     zsr2 == zsr &&
     xmax2 == xmax &&
     ymax2 == ymax &&
     zmax2 == zmax) f=1
  else{
    xmin == xmin2
    ymin == ymin2
    zmin == zmin2
    xsr == xsr2
    ysr == ysr2
    zsr == zsr2
    xmax == xmax2 
    ymax == ymax2
    zmax == zmax2
  }
  
}
area
vect <- c(area = area)
write.csv(vect, file = "result6.csv")
getwd()
