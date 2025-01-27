library(readxl)
setwd("D:/FIIT20Semenov")
getwd()
data  =  read_excel("lab3data.xlsx", sheet = "Вариант 15")
data
y  =  data$y
x1  =  data$x1
x2  =  data$x2
a <- matrix(seq(0,0), nrow  =  3, ncol  =  4)
a[1,1] = length(y)
a[1,2] = sum(x1)
a[1,3] = sum(x2)
a[1,4] = sum(y)
a[2,1] = sum(x1)
a[2,2] = sum(x1*x1)
a[2,3] = sum(x1*x2)
a[2,4] = sum(x1*y)
a[3,1] = sum(x2)
a[3,2] = sum(x1*x2)
a[3,3] = sum(x2*x2)
a[3,4] = sum(x2*y)
a
d = a[1,1]*a[2,2]*a[3,3] + a[1,2]*a[2,3]*a[3,1] + a[1,3]*a[2,1]*a[3,2] - a[1,3]*a[2,2]*a[3,1] - a[1,1]*a[2,3]*a[3,2] - a[1,2]*a[2,1]*a[3,3]
d
da = a[1,4]*a[2,2]*a[3,3] + a[1,2]*a[2,3]*a[3,4] + a[1,3]*a[2,4]*a[3,2] - a[1,3]*a[2,2]*a[3,4] - a[1,4]*a[2,3]*a[3,2] - a[1,2]*a[2,4]*a[3,3]
db = a[1,1]*a[2,4]*a[3,3] + a[1,4]*a[2,3]*a[3,1] + a[1,3]*a[2,1]*a[3,4] - a[1,3]*a[2,4]*a[3,1] - a[1,1]*a[2,3]*a[3,4] - a[1,4]*a[2,1]*a[3,3]
dc = a[1,1]*a[2,2]*a[3,4] + a[1,2]*a[2,4]*a[3,1] + a[1,4]*a[2,1]*a[3,2] - a[1,4]*a[2,2]*a[3,1] - a[1,1]*a[2,4]*a[3,2] - a[1,2]*a[2,1]*a[3,4]
a0 = da/d
a1 = db/d
a2 = dc/d
a0
a1
a2
s1 = 0
s2 = 0
ys = sum(y)/length(y)
ys
for(i in 1:14){
  yt = a0 + a1*x1[i] + a2*x2[i]
  s1 = s1 + (y[i] - yt)^2
  s2 = s2 + (y[i] - ys)^2
}
R = 1 - s1/s2
R
F = (R/(1 - R))*((14 - 2 - 1)/2)
F
#F_табл(по фишеру) = 0,532
#коэффициент детерминации статистически значим, так как 1,295>0,532

S = s1/(14 - 2 - 1)
S
t1 = 0
t2 = 0

for(i in 1:14){
  t1 = t1 + x1[i]
  t2 = t2 + x2[i]
}
x1sr = t1/14
x2sr = t2/14
sx1 = 0
sx2 = 0
sx1x2 = 0
for(i in 1:14){
  sx1 = sx1 + (x1[i] - x1sr)^2
  sx2 = sx2 + (x2[i] - x2sr)^2
  sx1x2 = sx1x2 + (x1[i] - x1sr)*(x2[i] - x2sr)
}
Sa0 = (1/14 + (x1sr^2*sx2 + x2sr^2*sx1 - x1sr*x2sr*sx1x2)/(sx1*sx2 - sx1x2^2))*S
Sa1 = (sx2/(sx1*sx2 - sx1x2^2))*S
Sa2 = (sx1/(sx1*sx2 - sx1x2^2))*S

ta0 = a0/Sa0
ta1 = a1/Sa1
ta2 = a2/Sa2
ta0
ta1
ta2
#tтабл = 2,201
#все коэффициенты регрессионного уравнения статистически не значимы, так как они меньше 2,201
