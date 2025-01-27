
i = 15


my_vector <- c(20 + i, 41 + i, 50 + i, 42 + i, 
               30 + i, 60 + i, 70 + i, 62 + i, 
               35 + i, 82 + i, 95 + i, 80 + i,
               45 + i, 110 + i, 126 + i, 92 + i)
my_vector
smoothed_data <- c(20 + i)
smoothed_data
alpha = 0.2
#Определить аномальные уровни ряда. Если имеются такие значения, то надо их выровнять методом экспоненциального сглаживания.
# Применение экспоненциального сглаживания
for (i in 1:12){
  smoothed_value = alpha * my_vector[i] + (1 - alpha) * smoothed_data[i - 1]
  smoothed_data <- append(smoothed_data, smoothed_value)
}
my_vector
smoothed_data






#Определить сезонные компоненты для аддитивной модели временного ряда.
#столбец Итого за 4 квартала
itogo_za_4_kvartala <- c()
for (i in 2: 11){
  buffer = my_vector[i-2] + my_vector[i-1] + my_vector[i] + my_vector[i+1]
  itogo_za_4_kvartala <- append(itogo_za_4_kvartala, buffer)
}
itogo_za_4_kvartala

#Скользящая средняя за 4 квартала
skolzhyashya_srednyaya_za_4_kvartala <- c()
for (i in 0:9){
  buffer = itogo_za_4_kvartala[i] / 4
  skolzhyashya_srednyaya_za_4_kvartala <- append(skolzhyashya_srednyaya_za_4_kvartala, buffer)
}
skolzhyashya_srednyaya_za_4_kvartala

#Центрированная скользящая средняя
centrirovannya_skolzhyashyaya_srednyaya <- c()
for (i in 0:8){
  buffer = (skolzhyashya_srednyaya_za_4_kvartala[i] + skolzhyashya_srednyaya_za_4_kvartala[i + 1]) / 2
  centrirovannya_skolzhyashyaya_srednyaya <- append(centrirovannya_skolzhyashyaya_srednyaya, buffer)
}
centrirovannya_skolzhyashyaya_srednyaya

#Оценка сезонной компоненты A/T=S*E 
otsenka_sezonnoi_komponenty <- c()
for (i in 2:10) {
  otsenka_sezonnoi_komponenty <- append(otsenka_sezonnoi_komponenty, my_vector[i] / centrirovannya_skolzhyashyaya_srednyaya[i - 2])
}
otsenka_sezonnoi_komponenty





#Определить тренд для мультипликативной модели временного ряда.
skorrektirovannaya_sezonnaya_komponenta <- c(0, 0, 0, 0)
n <- c(0, 0, 0, 0)
for (i in 1:length(otsenka_sezonnoi_komponenty)) {
  n[(i + 2) %% 4 + 1] = n[(i + 2) %% 4 + 1] + 1
  print((i + 2) %% 4 + 1)
  print(otsenka_sezonnoi_komponenty[i])
  skorrektirovannaya_sezonnaya_komponenta[(i + 2) %% 4 + 1] <- skorrektirovannaya_sezonnaya_komponenta[(i + 2) %% 4 + 1] + otsenka_sezonnoi_komponenty[i]
}
for (i in 1:length(skorrektirovannaya_sezonnaya_komponenta)) {
  skorrektirovannaya_sezonnaya_komponenta[i] = skorrektirovannaya_sezonnaya_komponenta[i] / n[i]
}
skorrektirovannaya_sezonnaya_komponenta

desezonalizirovannyy_obyem_prodazh <- c()
for (i in 1:length(my_vector)){
  buffer = my_vector[i] / skorrektirovannaya_sezonnaya_komponenta[i %% 4 + 1]
  desezonalizirovannyy_obyem_prodazh <- append(desezonalizirovannyy_obyem_prodazh, buffer)
}
desezonalizirovannyy_obyem_prodazh
x <- c()
x <- seq(1, length(desezonalizirovannyy_obyem_prodazh))
plot(x, desezonalizirovannyy_obyem_prodazh, type = "o", col = "blue", xlab = "X", ylab = "Y", main = "Десезонализация временного ряда")

# Вычисление среднего значений
mean_x <- mean(x)
mean_y <- mean(desezonalizirovannyy_obyem_prodazh)
# Вычисление числителя и знаменателя
numerator <- sum((x - mean_x) * (desezonalizirovannyy_obyem_prodazh - mean_y))
denominator <- sum((x - mean_x)^2)
# Вычисление коэффициента регрессии b
b <- numerator / denominator
a = (mean_y - mean_x * b)
a
b
x
# y = 42.0976 * x + 4.384633
trendovoe_znachenie = a + b * x
trendovoe_znachenie

#Определить случайную компоненту временного ряда.
random_component = 0
random_component <- rnorm(length(my_vector), mean = mean(my_vector), sd = sd(my_vector))
random_component
my_vector


#Рассчитать величину среднеквадратической ошибки.
sezonnaya_komponenta <- c()
for (i in 1:length(my_vector)){
  sezonnaya_komponenta <- append(sezonnaya_komponenta, skorrektirovannaya_sezonnaya_komponenta[i %% 4 + 1])
}
sezonnaya_komponenta
oshibka = my_vector / (sezonnaya_komponenta * trendovoe_znachenie)
oshibka
E = 0
for (i in 1:length(my_vector)){
  E = E + abs(oshibka[i])
}
MSE = E * E / length(my_vector)
MSE
procent = MSE / mean(my_vector) * 100
procent




#Рассчитать прогнозные значения валового внутреннего продукта на 2023 г. и 2024 г. 
delta = trendovoe_znachenie[2] - trendovoe_znachenie [1]
delta
print(trendovoe_znachenie)
predict <- c(0, 0, 0, 0, 0, 0, 0, 0)
skorrektirovannaya_sezonnaya_komponenta
for (i in 1:length(my_vector)) {
  predict[i] = trendovoe_znachenie[1] * skorrektirovannaya_sezonnaya_komponenta[(i - 1) %% 4 + 1]
}
predict

