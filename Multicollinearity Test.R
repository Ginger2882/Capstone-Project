#install car package
install.packages("car")

#load car package
library(car)

#calculate vifs for each best model
vif_values1 <- vif(model255)
vif_values2 <- vif(model510)
vif_values3 <- vif(model765)
vif_values4 <- vif(model1020)
vif_values5 <- vif(model1275)

#print all vif values
print(vif_values1)
print(vif_values2)
print(vif_values3)
print(vif_values4)
print(vif_values5)
