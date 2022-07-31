# TALLER-FINAL-CURSO-R








.
# punto 3.


reg <- lm(Price ~ Food + Service + Decor + East, data = base_com)

summary(reg) 

# se puede indicar que la varibale service no es estadisticamente
# representativa para la regresion en almenos 0.05
# las demas variables son estadisticamente significativas para la regresion y 
# en particular la varibale que influye mas en el precio de la comida es East
