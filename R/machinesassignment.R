wb2 = loadWorkbook("data/machines.xlsx")
data2 = readWorksheet(wb2, sheet = "machines", header = TRUE)

#Length is exp(theta) distributed. 

#From wikipedia, for i.i.d. Lengths
#likelihood is \prod_{i=1}^n \theta \exp{-\theta x_{i}}
# = \theta^{n} \exp{-\theta n \bar{x}}

# where \bar{x} is the sample mean of lengths x_{i}.

#loglikelihood is n \ln{\theta} - \theta n \bar{x}
#which has maximum when \theta = \dfrac{1}{\bar{x}}



#\ln{\left(p(x | \theta) p(\theta)\right)} computes
#the logarithm of the relative A posteriori probabilities p(\theta | x)