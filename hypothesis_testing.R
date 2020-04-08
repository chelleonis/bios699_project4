#extract data from basic_sim.R
library(ggplot2)
library(pwr)
library(scales)
#analysis of simulated data
#one-sided t-test
#reject if difference > 0.4
#accept null if diff < 0.4 standard

data_set <- as.data.frame(mean_vector)


ggplot(data_set,aes(x = n_patients)) + 
  geom_histogram(binwidth = 1) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18)) +
  labs(title = "Patients per Simulation", x="Number of Patients",y = "Count") +
  scale_x_continuous(breaks = pretty_breaks(n=7))
  

difference <- (data_set[,2]-data_set[,1])/data_set[,2]

sum(difference > 0.4)

ggplot(data_set) +
  geom_density(aes(x = data_set[,2], colour = "standard needle (simulation)"),size = 1.25) +
  geom_density(aes(x = data_set[,1], colour = "new needle"),size =1.25) +
  labs(title = "New vs Standard Needle Simulation Density Plot", x = "Mean Deflection (mm)") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 18), legend.position = c(0.95,0.9),
        legend.justification = c("right","top"), 
        legend.background=element_blank(), legend.title = element_blank()) +
  scale_x_continuous(breaks = pretty_breaks(n=8)) +
  scale_colour_manual(values = c("green","red")) 
  

#literature comparison
#for the comparison, draw from 10,000 samples:
# median 1.77 mm
# sd = ???

test_lit <- rnorm(10000, mean = 1.77, sd = 1)

data_set$yo <- test_lit 

difference_2 <- (test_lit-data_set[,1])/test_lit

sum(difference_2 > 0.4)

ggplot(data_set) +
  geom_density(aes(x = yo, colour = "standard needle (literature)"),size = 1.25) +
  geom_density(aes(x = new_mean, colour = "new needle"),size =1.25) +
  labs(title = "New vs Literature Standard Density Plot", x = "Mean Deflection (mm)") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 18), legend.position = c(0.95,0.9),
        legend.justification = c("right","top"), 
        legend.background=element_blank(), legend.title = element_blank()) +
  scale_x_continuous(expand = 0,breaks = pretty_breaks(n=8)) +
  scale_colour_manual(values = c("green","red")) 

quantile(data_set[,3],.10)
#t-test of ratios?
library(mratios)

ttestratio(x=data_set$new_mean,y = data_set$standard_mean , data=data_set, alternative="greater",
             rho=0.6, var.equal=FALSE)

ttestratio(x=data_set$new_mean,y = data_set$yo , data=data_set, alternative="less",
           rho=0.6, var.equal=FALSE)

ps5 <- rnorm(568, mean = 1.77, sd = 1)


ttestratio(x=data_set$new_mean,y = ps5 , data=data_set, alternative="less",
           rho=0.6, var.equal=FALSE)
#power curves for our sample range 
#alpha = 0.025 (one-sided)
#power = 0.8 (i think)
#effect size: difference = 40% difference 

alpha = 0.025
n = c(50:83)
effect_size = log(1/0.6) #ln(1/0.6)

#calculate power from these ops

#paired is ok since we are performing these tests on similar subjects i think

oWo <- pwr.t.test(d = effect_size, n = 60, sig.level = alpha, power = NULL,
            alternative = "greater", type = "paired")
oWo

#or else we gotta use 2 sample
oWo2 <- pwr.t.test(d = effect_size, n = 60, sig.level = alpha, power = NULL,
                  alternative = "greater", type = "two.sample")
oWo2

plot(oWo_temp) +
  geom_hline(yintercept = 0.8, color = 'green',linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))


#"power" for above oWo is 0.3912867
# 10% quantile is 72
# min = 69, max = 82
data_set_600 <- as.data.frame(mean_vector_2)

oWo_600 <- pwr.t.test(d = NULL, n = 69, sig.level = alpha, power = 0.9733,
                      alternative = "greater", type = "paired")
oWo_600

# exp(0.2011) = 1.227 which is approx a 19% decrease
