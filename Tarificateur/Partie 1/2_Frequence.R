##################frequence##################################
hist(TabNA$freq,prob=TRUE)
lines(dpois(x=0:nrow(TabNA),lambda=mean(TabNA$freq)),type="l", col = "red")
#tester si les donnees sont sous la forme d'une distribution Poisson
dispersion_test <- function(x) 
{
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  
  cat(
    "Mean: ",mean(x),"\n",
    "Variance: ",var(x),"\n",
    "Probability of being drawn from Poisson distribution: ", 
    round(res, 3),"\n", sep = "")
  
  invisible(res)
}
dispersion_test(TabNA$freq)
#It's not Poisson datas

#for negative binomial:
model0nb = glm.nb(freq~1,
                  data=Train
)
modelfullnb = glm.nb(freq~pol_bonus+pol_coverage+pol_pay_freq+
                       vh_sale_end+vh_value+vh_age+vh_cyl+vh_din+vh_fuel+vh_sale_begin+vh_speed+vh_value+vh_weight+
                       drv_sex1+drv_drv2
                     +drv_age1+drv_age_lic1+pol_sit_duration+pol_duration,
                     data=Train
)
# modnb = step(model0nb,
#            scope = list(upper=modelfullnb),
#            direction="both",
#            test="Chisq",
#            data=Train)

#for poisson:
model0p = glm(freq~1,
              data=Train,
              family=poisson()
)
modelfullp = glm(freq~pol_bonus+pol_coverage+pol_pay_freq+
                   vh_sale_end+vh_value+vh_age+vh_cyl+vh_din+vh_fuel+vh_sale_begin+vh_speed+vh_value+vh_weight+
                   drv_sex1+drv_drv2
                 +drv_age1+drv_age_lic1+pol_sit_duration+pol_duration,
                 data=Train,
                 family=poisson()
)
# mod = step(modelfullp,
#            scope = list(upper=model0p),
#            direction="both",
#            test="Chisq",
#            data=Train)

modelfinal = glm(freq~ 
                   pol_bonus + pol_coverage,
                 data=TabNA,
                 family=poisson()
)
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus + vh_speed + vh_din + vh_sale_begin,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#on retire la variable vh_sale_begin:
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus + vh_din + vh_speed,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#on peut retirer vh_din:
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus + vh_sale_begin,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#on peut retirer vh_sale_begin:
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#Toutes les variables semblent expliquatives

autre1=glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                drv_age1 + pol_bonus,data=TabNA)
(anova(autre1,test='Chisq'))

#le glm avec toutes les donnees:
modelefreq = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                      pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                      drv_age1 + pol_bonus,
                    data=TabNA
)
summary(modelefreq)



qqnorm(log(modelefreq$residuals));qqline(log(modelefreq$residuals))
dwt(modelefreq,method='resample', alternative='two.sided')
#plot(TabNA$freq,modelefreq$fitted.values)

#on compare a un modele qui a l'air de bien marcher:)
(anova(modelfinal,test='Chisq'))
compareGLM(modelfinal,autre1)


prednb=predict(modelfinal,newdata=Train,level=0.95,type='response')
valid=predict(modelfinal,newdata=Test,level=0.95,type='response')
abs(sum(Test$freq)-sum(valid)) #erreur absolue
abs(sum(Test$freq-valid))/sum(Test$freq)*100 #erreur relative

erreurp=abs(Test$freq-valid)
mean(erreurp)
summary(erreurp)

arrp=round(valid)
erreurarrp=abs(Test$freq-arrp)
mean(erreurarrp)
summary(erreurarrp)

erreurposp=erreurp[which(erreurarrp>0)]
mean(erreurposp)
summary(erreurposp)

prednbnb=predict(modelfinalnb,newdata=Train,level=0.95,type='response')
validnb=predict(modelfinalnb,newdata=Test,level=0.95,type='response')
abs(sum(Test$freq)-sum(validnb)) #erreur absolue
abs(sum(Test$freq-validnb))/sum(Test$freq)*100 #erreur relative

erreur=abs(Test$freq-validnb)
mean(erreur)
summary(erreur)

arr=round(validnb)
erreurarr=abs(Test$freq-arr)
mean(erreurarr)
summary(erreurarr)

erreurpos=erreur[which(erreurarr>0)]
mean(erreurpos)
summary(erreurpos)


