diff.in.means.results <- ictreg(y ~ 1, data = race,
                                treat = "treat", J = 3, method = "lm")

summary(diff.in.means.results)

test.value.affirm <- ict.test(na.omit(q860b_homo), na.omit(q860b_homo_treatment), J = 4, gms = TRUE)
print(test.value.affirm)

q860b_homo = ifelse(abv_en$Q860A%in%c(NA), abv_en$Q860B,abv_en$Q860A)
q860b_homo= ifelse(q860b_homo%in%c(NA), abv_en$Q860C,q860b_homo)
q860b_homo= ifelse(q860b_homo%in%c(NA), abv_en$Q860D,q860b_homo)

q860b_homo_treatment = ifelse(abv_en$Q860B%in%c(0:10),1,0)

q860b_homo_treatment = ifelse(q860b_homo%in%c(NA), NA,q860b_homo_treatment )

set.seed(1)
lebanon_data= filter(abv, country==10)
diff.in.means.results <- ictreg(q860b_homo~1, data = data.frame(lebanon_data), treat = "q860b_homo_treatment", J = 4, method = "lm")

summary(diff.in.means.results)