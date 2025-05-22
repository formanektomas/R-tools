library(plm)
library(splm)

# Note I am using plm regression here, rather than just lm
data(Produc, package = "plm")
data(usaww)

plm_model <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)

res1 <- slmtest(plm_model, usaww, test = "lml", model = "within", zero.policy = TRUE)
res2 <- slmtest(plm_model, usaww, test = "lme", model = "within", zero.policy = TRUE)
res3 <- slmtest(plm_model, usaww, test = "rlml", model = "within", zero.policy = TRUE)
res4 <- slmtest(plm_model, usaww, test = "rlme", model = "within", zero.policy = TRUE)

res1
res2
res3
res4