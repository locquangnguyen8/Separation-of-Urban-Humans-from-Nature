# Load data and package
data1<-read.csv("C:/Users/ACER/Dropbox/Research/@_Team/@_Books/@_Biodiversity book/Results/Data/Data_535 (cleaned) v2.csv",header = TRUE,stringsAsFactors = TRUE)
library(bayesvl)
library(cowplot)
library(ggplot2)

data1$AnimalDiversity <- data1$C2_3
data1$PlantDiversity <- data1$C1_3
data1$HomeComfortable <- data1$C3_2

require(dplyr)

data1$BioLossBelief <- 
  case_when(
    data1$B2 %in% c("3") ~ 1,
    data1$B2 %in% c("1","2") ~ 0
  )


keeps <- c("PlantDiversity","BioLossBelief","AnimalDiversity","HomeComfortable")
data1 <- data1[keeps]
data1<-na.omit(data1)


# Model construction: Model 1
model1<-bayesvl()
model1<-bvl_addNode(model1,"BioLossBelief","binom")
model1<-bvl_addNode(model1,"AnimalDiversity","norm")
model1<-bvl_addNode(model1,"PlantDiversity","norm")

model1<-bvl_addArc(model1,"AnimalDiversity","BioLossBelief","slope")
model1<-bvl_addArc(model1,"PlantDiversity","BioLossBelief","slope")


bvl_bnPlot(model1)

# Generate Stan code
model_string1<- bvl_model2Stan(model1)
cat(model_string1) 

# Model Fit
model1<-bvl_modelFit(model1, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model1)

bvl_plotTrace(model1)
bvl_plotGelmans(model1,param=NULL,1,3)
bvl_plotAcfs(model1,param=NULL,1,3)
bvl_plotDensity(model1,params = c("b_AnimalDiversity_BioLossBelief",
                                  "b_PlantDiversity_BioLossBelief"))+theme_bw()
bvl_plotIntervals(model1,params = c("b_AnimalDiversity_BioLossBelief",
                                    "b_PlantDiversity_BioLossBelief"))+theme_bw()
bvl_plotParams(model1,1,3,credMass = 0.90,params = NULL)

loo1<-bvl_stanLoo(model1)
plot(loo1)


# Model construction: Model 2
model2<-bayesvl()
model2<-bvl_addNode(model2,"BioLossBelief","binom")
model2<-bvl_addNode(model2,"AnimalDiversity","norm")
model2<-bvl_addNode(model2,"PlantDiversity","norm")
model2<-bvl_addNode(model2,"HomeComfortable","binom")

model2<-bvl_addNode(model2,"Animal_Comfort","trans")

model2<-bvl_addArc(model2,"AnimalDiversity","Animal_Comfort","*")
model2<-bvl_addArc(model2,"HomeComfortable","Animal_Comfort","*")

model2<-bvl_addNode(model2,"Plant_Comfort","trans")

model2<-bvl_addArc(model2,"PlantDiversity","Plant_Comfort","*")
model2<-bvl_addArc(model2,"HomeComfortable","Plant_Comfort","*")

model2<-bvl_addArc(model2,"AnimalDiversity","BioLossBelief","slope")
model2<-bvl_addArc(model2,"Animal_Comfort","BioLossBelief","slope")
model2<-bvl_addArc(model2,"PlantDiversity","BioLossBelief","slope")
model2<-bvl_addArc(model2,"Plant_Comfort","BioLossBelief","slope")
model2<-bvl_addArc(model2,"HomeComfortable","BioLossBelief","slope")

bvl_bnPlot(model2)

# Generate Stan code
model_string1<- bvl_model2Stan(model2)
cat(model_string1) 

# Model Fit
model2<-bvl_modelFit(model2, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model2)

bvl_plotTrace(model2)
bvl_plotGelmans(model2,param=NULL,2,3)
bvl_plotAcfs(model2,param=NULL,2,3)
bvl_plotDensity(model2,params = c("b_HomeComfortable_BioLossBelief",
                                    "b_AnimalDiversity_BioLossBelief",
                                    "b_Animal_Comfort_BioLossBelief",
                                    "b_PlantDiversity_BioLossBelief",
                                    "b_Plant_Comfort_BioLossBelief"))+theme_bw()
bvl_plotIntervals(model2,params = c("b_HomeComfortable_BioLossBelief",
                                    "b_AnimalDiversity_BioLossBelief",
                                    "b_Animal_Comfort_BioLossBelief",
                                    "b_PlantDiversity_BioLossBelief",
                                    "b_Plant_Comfort_BioLossBelief"))+theme_bw()
bvl_plotParams(model2,2,3,credMass = 0.90,params = NULL)

loo2<-bvl_stanLoo(model2)
plot(loo2)




# Model construction: Model 2_Informative priors: norm(0,0.5)
model2b<-bayesvl()
model2b<-bvl_addNode(model2b,"BioLossBelief","binom")
model2b<-bvl_addNode(model2b,"AnimalDiversity","norm")
model2b<-bvl_addNode(model2b,"PlantDiversity","norm")
model2b<-bvl_addNode(model2b,"HomeComfortable","binom")

model2b<-bvl_addNode(model2b,"Animal_Comfort","trans")

model2b<-bvl_addArc(model2b,"AnimalDiversity","Animal_Comfort","*")
model2b<-bvl_addArc(model2b,"HomeComfortable","Animal_Comfort","*")

model2b<-bvl_addNode(model2b,"Plant_Comfort","trans")

model2b<-bvl_addArc(model2b,"PlantDiversity","Plant_Comfort","*")
model2b<-bvl_addArc(model2b,"HomeComfortable","Plant_Comfort","*")

model2b<-bvl_addArc(model2b,"AnimalDiversity","BioLossBelief","slope",priors = c("b_AnimalDiversity_BioLossBelief ~ normal (0,0.5)"))
model2b<-bvl_addArc(model2b,"Animal_Comfort","BioLossBelief","slope",priors = c("b_Animal_Comfort_BioLossBelief ~ normal (0,0.5)"))
model2b<-bvl_addArc(model2b,"PlantDiversity","BioLossBelief","slope",priors = c("b_PlantDiversity_BioLossBelief ~ normal (0,0.5)"))
model2b<-bvl_addArc(model2b,"Plant_Comfort","BioLossBelief","slope",priors = c("b_Plant_Comfort_BioLossBelief ~ normal (0,0.5)"))
model2b<-bvl_addArc(model2b,"HomeComfortable","BioLossBelief","slope",priors = c("b_HomeComfortable_BioLossBelief ~ normal (0,0.5)"))

bvl_bnPlot(model2b)

# Generate Stan code
model_string1<- bvl_model2Stan(model2b)
cat(model_string1) 

# Model Fit
model2b<-bvl_modelFit(model2b, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model2b)



bvl_plotIntervals(model2b,params = c("b_HomeComfortable_BioLossBelief",
                                    "b_AnimalDiversity_BioLossBelief",
                                    "b_Animal_Comfort_BioLossBelief",
                                    "b_PlantDiversity_BioLossBelief",
                                    "b_Plant_Comfort_BioLossBelief"))


# Weight 
library("loo")

log_lik_1 <- extract_log_lik(model1@stanfit, parameter_name = "log_lik_BioLossBelief", merge_chains = FALSE)
r_eff1 <- relative_eff(exp(log_lik_1))
loo_1 <- loo(log_lik_1, r_eff = r_eff1, cores = 2)

log_lik_2 <- extract_log_lik(model2@stanfit, parameter_name = "log_lik_BioLossBelief", merge_chains = FALSE)
r_eff2 <- relative_eff(exp(log_lik_2))
loo_2 <- loo(log_lik_2, r_eff = r_eff2, cores = 2)


loo_list <- list(model1=loo_1, model2=loo_2)

stacking_wts <-loo_model_weights(loo_list) # stacking weight
pbma_BB_wts<-loo_model_weights(loo_list, method = "pseudobma") # pseudo-BMA+ weight
pbma_wts <-loo_model_weights(loo_list, method = "pseudobma", BB = FALSE) # pseudo-BMA weight

(waic1 <- waic(log_lik_1))
(waic2 <- waic(log_lik_2))

waics <- c(
  waic1$estimates["elpd_waic", 1],
  waic2$estimates["elpd_waic", 1]
)

waic_wts <- exp(waics) / sum(exp(waics))  # WAIC weight

round(cbind(waic_wts, pbma_wts, pbma_BB_wts, stacking_wts),2 )
