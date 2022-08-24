#### Does Public Support Help Democracy Survive?
#### Christopher Claassen
#### AJPS Replication file: Stan models for estimating satisfaction with democracy

# install.packages(c('MASS','arm','dplyr','tidyr','ggplot2','RColorBrewer','rstan','loo'))


library(MASS)
library(arm)
library(dplyr)
library(tidyr)
library(rstan)
library(loo)
library(ggplot2)
library(RColorBrewer)

# options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# working directory
# WD = getwd()
# setwd(WD)

# read satisfaction with dem data
satis = read.csv("./replication/supdem_democracy_ajps_replication/satisdem raw survey marginals.csv")
# edit for wvs
satis = wvs_res
table(satis$Project)
satis <- satis %>% 
  dplyr::filter(Response != 0 & Sample != 0)
satis = satis[!satis$Response==0, ]
# satis_opponent <- satis %>%
#   dplyr::filter(Item != c("E233A", "E229", "E227" ,"E224",))
satis <- satis %>%
  filter(Item %in% c("E233A","E227" ,"E224","E230",
                     "E001R",
                     "E003R",
                     "E005R"
  ))

# create year by country indicators
min(satis$Year) # 1973
year0 = min(satis$Year) - 1
year1 = max(satis$Year)
satis1 = satis[satis$Year > year0,]
satis1 = unite(satis1, YearCountry, c(Year, Country), sep = "_", remove = FALSE)
satis1$YearCountry = as.factor(satis1$YearCountry)

# create project by country indicators
satis1 = unite(satis1, ProjCnt, c(Project, Country), sep = "_", remove = FALSE)
satis1$ProjCnt = as.factor(satis1$ProjCnt)

# create year by project indicators
satis1 = unite(satis1, YrProjCnt, c(Year, Project, Country), sep = "_", remove = FALSE)
satis1$YrProjCnt = as.factor(satis1$YrProjCnt)

# factorise
satis1$Country = as.factor(as.character(satis1$Country))
satis1$Year = as.factor(satis1$Year-year0)

# count data in original data
length(unique(satis1$Country)) # 143 countries
length(unique(satis1$Year)) # 43 years
length(unique(satis1$YearCountry)) # 1621 country-years
length(unique(satis1$Project)) # 14 project-items
length(unique(satis1$YrProjCnt)) # 2013 national surveys

# drop countries with less than 2 years of data
cnt.obs.years = rowSums(table(satis1$Country, satis1$Year) > 0)
satis2 = satis1[satis1$Country %in% levels(satis1$Country)[cnt.obs.years > 1], ]

# count data in trimmed
length(unique(satis2$Country)) # 132 countries
length(unique(satis2$Year)) # 43 years
length(unique(satis2$YearCountry)) # 1610 country-years
length(unique(satis2$Project)) # 14 projects



#### Stan estimation


# prepare data for stan

n.cntrys = length(unique(satis2$Country))
# 2017 edit for wvs
n.yrs = year1-year0
n.proj = length(unique(satis2$Project))
n.resp = dim(satis2)[1]
n.cntry.yrs = n.cntrys * n.yrs
n.cntry.proj = length(unique(satis2$ProjCnt))
cntrys = as.numeric(factor(satis2$Country))
cnt.names = sort(unique(satis2$Country))
# cnt.ccode = satis2[match(cnt.names, satis2$Country), "COWCode"]
cnt.ccode = satis2[match(cnt.names, satis2$Country), "Country"]
yrs = as.numeric(satis2$Year)
projs = as.numeric(factor(satis2$Project))
cntry.yrs = as.numeric(satis2$YearCountry)
satis2$ProjCnt = as.factor(as.character(satis2$ProjCnt))
cntry.prjs = as.numeric(satis2$ProjCnt)

# specify data for stan

dat.1 = list(N=n.resp, K=n.proj, T=n.yrs, J=n.cntrys, P=n.cntry.proj, jj=cntrys, tt=yrs, kk=projs, 
             pp=cntry.prjs, x=satis2$Response, samp=satis2$Sample)
dat.1$x = as.integer(dat.1$x)
# dat.1$x = scale(dat.1$x)
# dat.1$samp = scale(dat.1$samp)
lapply(dat.1, summary)

# df = as.tibble(dat.1)
# pars

pars.1 = c("sigma_lambda","sigma_theta","sigma_delta","phi","lambda","delta","theta",
           "x_pred","log_lik")

# pars.1 = c("sigma_lambda","sigma_theta","sigma_delta","phi","lambda","delta","theta")

# run model

# n.iter = 500
# n.warm = 250
# n.chn = 8
# n.thin = 1

n.iter = 500
n.warm = 250
n.chn = 9
n.thin = 1

stan.mod.2 = stan(file='./replication/supdem_democracy_ajps_replication/stan.satisdem.mod.2.stan', data=dat.1, pars=pars.1, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin,
                  # init_r=0.1,
                  control=list(adapt_delta=0.99, stepsize=0.01, max_treedepth=13))
# save(stan.mod.2,file = './data/stan_claassen_satis_model.rda')
save(stan.mod.2,file = './data/stan_peformance_model_6.rda')
# init = 1, init_r = 100,
# model <- stan_model('./replication/supdem_democracy_ajps_replication/stan.satisdem.mod.2.stan')
# fit <-  sampling(model, data=dat.1, pars=pars.1,iter=n.iter, chains=n.chn,  init = 0.01)

## Evaluate model fit deprecated ----

pdf("figure_S7B.pdf", width=8, height=5)
rstan::traceplot(stan.mod.2, ncol=4, nrow=2, alpha=0.8, size=0.3,
                 pars=c("sigma_theta","sigma_delta","sigma_lambda","phi",
                        "lambda[2]","delta[44]","theta[14,57]","theta[22,87]"
                 ))
dev.off()

pdf("figure_S7A.pdf", width=6, height=4)
stan_rhat(stan.mod.2)
dev.off()

## dignose -----
library(shinystan)
launch_shinystan(stan.mod.2)
# monitor(stan.mod.2)
## Extract satisfaction with democracy estimates ----

theta.out = rstan::extract(stan.mod.2, pars = c("theta"))[[1]]

# standardize
theta.std = (theta.out - mean(as.vector(theta.out))) / sd(as.vector(theta.out)) 
theta.out.t = apply( theta.std, 1, function(x) t(x) )
# theta.out.df = data.frame(Country=rep(cnt.names, length.out=n.cntrys*n.yrs), 
#                           Year=rep(1973:2017, each=n.cntrys), theta.out.t)
theta.out.df = data.frame(Country=rep(cnt.names, length.out=n.cntrys*n.yrs), 
                          Year=rep((year0+1):year1, each=n.cntrys), theta.out.t)
theta.pe = theta.out.df[,1:2]
theta.pe$Satis_trim = apply(theta.out.df[,3:dim(theta.out.df)[2]], 1, mean)

# add variable to indicate first year survey data were available for that country
first.yr = data.frame(Country=levels(satis2$Country), 
                      First_yr = as.vector(by(satis2, satis2$Country, function(x) min(as.numeric(x$Year))+year0)))
theta.pe = merge(theta.pe, first.yr, by="Country", all.x=TRUE)
cnts = theta.pe[theta.pe$Year==2008, "Country"]
frst.yr = theta.pe[theta.pe$Year==2008, "First_yr"]
for(i in 1:length(cnts)) {
  theta.pe[theta.pe$Country==cnts[i] & theta.pe$Year<frst.yr[i], "Satis_trim"] = NA
}

saveRDS(theta.pe, "./data/stan_performance.rds")



# toy ----
N <- 100
Y <- rnorm(N,1.6,0.2)
hist(Y)

library(rstan)

model <- stan_model('./src/toy.stan')
fit <-  sampling(model, list(N=N, Y =Y), iter = 200, chains = 10)

# dignose
print(stan.mod.2)

# graph

params <- extract(fit)
hist(params$sigma)

# dignose
library(shinystan)
launch_shinystan(stan.mod.2)


sample_priors <- function() {
  # Initialize each parameter less than or equal to their lower bound
  sample <- 
    list("beta" = 0.4
    )
  return (sample)  
}

N_chains <- 1
init_values <- vector("list", N_chains)
for (n in 1:N_chains) init_values[[n]] <- sample_priors()
init_values
model_fit <- 
  sampling(
    stan_model,
    init = init_values,
    chains = N_chains
  ) 


# 载入包
library(arm) # 利用里面的invlogit函数
library(emdbook) # 利用里面的rbetabinom function函数
library(rstanarm) # 利用launch_shinystan函数
# 生成服从负二项分布的仿真数据
# 解释变量
N <- 100 # 样本量
dat <- data.frame(x1 = runif(N, -2, 2), x2 = runif(N, -2, 2))
# 模型
X <- model.matrix( ~ x1*x2, dat)
K <- dim(X)[2] # 回归系数维度
# 回归的斜率
betas <- runif(K, -1, 1)
# 设定仿真数据的过离散度
phi<- 5
# 生成响应变量

# 生成Beta-Binomial数据
W <- rep(20, 100) # 试验次数
y_bb <- rbetabinom(100, prob = invlogit(X%*%betas), size = W, theta = phi)

# 模型
m_bb <- stan(file = "./src/toy.stan", data = list(N=N, W=W, K=K, X=X, y=y_bb), pars=c("betas", "phi", "y_rep"))

# 模型检验
launch_shinystan(m_bb)