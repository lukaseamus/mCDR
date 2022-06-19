kdf <- read.csv("~/PATH/k.csv")
require(ggplot2)

#### 1 ####
m1 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[1:4,])
coef(m1)
exp(-3.3483260) # k = 0.03514313

new <- data.frame(Days = seq(min(kdf[1:4,]$Days), 
                             max(kdf[1:4,]$Days),
                             by = 1))

new$fit <- predict(m1, newdata = new)

ggplot() +
  geom_point(data = kdf[1:4,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 2 ####
m2 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[5:9,])
coef(m2)
exp(-1.9961204) # k = 0.1358613

new <- data.frame(Days = seq(min(kdf[5:9,]$Days), 
                             max(kdf[5:9,]$Days),
                             by = 1))

new$fit <- predict(m2, newdata = new)

ggplot() +
  geom_point(data = kdf[5:9,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 3 ####
m3 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[10:15,])
coef(m3)
exp(-2.0143622) # k = 0.1334055, A = 0.1202806

new <- data.frame(Days = seq(min(kdf[10:15,]$Days), 
                             max(kdf[10:15,]$Days),
                             by = 1))

new$fit <- predict(m3, newdata = new)

ggplot() +
  geom_point(data = kdf[10:15,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 4 ####
m4 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[16:20,])
coef(m4)
exp(-2.11621499) # k = 0.1204868, A = 0.09952685

new <- data.frame(Days = seq(min(kdf[16:20,]$Days), 
                             max(kdf[16:20,]$Days),
                             by = 1))

new$fit <- predict(m4, newdata = new)

ggplot() +
  geom_point(data = kdf[16:20,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 5 ####
m5 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[21:30,])
coef(m5)
exp(-3.4936934) # k = 0.03038843

new <- data.frame(Days = seq(min(kdf[21:30,]$Days), 
                             max(kdf[21:30,]$Days),
                             by = 1))

new$fit <- predict(m5, newdata = new)

ggplot() +
  geom_point(data = kdf[21:30,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.8830514 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m5b <- nls(Proportion ~ exp(k*Days),
          start = c(k = -0.03038843),
          data = kdf[21:30,])
coef(m5b) # k = 0.04487819

new$fit <- predict(m5b, newdata = new)

ggplot() +
  geom_point(data = kdf[21:30,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 6 ####
m6 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[31:40,])
coef(m6)
exp(-1.5175148) # k = 0.2192561, A = 0.4145205

new <- data.frame(Days = seq(min(kdf[31:40,]$Days), 
                             max(kdf[31:40,]$Days),
                             by = 1))

new$fit <- predict(m6, newdata = new)

ggplot() +
  geom_point(data = kdf[31:40,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 7 ####
m7 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[41:51,])
coef(m7)
exp(-3.7026017) # k = 0.02465929

new <- data.frame(Days = seq(min(kdf[41:51,]$Days), 
                             max(kdf[41:51,]$Days),
                             by = 1))

new$fit <- predict(m7, newdata = new)

ggplot() +
  geom_point(data = kdf[41:51,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 8 ####
m8 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[52:62,])
coef(m8)
exp(-2.0875901) # k = 0.1239856, A = 0.4591293

new <- data.frame(Days = seq(min(kdf[52:62,]$Days), 
                             max(kdf[52:62,]$Days),
                             by = 1))

new$fit <- predict(m8, newdata = new)

ggplot() +
  geom_point(data = kdf[52:62,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 9 ####
m9 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[63:73,])
coef(m9)
exp(-5.3544312) # k = 0.004727158

new <- data.frame(Days = seq(min(kdf[63:73,]$Days), 
                             max(kdf[63:73,]$Days),
                             by = 1))

new$fit <- predict(m9, newdata = new)

ggplot() +
  geom_point(data = kdf[63:73,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 10 ####
m10 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
          data = kdf[74:84,])
coef(m10)
exp(-1.7415091) # k = 0.1752557, A = 0.8095707

new <- data.frame(Days = seq(min(kdf[74:84,]$Days), 
                             max(kdf[74:84,]$Days),
                             by = 1))

new$fit <- predict(m10, newdata = new)

ggplot() +
  geom_point(data = kdf[74:84,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 11 ####
m11 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[85:89,])
coef(m11)
exp(-2.4243990) # k = 0.08853131, A = 0.5401334

new <- data.frame(Days = seq(min(kdf[85:89,]$Days), 
                             max(kdf[85:89,]$Days),
                             by = 1))

new$fit <- predict(m11, newdata = new)

ggplot() +
  geom_point(data = kdf[85:89,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 12 ####
m12 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[90:94,])
coef(m12)
exp(-3.29083296) # k = 0.03722283

new <- data.frame(Days = seq(min(kdf[90:94,]$Days), 
                             max(kdf[90:94,]$Days),
                             by = 1))

new$fit <- predict(m12, newdata = new)

ggplot() +
  geom_point(data = kdf[90:94,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 13 ####
m13 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[95:98,])
# no convergence
m13 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.005),
           data = kdf[95:98,])
coef(m13) # k = 0.01907279

new <- data.frame(Days = seq(min(kdf[95:98,]$Days), 
                             max(kdf[95:98,]$Days),
                             by = 1))

new$fit <- predict(m13, newdata = new)

ggplot() +
  geom_point(data = kdf[95:98,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 14 ####
m14 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[99:108,])
coef(m14)
exp(-3.9420728) # k = 0.01940794; cf. 0.019 in paper

new <- data.frame(Days = seq(min(kdf[99:108,]$Days), 
                             max(kdf[99:108,]$Days),
                             by = 1))

new$fit <- predict(m14, newdata = new)

ggplot() +
  geom_point(data = kdf[99:108,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 15 ####
m15 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[109:118,])
coef(m15)
exp(-4.1678318) # k = 0.0154858

new <- data.frame(Days = seq(min(kdf[109:118,]$Days), 
                             max(kdf[109:118,]$Days),
                             by = 1))

new$fit <- predict(m15, newdata = new)

ggplot() +
  geom_point(data = kdf[109:118,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0286924 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m15b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.0154858),
            data = kdf[109:118,])
coef(m15b) # k = 0.0164941; cf. 0.016 in paper

new$fit <- predict(m15b, newdata = new)

ggplot() +
  geom_point(data = kdf[109:118,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()


#### 16 ####
m16 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[119:124,])
coef(m16)
exp(-2.24253886) # k = 0.1061886 

new <- data.frame(Days = seq(min(kdf[119:124,]$Days), 
                             max(kdf[119:124,]$Days),
                             by = 1))

new$fit <- predict(m16, newdata = new)

ggplot() +
  geom_point(data = kdf[119:124,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.97612326 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m16b <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.1061886),
           data = kdf[119:124,])
coef(m16b) # k = 0.1022012; cf. 0.084 in paper

new$fit <- predict(m16b, newdata = new)

ggplot() +
  geom_point(data = kdf[119:124,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 17 ####
m17 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[125:130,])
coef(m17)
exp(-2.20981555) # k = 0.1097209

new <- data.frame(Days = seq(min(kdf[125:130,]$Days), 
                             max(kdf[125:130,]$Days),
                             by = 1))

new$fit <- predict(m17, newdata = new)

ggplot() +
  geom_point(data = kdf[125:130,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.95946346 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m17b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.1097209),
            data = kdf[125:130,])
coef(m17b) # k = 0.1070388; cf. 0.079 in paper

new$fit <- predict(m17b, newdata = new)

ggplot() +
  geom_point(data = kdf[125:130,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 18 ####
m18 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[131:136,])
coef(m18)
exp(-2.35892425) # k = 0.09452185

new <- data.frame(Days = seq(min(kdf[131:136,]$Days), 
                             max(kdf[131:136,]$Days),
                             by = 1))

new$fit <- predict(m18, newdata = new)

ggplot() +
  geom_point(data = kdf[131:136,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.95921704 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m18b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.09452185),
            data = kdf[131:136,])
coef(m18b) # k = 0.09203007; cf. 0.073 in paper

new$fit <- predict(m18b, newdata = new)

ggplot() +
  geom_point(data = kdf[131:136,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 19 ####
m19 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[137:142,])
coef(m19)
exp(-2.2375121) # k = 0.1067237

new <- data.frame(Days = seq(min(kdf[137:142,]$Days), 
                             max(kdf[137:142,]$Days),
                             by = 1))

new$fit <- predict(m19, newdata = new)

ggplot() +
  geom_point(data = kdf[137:142,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.9462399 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m19b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.1067237),
            data = kdf[137:142,])
coef(m19b) # k = 0.0754055; cf. 0.051 in paper

new$fit <- predict(m19b, newdata = new)

ggplot() +
  geom_point(data = kdf[137:142,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 20 ####
m20 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[143:148,])
coef(m20)
exp(-2.7686017) # k = 0.06274969; cf. 0.018 in paper

new <- data.frame(Days = seq(min(kdf[143:148,]$Days), 
                             max(kdf[143:148,]$Days),
                             by = 1))

new$fit <- predict(m20, newdata = new)

ggplot() +
  geom_point(data = kdf[143:148,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.8928750 (too far below 1)
# no solution possible because asymptote > 0

#### 21 ####
m21 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[149:157,])
coef(m21)
exp(-0.418797143) # k = 0.6578376

new <- data.frame(Days = seq(min(kdf[149:157,]$Days), 
                             max(kdf[149:157,]$Days),
                             by = 1))

new$fit <- predict(m21, newdata = new)

ggplot() +
  geom_point(data = kdf[149:157,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.021452328 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m21b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.6578376),
            data = kdf[149:157,])
coef(m21b) # k = 0.6531228

new$fit <- predict(m21b, newdata = new)

ggplot() +
  geom_point(data = kdf[149:157,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 22 ####
m22 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[158:166,])
coef(m22)
exp(-2.27526308) # k = 0.1027699

new <- data.frame(Days = seq(min(kdf[158:166,]$Days), 
                             max(kdf[158:166,]$Days),
                             by = 1))

new$fit <- predict(m22, newdata = new)

ggplot() +
  geom_point(data = kdf[158:166,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.10391243 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m22b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.1027699),
            data = kdf[158:166,])
coef(m22b) # k = 0.09487339

new$fit <- predict(m22b, newdata = new)

ggplot() +
  geom_point(data = kdf[158:166,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 23 ####
m23 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[167:175,])
coef(m23)
exp(-1.90399561) # k = 0.1489722

new <- data.frame(Days = seq(min(kdf[167:175,]$Days), 
                             max(kdf[167:175,]$Days),
                             by = 1))

new$fit <- predict(m23, newdata = new)

ggplot() +
  geom_point(data = kdf[167:175,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.03384119 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m23b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.1489722),
            data = kdf[167:175,])
coef(m23b) # k = 0.1364802

new$fit <- predict(m23b, newdata = new)

ggplot() +
  geom_point(data = kdf[167:175,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 24 ####
m24 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[176:184,])
coef(m24)
exp(-2.12802096) # k = 0.1190727

new <- data.frame(Days = seq(min(kdf[176:184,]$Days), 
                             max(kdf[176:184,]$Days),
                             by = 1))

new$fit <- predict(m24, newdata = new)

ggplot() +
  geom_point(data = kdf[176:184,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.04039950 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m24b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.1190727),
            data = kdf[176:184,])
coef(m24b) # k = 0.1146204

new$fit <- predict(m24b, newdata = new)

ggplot() +
  geom_point(data = kdf[176:184,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 25 ####
m25 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[185:193,])
coef(m25)
exp(-0.167340270) # k = 0.8459117

new <- data.frame(Days = seq(min(kdf[185:193,]$Days), 
                             max(kdf[185:193,]$Days),
                             by = 1))

new$fit <- predict(m25, newdata = new)

ggplot() +
  geom_point(data = kdf[185:193,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 26 ####
m26 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[194:199,])
# no convergence
m26 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.005),
           data = kdf[194:199,])
coef(m26) # k = 0.005398149

new <- data.frame(Days = seq(min(kdf[194:199,]$Days), 
                             max(kdf[194:199,]$Days),
                             by = 1))

new$fit <- predict(m26, newdata = new)

ggplot() +
  geom_point(data = kdf[194:199,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 27 ####
m27 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[200:204,])
# no convergence
m27 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.05),
           data = kdf[200:204,])
coef(m27) # k = 0.02841726

new <- data.frame(Days = seq(min(kdf[200:204,]$Days), 
                             max(kdf[200:204,]$Days),
                             by = 1))

new$fit <- predict(m27, newdata = new)

ggplot() +
  geom_point(data = kdf[200:204,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 28 ####
m28 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[205:210,])
coef(m28)
exp(-2.5657268) # k = 0.0768633

new <- data.frame(Days = seq(min(kdf[205:210,]$Days), 
                             max(kdf[205:210,]$Days),
                             by = 1))

new$fit <- predict(m28, newdata = new)

ggplot() +
  geom_point(data = kdf[205:210,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 29 ####
m29 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[211:215,])
coef(m29)
exp(-2.624909) # k = 0.07244635, A = 0.584109

new <- data.frame(Days = seq(min(kdf[211:215,]$Days), 
                             max(kdf[211:215,]$Days),
                             by = 1))

new$fit <- predict(m29, newdata = new)

ggplot() +
  geom_point(data = kdf[211:215,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.973316 (too far below 1)
# no solution possible because asymptote > 0

#### 30 ####
m30 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[216:220,])
coef(m30)
exp(-2.6357900) # k = 0.07166233, A = 0.4604946

new <- data.frame(Days = seq(min(kdf[216:220,]$Days), 
                             max(kdf[216:220,]$Days),
                             by = 1))

new$fit <- predict(m30, newdata = new)

ggplot() +
  geom_point(data = kdf[216:220,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.9818817 (too far below 1)
# no solution possible because asymptote > 0

#### 31 ####
m31 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[221:225,])
coef(m31)
exp(-2.6878493) # k = 0.06802709, A = 0.4587317

new <- data.frame(Days = seq(min(kdf[221:225,]$Days), 
                             max(kdf[221:225,]$Days),
                             by = 1))

new$fit <- predict(m31, newdata = new)

ggplot() +
  geom_point(data = kdf[221:225,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.9803790 (too far below 1)
# no solution possible because asymptote > 0

#### 32 ####
m32 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[226:230,])
coef(m32)
exp(-2.8742860) # k = 0.05645644, A = 0.7272601

new <- data.frame(Days = seq(min(kdf[226:230,]$Days), 
                             max(kdf[226:230,]$Days),
                             by = 1))

new$fit <- predict(m32, newdata = new)

ggplot() +
  geom_point(data = kdf[226:230,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0227176 (too far above 1)
# no solution possible because asymptote > 0

#### 33 ####
m33 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[231:235,])
coef(m33)
exp(-2.2174238) # k = 0.1088893, A = 0.5567825

new <- data.frame(Days = seq(min(kdf[231:235,]$Days), 
                             max(kdf[231:235,]$Days),
                             by = 1))

new$fit <- predict(m33, newdata = new)

ggplot() +
  geom_point(data = kdf[231:235,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 34 ####
m34 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[236:240,])
coef(m34)
exp(-2.0853915) # k = 0.1242585, A = 0.5583563

new <- data.frame(Days = seq(min(kdf[236:240,]$Days), 
                             max(kdf[236:240,]$Days),
                             by = 1))

new$fit <- predict(m34, newdata = new)

ggplot() +
  geom_point(data = kdf[236:240,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 35 ####
m35 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[241:248,])
coef(m35)
exp(-2.796895335) # k = 0.06099915

new <- data.frame(Days = seq(min(kdf[241:248,]$Days), 
                             max(kdf[241:248,]$Days),
                             by = 1))

new$fit <- predict(m35, newdata = new)

ggplot() +
  geom_point(data = kdf[241:248,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 36 ####
m36 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[249:256,])
coef(m36)
exp(-4.2478781) # k = 0.01429453

new <- data.frame(Days = seq(min(kdf[249:256,]$Days), 
                             max(kdf[249:256,]$Days),
                             by = 1))

new$fit <- predict(m36, newdata = new)

ggplot() +
  geom_point(data = kdf[249:256,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0725128 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m36b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.01429453),
            data = kdf[249:256,])
coef(m36b) # k = 0.03245092

new$fit <- predict(m36b, newdata = new)

ggplot() +
  geom_point(data = kdf[249:256,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# but the fit is much worse -> stick with previous k

#### 37 ####
m37 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[257:263,])
coef(m37)
exp(-4.724684) # k = 0.008873517

new <- data.frame(Days = seq(min(kdf[257:263,]$Days), 
                             max(kdf[257:263,]$Days),
                             by = 1))

new$fit <- predict(m37, newdata = new)

ggplot() +
  geom_point(data = kdf[257:263,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.029902 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m37b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.008873517),
            data = kdf[257:263,])
coef(m37b) # k = 0.03388704

new$fit <- predict(m37b, newdata = new)

ggplot() +
  geom_point(data = kdf[257:263,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# but the fit is much worse -> stick with previous k

#### 38 ####
m38 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[264:273,])
coef(m38)
exp(-4.474772) # k = 0.01139282

new <- data.frame(Days = seq(min(kdf[264:273,]$Days), 
                             max(kdf[264:273,]$Days),
                             by = 1))

new$fit <- predict(m38, newdata = new)

ggplot() +
  geom_point(data = kdf[264:273,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.111264 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m38b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.01139282),
            data = kdf[264:273,])
coef(m38b) # k = 0.03207732

new$fit <- predict(m38b, newdata = new)

ggplot() +
  geom_point(data = kdf[264:273,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# but the fit is much worse -> stick with previous k

#### 39 ####
m39 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[274:277,])
coef(m39)
exp(-2.6423635) # k = 0.07119281

new <- data.frame(Days = seq(min(kdf[274:277,]$Days), 
                             max(kdf[274:277,]$Days),
                             by = 1))

new$fit <- predict(m39, newdata = new)

ggplot() +
  geom_point(data = kdf[274:277,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0596687 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m39b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.07119281),
            data = kdf[274:277,])
coef(m39b) # k = 0.07798217

new$fit <- predict(m39b, newdata = new)

ggplot() +
  geom_point(data = kdf[274:277,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 40 ####
m40 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[278:281,])
coef(m40)
exp(-4.248529) # k = 0.01428523

new <- data.frame(Days = seq(min(kdf[278:281,]$Days), 
                             max(kdf[278:281,]$Days),
                             by = 1))

new$fit <- predict(m40, newdata = new)

ggplot() +
  geom_point(data = kdf[278:281,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 41 ####
m41 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[282:288,])
# no convergence
m41 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[282:288,])
coef(m41) # k = 0.02141361

new <- data.frame(Days = seq(min(kdf[282:288,]$Days), 
                             max(kdf[282:288,]$Days),
                             by = 1))

new$fit <- predict(m41, newdata = new)

ggplot() +
  geom_point(data = kdf[282:288,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 42 ####
m42 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[289:295,])
# no convergence
m42 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[289:295,])
coef(m42) # k = 0.03666856

new <- data.frame(Days = seq(min(kdf[289:295,]$Days), 
                             max(kdf[289:295,]$Days),
                             by = 1))

new$fit <- predict(m42, newdata = new)

ggplot() +
  geom_point(data = kdf[289:295,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 43 ####
m43 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[296:302,])
# no convergence
m43 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[296:302,])
coef(m43) # k = 0.03491098

new <- data.frame(Days = seq(min(kdf[296:302,]$Days), 
                             max(kdf[296:302,]$Days),
                             by = 1))

new$fit <- predict(m43, newdata = new)

ggplot() +
  geom_point(data = kdf[296:302,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 44 ####
m44 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[303:308,])
coef(m44)
exp(-1.4881845) # k = 0.2257822; cf. 0.082 in paper

new <- data.frame(Days = seq(min(kdf[303:308,]$Days), 
                             max(kdf[303:308,]$Days),
                             by = 1))

new$fit <- predict(m44, newdata = new)

ggplot() +
  geom_point(data = kdf[303:308,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 45 ####
m45 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[309:316,])
# no convergence
m45 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[309:316,])
coef(m45) # k = 0.01943061

new <- data.frame(Days = seq(min(kdf[309:316,]$Days), 
                             max(kdf[309:316,]$Days),
                             by = 1))

new$fit <- predict(m45, newdata = new)

ggplot() +
  geom_point(data = kdf[309:316,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 46 ####
m46 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[317:323,])
# no convergence
m46 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[317:323,])
coef(m46) # k = 0.01678631

new <- data.frame(Days = seq(min(kdf[317:323,]$Days), 
                             max(kdf[317:323,]$Days),
                             by = 1))

new$fit <- predict(m46, newdata = new)

ggplot() +
  geom_point(data = kdf[317:323,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 47 ####
m47 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[324:336,])
# no convergence
m47 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.001),
           data = kdf[324:336,])
coef(m47) # k = 0.0007787009

new <- data.frame(Days = seq(min(kdf[324:336,]$Days), 
                             max(kdf[324:336,]$Days),
                             by = 1))

new$fit <- predict(m47, newdata = new)

ggplot() +
  geom_point(data = kdf[324:336,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 48 ####
m48 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[337:346,])

coef(m48) 
exp(-4.1033003) # k = 0.01651807

new <- data.frame(Days = seq(min(kdf[337:346,]$Days), 
                             max(kdf[337:346,]$Days),
                             by = 1))

new$fit <- predict(m48, newdata = new)

ggplot() +
  geom_point(data = kdf[337:346,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0226832 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m48b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.01651807),
            data = kdf[337:346,])
coef(m48b) # k = 0.02983905

new$fit <- predict(m48b, newdata = new)

ggplot() +
  geom_point(data = kdf[337:346,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# but the fit is much worse -> stick with previous k

#### 49 ####
m49 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[347:352,])

coef(m49) 
exp(-1.7210446) # k = 0.1788792

new <- data.frame(Days = seq(min(kdf[347:352,]$Days), 
                             max(kdf[347:352,]$Days),
                             by = 1))

new$fit <- predict(m49, newdata = new)

ggplot() +
  geom_point(data = kdf[347:352,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0317548 (too far above 1)
# no solution possible because asymptote > 0

#### 50 ####
m50 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[353:358,])

coef(m50) 
exp(-1.9540447) # k = 0.1416998

new <- data.frame(Days = seq(min(kdf[353:358,]$Days), 
                             max(kdf[353:358,]$Days),
                             by = 1))

new$fit <- predict(m50, newdata = new)

ggplot() +
  geom_point(data = kdf[353:358,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 51 ####
m51 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[359:372,])

coef(m51) 
exp(-2.9881816) # k = 0.05037896

new <- data.frame(Days = seq(min(kdf[359:372,]$Days), 
                             max(kdf[359:372,]$Days),
                             by = 1))

new$fit <- predict(m51, newdata = new)

ggplot() +
  geom_point(data = kdf[359:372,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 52 ####
m52 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[373:386,])

coef(m52) 
exp(-2.6907961) # k = 0.06782692

new <- data.frame(Days = seq(min(kdf[373:386,]$Days), 
                             max(kdf[373:386,]$Days),
                             by = 1))

new$fit <- predict(m52, newdata = new)

ggplot() +
  geom_point(data = kdf[373:386,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0229965 (too far above 1)
# no solution possible because asymptote > 0

#### 53 ####
m53 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[387:400,])

coef(m53) 
exp(-2.3542084) # k = 0.09496865

new <- data.frame(Days = seq(min(kdf[387:400,]$Days), 
                             max(kdf[387:400,]$Days),
                             by = 1))

new$fit <- predict(m53, newdata = new)

ggplot() +
  geom_point(data = kdf[387:400,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 54 ####
m54 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[401:414,])

coef(m54) 
exp(-1.9592237) # k = 0.1409678

new <- data.frame(Days = seq(min(kdf[401:414,]$Days), 
                             max(kdf[401:414,]$Days),
                             by = 1))

new$fit <- predict(m54, newdata = new)

ggplot() +
  geom_point(data = kdf[401:414,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 55 ####
m55 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[415:428,])

coef(m55) 
exp(-3.0136148) # k = 0.04911382

new <- data.frame(Days = seq(min(kdf[415:428,]$Days), 
                             max(kdf[415:428,]$Days),
                             by = 1))

new$fit <- predict(m55, newdata = new)

ggplot() +
  geom_point(data = kdf[415:428,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 56 ####
m56 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[429:442,])

coef(m56) 
exp(-2.9394362) # k = 0.05289554

new <- data.frame(Days = seq(min(kdf[429:442,]$Days), 
                             max(kdf[429:442,]$Days),
                             by = 1))

new$fit <- predict(m56, newdata = new)

ggplot() +
  geom_point(data = kdf[429:442,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0215902 (too far above 1)
# no solution possible because asymptote > 0

#### 57 ####
m57 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[443:447,])

coef(m57) 
exp(-3.6283369) # k = 0.02656032

new <- data.frame(Days = seq(min(kdf[443:447,]$Days), 
                             max(kdf[443:447,]$Days),
                             by = 1))

new$fit <- predict(m57, newdata = new)

ggplot() +
  geom_point(data = kdf[443:447,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0711543 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m57b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.02656032),
            data = kdf[443:447,])
coef(m57b) # k = 0.0705176

new$fit <- predict(m57b, newdata = new)

ggplot() +
  geom_point(data = kdf[443:447,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# but the fit is much worse -> stick with previous k

#### 58 ####
m58 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[448:450,])
# no convergence
m58 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.02),
           data = kdf[448:450,])

coef(m58) # k = 0.2371343

new <- data.frame(Days = seq(min(kdf[448:450,]$Days), 
                             max(kdf[448:450,]$Days),
                             by = 1))

new$fit <- predict(m58, newdata = new)

ggplot() +
  geom_point(data = kdf[448:450,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 59 ####
m59 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[451:455,])

coef(m59) 
exp(-2.447064) # k = 0.08654732, A = 0.152125

new <- data.frame(Days = seq(min(kdf[451:455,]$Days), 
                             max(kdf[451:455,]$Days),
                             by = 1))

new$fit <- predict(m59, newdata = new)

ggplot() +
  geom_point(data = kdf[451:455,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 60 ####
m60 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[456:481,])
# no convergence
m60 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.0002),
           data = kdf[456:481,])

coef(m60) # k = 0.001033044

new <- data.frame(Days = seq(min(kdf[456:481,]$Days), 
                             max(kdf[456:481,]$Days),
                             by = 1))

new$fit <- predict(m60, newdata = new)

ggplot() +
  geom_point(data = kdf[456:481,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 61 ####
m61 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[482:507,])
# no convergence
m61 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.0002),
           data = kdf[482:507,])

coef(m61) # k = 0.00407597

new <- data.frame(Days = seq(min(kdf[482:507,]$Days), 
                             max(kdf[482:507,]$Days),
                             by = 1))

new$fit <- predict(m61, newdata = new)

ggplot() +
  geom_point(data = kdf[482:507,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 62 ####
m62 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[508:527,])
# no convergence
m62 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.0002),
           data = kdf[508:527,])

coef(m62) # k = 0.009786239

new <- data.frame(Days = seq(min(kdf[508:527,]$Days), 
                             max(kdf[508:527,]$Days),
                             by = 1))

new$fit <- predict(m62, newdata = new)

ggplot() +
  geom_point(data = kdf[508:527,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 63 ####
m63 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[528:547,])
# no convergence
m63 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.02),
           data = kdf[528:547,])

coef(m63) # k = 0.02295149

new <- data.frame(Days = seq(min(kdf[528:547,]$Days), 
                             max(kdf[528:547,]$Days),
                             by = 1))

new$fit <- predict(m63, newdata = new)

ggplot() +
  geom_point(data = kdf[528:547,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 64 ####
m64 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[548:575,])
# no convergence
m64 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.002),
           data = kdf[548:575,])

coef(m64) # k = 0.006958252

new <- data.frame(Days = seq(min(kdf[548:575,]$Days), 
                             max(kdf[548:575,]$Days),
                             by = 1))

new$fit <- predict(m64, newdata = new)

ggplot() +
  geom_point(data = kdf[548:575,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 65 ####
m65 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[576:603,])
# no convergence
m65 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.002),
           data = kdf[576:603,])

coef(m65) # k = 0.00506219

new <- data.frame(Days = seq(min(kdf[576:603,]$Days), 
                             max(kdf[576:603,]$Days),
                             by = 1))

new$fit <- predict(m65, newdata = new)

ggplot() +
  geom_point(data = kdf[576:603,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 66 ####
m66 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[604:631,])
# no convergence
m66 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[604:631,])

coef(m66) # k = 0.01454277

new <- data.frame(Days = seq(min(kdf[604:631,]$Days), 
                             max(kdf[604:631,]$Days),
                             by = 1))

new$fit <- predict(m66, newdata = new)

ggplot() +
  geom_point(data = kdf[604:631,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 67 ####
m67 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[632:668,])
# no convergence
m67 <- nls(Proportion ~ exp(k*Days),
           start = c(k = 0.001),
           data = kdf[632:668,])

coef(m67) # k = 0.001687343 (exponential growth!)

new <- data.frame(Days = seq(min(kdf[632:668,]$Days), 
                             max(kdf[632:668,]$Days),
                             by = 1))

new$fit <- predict(m67, newdata = new)

ggplot() +
  geom_point(data = kdf[632:668,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 68 ####
m68 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[669:705,])
# no convergence
m68 <- nls(Proportion ~ exp(k*Days),
           start = c(k = 0.001),
           data = kdf[669:705,])

coef(m68) # k = 0.001333367 (exponential growth!)

new <- data.frame(Days = seq(min(kdf[669:705,]$Days), 
                             max(kdf[669:705,]$Days),
                             by = 1))

new$fit <- predict(m68, newdata = new)

ggplot() +
  geom_point(data = kdf[669:705,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 69 ####
m69 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[706:742,])

coef(m69)
exp(-4.0338382) # k = 0.01770624

new <- data.frame(Days = seq(min(kdf[706:742,]$Days), 
                             max(kdf[706:742,]$Days),
                             by = 1))

new$fit <- predict(m69, newdata = new)

ggplot() +
  geom_point(data = kdf[706:742,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 70 ####
m70 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[743:779,])
# no convergence
m70 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.001),
           data = kdf[743:779,])

coef(m70) # k = 0.00001398481

new <- data.frame(Days = seq(min(kdf[743:779,]$Days), 
                             max(kdf[743:779,]$Days),
                             by = 1))

new$fit <- predict(m70, newdata = new)

ggplot() +
  geom_point(data = kdf[743:779,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 71 ####
m71 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[780:816,])
# no convergence
m71 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.001),
           data = kdf[780:816,])

coef(m71) # k = 0.004654517

new <- data.frame(Days = seq(min(kdf[780:816,]$Days), 
                             max(kdf[780:816,]$Days),
                             by = 1))

new$fit <- predict(m71, newdata = new)

ggplot() +
  geom_point(data = kdf[780:816,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 72 ####
m72 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[817:853,])
# no convergence
m72 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.01),
           data = kdf[817:853,])

coef(m72) # k = 0.01057395

new <- data.frame(Days = seq(min(kdf[817:853,]$Days), 
                             max(kdf[817:853,]$Days),
                             by = 1))

new$fit <- predict(m72, newdata = new)

ggplot() +
  geom_point(data = kdf[817:853,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 73 ####
m73 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[854:870,])
# no convergence
m73 <- nls(Proportion ~ exp(k*Days),
           start = c(k = -0.001),
           data = kdf[854:870,])

coef(m73) # k = 0.003616756

new <- data.frame(Days = seq(min(kdf[854:870,]$Days), 
                             max(kdf[854:870,]$Days),
                             by = 1))

new$fit <- predict(m73, newdata = new)

ggplot() +
  geom_point(data = kdf[854:870,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 74 ####
m74 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[871:887,])

coef(m74)
exp(-3.73404596) # k = 0.02389596

new <- data.frame(Days = seq(min(kdf[871:887,]$Days), 
                             max(kdf[871:887,]$Days),
                             by = 1))

new$fit <- predict(m74, newdata = new)

ggplot() +
  geom_point(data = kdf[871:887,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.92226452 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m74b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.02389596),
            data = kdf[871:887,])
coef(m74b) # k = 0.02703591

new$fit <- predict(m74b, newdata = new)

ggplot() +
  geom_point(data = kdf[871:887,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 75 ####
m75 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[888:904,])

coef(m75) 
exp(-4.2422004) # k = 0.01437592 

new <- data.frame(Days = seq(min(kdf[888:904,]$Days), 
                             max(kdf[888:904,]$Days),
                             by = 1))

new$fit <- predict(m75, newdata = new)

ggplot() +
  geom_point(data = kdf[888:904,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# growth but of the negative exponential decay type: -exp(-k*Days)

m75b <- nls(Proportion ~ exp(k*Days),
            start = c(k = 0.001),
            data = kdf[888:904,])
coef(m75b) # k = 0.003155378 (exponential growth!)

new$fit <- predict(m75b, newdata = new)

ggplot() +
  geom_point(data = kdf[888:904,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 76 ####
m76 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[905:923,])

coef(m76)
exp(-1.3349682) # k = 0.2631665, A = 0.6354903

new <- data.frame(Days = seq(min(kdf[905:923,]$Days), 
                             max(kdf[905:923,]$Days),
                             by = 1))

new$fit <- predict(m76, newdata = new)

ggplot() +
  geom_point(data = kdf[905:923,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.9665744 (too far below 1)
# no solution possible because asymptote > 0

#### 77 ####
m77 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[924:939,])

coef(m77)
exp(-1.1395537) # k = 0.3199618, A = 0.6422087

new <- data.frame(Days = seq(min(kdf[924:939,]$Days), 
                             max(kdf[924:939,]$Days),
                             by = 1))

new$fit <- predict(m77, newdata = new)

ggplot() +
  geom_point(data = kdf[924:939,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 78 ####
m78 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[940:955,])

coef(m78)
exp(-1.6371734) # k = 0.1945291, A = 0.4119004

new <- data.frame(Days = seq(min(kdf[940:955,]$Days), 
                             max(kdf[940:955,]$Days),
                             by = 1))

new$fit <- predict(m78, newdata = new)

ggplot() +
  geom_point(data = kdf[940:955,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 79 ####
m79 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[956:973,])

coef(m79)
exp(-1.4824822) # k = 0.2270733, A = 0.4986344

new <- data.frame(Days = seq(min(kdf[956:973,]$Days), 
                             max(kdf[956:973,]$Days),
                             by = 1))

new$fit <- predict(m79, newdata = new)

ggplot() +
  geom_point(data = kdf[956:973,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 0.9233658 (too far below 1)
# solution: fix intercept to 1 and asymptote to 0
m79b <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.2),
            data = kdf[956:973,])

coef(m79b) # k = 0.06836332

new$fit <- predict(m79b, newdata = new)

ggplot() +
  geom_point(data = kdf[956:973,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# but the fit is much worse -> stick with previous k

#### 80 ####
m80 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[974:987,])

coef(m80)
exp(-0.8438216) # k = 0.4300638, A = 0.7305928

new <- data.frame(Days = seq(min(kdf[974:987,]$Days), 
                             max(kdf[974:987,]$Days),
                             by = 1))

new$fit <- predict(m80, newdata = new)

ggplot() +
  geom_point(data = kdf[974:987,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 81 ####
m81 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[988:1003,])

coef(m81)
exp(-0.8209340) # k = 0.4400205, A = 0.6572162

new <- data.frame(Days = seq(min(kdf[988:1003,]$Days), 
                             max(kdf[988:1003,]$Days),
                             by = 1))

new$fit <- predict(m81, newdata = new)

ggplot() +
  geom_point(data = kdf[988:1003,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 82 ####
m82 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1004:1022,])

coef(m82)
exp(-5.6701544) # k = 0.003447333

new <- data.frame(Days = seq(min(kdf[1004:1022,]$Days), 
                             max(kdf[1004:1022,]$Days),
                             by = 1))

new$fit <- predict(m82, newdata = new)

ggplot() +
  geom_point(data = kdf[1004:1022,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# data don't fit exponential decay -> remove outlying data
m82 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[c(1004,1013:1022),])

coef(m82)
exp(-2.5762944) # k = 0.07605531

new <- data.frame(Days = seq(min(kdf[c(1004,1013:1022),]$Days), 
                             max(kdf[c(1004,1013:1022),]$Days),
                             by = 1))

new$fit <- predict(m82, newdata = new)

ggplot() +
  geom_point(data = kdf[c(1004,1013:1022),], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 83 ####
m83 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1023:1040,])
# no convergence 
ggplot() +
  geom_point(data = kdf[1023:1040,], aes(Days, Proportion)) +
  theme_minimal()
# data don't fit exponential decay -> remove outlying data
m83 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[c(1023,1028:1040),])

coef(m83)
exp(-2.6298220) # k = 0.07209129

new <- data.frame(Days = seq(min(kdf[c(1023,1028:1040),]$Days), 
                             max(kdf[c(1023,1028:1040),]$Days),
                             by = 1))

new$fit <- predict(m83, newdata = new)

ggplot() +
  geom_point(data = kdf[c(1023,1028:1040),], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()


#### 84 ####
m84 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1041:1059,])

coef(m84)
exp(-2.9308612) # k = 0.05335107

new <- data.frame(Days = seq(min(kdf[1041:1059,]$Days), 
                             max(kdf[1041:1059,]$Days),
                             by = 1))

new$fit <- predict(m84, newdata = new)

ggplot() +
  geom_point(data = kdf[1041:1059,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# data don't fit exponential decay -> remove outlying data

m84 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[c(1041,1050:1059),])

coef(m84)
exp(-1.6011647) # k = 0.2016615

new <- data.frame(Days = seq(min(kdf[c(1041,1050:1059),]$Days), 
                             max(kdf[c(1041,1050:1059),]$Days),
                             by = 1))

new$fit <- predict(m84, newdata = new)

ggplot() +
  geom_point(data = kdf[c(1041,1050:1059),], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 85 ####
m85 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1060:1074,])

coef(m85)
exp(-0.4865799) # k = 0.6147252

new <- data.frame(Days = seq(min(kdf[1060:1074,]$Days), 
                             max(kdf[1060:1074,]$Days),
                             by = 1))

new$fit <- predict(m85, newdata = new)

ggplot() +
  geom_point(data = kdf[1060:1074,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# data don't fit exponential decay -> remove outlying data

m85 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[c(1060,1065:1074),])

coef(m85)
exp(-0.3882928) # k = 0.6782137

new <- data.frame(Days = seq(min(kdf[c(1060,1065:1074),]$Days), 
                             max(kdf[c(1060,1065:1074),]$Days),
                             by = 1))

new$fit <- predict(m85, newdata = new)

ggplot() +
  geom_point(data = kdf[c(1060,1065:1074),], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 86 ####
m86 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1075:1091,])

coef(m86)
exp(0.5269409) # k = 1.693743

new <- data.frame(Days = seq(min(kdf[1075:1091,]$Days), 
                             max(kdf[1075:1091,]$Days),
                             by = 1))

new$fit <- predict(m86, newdata = new)

ggplot() +
  geom_point(data = kdf[1075:1091,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# data don't fit exponential decay -> remove outlying data

m86 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[c(1075,1078:1091),])

coef(m86)
exp(0.4631888) # k = 1.544643, A = 0.2555963

new <- data.frame(Days = seq(min(kdf[c(1075,1078:1091),]$Days), 
                             max(kdf[c(1075,1078:1091),]$Days),
                             by = 1))

new$fit <- predict(m86, newdata = new)

ggplot() +
  geom_point(data = kdf[c(1075,1078:1091),], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 87 ####
m87 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1092:1110,])

coef(m87)
exp(-0.01294494) # k = 0.9871385, A = 0.17218311

new <- data.frame(Days = seq(min(kdf[1092:1110,]$Days), 
                             max(kdf[1092:1110,]$Days),
                             by = 1))

new$fit <- predict(m87, newdata = new)

ggplot() +
  geom_point(data = kdf[1092:1110,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 88 ####
m88 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1111:1128,])

coef(m88)
exp(-0.03848029) # k = 0.9622507, A = 0.03510906

new <- data.frame(Days = seq(min(kdf[1111:1128,]$Days), 
                             max(kdf[1111:1128,]$Days),
                             by = 1))

new$fit <- predict(m88, newdata = new)

ggplot() +
  geom_point(data = kdf[1111:1128,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 89 ####
m89 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1129:1146,])

coef(m89)
exp(-0.3987247) # k = 0.6711755, A = -0.3605535

new <- data.frame(Days = seq(min(kdf[1129:1146,]$Days), 
                             max(kdf[1129:1146,]$Days),
                             by = 1))

new$fit <- predict(m89, newdata = new)

ggplot() +
  geom_point(data = kdf[1129:1146,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 90 ####
m90 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1147:1157,])

coef(m90)
exp(-1.7518002) # k = 0.1734614

new <- data.frame(Days = seq(min(kdf[1147:1157,]$Days), 
                             max(kdf[1147:1157,]$Days),
                             by = 1))

new$fit <- predict(m90, newdata = new)

ggplot() +
  geom_point(data = kdf[1147:1157,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 91 ####
m91 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1158:1168,])

coef(m91)
exp(-2.0195001) # k = 0.1327218

new <- data.frame(Days = seq(min(kdf[1158:1168,]$Days), 
                             max(kdf[1158:1168,]$Days),
                             by = 1))

new$fit <- predict(m91, newdata = new)

ggplot() +
  geom_point(data = kdf[1158:1168,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 92 ####
m92 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1169:1179,])

coef(m92)
exp(-0.2175681) # k = 0.8044728

new <- data.frame(Days = seq(min(kdf[1169:1179,]$Days), 
                             max(kdf[1169:1179,]$Days),
                             by = 1))

new$fit <- predict(m92, newdata = new)

ggplot() +
  geom_point(data = kdf[1169:1179,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 93 ####
m93 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1180:1190,])

coef(m93)
exp(-0.9716695) # k = 0.3784507

new <- data.frame(Days = seq(min(kdf[1180:1190,]$Days), 
                             max(kdf[1180:1190,]$Days),
                             by = 1))

new$fit <- predict(m93, newdata = new)

ggplot() +
  geom_point(data = kdf[1180:1190,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 94 ####
m94 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1191:1201,])

coef(m94)
exp(-1.3912895) # k = 0.2487543

new <- data.frame(Days = seq(min(kdf[1191:1201,]$Days), 
                             max(kdf[1191:1201,]$Days),
                             by = 1))

new$fit <- predict(m94, newdata = new)

ggplot() +
  geom_point(data = kdf[1191:1201,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 95 ####
m95 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1202:1212,])

coef(m95)
exp(-1.5462804) # k = 0.2130389

new <- data.frame(Days = seq(min(kdf[1202:1212,]$Days), 
                             max(kdf[1202:1212,]$Days),
                             by = 1))

new$fit <- predict(m95, newdata = new)

ggplot() +
  geom_point(data = kdf[1202:1212,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 96 ####
m96 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1213:1223,])

coef(m96)
exp(-0.3824959) # k = 0.6821567

new <- data.frame(Days = seq(min(kdf[1213:1223,]$Days), 
                             max(kdf[1213:1223,]$Days),
                             by = 1))

new$fit <- predict(m96, newdata = new)

ggplot() +
  geom_point(data = kdf[1213:1223,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 97 ####
m97 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1224:1234,])

coef(m97)
exp(-0.7745258) # k = 0.4609223

new <- data.frame(Days = seq(min(kdf[1224:1234,]$Days), 
                             max(kdf[1224:1234,]$Days),
                             by = 1))

new$fit <- predict(m97, newdata = new)

ggplot() +
  geom_point(data = kdf[1224:1234,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 98 ####
m98 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1235:1245,])

coef(m98)
exp(-0.3537131) # k = 0.7020764

new <- data.frame(Days = seq(min(kdf[1235:1245,]$Days), 
                             max(kdf[1235:1245,]$Days),
                             by = 1))

new$fit <- predict(m98, newdata = new)

ggplot() +
  geom_point(data = kdf[1235:1245,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 99 ####
m99 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
           data = kdf[1246:1256,])

coef(m99)
exp(-0.5465631) # k = 0.5789361

new <- data.frame(Days = seq(min(kdf[1246:1256,]$Days), 
                             max(kdf[1246:1256,]$Days),
                             by = 1))

new$fit <- predict(m99, newdata = new)

ggplot() +
  geom_point(data = kdf[1246:1256,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 100 ####
m100 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1257:1267,])

coef(m100)
exp(-0.5707060) # k = 0.5651263

new <- data.frame(Days = seq(min(kdf[1257:1267,]$Days), 
                             max(kdf[1257:1267,]$Days),
                             by = 1))

new$fit <- predict(m100, newdata = new)

ggplot() +
  geom_point(data = kdf[1257:1267,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 101 ####
m101 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1268:1278,])

coef(m101)
exp(-0.8602941) # k = 0.4230376

new <- data.frame(Days = seq(min(kdf[1268:1278,]$Days), 
                             max(kdf[1268:1278,]$Days),
                             by = 1))

new$fit <- predict(m101, newdata = new)

ggplot() +
  geom_point(data = kdf[1268:1278,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 102 ####
m102 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1279:1289,])

coef(m102)
exp(-2.9383116) # k = 0.05295506

new <- data.frame(Days = seq(min(kdf[1279:1289,]$Days), 
                             max(kdf[1279:1289,]$Days),
                             by = 1))

new$fit <- predict(m102, newdata = new)

ggplot() +
  geom_point(data = kdf[1279:1289,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 103 ####
m103 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1290:1300,])

coef(m103)
exp(-3.0012966) # k = 0.04972256

new <- data.frame(Days = seq(min(kdf[1290:1300,]$Days), 
                             max(kdf[1290:1300,]$Days),
                             by = 1))

new$fit <- predict(m103, newdata = new)

ggplot() +
  geom_point(data = kdf[1290:1300,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 104 ####
m104 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1301:1311,])

coef(m104)
exp(-1.9005901) # k = 0.1494804

new <- data.frame(Days = seq(min(kdf[1301:1311,]$Days), 
                             max(kdf[1301:1311,]$Days),
                             by = 1))

new$fit <- predict(m104, newdata = new)

ggplot() +
  geom_point(data = kdf[1301:1311,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 105 ####
m105 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1312:1322,])

coef(m105)
exp(-2.0745036) # k = 0.1256188

new <- data.frame(Days = seq(min(kdf[1312:1322,]$Days), 
                             max(kdf[1312:1322,]$Days),
                             by = 1))

new$fit <- predict(m105, newdata = new)

ggplot() +
  geom_point(data = kdf[1312:1322,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 106 ####
m106 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1323:1333,])

coef(m106)
exp(-3.61545661) # k = 0.02690464

new <- data.frame(Days = seq(min(kdf[1323:1333,]$Days), 
                             max(kdf[1323:1333,]$Days),
                             by = 1))

new$fit <- predict(m106, newdata = new)

ggplot() +
  geom_point(data = kdf[1323:1333,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 107 ####
m107 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1334:1344,])

coef(m107)
exp(-3.80386216) # k = 0.02228454

new <- data.frame(Days = seq(min(kdf[1334:1344,]$Days), 
                             max(kdf[1334:1344,]$Days),
                             by = 1))

new$fit <- predict(m107, newdata = new)

ggplot() +
  geom_point(data = kdf[1334:1344,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 108 ####
m108 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1345:1355,])

coef(m108)
exp(-1.8622549) # k = 0.155322

new <- data.frame(Days = seq(min(kdf[1345:1355,]$Days), 
                             max(kdf[1345:1355,]$Days),
                             by = 1))

new$fit <- predict(m108, newdata = new)

ggplot() +
  geom_point(data = kdf[1345:1355,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 109 ####
m109 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1356:1366,])

coef(m109)
exp(-1.8620997) # k = 0.1553461

new <- data.frame(Days = seq(min(kdf[1356:1366,]$Days), 
                             max(kdf[1356:1366,]$Days),
                             by = 1))

new$fit <- predict(m109, newdata = new)

ggplot() +
  geom_point(data = kdf[1356:1366,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 110 ####
m110 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1367:1377,])

coef(m110)
exp(-2.1130437) # k = 0.1208695

new <- data.frame(Days = seq(min(kdf[1367:1377,]$Days), 
                             max(kdf[1367:1377,]$Days),
                             by = 1))

new$fit <- predict(m110, newdata = new)

ggplot() +
  geom_point(data = kdf[1367:1377,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 111 ####
m111 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1378:1388,])

coef(m111)
exp(-2.142771) # k = 0.1173293

new <- data.frame(Days = seq(min(kdf[1378:1388,]$Days), 
                             max(kdf[1378:1388,]$Days),
                             by = 1))

new$fit <- predict(m111, newdata = new)

ggplot() +
  geom_point(data = kdf[1378:1388,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 112 ####
m112 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1389:1399,])

coef(m112)
exp(-2.0919571) # k = 0.1234453

new <- data.frame(Days = seq(min(kdf[1389:1399,]$Days), 
                             max(kdf[1389:1399,]$Days),
                             by = 1))

new$fit <- predict(m112, newdata = new)

ggplot() +
  geom_point(data = kdf[1389:1399,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 113 ####
m113 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1400:1410,])

coef(m113)
exp(-2.5399750) # k = 0.07886837

new <- data.frame(Days = seq(min(kdf[1400:1410,]$Days), 
                             max(kdf[1400:1410,]$Days),
                             by = 1))

new$fit <- predict(m113, newdata = new)

ggplot() +
  geom_point(data = kdf[1400:1410,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 114 ####
m114 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1411:1419,])

coef(m114)
exp(-1.2990745) # k = 0.2727841

new <- data.frame(Days = seq(min(kdf[1411:1419,]$Days), 
                             max(kdf[1411:1419,]$Days),
                             by = 1))

new$fit <- predict(m114, newdata = new)

ggplot() +
  geom_point(data = kdf[1411:1419,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 115 ####
m115 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1420:1428,])

coef(m115)
exp(-0.2018378) # k = 0.8172275

new <- data.frame(Days = seq(min(kdf[1420:1428,]$Days), 
                             max(kdf[1420:1428,]$Days),
                             by = 1))

new$fit <- predict(m115, newdata = new)

ggplot() +
  geom_point(data = kdf[1420:1428,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 116 ####
m116 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1429:1437,])

coef(m116)
exp(-1.3386600) # k = 0.2621968

new <- data.frame(Days = seq(min(kdf[1429:1437,]$Days), 
                             max(kdf[1429:1437,]$Days),
                             by = 1))

new$fit <- predict(m116, newdata = new)

ggplot() +
  geom_point(data = kdf[1429:1437,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 117 ####
m117 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1438:1446,])

coef(m117)
exp(-2.5247182) # k = 0.08008088

new <- data.frame(Days = seq(min(kdf[1438:1446,]$Days), 
                             max(kdf[1438:1446,]$Days),
                             by = 1))

new$fit <- predict(m117, newdata = new)

ggplot() +
  geom_point(data = kdf[1438:1446,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 118 ####
m118 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1447:1454,])

coef(m118)
exp(-1.5701899) # k = 0.2080057

new <- data.frame(Days = seq(min(kdf[1447:1454,]$Days), 
                             max(kdf[1447:1454,]$Days),
                             by = 1))

new$fit <- predict(m118, newdata = new)

ggplot() +
  geom_point(data = kdf[1447:1454,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 119 ####
m119 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1455:1471,])

coef(m119)
exp(-3.0796033) # k = 0.04597749

new <- data.frame(Days = seq(min(kdf[1455:1471,]$Days), 
                             max(kdf[1455:1471,]$Days),
                             by = 1))

new$fit <- predict(m119, newdata = new)

ggplot() +
  geom_point(data = kdf[1455:1471,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 120 ####
m120 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1472:1481,])

coef(m120)
exp(-1.9033344) # k = 0.1490707 cf. 0.004493151 in paper, A = 0.6194444

new <- data.frame(Days = seq(min(kdf[1472:1481,]$Days), 
                             max(kdf[1472:1481,]$Days),
                             by = 1))

new$fit <- predict(m120, newdata = new)

ggplot() +
  geom_point(data = kdf[1472:1481,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 121 ####
m121 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1482:1490,])

coef(m121)
exp(-2.290925) # k = 0.1011728, A = 0.104147

new <- data.frame(Days = seq(min(kdf[1482:1490,]$Days), 
                             max(kdf[1482:1490,]$Days),
                             by = 1))

new$fit <- predict(m121, newdata = new)

ggplot() +
  geom_point(data = kdf[1482:1490,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 122 ####
m122 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1491:1499,])

coef(m122)
exp(-3.1239385) # k = 0.0439836 cf. 0.006383562 in paper, A = 0.5660852

new <- data.frame(Days = seq(min(kdf[1491:1499,]$Days), 
                             max(kdf[1491:1499,]$Days),
                             by = 1))

new$fit <- predict(m122, newdata = new)

ggplot() +
  geom_point(data = kdf[1491:1499,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 123 ####
m123 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1500:1507,])

coef(m123)
exp(-0.6992840) # k = 0.496941, A = 0.3125692

new <- data.frame(Days = seq(min(kdf[1500:1507,]$Days), 
                             max(kdf[1500:1507,]$Days),
                             by = .1))

new$fit <- predict(m123, newdata = new)

ggplot() +
  geom_point(data = kdf[1500:1507,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 124 ####
m124 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1508:1515,])

coef(m124)
exp(-1.2088125) # k = 0.2985516, A = 0.1033804

new <- data.frame(Days = seq(min(kdf[1508:1515,]$Days), 
                             max(kdf[1508:1515,]$Days),
                             by = .1))

new$fit <- predict(m124, newdata = new)

ggplot() +
  geom_point(data = kdf[1508:1515,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 125 ####
m125 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1516:1525,])

coef(m125)
exp(-3.3346821) # k = 0.03562591

new <- data.frame(Days = seq(min(kdf[1516:1525,]$Days), 
                             max(kdf[1516:1525,]$Days),
                             by = 1))

new$fit <- predict(m125, newdata = new)

ggplot() +
  geom_point(data = kdf[1516:1525,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

# intercept = 1.0837403 (too far above 1)
# solution: fix intercept to 1 and asymptote to 0
m125b <- nls(Proportion ~ exp(k*Days),
             start = c(k = -0.03562591),
             data = kdf[1516:1525,])

coef(m125b) # k = 0.06845273

new$fit <- predict(m125b, newdata = new)

ggplot() +
  geom_point(data = kdf[1516:1525,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 126 ####
m126 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1526:1535,])

coef(m126)
exp(-4.9977313) # k = 0.006753251, A = 0.1969961

new <- data.frame(Days = seq(min(kdf[1526:1535,]$Days), 
                             max(kdf[1526:1535,]$Days),
                             by = 1))

new$fit <- predict(m126, newdata = new)

ggplot() +
  geom_point(data = kdf[1526:1535,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 127 ####
m127 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1536:1543,])

coef(m127)
exp(-4.6291177) # k = 0.00976337

new <- data.frame(Days = seq(min(kdf[1536:1543,]$Days), 
                             max(kdf[1536:1543,]$Days),
                             by = 1))

new$fit <- predict(m127, newdata = new)

ggplot() +
  geom_point(data = kdf[1536:1543,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 128 ####
m128 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1544:1558,])

coef(m128)
exp(-4.4357828) # k = 0.01184579, A = 0.1914884

new <- data.frame(Days = seq(min(kdf[1544:1558,]$Days), 
                             max(kdf[1544:1558,]$Days),
                             by = 1))

new$fit <- predict(m128, newdata = new)

ggplot() +
  geom_point(data = kdf[1544:1558,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 129 ####
m129 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1559:1561,])
# no convergence
m129 <- nls(Proportion ~ exp(k*Days),
            start = c(k = 0.001),
            data = kdf[1559:1561,])
coef(m129) # k = 0.001946614 (exponential growth!)

new <- data.frame(Days = seq(min(kdf[1559:1561,]$Days), 
                             max(kdf[1559:1561,]$Days),
                             by = 1))

new$fit <- predict(m129, newdata = new)

ggplot() +
  geom_point(data = kdf[1559:1561,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 130 ####
m130 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1562:1563,])
# no convergence
m130 <- nls(Proportion ~ exp(k*Days),
            start = c(k = -0.01),
            data = kdf[1562:1563,])
# no convergence -> too few data

#### 131 ####
m131 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1564:1570,])

coef(m131)
exp(-3.0387744) # k = 0.04789355 cf. 0.00438 in paper, A = 0.4922648

new <- data.frame(Days = seq(min(kdf[1564:1570,]$Days), 
                             max(kdf[1564:1570,]$Days),
                             by = 1))

new$fit <- predict(m131, newdata = new)

ggplot() +
  geom_point(data = kdf[1564:1570,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 132 ####
m132 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1571:1577,])

coef(m132)
exp(-3.1697150) # k = 0.04201557 cf. 0.00662 in paper, A = 0.3618671

new <- data.frame(Days = seq(min(kdf[1571:1577,]$Days), 
                             max(kdf[1571:1577,]$Days),
                             by = 1))

new$fit <- predict(m132, newdata = new)

ggplot() +
  geom_point(data = kdf[1571:1577,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 133 ####
m133 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1578:1584,])

coef(m133)
exp(-3.4731940) # k = 0.0310178 cf. 0.00149 in paper, A = 0.7745286

new <- data.frame(Days = seq(min(kdf[1578:1584,]$Days), 
                             max(kdf[1578:1584,]$Days),
                             by = 1))

new$fit <- predict(m133, newdata = new)

ggplot() +
  geom_point(data = kdf[1578:1584,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 134 ####
m134 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1585:1591,])

coef(m134)
exp(-3.3305644) # k = 0.03577291 cf. 0.00363 in paper, A = 0.5459824

new <- data.frame(Days = seq(min(kdf[1585:1591,]$Days), 
                             max(kdf[1585:1591,]$Days),
                             by = 1))

new$fit <- predict(m134, newdata = new)

ggplot() +
  geom_point(data = kdf[1585:1591,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()

#### 135 ####
m135 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1592:1596,])
# no convergence
m135 <- nls(Proportion ~ exp(k*Days),
            start = c(k = 0.1),
            data = kdf[1592:1596,])
coef(m135) # k = 0.3196262

new <- data.frame(Days = seq(min(kdf[1592:1596,]$Days), 
                             max(kdf[1592:1596,]$Days),
                             by = 1))

new$fit <- predict(m135, newdata = new)

ggplot() +
  geom_point(data = kdf[1592:1596,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
# fit is not good enough

#### 136 ####
m136 <- nls(Proportion ~ SSasymp(Days, A, G, lnk),
            data = kdf[1597:1602,])

coef(m136) 
exp(-0.9367556) # k = 0.3918972, A = 0.2159011

new <- data.frame(Days = seq(min(kdf[1597:1602,]$Days), 
                             max(kdf[1597:1602,]$Days),
                             by = 1))

new$fit <- predict(m136, newdata = new)

ggplot() +
  geom_point(data = kdf[1597:1602,], aes(Days, Proportion)) +
  geom_line(data = new, aes(Days, fit)) +
  theme_minimal()
