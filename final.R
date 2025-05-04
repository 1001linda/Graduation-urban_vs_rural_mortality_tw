library(demography)
library(ggplot2)
setwd("C:/修勻學/final")
south_urban_m <- read.csv("高雄市男.csv")
south_urban_f <- read.csv("高雄市女.csv")
south_county_m <- read.csv("屏東縣男.csv")
south_county_f <- read.csv("屏東縣女.csv")

middle_urban_m <- read.csv("台中市男.csv")
middle_urban_f <- read.csv("台中市女.csv")
middle_county_m <- read.csv("南投縣男.csv")
middle_county_f <- read.csv("南投縣女.csv")

s_u_m <- as.matrix(south_urban_m[,-1])
s_u_f <- as.matrix(south_urban_f[,-1])
s_c_m <- as.matrix(south_county_m[,-1])
s_c_f <- as.matrix(south_county_f[,-1])

m_u_m <- as.matrix(middle_urban_m[,-1])
m_u_f <- as.matrix(middle_urban_f[,-1])
m_c_m <- as.matrix(middle_county_m[,-1])
m_c_f <- as.matrix(middle_county_f[,-1])

years = c(2008:2022) 
ages = seq(0, 90, by = 5)
age_labels = paste(ages, ages + 4, sep = "-")

# 中部
MUM = demogdata( data = m_u_m,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "台中市男性",name= "total")
MUF = demogdata( data = m_u_f,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "台中市女性",name= "total")
MCM = demogdata( data = m_c_m,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "南投縣男性",name= "total")
MCF = demogdata( data = m_c_f,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "南投縣女性",name= "total")

# 南部
SUM = demogdata( data = s_u_m,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "高雄市男性",name= "total")
SUF = demogdata( data = s_u_f,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "高雄市女性",name= "total")
SCM = demogdata( data = s_c_m,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "屏東縣男性",name= "total")
SCF = demogdata( data = s_c_f,
                 pop = matrix(1, nrow = length(ages), ncol = length(years)),
                 ages = ages, years = years, type = "mortality", label = "屏東縣女性",name= "total")

# lee Carter svd
# 中部
lc_mum = lca(MUM, adjust = "none")
lc_muf = lca(MUF, adjust = "none")
lc_mcm = lca(MCM, adjust = "none")
lc_mcf = lca(MCF, adjust = "none")

#lcfit$ax ; lcfit$bx ; lc$kt ; #estimated parameters
#lcfit$fitted #fitted value: ax + bx*kt

# 南部
lc_sum = lca(SUM, adjust = "none")
lc_suf = lca(SUF, adjust = "none")
lc_scm = lca(SCM, adjust = "none")
lc_scf = lca(SCF, adjust = "none")

# bx圖
par(mfrow = c(1, 2))
plot(ages, lc_mcm$bx, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "age", ylab = "bx", main = "中部男性 bx", col = "red", pch = 16)
lines(ages, lc_mum$bx, col = "blue", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("南投縣", "台中市"), col = c("red", "blue"),
        lty = 1, pch = 16, lwd = 2, cex = 0.6)

plot(ages, lc_mcf$bx, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "age", ylab = "bx", main = "中部女性 bx", col = "red", pch = 16)
lines(ages, lc_muf$bx, col = "blue", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("南投縣", "台中市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.6)

par(mfrow = c(1, 2))
plot(ages, lc_scm$bx, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "age", ylab = "bx", main = "南部男性 bx", col = "red", pch = 16)
lines(ages, lc_sum$bx, col = "blue", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("屏東縣", "高雄市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.6)

plot(ages, lc_scf$bx, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "age", ylab = "bx", main = "南部女性 bx", col = "red", pch = 16)
lines(ages, lc_suf$bx, col = "blue", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("屏東縣", "高雄市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.6)

bx_value <- data.frame(
  x = paste(ages, ages + 4, sep = "-"),  # 年齡區間標籤
  台中市男性 = lc_mum$bx,
  南投縣男性 = lc_mcm$bx,
  台中市女性 = lc_muf$bx,
  南投縣女性 = lc_mcf$bx,
  高雄市男性 = lc_sum$bx,
  屏東縣男性 = lc_scm$bx,
  高雄市女性 = lc_suf$bx,
  屏東縣女性 = lc_scf$bx
)
write.csv(bx_value, "bx_value.csv", row.names = FALSE, quote = TRUE)

# kt圖
par(mfrow = c(1, 2))
plot(years, lc_mum$kt, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "year", ylab = "kt", main = "中部男性 kt", col = "blue", pch = 16)
lines(years, lc_mcm$kt, col = "red", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("南投縣", "台中市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.6)

plot(years, lc_muf$kt, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "year", ylab = "kt", main = "中部女性 kt", col = "blue", pch = 16)
lines(years, lc_mcf$kt, col = "red", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("南投縣", "台中市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.6)

par(mfrow = c(1, 2))
plot(years, lc_sum$kt, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "year", ylab = "kt", main = "南部男性 kt", col = "blue", pch = 16)
lines(years, lc_scm$kt, col = "red", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("屏東縣", "高雄市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.5)

plot(years, lc_suf$kt, type = "o", lwd = 2,  # 禁用默認 x 軸
     xlab = "year", ylab = "kt", main = "南部女性 kt", col = "blue", pch = 16)
lines(years, lc_scf$kt, col = "red", type = "o", pch = 16, lwd = 2)
legend("topright", legend = c("屏東縣", "高雄市"), col = c("red", "blue"),
       lty = 1, pch = 16, lwd = 2, cex = 0.5)

kt_value <- data.frame(
  t = years,
  台中市男性 = lc_mum$kt,
  南投縣男性 = lc_mcm$kt,
  台中市女性 = lc_muf$kt,
  南投縣女性 = lc_mcf$kt,
  高雄市男性 = lc_sum$kt,
  屏東縣男性 = lc_scm$kt,
  高雄市女性 = lc_suf$kt,
  屏東縣女性 = lc_scf$kt
)
write.csv(kt_value, "kt_value.csv", row.names = FALSE, quote = TRUE)

# 死亡率趨勢 bx*kt
# 高雄市男性
bx_kt_sum <- outer(lc_sum$bx, lc_sum$kt, FUN = "*")
data_sum <- expand.grid(age = age_labels, year = years)
data_sum$bx_kt <- as.vector(bx_kt_sum)

ggplot(data_sum, aes(x = age, y = data_sum$bx_kt, color = factor(year), group = year)) +
  geom_line(linewidth = 0.6) +
  labs(
    x = "age",
    y = "bx*kt",
    color = "year",
    title = "高雄市男性死亡率趨勢"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# 高雄市女性
bx_kt_suf <- outer(lc_suf$bx, lc_suf$kt, FUN = "*")
data_suf <- expand.grid(age = age_labels, year = years)
data_suf$bx_kt <- as.vector(bx_kt_suf)

ggplot(data_suf, aes(x = age, y = data_suf$bx_kt, color = factor(year), group = year)) +
  geom_line(linewidth = 0.6) +
  labs(
    x = "age",
    y = "bx*kt",
    color = "year",
    title = "高雄市女性死亡率趨勢"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# 繪製分組圖
# 中部
bx_kt_mum <- outer(lc_mum$bx, lc_mum$kt, FUN = "*")
data_mum <- expand.grid(age = age_labels, year = years)
data_mum$bx_kt <- as.vector(bx_kt_mum)
bx_kt_muf <- outer(lc_muf$bx, lc_muf$kt, FUN = "*")
data_muf <- expand.grid(age = age_labels, year = years)
data_muf$bx_kt <- as.vector(bx_kt_muf)

bx_kt_mcm <- outer(lc_mcm$bx, lc_mcm$kt, FUN = "*")
data_mcm <- expand.grid(age = age_labels, year = years)
data_mcm$bx_kt <- as.vector(bx_kt_mcm)
bx_kt_mcf <- outer(lc_mcf$bx, lc_mcf$kt, FUN = "*")
data_mcf <- expand.grid(age = age_labels, year = years)
data_mcf$bx_kt <- as.vector(bx_kt_mcf)

# 南部
bx_kt_sum <- outer(lc_sum$bx, lc_sum$kt, FUN = "*")
data_sum <- expand.grid(age = age_labels, year = years)
data_sum$bx_kt <- as.vector(bx_kt_sum)
bx_kt_suf <- outer(lc_suf$bx, lc_suf$kt, FUN = "*")
data_suf <- expand.grid(age = age_labels, year = years)
data_suf$bx_kt <- as.vector(bx_kt_suf)

bx_kt_scm <- outer(lc_scm$bx, lc_scm$kt, FUN = "*")
data_scm <- expand.grid(age = age_labels, year = years)
data_scm$bx_kt <- as.vector(bx_kt_scm)
bx_kt_scf <- outer(lc_scf$bx, lc_scf$kt, FUN = "*")
data_scf <- expand.grid(age = age_labels, year = years)
data_scf$bx_kt <- as.vector(bx_kt_scf)

data_muf$period <- cut(
  data_muf$year,
  breaks = c(2007, 2015, 2022),
  labels = c("2008-2015", "2016-2022"),
  include.lowest = TRUE
)

data_group <- subset(data_muf, period == "2008-2015")

ggplot(data_group, aes(x = age, y = bx_kt, color = factor(year), group = year)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~period, ncol = 1) +  # 每個時期一個子圖
  labs(
    x = "Age Group",
    y = "bx*kt",
    color = "Year",
    title = "台中市女性死亡率趨勢(2008-2015)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )


# 分段
south_urban <- read.csv("高雄市.csv")
south_county <- read.csv("屏東縣.csv")
middle_urban <- read.csv("台中市.csv")
middle_county <- read.csv("南投縣.csv")

s_u <- as.matrix(south_urban[,-1])
s_c <- as.matrix(south_county[,-1])
m_u <- as.matrix(middle_urban[,-1])
m_c <- as.matrix(middle_county[,-1])

# 中部南部
MU = demogdata( data = m_u,
                pop = matrix(1, nrow = length(ages), ncol = length(years)),
                ages = ages, years = years, type = "mortality", label = "台中市",name= "total")
MC = demogdata( data = m_c,
                pop = matrix(1, nrow = length(ages), ncol = length(years)),
                ages = ages, years = years, type = "mortality", label = "南投縣",name= "total")
SU = demogdata( data = s_u,
                pop = matrix(1, nrow = length(ages), ncol = length(years)),
                ages = ages, years = years, type = "mortality", label = "高雄市",name= "total")
SC = demogdata( data = s_c,
                pop = matrix(1, nrow = length(ages), ncol = length(years)),
                ages = ages, years = years, type = "mortality", label = "屏東縣",name= "total")

#Lee-Carter SVD
lc_mu = lca(MU, adjust = "none")
lc_mc = lca(MC, adjust = "none")
lc_su = lca(SU, adjust = "none")
lc_sc = lca(SC, adjust = "none")

# 繪製分組圖
# 中部
bx_kt_mu <- outer(lc_mu$bx, lc_mu$kt, FUN = "*")
data_mu <- expand.grid(age = age_labels, year = years)
data_mu$bx_kt <- as.vector(bx_kt_mu)

bx_kt_mc <- outer(lc_mc$bx, lc_mc$kt, FUN = "*")
data_mc <- expand.grid(age = age_labels, year = years)
data_mc$bx_kt <- as.vector(bx_kt_mc)

# 南部
bx_kt_su <- outer(lc_su$bx, lc_su$kt, FUN = "*")
data_su <- expand.grid(age = age_labels, year = years)
data_su$bx_kt <- as.vector(bx_kt_su)

bx_kt_sc <- outer(lc_sc$bx, lc_sc$kt, FUN = "*")
data_sc <- expand.grid(age = age_labels, year = years)
data_sc$bx_kt <- as.vector(bx_kt_sc)

data_sc$period <- cut(
  data_sc$year,
  breaks = c(2007, 2015, 2022),
  labels = c("2008-2015", "2016-2022"),
  include.lowest = TRUE
)

data_group <- subset(data_sc, period == "2016-2022")

ggplot(data_group, aes(x = age, y = bx_kt, color = factor(year), group = year)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~period, ncol = 1) +  # 每個時期一個子圖
  labs(
    x = "age",
    y = "bx*kt",
    color = "Year",
    title = "屏東縣死亡率趨勢(2016-2022)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )

