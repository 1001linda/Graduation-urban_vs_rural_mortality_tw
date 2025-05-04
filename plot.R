setwd("C:/修勻學/final")
library(demography)
library(ggplot2)
library(forecast)

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

# 使用ARIMA模型模擬到2008-2017年的kt
kt_mum <- lc_mum$kt[1:10]  # 2008-2017 年的 k_t
arima_mum <- auto.arima(kt_mum)

# 預測 2018-2022 年的 kt
kt_for_mum <- forecast(arima_mum, h = 5)
kt_pre_mum <- as.numeric(kt_for_mum$mean)  # 提取預測值

kt_full <- c(kt_mum, kt_pre_mum)  # 結合實際與預測的 k_t
names(kt_full) <- c(2008:2022)  # 添加年份標籤

# 初始化矩陣儲存 ln(m_x,t)
ln_m_xt <- matrix(NA, nrow = length(ages), ncol = length(kt_full))
rownames(ln_m_xt) <- age_labels  # 行名為年齡段
colnames(ln_m_xt) <- names(kt_full)  # 列名為年份

# 計算 ln(m_x,t)
for (t in 1:length(kt_full)) {
  ln_m_xt[, t] <- lc_mum$ax + lc_mum$bx * kt_full[t]
}

m_xt <- exp(ln_m_xt)
mape_age <- rowMeans(abs((m_xt[, 11:15] - m_u_m[, 11:15]) / m_u_m[, 11:15]), na.rm = TRUE) * 100
