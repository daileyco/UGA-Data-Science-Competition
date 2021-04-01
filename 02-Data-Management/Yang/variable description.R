# =============================================
# variable decription
# =============================================
library(tidyverse)
library(here)
library(patchwork)

dat <- read.csv(file = here("01-Data/00-Raw/Simulated_Data_Train.csv")) %>% 
  mutate(Default_ind = factor(Default_ind, levels = c(0,1), labels = c("Not \ndefaulted","Account \ndefaulted")),
         ind_acc_XYZ = factor(ind_acc_XYZ, levels = c(0,1), labels = c("Yes=1", "No=0")))
oppath ="03-Exploratory-Analyses/"
summary(dat)
# =============================================
dpiset = 600
colnames(dat)
# outcome
y = "Indicator of Default"

# Credit Debt
g_cdl <- c("Total debt", "Average monthly debt")
g_cdv <- c("tot_credit_debt", "avg_card_debt")

plotg_cd <- list()
for (i in 1:2) {
  set.seed(123456)
  sample <- sample_frac(tbl = dat,size = 0.2) %>% mutate(y = get(g_cdv[i]))
  plotg_cd[[i]] <- dat %>% mutate(y = get(g_cdv[i])) %>% 
    ggplot(aes(x = Default_ind, y = y)) +
    geom_jitter(data = sample,height = 0)  +
    geom_violin(alpha = 0.6) +
    theme_bw() +
    labs(x = y, y = g_cdl[i]) +
    theme(axis.title.x.bottom = element_blank())
}

ggsave(filename = here::here(paste0(oppath,"g_cd.png")), plot = wrap_plots(plotg_cd,ncol = 2) + plot_layout(guides='collect') &  theme(legend.position='bottom'), 
       width = 4, height = 4, dpi= dpiset)

# ============================
# Credit Tenure / Age
g_ctal <- c("Age in months of  first credit product","Age in months of first credit product (Good)","Age in months of  first credit card")
g_ctav <- c("credit_age","credit_good_age","card_age")

plotg_cta <- list()
for (i in 1:3) {
  set.seed(123456)
  sample <- sample_frac(tbl = dat,size = 0.1) %>% mutate(y = get(g_ctav[i]))
  plotg_cta[[i]] <- dat %>% mutate(y = get(g_ctav[i])) %>% 
    ggplot(aes(x = Default_ind, y = y)) +
    geom_jitter(data = sample,height = 0)  +
    geom_violin(alpha = 0.6) +
    theme_bw() +
    labs(x = y, y = g_ctal[i]) +
    theme(axis.title.x.bottom = element_blank())
}

ggsave(filename = here::here(paste0(oppath,"g_cta.png")), plot = wrap_plots(plotg_cta,ncol = 3) + plot_layout(guides='collect') &  theme(legend.position='bottom'), 
       width = 6, height = 4, dpi= dpiset)

# ============================
# Past Credit Delinquency Information
g_pcdv <- c("non_mtg_acc_past_due_12_months_num","non_mtg_acc_past_due_6_months_num","mortgages_past_due_6_months_num","credit_past_due_amount")
g_pcdl <- c("# of non-mortgage accounts delinquent 12 months","# of non-mortgage accounts delinquent 6 months",
            "# of mortgage delinquent 6 months","Total amount of money past due")

plotg_pcd <- list()
for (i in 1:4) {
  set.seed(123456)
  sample <- sample_frac(tbl = dat,size = 0.1) %>% mutate(y = get(g_pcdv[i]))
  plotg_pcd[[i]] <- dat %>% mutate(y = get(g_pcdv[i])) %>% 
    ggplot(aes(x = Default_ind, y = y)) +
    geom_jitter(data = sample,height = 0) +
    geom_violin(alpha = 0.6) +
    theme_bw() +
    labs(x = y, y = g_pcdl[i]) +
    theme(axis.title.x.bottom = element_blank())
}

ggsave(filename = here::here(paste0(oppath,"g_pcd.png")), plot = wrap_plots(plotg_pcd,ncol = 4) + plot_layout(guides='collect') &  theme(legend.position='bottom'), 
       width = 8, height = 4, dpi= dpiset)

# ============================
# Credit Inquiries
g_civ <- c("inq_12_month_num","card_inq_24_month_num")
g_cil <- c("# of credit inquiries in last 12 months","# of credit inquiries in last 24 months")

plotg_ci <- list()
for (i in 1:2) {
  set.seed(123456)
  sample <- sample_frac(tbl = dat,size = 0.1) %>% mutate(y = get(g_civ[i]))
  plotg_ci[[i]] <- dat %>% mutate(y = get(g_civ[i])) %>% 
    ggplot(aes(x = Default_ind, y = y)) +
    geom_jitter(data = sample,height = 0) +
    geom_violin(alpha = 0.6) +
    theme_bw() +
    labs(x = y, y = g_cil[i]) +
    theme(axis.title.x.bottom = element_blank())
}

ggsave(filename = here::here(paste0(oppath,"g_civ.png")), plot = wrap_plots(plotg_ci,ncol = 2) + plot_layout(guides='collect') &  theme(legend.position='bottom'), 
       width = 4, height = 4, dpi= dpiset)

# ============================
# Number of Accounts, Balances, and Utilization
g_nabuv <- c("card_open_36_month_num","auto_open_.36_month_num","uti_card","uti_50plus_pct",'uti_max_credit_line',"uti_card_50plus_pct")
g_nabul <-  c("# of credit cards opened 36 months","# of loans opened 36 months","Utilization of credit card accounts",
              "% of credit products over 50% utilization","Utilization with highest credit limit","% of credit cards over 50% utilization")

plotg_nabu <- list()
for (i in 1:6) {
  set.seed(123456)
  sample <- sample_frac(tbl = dat,size = 0.1) %>% mutate(y = get(g_nabuv[i]))
  plotg_nabu[[i]] <- dat %>% mutate(y = get(g_nabuv[i])) %>% 
    ggplot(aes(x = Default_ind, y = y)) +
    geom_jitter(data = sample,height = 0) +
    geom_violin(alpha = 0.6) +
    theme_bw() +
    labs(x = y, y = g_nabul[i]) +
    theme(axis.title.x.bottom = element_blank())
}

ggsave(filename = here::here(paste0(oppath,"g_nabuv.png")), plot = wrap_plots(plotg_nabu,ncol = 6) + plot_layout(guides='collect') &  theme(legend.position='bottom'), 
       width = 12, height = 4, dpi= dpiset)

# ============================
# Additional information
g_aiv <- c("ind_acc_XYZ","rep_income",'States')
g_ail <- c("Account with other banks (Y=1, N=0)","Annual income (self-reported)",'Residence state')

plotg_ai <- list()
for (i in 2) {
  set.seed(123456)
  sample <- sample_frac(tbl = dat,size = 0.1) %>% mutate(y = get(g_aiv[i]))
  plotg_ai[[i]] <- dat %>% mutate(y = get(g_aiv[i])) %>% 
    ggplot(aes(x = Default_ind, y = y)) +
    geom_jitter(data = sample,height = 0) +
    geom_violin(alpha = 0.6) +
    theme_bw() +
    labs(x = y, y = g_ail[i]) +
    theme(axis.title.x.bottom = element_blank())
}

ggsave(filename = here::here(paste0(oppath,"g_aiv.png")), plot = wrap_plots(plotg_ai[[2]],ncol = 1) + plot_layout(guides='collect') &  theme(legend.position='bottom'), 
       width = 2, height = 4, dpi= dpiset)
# =============================================

