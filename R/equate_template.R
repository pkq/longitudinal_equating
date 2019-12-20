
library(rio)
library(pROC)
library(broom)
library(janitor)
library(tidyverse)
source("R/mv_linear_equate_x_to_y.R")

measure <- "orf"
ttl <- import("~/Code/CTL/aip/validation/2017/output/anon_scores.xlsx", which = 3)

## Subset grade
grd <- 2
ttl_g <- ttl[ttl$grade == grd, ]

benchmarkId <- c(1, 2, 3)
anchorId <- 4
seasonId <- sort(unique(ttl_g$season))

ttl_g <- ttl_g[ttl_g$probe_id %in% c(benchmarkId, anchorId), ]

## subset forms
table(ttl_g$probe_id, ttl_g$season)

yEmpMeanVec <- c(mean(ttl_g$total[ttl_g$probe_id == 4 & ttl_g$season == 1]),
                 mean(ttl_g$total[ttl_g$probe_id == 4 & ttl_g$season == 2]),
                 mean(ttl_g$total[ttl_g$probe_id == 4 & ttl_g$season == 3]))

yEmpVarVec <- c(var(ttl_g$total[ttl_g$probe_id == 4 & ttl_g$season == 1]),
                var(ttl_g$total[ttl_g$probe_id == 4 & ttl_g$season == 2]),
                var(ttl_g$total[ttl_g$probe_id == 4 & ttl_g$season == 3]))

xEmpMeanVec <- c(mean(ttl_g$total[ttl_g$probe_id == 1]),
                 mean(ttl_g$total[ttl_g$probe_id == 2]),
                 mean(ttl_g$total[ttl_g$probe_id == 3]))

xEmpVarVec <- c(var(ttl_g$total[ttl_g$probe_id == 1]),
                var(ttl_g$total[ttl_g$probe_id == 2]),
                var(ttl_g$total[ttl_g$probe_id == 3]))

ttl_g$season_probe <- paste(ttl_g$season, ttl_g$probe_id, sep = "-")

## Reshape to wide
ttl_g_w <- reshape(ttl_g[, c("anon_id", "season_probe", "total")],
                   timevar = "season_probe",
                   idvar = c("anon_id"),
                   direction = "wide", sep = "-")

anchorCols <- paste("total", seasonId, anchorId, sep = "-")
benchmarkCols <- paste("total", seasonId, benchmarkId, sep = "-")
ttl_g_w <- ttl_g_w[, c("anon_id", anchorCols , benchmarkCols)]

## Form pattern
ttl_g_w$s1a <- ifelse(is.na(ttl_g_w[, anchorCols[1]]), "0", "A")
ttl_g_w$s2a <- ifelse(is.na(ttl_g_w[, anchorCols[2]]), "0", "A")
ttl_g_w$s3a <- ifelse(is.na(ttl_g_w[, anchorCols[3]]), "0", "A")
ttl_g_w$s1b <- ifelse(is.na(ttl_g_w[, benchmarkCols[1]]), "0", "B")
ttl_g_w$s2b <- ifelse(is.na(ttl_g_w[, benchmarkCols[2]]), "0", "B")
ttl_g_w$s3b <- ifelse(is.na(ttl_g_w[, benchmarkCols[3]]), "0", "B")

ttl_g_w$pattern <- paste0(ttl_g_w$s1a, ttl_g_w$s1b, "-",
                          ttl_g_w$s2a, ttl_g_w$s2b, "-",
                          ttl_g_w$s3a, ttl_g_w$s3b)

formPattern <- table(Pattern = ttl_g_w$pattern)

sub_ttl_g_w <- (ttl_g_w[ttl_g_w$pattern == "AB-AB-AB", ])
sub1_ttl_g_w <- sub_ttl_g_w[, c("total-1-1", "total-2-2", "total-3-3")]
sub2_ttl_g_w <- sub_ttl_g_w[, c("total-1-4", "total-2-4", "total-3-4")]

# compute equated scores
compare_equate_out2 <- list()

for (jj in 1:nrow(sub1_ttl_g_w))
{
  xVec_j <-  t(unname(as.matrix(sub1_ttl_g_w[jj, ])))

  mvS1Out <- mv_linear_equate_x_to_y(xVec    = xVec_j,
                                     yMeanVec = yEmpMeanVec,
                                     yCovMat  = diag(yEmpVarVec),
                                     xMeanVec = xEmpMeanVec,
                                     xCovMat  = diag(xEmpVarVec))

  compare_equate_out2[[jj]] <- cbind("raw"   = mvS1Out[, 1],
                                     "s-eq1" = mvS1Out[, 2])
  rownames(compare_equate_out2[[jj]]) <- paste("Season", 1:3)
}

# convert results to data frame
dfs <- lapply(compare_equate_out2, data.frame, stringsAsFactors = FALSE)
dfs <- lapply(dfs, rownames_to_column)

equate_flat <- bind_rows(dfs) %>%
  rename(season = rowname) %>%
  mutate(season = str_remove(season, "Season "),
         season = as.integer(season)) %>%
  arrange(season, raw)

# plot
ggplot(data = equate_flat,
       aes(x = raw,
           y = s.eq1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~season) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# lookups
# define function
equate_lm <- function(df){
  lm(s.eq1 ~ raw, data = df)
}

# nested
equate_nest <- equate_flat %>%
  # nest by grade, season, measure
  nest(-season) %>%
  # map linear models
  mutate(model = map(data, equate_lm)) %>%
  # extract estimates from models
  mutate(coeffs = map(model, tidy))

lookup <- equate_nest %>%
  select(season, coeffs) %>%
  unnest() %>%
  select(season, term, estimate) %>%
  spread(term, estimate) %>%
  clean_names()

## season 1
equate_lm_1 <- lm(s.eq1 ~ raw, data = equate_flat[equate_flat$season == 1, ])
summary(equate_lm_1)

lookup_2_1 <- tibble(raw = 0:200, equated = raw * 1.1428570 + 9.0074003) %>%
  mutate(equated = round(equated),
         grade = 2,
         season = 1,
         probe_id = 1,
         measure = "orf")

## season 2
equate_lm_2 <- lm(s.eq1 ~ raw, data = equate_flat[equate_flat$season == 2, ])
summary(equate_lm_2)

lookup_2_2 <- tibble(raw = 0:200, equated = raw * 1.1428570 + 9.0074003) %>%
  mutate(equated = round(equated),
         grade = 2,
         season = 1,
         probe_id = 1,
         measure = "orf")


# import aip data
needs(fs, rio, here, tidyverse)
onedrive <- "~/OneDrive/University Of Oregon/Passages - Assessment Innovation/"

## aip scores
aip_orf <- import(file.path(onedrive, "Validation study 2017-18/Data/Output/AIP scores/aip_orf_scored.csv"))

aip_orf_2_1 <- aip_orf %>%
  mutate(measure = "orf") %>%
  filter(grade == 2 & season == 1) %>%
  rename(raw = total) %>%
  left_join(lookup_2_1) %>%
  mutate(equated = as.integer(equated),
         equated = case_when(probe_id == 4 ~ raw,
                             TRUE ~ equated)) %>%
  filter(!is.na(equated))

aip_orf_2_1 %>%
  count(probe_id)

## iowa
iowa_tr <- import(file.path(onedrive,
                            "Validation study 2017-18/Data/Output/Iowa scores/iowa_20180706.csv"))

roc_orf_2_1 <- aip_orf_2_1 %>%
  left_join(iowa_tr)

roc_orf_2_1 %>%
  count(iowa_grade)

# functions
## generate table of roc results
roc_table <- function(data, out, pred) {
  x <- data %>%
    # drop cases with no outcome data
    filter(!is.na(!!rlang::ensym(out))) %>%
    # group by values of the predictor
    group_by(!!rlang::ensym(pred)) %>%
    # compute sensitivity & specificity
    summarise(positive = sum(!!rlang::ensym(out)),
              negative = n() - sum(!!rlang::ensym(out))) %>%
    mutate(sensitivity = cumsum(positive) / sum(positive),
           specificity = 1 - (cumsum(negative) / sum(negative)))
}

# define models
roc_iowa_tr_lt20 <- function(df){
  roc(iowa_lt20 ~ equated, data = df, ci=TRUE)
}

roc_iowa_tr_lt40 <- function(df) {
  roc(iowa_lt40 ~ equated, data = df, ci=TRUE)
}

# generate models
roc_models <- roc_orf_2_1 %>%
  # nest by grade, season, measure
  nest(-grade, -season, -measure) %>%
  # map each model (roc & threshold calcs) to the data
  mutate(roc_iowa_tr_lt20 = map(data, roc_iowa_tr_lt20),
         roc_iowa_tr_lt40 = map(data, roc_iowa_tr_lt40),
         tbl_iowa_tr_lt20 = map(data, roc_table, iowa_lt20, equated),
         tbl_iowa_tr_lt40 = map(data, roc_table, iowa_lt40, equated)) %>%
  # convert from wide to long
  gather(label, model, roc_iowa_tr_lt20, tbl_iowa_tr_lt20,
         roc_iowa_tr_lt40, tbl_iowa_tr_lt40) %>%
  separate(label, c("data_type", "assessment", "sub", "prediction")) %>%
  unite(predicting, assessment, sub) %>%
  arrange(grade, season, measure) %>%
  spread(data_type, model)

# generate summary table
roc_tables <- roc_models %>%
  # extract auc, scores, threshold counts,
  # sensitivity, & specificity from models
  mutate(auc           = map_dbl(roc, "auc"),
         equated_score = map(tbl, "equated"),
         below_thresh  = map(tbl, "positive"),
         above_thresh  = map(tbl, "negative"),
         sensitivity   = map(tbl, "sensitivity"),
         specificity   = map(tbl, "specificity")) %>%
  # drop list variables & unnest dataframe
  select(-data, -roc, -tbl) %>%
  unnest() %>%
  # compute summary statistics
  group_by(grade, season, measure) %>%
  mutate(sum_sens_spec = sensitivity + specificity,
         dif_sens_spec = abs(sensitivity - specificity),
         sum_minus_dif = sum_sens_spec - dif_sens_spec,
         # determine which models meet minimum NCII requirements
         ncii_sens = case_when(sensitivity >= .7 ~ 1, TRUE ~ 0),
         ncii_spec = case_when(specificity >= .8 ~ 1, TRUE ~ 0),
         ncii_both = case_when(ncii_sens == 1 & ncii_spec == 1 ~ 1,
                               TRUE ~ 0)) %>%
  ungroup()
