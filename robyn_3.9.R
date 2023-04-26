install.packages("Robyn")
####Robyn 3.9 
library('doParallel')

#library('reticulate')
#use_virtualenv("r-reticulate", required = TRUE) -- Do not need in v3.9
library('Robyn')
library('tidyverse')
library('ggplot2')
library('data.table')
library('ggridges')
library('lubridate')
#### Step 1: Load data
#add weekly data file

data <- read.csv("first_pays_3_10_23.csv", header = T)

#Load calibration data
dt_calibration <- read.csv("calibration_test.csv", header = T)
dt_calibration$liftStartDate <-as.Date(dt_calibration$liftStartDate)
dt_calibration$liftEndDate <-as.Date(dt_calibration$liftEndDate)
dt_calibration[is.na(dt_calibration)] = 0
calibration_input <- dt_calibration
head(dt_calibration)

data$date <- as.Date(data$date)
#data$date <- as.Date(data$ad_date, format="%Y-%m-%d")
#data <- arrange(data, date)
data <- data[data$date < '2023-03-01',]
data$brand_s = ksmooth(time(data$brand_s), data$brand_s, bandwidth=4)$y

#data$acquisition_other_s = ksmooth(time(data$acquisition_other_s), data$acquisition_other_s, bandwidth=31)$y
data[is.na(data)] = 0

dt_prophet_holidays <- read.csv("dt_prophet_holidays.csv", header = T)
dt_prophet_holidays$ds <-as.Date(dt_prophet_holidays$ds)
dt_prophet_holidays$ds <-as.IDate(dt_prophet_holidays$ds)


## Set robyn_object. It must have extension .RDS. The object name can be different than Robyn:

robyn_object <- "~/"
#### Step 2 Specify input variables
InputCollect <- robyn_inputs(
  dt_input = data
  ,dt_holidays = dt_prophet_holidays
  ,date_var = "date" # date format must be "2020-01-01"
  ,dep_var = "conversions" # there should be only one dependent variable
  ,dep_var_type = "conversion" # "revenue" or "conversion"
  ,prophet_vars = c("trend", "season", "holiday") # "trend","season", "weekday" & "holiday"
  ,prophet_country = "US"
  ,paid_media_spends = c( "fb_s",
                          "google_brand_s","google_non_brand_s", 
                          "amazon_s", "cj_s", "roku_s", "apple_s","youtube_s",
                          "acquisition_other_s","brand_s"
  )
  ,paid_media_vars =  c( "fb_s", 
                         "google_brand_s","google_non_brand_s", 
                         "amazon_s", "cj_s", "roku_s", "apple_s","youtube_s",
                         "acquisition_other_s","brand_s"
  )
  ,context_vars <- c() 
  ,factor_vars <- c() 
  ,organic_vars <-  c()
  ,calibration_input = calibration_input
  ,window_start = "2019-03-04"
  ,window_end = "2023-02-28"
  ,adstock = "weibull_cdf" # geometric, weibull_cdf or weibull_pdf
)

##Step 4 Define hyperparameters
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

multipler <- 1
hyperparameters <- list(
  
  fb_s_alphas = c(0.5, 3) 
  ,fb_s_gammas = c(0.3, 1) 
  ,fb_s_shapes = c(0.0001, 2) ^multipler
  ,fb_s_scales = c(0, 0.1) ^multipler
  
  
  ,google_brand_s_alphas = c(0.5, 3) 
  ,google_brand_s_gammas = c(0.3, 1) 
  ,google_brand_s_shapes = c(0.0001, 2) ^multipler
  ,google_brand_s_scales = c(0, 0.1) ^multipler
  
  
  ,google_non_brand_s_alphas = c(0.5, 3) 
  ,google_non_brand_s_gammas = c(0.3, 1) 
  ,google_non_brand_s_shapes = c(0.0001, 2) ^multipler
  ,google_non_brand_s_scales = c(0, 0.1) ^multipler
  
  ,amazon_s_alphas = c(0.5, 3) 
  ,amazon_s_gammas = c(0.3, 1) 
  ,amazon_s_shapes = c(0.0001, 2) ^multipler
  ,amazon_s_scales = c(0, 0.1) ^multipler
  
  ,cj_s_alphas = c(0.5, 3) 
  ,cj_s_gammas = c(0.3, 1) 
  ,cj_s_shapes = c(0.0001, 2) ^multipler
  ,cj_s_scales = c(0, 0.1) ^multipler
  
  ,roku_s_alphas = c(0.5, 3) 
  ,roku_s_gammas = c(0.3, 1) 
  ,roku_s_shapes = c(0.0001, 2) ^multipler
  ,roku_s_scales = c(0, 0.1) ^multipler
  
  ,apple_s_alphas = c(0.5, 3) 
  ,apple_s_gammas = c(0.3, 1) 
  ,apple_s_shapes = c(0.0001, 2) ^multipler
  ,apple_s_scales = c(0, 0.1) ^multipler
  
  ,youtube_s_alphas = c(0.5, 3) 
  ,youtube_s_gammas = c(0.3, 1) 
  ,youtube_s_shapes = c(0.0001, 2) ^multipler
  ,youtube_s_scales = c(0, 0.1) ^multipler
  
  ,acquisition_other_s_alphas = c(0.5, 3) 
  ,acquisition_other_s_gammas = c(0.3, 1)
  ,acquisition_other_s_shapes = c(0.0001, 2) ^multipler
  ,acquisition_other_s_scales = c(0, 0.1) ^multipler
  
  
  ,brand_s_alphas = c(0.5, 3) 
  ,brand_s_gammas = c(0.3, 1) 
  ,brand_s_shapes = c(0.0001, 2) ^multipler
  ,brand_s_scales = c(0, 0.1) ^multipler

  
)


#### Step 4: add hyperparameters into robyn_inputs()

#InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)

InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input, hyperparameters = hyperparameters)

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}


OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = 8, # NULL defaults to max available - 1
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  calibration_constraint = 0.1,
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = FALSE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)

print(OutputModels)


OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = FALSE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = FALSE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object, # path for plots export
  export = TRUE # this will create files locally
)
print(OutputCollect)


top_models <- unique(OutputCollect$allPareto$xDecompAgg$solID[OutputCollect$allPareto$xDecompAgg$decomp.rssd < 0.3 & OutputCollect$allPareto$xDecompAgg$rsq_train > 0.70])
pareto_models <- unique(names(OutputCollect$allPareto$plotDataCollect))
top_models <- top_models[top_models %in% pareto_models]

for (model in top_models) { 
  try(robyn_onepagers(InputCollect, OutputCollect, select_model = model))
}
top_model_summaries <- OutputCollect$allPareto$xDecompAgg[OutputCollect$allPareto$xDecompAgg$solID %in% top_models,]

#plot_vars <- colnames(models)[3:length(colnames(models))]
plot_vars <- colnames(top_model_summaries)[3:length(colnames(top_model_summaries))]

#library(tidyverse)
#library(ggplot2)
#models$cpa_total[is.infinite(models$cpa_total)]<- NA
#models$cpa_total[models$cpa_total>999]<- NA
top_model_summaries$cpa_total[is.infinite(top_model_summaries$cpa_total)]<- NA
top_model_summaries$cpa_total[top_model_summaries$cpa_total>999]<- NA
models <- top_model_summaries
plot_folder <- OutputCollect$plot_folder
for (est in plot_vars) {
  print(est)
  
  
  p <- models %>%
    mutate(channel = fct_reorder(rn, spend_share)) %>%
    ggplot(aes(x=.data[[est]], y=channel, colour=rsq_train))+
    geom_point()
  #+
  # geom_point(aes(x=next_unit_response, y=channel), shape=3, size=2, colour=1)
  png(filename=paste(plot_folder, "dotplot_", est, ".png", sep=""), width=800, height=1200, res=120)
  print(p)
  dev.off()
  
  p<-models %>%
    mutate(channel = fct_reorder(rn, spend_share)) %>%
    ggplot( aes(y=channel, x=.data[[est]])) + #,  fill=channel
    geom_density_ridges(scale = 0.9,quantile_lines = TRUE, quantiles = 2) +
    #geom_point(aes(x=.data[[est]], y=channel, fill=channel), shape=21, size=3, colour="black")+
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab(est) +
    ylab("Channel")+
    theme_ridges()
  png(filename=paste(plot_folder, "ridgeplots_",est, ".png", sep=""), width=800, height=1200, res=120)
  print(p)
  dev.off()
  
  if (est == "effect_share") {
    p <- models %>%
      mutate(channel = fct_reorder(rn, spend_share)) %>%
      ggplot(aes(x=.data[[est]], y=channel, colour=rsq_train))+
      geom_point()+
      geom_point(aes(x=spend_share, y=channel), shape=3, size=2, colour=1)
    
    png(filename=paste(plot_folder, "dotplot_", est, ".png", sep=""), width=800, height=1200, res=120)
    print(p)
    dev.off()
    
    p<-models %>%
      mutate(channel = fct_reorder(rn, spend_share)) %>%
      ggplot( aes(y=channel, x=.data[[est]])) + #,  fill=channel
      geom_density_ridges(scale = 0.9,quantile_lines = TRUE, quantiles = 2) +
      geom_point(aes(x=spend_share, y=channel, fill=channel), shape=21, size=2, colour="black")+
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      xlab(est) +
      ylab("Channel")+
      theme_ridges()
    png(filename=paste(plot_folder, "ridgeplots_",est, ".png", sep=""), width=800, height=1200, res=120)
    print(p)
    dev.off()
  }
  
}

channel_effects <- top_model_summaries %>%
  filter(!is.na(effect_share)) %>%	
  group_by(rn) %>% 
  summarise(median_effect=median(effect_share))

top_model_summaries_eval <- merge(x=top_model_summaries, y=channel_effects, by="rn", all.x=TRUE)

model_perf <- top_model_summaries_eval %>% group_by(solID) %>% filter(!is.na(effect_share)) %>% summarise(err=sum(abs(median_effect-effect_share)))
model_perf <- model_perf[order(model_perf$err),]

print(channel_effects)
print(model_perf[order(model_perf$err),])

select_model <- model_perf$solID[1]
robyn_object <- paste(plot_folder, select_model, ".RDS", sep="")

ExportedModel <- robyn_save(
 # robyn_object = robyn_object # model object location and name
   select_model = select_model # selected model ID
  , InputCollect = InputCollect # all model input
  , OutputCollect = OutputCollect # all model output
)

Robyn <- robyn_refresh(
  robyn_object = robyn_object
  , dt_input = data
  , dt_holidays = dt_prophet_holidays
  , refresh_steps = 4
  , refresh_iters = 5000 # 1k is estimation. Use refresh_mode = "manual" to try out.
  , refresh_trials = 3
  , clusters = FALSE
  , plot_pareto = FALSE
)

last_refresh_num <- sum(grepl('listRefresh', names(Robyn))) + 1 # Pick any refresh.
#Here's the final refresh using the model recommended by least combined normalized nrmse and decomp.rssd
ExportedRefreshModel <- robyn_save(
  robyn_object = robyn_object
  , select_model = Robyn[[last_refresh_num]]$OutputCollect$selectID
  , InputCollect = Robyn[[last_refresh_num]]$InputCollect
  , OutputCollect = Robyn[[last_refresh_num]]$OutputCollect
)
######################### Refresh#############################################

robyn_object <- "~/Robyn_202302150413_init/"

json_file <- "~/Robyn_202302150413_init/RobynModel-2_232_56.json "

json_data <- robyn_read(json_file)

robyn_write(InputCollect, dir = "~/Robyn_202302150413_init/")

            
# Re-create InputCollect
InputCollect <- robyn_inputs(
dt_input = data,
dt_holidays = dt_prophet_holidays,
json_file = json_file)
            
# Re-create OutputCollect
            OutputCollect <- robyn_run(
              InputCollect = InputCollect,
              select_model = select_model,
              json_file = json_file,
              export =TRUE)
            
            
            InputCollectX <- robyn_inputs(
              dt_input = data,
              dt_holidays = dt_prophet_holidays,
              json_file = json_file)
            
            # Re-create OutputCollect
            OutputCollectX <- robyn_run(
              InputCollect = InputCollectX,
              json_file = json_file,
              export = FALSE)
            
