#packages required
library(tidymodels)
library(readr)
library(broom.mixed)
library(dotwhisker)
library(viridis)

#load raw data
urchins<-readr::read_csv("https://tidymodels.org/start/models/urchins.csv")

#prepare data for analysis by renaming columns
urchins <-
  urchins %>%
  setNames(c("food_regime","initial_volume","width"))%>%
  mutate(food_regime=factor(food_regime,levels=c("Initial","Low","High")))

ggplot(urchins,
       aes(x=initial_volume,
           y=width,
       group=food_regime,
       col=food_regime)
       )+
  geom_point()+
  scale_color_viridis_d(option="turbo",end=0.6)+
  facet_wrap(~food_regime)
  
#linear regression using the parsnip package: model object
lm_mod<-
linear_reg() %>%
  set_engine("lm")

#fit the data to the model object

lm_fit<-
  lm_mod%>%
  fit(width~initial_volume*food_regime,data=urchins)

tidy(lm_fit)

#Grid to use the predict function to predict from the model
new_points<-expand.grid(initial_volume=20,
                        food_regime=c("Initial","Low","High"))

#mean predicted values
mean_pred<-predict(lm_fit,new_data=new_points)

conf_int_pred<-predict(lm_fit,
                       new_data=new_points,
                       type="conf_int")
#put in a tibble
plot_data<-
  new_points %>%
  bind_cols(mean_pred)%>%
  bind_cols(conf_int_pred)

# #fitting a Bayesian model
# 
# prior_dist<-rstanarm::student_t(df=1)
# set.seed(123)
# 
# #parsnip model
# 
# bayes_mod<-
#   linear_reg() %>%
#   set_engine("stan",
#              prior_intercept=prior_dist,
#              prior=prior_dist)
# 
# #train model


####Recipes#######
library(nycflights13)
library(skimr)

set.seed(123)  

flight_data<-
  flights%>%
  mutate(arr_delay=ifelse(arr_delay>=30,"late","on_time"),
         arr_delay=factor(arr_delay),
         date=as.Date(time_hour)
  ) %>%
  inner_join(weather,by=c("origin","time_hour")) %>%
  select(dep_time,flight,origin,dest,air_time,distance,carrier,
         date,arr_delay,time_hour) %>%
  na.omit()%>%
  mutate_if(is.character,as.factor)

glimpse(flight_data)

#how many carriers and destinations?

flight_data %>%
  skimr::skim(dest,carrier)

#split in training and testing datasets

set.seed(555)
#put 75% of the data in the training set
data_split <- initial_split(flight_data,prop=3/4)         

#create data frames for testing and training
train_data<-training(data_split)
test_data<-testing(data_split)

#create recipe and roles

flights_rec <-
  recipe(arr_delay~.,data=train_data)

#set flight and time_hour as IDs

flights_rec <-
  recipe(arr_delay~.,data=train_data) %>%
  update_role(flight,time_hour,new_role="ID")

#get day of the week, month and if date is a holiday in the dataframe

flights_rec <-
  recipe(arr_delay~.,data=train_data) %>%
  update_role(flight,time_hour,new_role="ID") %>%
  step_date(date,features=c("dow","month")) %>%
  step_holiday(date, holidays=timeDate::listHolidays("US"))%>%
  step_rm(date)

#change factors to dummy variables, and remove variables with single values
flights_rec <-
  recipe(arr_delay~.,data=train_data) %>%
  update_role(flight,time_hour,new_role="ID") %>%
  step_date(date,features=c("dow","month")) %>%
  step_holiday(date, holidays=timeDate::listHolidays("US"))%>%
  step_rm(date)%>%
  step_dummy(all_nominal(),-all_outcomes()) %>% 
  step_zv(all_predictors())

lr_mod<-
  logistic_reg()%>%
  set_engine("glm")

#use model workflow

flights_wflow<-
  workflow()%>%
  add_model(lr_mod)%>%
  add_recipe(flights_rec)

flights_wflow

flights_fit<-
  flights_wflow %>%
  fit(data=train_data)

#get model coefficients

flights_fit %>%
  pull_workflow_fit() %>%
  tidy()
