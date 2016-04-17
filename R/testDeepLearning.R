require(h2o)

h2o.init(nthreads = 3, max_mem_size="6g")
set.seed(432123)
testDate <- dmy("05/4/2016")

price_tmp <- price %>% 
  mutate(DoW5 = NA) %>% 
  select(-c(Year, Month, Weekend, DoY, Date, DoW2, DoW3, DoW4, DoW5)) %>% 
  select(ts, Price, wind_speed_mean, wind_speed_sd, temperature_sd,
         DoW, Holiday, Hour, Price_l168) %>% 
  do(data.frame(., model.matrix(~DoW, .)))


df_train <- price_tmp %>% 
  filter(ts < testDate) %>% 
  select(-c(ts, DoW)) %>% 
  as.h2o()


#Supervised
price_dl <- h2o.deeplearning(x=2:dim(df_train)[2], 
                             y=1, 
                             training_frame = df_train,
                             hidden = c(400, 200, 100, 10))

#Unsupervised
price_dl_us <- h2o.deeplearning(x=2:dim(df_train)[2],
                             training_frame = df_train,
                             hidden = c(400, 200, 100, 10),
                             activation = "Tanh",
                             autoencoder = TRUE)

train_features2 <- h2o.deepfeatures(price_dl_us, df_train, layer=3)
plotdata2 <- bind_cols(as.data.frame(train_features2),
                       price_tmp[1:dim(train_features2)[1],]) %>% 
  mutate(PriceRange = cut(Price, breaks=5))
qplot(DF.L3.C1, DF.L3.C2, 
      data = plotdata2, 
      color = DoW, 
      main = "Neural network")

# predictions <- h2o.predict(price_dl, df_train)
# predictions <- as.data.frame(predictions)
# 
# predictions <- price %>% 
#   filter(ts < testDate) %>% 
#   do(data.frame(., predictions))
# 
# 
# predictions %>% 
#   select(ts, Price, predict) %>% 
#   gather(var, val, -ts) %>% 
#   ggplot(aes(x=ts, y=val, colour=var)) + 
#   geom_line()



#### Test set =================================================================
df_test <- price_tmp %>% 
  filter(ts >= testDate) %>% 
  select(-c(ts, DoW)) %>% 
  as.h2o()

predictions <- h2o.predict(price_dl, df_test)
predictions <- as.data.frame(predictions)

predictions <- price_tmp %>% 
  filter(ts >= testDate) %>% 
  do(data.frame(., predictions))


predictions %>% 
  select(ts, Price, predict) %>% 
  gather(var, val, -ts) %>% 
  ggplot(aes(x=ts, y=val, colour=var)) + 
  geom_line()


# Calc MAE
(performance <- predictions %>% 
  mutate(ae = abs(Price - predict),
         Date = floor_date(ts, "day")) %>% 
  group_by(Date) %>% 
  summarise(mae = mean(ae)))

mean(performance$mae)
