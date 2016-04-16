require(h2o)

h2o.init(nthreads = 3, max_mem_size="6g")

testDate <- dmy("10/4/2016")

df_train <- price %>% 
  filter(ts < testDate) %>% 
  mutate(DoW5 = NA) %>% 
  select(-c(ts, Year, Month, Weekend, DoY, Date, DoW2, DoW3, DoW4, DoW5))
df_test <- price %>% 
  filter(ts >= testDate)

df_train <- as.h2o(df_train)

price_dl <- h2o.deeplearning(x=2:dim(price)[2], y=1, training_frame = df_train)

predictions <- h2o.predict(price_dl, df_train)
predictions <- as.data.frame(predictions)

predictions <- price %>% 
  filter(ts < testDate) %>% 
  do(data.frame(., predictions))


predictions %>% 
  select(ts, Price, predict) %>% 
  gather(var, val, -ts) %>% 
  ggplot(aes(x=ts, y=val, colour=var)) + 
  geom_line()



#### Test set =================================================================
df_test <- price %>% 
  filter(ts >= testDate) %>% 
  mutate(DoW5 = NA) %>% 
  select(-c(ts, Year, Month, Weekend, DoY, Date, DoW2, DoW3, DoW4, DoW5))
df_test <- as.h2o(df_test)

predictions <- h2o.predict(price_dl, df_test)
predictions <- as.data.frame(predictions)

predictions <- price %>% 
  filter(ts >= testDate) %>% 
  do(data.frame(., predictions))


predictions %>% 
  select(ts, Price, predict) %>% 
  gather(var, val, -ts) %>% 
  ggplot(aes(x=ts, y=val, colour=var)) + 
  geom_line()


# Calc MAE
(performance <- predictions %>% 
  mutate(ae = abs(Price - predict)) %>% 
  group_by(Date) %>% 
  summarise(mae = mean(ae)))

mean(performance$mae)
