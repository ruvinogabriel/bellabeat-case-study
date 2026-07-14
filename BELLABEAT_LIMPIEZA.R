# ==================================
# BELLABEAT CASE STUDY
# AUTOR: GABRIEL RUVIÑO
# ==================================
library(tidyverse)
library(lubridate)
getwd()
list.files()
activity <- read.csv("data/dailyActivity_merged.csv")
nrow(activity)
glimpse(activity)
# Ahora revisamos la calidad de los datos.
# Valores faltantes
colSums(is.na(activity))
# No hay valores NA

# Duplicados
sum(duplicated(activity))
# No hay valores duplicados

# Usuarios Unicos
n_distinct(activity$Id)
# Son 35 usuarios únicos.

head(activity$ActivityDate)
activity$ActivityDate <- mdy(activity$ActivityDate)
glimpse(activity)

summary(activity)

library(lubridate)

activity <- activity %>%
  mutate(day = wday(ActivityDate, label = TRUE))
summary(activity)

activity$day_of_week <- weekdays(activity$ActivityDate)
head(activity[, c("ActivityDate", "day_of_week")])
activity %>%
    group_by(day_of_week) %>%
  summarise(promedio_pasos = mean(TotalSteps))

activity %>%
  group_by(day_of_week) %>%
  summarise(promedio_pasos = mean(TotalSteps)) %>%
  ggplot(aes(x = day_of_week, y = promedio_pasos)) +
  geom_col()

activity %>%
  group_by(day_of_week) %>%
  summarise(promedio_pasos = mean(TotalSteps)) %>%
  arrange(desc(promedio_pasos))

##Día 2

library(tidyverse)
library(lubridate)
getwd()
list.files()
setwd("C:/Users/ruvin/Desktop/TODO/DATA ANALYTICS/Datos portafolio/CASO 2/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/Bellabeat Portafolio")
activity <- read_csv("data/dailyActivity_merged.csv")
cor(activity$TotalSteps, activity$Calories)
ggplot(activity, aes(x = TotalSteps, y = Calories)) +
  geom_point() + 
  labs(
    title = "Relación entre pasos y calorías",
    x = "Total de pasos",
    y = "calorías quemadas"
  )

activity <- activity %>%
  mutate(
    activity_level = case_when(
      TotalSteps < 5000 ~ "Baja actividad",
      TotalSteps < 10000 ~ "Actividad moderada",
      TRUE ~ "Alta actividad"
    )
  )
activity %>%
  count(activity_level)

ggplot(activity, aes(x = activity_level)) +
  geom_bar() +
  labs(
    title = "Distribución de niveles de actividad",
    x = "Nivel de actividad",
    y = "Cantidad de registros"
  )
summary(activity$SedentaryMinutes)
mean(activity$SedentaryMinutes)
median(activity$SedentaryMinutes)
ggplot(activity, aes(x = SedentaryMinutes)) +
  geom_histogram(binwidth = 30) +
  labs(
    title = "Distribución de minutos sedentarios",
    x = "Minutos sedentarios",
    y= "Cantidad de registros"
  )

library(lubridate)

activity$ActivityDate <- mdy(activity$ActivityDate)

activity$day_of_week <- weekdays(activity$ActivityDate)
head(activity[, c("ActivityDate", "day_of_week")])

# Agregamos Sleep

sleep <- read_csv("data/minuteSleep_merged.csv")
glimpse(sleep)
colSums(is.na(sleep))
# ningun dato vacío
sum(duplicated(sleep))
# 525 datos por borrar
n_distinct(sleep$Id)
sleep %>%
  filter(duplicated(sleep))
sleep$date <- mdy_hms(sleep$date)
glimpse(sleep)
unique(sleep$value)
sleep %>%
  count(value)
sleep <- sleep %>%
  mutate(sleep_date = as.Date(date))
head(sleep[, c("date", "sleep_date")])
sleep %>%
  group_by(Id, sleep_date) %>%
  summarise(
    minutos_registrados = n(),
    .groups = "drop"
  )
sleep %>%
  filter(duplicated(sleep)) %>%
  head(20)
sleep %>%
  filter(
    Id == 4319703577,
    date >= ymd_hms("2016-04-05 22:50:00"),
    date <= ymd_hms("2016-04-05 23:09:00")
  )
sleep %>%
  count(Id, date, value, logId) %>%
  filter(n > 1)
sleep <- sleep %>%
  distinct()
sum(duplicated(sleep))
sleep <- sleep %>%
  mutate(sleep_date = as.Date(date))
sleep_daily <- sleep %>%
  group_by(Id, sleep_date) %>%
  summarise(
    minutos_sueno = n(),
    .groups = "drop"
  )
glimpse(sleep_daily)

head(sleep_daily)

summary(sleep_daily$minutos_sueno)

mean(sleep_daily$minutos_sueno)

median(sleep_daily$minutos_sueno)

ggplot(sleep_daily, aes(x = minutos_sueno)) +
  geom_histogram(binwidth = 30) +
  labs(
    title = "Distribución de minutos de sueño",
    x = "Minutos de sueño",
    y = "Cantidad de registros"
  )
glimpse(sleep_daily)

head(sleep_daily)

summary(sleep_daily$minutos_sueno)

mean(sleep_daily$minutos_sueno)

median(sleep_daily$minutos_sueno)

sleep_daily <- sleep_daily %>%
  rename(ActivityDate = sleep_date)
glimpse(sleep_daily)

activity_sleep <- left_join(
  activity,
  sleep_daily,
  by = c("Id", "ActivityDate")
)

glimpse(activity_sleep)
head(activity_sleep)
sum(is.na(activity_sleep$minutos_sueno))

cor(activity_sleep$TotalSteps,
    activity_sleep$minutos_sueno,
    use = "complete.obs")

ggplot(activity_sleep,
       aes(x = minutos_sueno,
           y = TotalSteps)) +
  geom_point() +
  labs(
    title = "Relación entre sueño y pasos diarios",
    x = "Minutos de sueño",
    y = "Pasos diarios"
  )