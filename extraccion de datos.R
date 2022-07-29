library(tidyverse)
library(RMariaDB) 
library(readr)
library(lubridate) 
library(paws) # For AWS integration
library(future) # For Async calls
library(promises) # For Async calls
library(pool)


# sets Env Vars to real values // later will come from Rporject
Sys.setenv("db_user"="admin")
Sys.setenv("db_host"="database-1.cbuap8cmrqz9.us-east-1.rds.amazonaws.com")
Sys.setenv("db_password"="mydbpassword")
Sys.setenv("db_port" = 3306)



##variables 
# stationid <- data.frame( 
#   as.numeric (c("ID_est1", "ID_est2", "ID_est3"))
# )

stationid <- c("03A0B149", "03B0B175", "03B0B177")



#en vivo se trabaja con la siguiente linea que obtiene la fecha real
#fecha <-now() %>% as.POSIXct(format="%Y/%m/%d %H:%M:%S")
fecha = as.POSIXct("2020-10-04 09:00:00")  



function_gigancesto <- function(stationid, fecha){
  df_datos <- stationid %>% map_df(leerdb)
  llu24 <- f24ho(df_lluvia, fecha)
  temp24 <- f24ho(df_temperatura, fecha)
  hum24 <- f24ho(df_humedad, fecha)
  
  ### lluvia acumulada temperatura min y max, humedad promedio de  las ultimas 24 horas
  acumllu24h <- llu24 %>% apply( MARGIN = 2, FUN = sum) %>% as.data.frame()
  temp_min24 <- temp24 %>% apply( MARGIN = 2, FUN = min) %>% as.data.frame()
  temp_max24 <- temp24 %>% apply( MARGIN = 2, FUN = max) %>% as.data.frame()
  hum_prom24 <- hum24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()
  
  tem_med_min24 <- temp_min24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()
  tem_med_max24 <- temp_max24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()
  hum_med_24 <-hum_prom24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()
  return(to DF)
}

## funciones
# Leer las tablas de la base de datos

leerdb <- function(station, now_date, dys){
  browser()
  dys <- 30
  now_date <- as.POSIXct("2022-01-01 09:00:00")  
  min_date <- now_date - days(dys)
  
  dfcrudo3 <- pool_db %>% dplyr::tbl("element_data_X")%>%collect()
  dfcrudo4 <- pool_db %>% dplyr::tbl("element_data_X")%>%
    dplyr::filter(element_id %in% station) %>%
    dplyr::filter(Timestamp > now_date)%>%collect()
lubridate::floor_date()

  dfcrudo2 <- dfcrudo3%>%dplyr::filter(station_id %in% station)%>%collect()
  dfcrudo <-  dfcrudo2 %>% select_if(names(.) %in% c('station_id','Timestamp', 'C21A', 'C11A', 'C31A' )) %>%
              filter(between(Timestamp, (as.POSIXct(fecha) - 2592000), as.POSIXct(fecha)))
  return(dfcrudo)
}

# Obtener los datos ordenados de forma descendente por fechas y horas de todas las estaciones 
datos<-function(df, variable){
  df_pre <- df %>% select('station_id','Timestamp', variable)
  df_pre <- df_pre %>% spread(key = station_id, value = variable)
  df_pre <- df_pre[order(df_pre$Timestamp, decreasing = TRUE), ]
  return(df_pre)
}

# filtro de datos por 24 horas
f24ho <- function(df, fecha){
  nc<- df %>% ncol() %>% as.numeric()
  df_24h <- df %>% filter(between(Timestamp, (as.POSIXct(fecha)-864000), as.POSIXct(fecha))) %>% 
            select( 2:nc)
  return(df_24h)
}

pool_db <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "odapes_dev",
  host = Sys.getenv("db_host"),
  user = Sys.getenv("db_user"),
  password = Sys.getenv("db_password"),
  port= as.integer(Sys.getenv("db_port"))
)

# ## parametros de conexion a la db
# pool_db <- pool::dbPool(
#   drv = RMariaDB::MariaDB(),
#   dbname = "db_name",
#   host = "host",
#   user = "user",
#   password = "password",
#   port= as.integer("puerto")
# )

# obtener los datos para trabajar

df_datos <- stationid %>% map_df(leerdb)

poolClose(pool_db)

###filtrado de datos por variable 

df_lluvia<-datos(df_datos, 'C21A')
df_temperatura<-datos(df_datos, 'C11A')
df_humedad<-datos(df_datos, 'C31A')

### lecturas de las 24 horas
llu24 <- f24ho(df_lluvia, fecha)
temp24 <- f24ho(df_temperatura, fecha)
hum24 <- f24ho(df_humedad, fecha)

### lluvia acumulada temperatura min y max, humedad promedio de  las ultimas 24 horas
acumllu24h <- llu24 %>% apply( MARGIN = 2, FUN = sum) %>% as.data.frame()
temp_min24 <- temp24 %>% apply( MARGIN = 2, FUN = min) %>% as.data.frame()
temp_max24 <- temp24 %>% apply( MARGIN = 2, FUN = max) %>% as.data.frame()
hum_prom24 <- hum24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()

tem_med_min24 <- temp_min24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()
tem_med_max24 <- temp_max24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()
hum_med_24 <-hum_prom24 %>% apply( MARGIN = 2, FUN = mean) %>% as.data.frame()

#dataframe para graficar a 24 horas

df_salida <- data_frame('Unidad' = acumllu24h %>% row.names() %>% as.data.frame(),
                        'Lluvia' = acumllu24h,
                        'Temperatura °C minima 24h'= temp_min24,
                        'Temperatura °C máxima 24h'= temp_max24,
                        'Humedad % media 24h'= hum_prom24
                         )


df_val_med24 <- data.frame('Temp media min'= tem_med_min24,
                           'Temp media max'= tem_med_max24,
                           'Humedad promedio'= hum_med_24
                            )


