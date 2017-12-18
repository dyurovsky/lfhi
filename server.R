
library(shiny)
library(rwunderground)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})

checkDataAvailability("KMKC", "2017-12-12", "id")

getCurrentTemperature("KMDW")

stations <- ncdc_stations()

#chicago
ncdc_stations(stationid='NEXRAD:KLOT')

#kcmo
NEXRAD:KEAX

c_data <- ncdc(datasetid = "GHCND", stationid = "NEXRAD:KLOT", datatypeid = "PRCP", 
     startdate = "2016-12-17", enddate = "2017-12-16")

id <- "NEXRAD:KLOT"

ncdc_stations(stationid = id)
ncdc_datacats(stationid = id)
ncdc_datasets(stationid = id)

ncdc_datatypes(datasetid = "NEXRAD3", stationid = "NEXRAD:KEAX")

ncdc_stations(stationid='NEXRAD:KLOT')

ncdc_datasets(stationid='NEXRAD:KLOT')

ncdc_datacats(stationid = id)

ncdc(datasetid="NEXRAD3", stationid = id, datacategoryid = "PRCP",
              startdate = "2016-10-01", enddate = "2015-10-02")

ncdc_datacats(datasetid="NEXRAD3", stationid = id, datacategoryid = "PRCP",
              startdate = "2016-10-01", enddate = "2015-10-02")

ncdc(datasetid = "NEXRAD2", stationid = id, 
     startdate = "2016-10-01", enddate = "2015-10-02")

c_data <- ncdc(datasetid='GHCND', locationid = "CITY:US170006",
     datatypeid = "PRCP",
     startdate = '2017-12-01',
     enddate = '2017-12-16',
     limit = 1000)


years <- 2000:2016
starts <- c(paste0(years, "-01-01"), "2017-01-01")
ends <- c(paste0(years, "-12-31"), "2017-11-30")

kc_id <- "GHCND:USW00013988"
chi_id <- "GHCND:USW00014819"
stpaul_id <- "GHCND:USW00014927"

ids <- c(kc_id, chi_id, stpaul_id)

places <- c("Kansas City", "St Paul", "Chicago")


place_data <- function(place, id, start_date, end_date) {
  
  precip <- ncdc(datasetid='GHCND', stationid = id,
                    datatypeid = "PRCP",
                    startdate = start_date,
                    enddate = end_date,
                    limit = 1000)$data
  
  
  tmp <- ncdc(datasetid='GHCND', stationid = id,
                 datatypeid = c("TMAX", "TMIN"),
                 startdate = start_date,
                 enddate = end_date,
                 limit = 1000)$data
  
  bind_rows(precip, tmp) %>%
    mutate(place = place) %>%
    as_tibble()
  
}

pull_data <- function(start, end) {
  
  all_places <- map2(places, ids, 
                     function(x, y) place_data(x, y, start, end)) %>%
    bind_rows() %>%
    mutate(year = word(start, sep = "-"))
}
  
all_data <- map2(starts, ends, pull_data)  

all_data %>%
  bind_rows() %>%
  select(-fl_q, -fl_so, -fl_t) %>%
  write_feather("data/daily_weather_slim.feather")

  
  stpaul_precip <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014927",
                        datatypeid = "PRCP",
                        startdate = '2016-12-17',
                        enddate = '2017-12-16',
                        limit = 1000)
  
  stpaul_tmp <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014927",
                     datatypeid = c("TMAX", "TMIN"),
                     startdate = '2016-12-17',
                     enddate = '2017-12-16',
                     limit = 1000)
  
  
  chi_precip <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014819",
                     datatypeid = "PRCP",
                     startdate = '2016-12-17',
                     enddate = '2017-12-16',
                     limit = 1000)
  
  chi_tmp <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014819",
                  datatypeid = c("TMAX", "TMIN"),
                  startdate = '2016-12-17',
                  enddate = '2017-12-16',
                  limit = 1000)
}


dates <- c("2016-12-17", "2016-12-17")




kc_precip <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00013988",
                datatypeid = "PRCP",
                startdate = '2016-12-17',
                enddate = '2017-12-16',
                limit = 1000)

kc_tmp <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00013988",
               datatypeid = c("TMAX", "TMIN"),
               startdate = '2016-12-17',
               enddate = '2017-12-16',
               limit = 1000)


stpaul_precip <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014927",
                  datatypeid = "PRCP",
                  startdate = '2016-12-17',
                  enddate = '2017-12-16',
                  limit = 1000)

stpaul_tmp <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014927",
               datatypeid = c("TMAX", "TMIN"),
               startdate = '2016-12-17',
               enddate = '2017-12-16',
               limit = 1000)


chi_precip <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014819",
                      datatypeid = "PRCP",
                      startdate = '2016-12-17',
                      enddate = '2017-12-16',
                      limit = 1000)

chi_tmp <- ncdc(datasetid='GHCND', stationid = "GHCND:USW00014819",
                   datatypeid = c("TMAX", "TMIN"),
                   startdate = '2016-12-17',
                   enddate = '2017-12-16',
                   limit = 1000)

all_tmps <- bind_rows(mutate(as_tibble(kc_tmp$data), place = "KCMO"),
                      mutate(as_tibble(stpaul_tmp$data), place = "St Paul"),
                      mutate(as_tibble(chi_tmp$data), place = "Chicago")) %>%
  mutate(date = as_date(date),
         value = celsius.to.fahrenheit(value/10),
         date_num = as.numeric(fct_inorder(factor(date))))



all_prcp <- bind_rows(mutate(as_tibble(kc_precip$data), place = "KCMO"),
                      mutate(as_tibble(stpaul_precip$data), place = "St Paul"),
                      mutate(as_tibble(chi_precip$data), place = "Chicago")) %>%
  mutate(date = as_date(date),
         value = value/25.4/10,
         date_num = as.numeric(fct_inorder(factor(date))))



ggplot(all_tmps, aes(x = date_num, y = value, color = datatype)) + 
  facet_wrap(~ place) + 
  geom_point()



ggplot(all_prcp, aes(x = date_num, y = value, color = datatype)) + 
  facet_wrap(~ place) + 
  geom_point() +
  geom_smooth() 


write


%>%
  ggplot(aes(x = date_num, y = value, color = datatype)) + 
  geom_line()
          



c_stats <- ncdc_stations(datasetid = "GHCND", startdate = '2016-12-17',
              enddate = '2017-12-16', locationid = "CITY:US170006",
              limit = 1000)


c_stats$data %>% as_tibble() %>%
  filter(datacoverage > .9) %>%
  arrange(desc(datacoverage)) %>%
  View()


USW00014927

kc_tmp$data %>%
  as_tibble() %>%
  mutate(date = as_date(date)) %>%
  mutate(date_num = 1:n(),
         value = celsius.to.fahrenheit(value/10)) %>%
  ggplot(aes(x = date_num, y = value, color = datatype)) + 
  geom_line()


stpaul_tmp$data %>%
  as_tibble() %>%
  mutate(date = as_date(date)) %>%
  mutate(date_num = 1:n(),
         value = celsius.to.fahrenheit(value/10)) %>%
  ggplot(aes(x = date_num, y = value, color = datatype)) + 
  geom_line()



ncdc_datasets(datasetid = 'GHCND', stationid = "USW00013988")

ncdc_datacats(datasetid = "GHCND", stationid = "GHCND:USW00013988")

stats <- ncdc_stations(locationid = "CITY:US290008", limit = 100)$data


CITY:US290008

kc_data$data %>%
  as_tibble() %>%
  mutate(date = as_date(date)) %>%
  mutate(date_num = 1:n()) %>%
  ggplot(aes(x = date_num, y = value)) + 
  geom_smooth()

