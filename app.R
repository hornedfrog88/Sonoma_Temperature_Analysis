# Shiny App for Sonoma Valley Temperature Analysis
#
#
library(shiny)
library(rlang)
library(tidyverse)
library(readr)
library(lubridate)

# read the data from a csv file provided by the NOAA
weather_data <- read_csv("weather_data.csv", 
                         col_types = cols(DATE = col_date(format = "%Y-%m-%d"), 
                                          TMAX = col_integer(), TMIN = col_integer(), 
                                          TOBS = col_integer()))

# filter rows prior to 1953 and after 2019 and filter rows outside of Sonoma 
weather_data_impute <- as.data.frame(weather_data)
weather_data_impute <- weather_data_impute %>% filter(DATE >= '1953-01-01'& 
                                                      DATE < '2020-01-01'& 
                                                      STATION == 'USC00048351') %>% 
                                                      select(DATE,TMAX,TMIN)

# replace any na values with the mean of the column(there may not be actual NAs in the data)
for (i in which(sapply(weather_data_impute,is.numeric))) {
    weather_data_impute[is.na(weather_data_impute[,i]),i] <- round(mean(weather_data_impute[,i],na.rm = TRUE),0)
}

# save the TMAX, TMIN values as integer
weather_data_impute$TMAX <- as.integer(weather_data_impute$TMAX)
weather_data_impute$TMIN <- as.integer(weather_data_impute$TMIN)

# separate (and save) the DATE column into 3 columns YEAR, MONTH and DAY
weather_data_impute <- weather_data_impute %>% 
    mutate(YEAR = year(DATE), MONTH = month(DATE), DAY = day(DATE))

#calculate and store average high for month/day of the imputed data
avg_high_by_mnth_day <- weather_data_impute %>% 
    select(TMAX,MONTH,DAY) %>% 
    group_by(MONTH,DAY) %>% 
    summarize(round(mean(TMAX),0))
#calculate and store average low for month/day of the imputed data
avg_low_by_mnth_day <- weather_data_impute %>% 
  select(TMIN,MONTH,DAY) %>% 
  group_by(MONTH,DAY) %>% 
  summarize(round(mean(TMIN),0))
#calculate and store the record high for month/day of the imputed data
rec_high_by_mnth_day <- weather_data_impute %>% 
  select(TMAX,MONTH,DAY) %>% 
  group_by(MONTH,DAY) %>% 
  summarize(max(TMAX))
#calculate and store the record low for month/day of the imputed data
rec_low_by_mnth_day <- weather_data_impute %>% 
  select(TMIN,MONTH,DAY) %>% 
  group_by(MONTH,DAY) %>% 
  summarize(min(TMIN))

# create a average high for the date field
weather_data_all <- weather_data_impute %>% inner_join(avg_high_by_mnth_day)
names(weather_data_all)[7] <- "AVG_HIGH_FOR_DAY"

# create a average low for the date field
weather_data_all <- weather_data_all %>% inner_join(avg_low_by_mnth_day)
names(weather_data_all)[8] <- "AVG_LOW_FOR_DAY"

# create a record high for the date field
weather_data_all <- weather_data_all %>% inner_join(rec_high_by_mnth_day)
names(weather_data_all)[9] <- "REC_HIGH_FOR_DAY"

# create a average low for the date field
weather_data_all <- weather_data_all %>% inner_join(rec_low_by_mnth_day)
names(weather_data_all)[10] <- "REC_LOW_FOR_DAY"

# create a column to identify months by text rather than number
weather_data_all <- weather_data_all %>% 
    mutate(
        MONTHTEXT = case_when(
            MONTH == 1 ~ "January",
            MONTH == 2 ~ "February",
            MONTH == 3 ~ "March",
            MONTH == 4 ~ "April",
            MONTH == 5 ~ "May",
            MONTH == 6 ~ "June",
            MONTH == 7 ~ "July",
            MONTH == 8 ~ "August",
            MONTH == 9 ~ "September",
            MONTH == 10 ~ "October",
            MONTH == 11 ~ "November",
            MONTH == 12 ~ "December"
        )
    )

# Define UI (fluid page) for application that creates the dotPlot(geom_point)
ui <- fluidPage(

    # Application title
    titlePanel(p("Sonoma Valley Historical High and Low Temperature Visualizations(data from 1/1/1953 through 12/31/2019)",
                 style={'color:maroon;font-size:20px;'})),

    # Sidebar with a select inputs
       selectInput(inputId = "month", 
                label = "Month",
                choices = unique(weather_data_all$MONTHTEXT), 
                selected = 1
                 ),
    
       selectInput(inputId = "year", 
                label = "Year",
                choices = unique(weather_data_all$YEAR), 
                selected = 1953
                 ),
     # Show Output
        mainPanel(
          h1(textOutput("Text"),style={'color:maroon;font-size: 20px;
                                 font-style: bold-italic;'}),
          plotOutput("dotPlot1"),
          plotOutput("dotPlot2")
         )
   )

# Define server logic required to draw a dot plot
server <- function(input, output) {
    
    weather_data_subset <- reactive({
        weather_data_all[weather_data_all$YEAR == input$year &
                            weather_data_all$MONTHTEXT == input$month,]
    })   
      mygridtheme <- theme(aspect.ratio = .65, plot.title = element_text(family = "NimbusMon", color = "blue", face = "bold", size = (15)), 
    # no legend title:   legend.title = element_text(color = "steelblue",  face = "bold.italic", family = "Helvetica", size = (12)), 
                         legend.text = element_text(face = "italic", color = "steelblue4", family = "NimbusMon"),
                         axis.title = element_text(family = "NimbusMon", size = (12), colour = "steelblue4"),
                         axis.text = element_text(family = "NimbusMon", color = "cornflowerblue", size = (12)),
                         axis.text.x = element_text(angle = 20))

     output$dotPlot1 <- renderPlot({
       checkfordata <- is.na(as.factor(weather_data_all %>% filter(MONTHTEXT == input$month, YEAR == input$year)))
       if (checkfordata == FALSE) { 
          ggplot(weather_data_subset(),
               aes(x=DAY,y=TMAX))+
               geom_jitter(color= "blue") + 
               scale_x_discrete(limits = c(min(weather_data_all$DAY):max(weather_data_all$DAY)))+
               geom_point(aes(x=DAY,y=AVG_HIGH_FOR_DAY,color = "Historical Average High Temp for that Date"))+
               geom_point(aes(x=DAY,y=REC_HIGH_FOR_DAY,color = "Record High Temp for that Date"))+
               mygridtheme +
               theme(legend.title=element_blank())+ 
               labs(title = paste("Daily High Temperatures for",input$month,input$year), x= "Day of the Month", y = "Daily High Temp (F)")
        }
      })
     output$dotPlot2 <- renderPlot({
       checkfordata <- is.na(as.factor(weather_data_all %>% filter(MONTHTEXT == input$month, YEAR == input$year)))
       if (checkfordata == FALSE) { 
            ggplot(weather_data_subset(),
            aes(x=DAY,y=TMIN))+
            geom_jitter(color= "blue") + 
            scale_x_discrete(limits = c(min(weather_data_all$DAY):max(weather_data_all$DAY)))+
            geom_point(aes(x=DAY,y=AVG_LOW_FOR_DAY,color = "Historical Average Low Temp for that Date"))+
            geom_point(aes(x=DAY,y=REC_LOW_FOR_DAY,color = "Record Low Temp for that Date"))+
            mygridtheme + 
            theme(legend.title=element_blank())+ 
            labs(title = paste("Daily Low Temperatures for",input$month,input$year), x= "Day of the Month", y = "Daily Low Temp (F)")
      }
     })
         
     output$Text <- renderText({
       checkfordata <- is.na(as.factor(weather_data_all %>% filter(MONTHTEXT == input$month, YEAR == input$year)))
       if (checkfordata == TRUE) { 
        paste('No Data Recorded for:',input$month,input$year)
       }
      })
  }

# Run the application 
shinyApp(ui = ui, server = server)