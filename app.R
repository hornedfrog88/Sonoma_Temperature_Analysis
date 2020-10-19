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

# create a NEW data frame containing statistics(average highs/lows 
# and record highs/lows)for month/day combinations

# calculate and store the historical average high for month/day of the imputed data
avg_high_by_mnth_day <- weather_data_impute %>% 
    select(TMAX,MONTH,DAY) %>% 
    group_by(MONTH,DAY) %>% 
    summarize(round(mean(TMAX),0))
# calculate and store the historical average low for month/day of the imputed data
avg_low_by_mnth_day <- weather_data_impute %>% 
  select(TMIN,MONTH,DAY) %>% 
  group_by(MONTH,DAY) %>% 
  summarize(round(mean(TMIN),0))
# calculate and store the historical record high for month/day of the imputed data
rec_high_by_mnth_day <- weather_data_impute %>% 
  select(TMAX,MONTH,DAY) %>% 
  group_by(MONTH,DAY) %>% 
  summarize(max(TMAX))
# calculate and store the historical record low for month/day of the imputed data
rec_low_by_mnth_day <- weather_data_impute %>% 
  select(TMIN,MONTH,DAY) %>% 
  group_by(MONTH,DAY) %>% 
  summarize(min(TMIN))

# create a average high for the date row and add it to weather_data_all-rename column
weather_data_all <- weather_data_impute %>% inner_join(avg_high_by_mnth_day)
names(weather_data_all)[7] <- "AVG_HIGH_FOR_DAY"

# create a average low for the date row and add it to weather_data_all-rename column
weather_data_all <- weather_data_all %>% inner_join(avg_low_by_mnth_day)
names(weather_data_all)[8] <- "AVG_LOW_FOR_DAY"

# create a record high for the date row and add it to weather_data_all-rename column
weather_data_all <- weather_data_all %>% inner_join(rec_high_by_mnth_day)
names(weather_data_all)[9] <- "REC_HIGH_FOR_DAY"

# create a average low for the date row and add it to weather_data_all-rename column
weather_data_all <- weather_data_all %>% inner_join(rec_low_by_mnth_day)
names(weather_data_all)[10] <- "REC_LOW_FOR_DAY"

# create a column in weather_data_all to identify months by text value
# rather than by numeric value
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
# create a NEW data frame containing statistics(median high/lows)for  
# month/year combinations

# create a data frame summarized by month/year
weather_data_mnth_year <- weather_data_impute %>% 
  select(MONTH,YEAR) %>% 
  group_by(MONTH,YEAR)
# combine the weather_data_mnth_year data frame rows by distinct MONTH & YEAR
weather_data_mnth_year <- distinct(weather_data_mnth_year,MONTH,YEAR)

# calculate and store the median high for month/year of the imputed data
med_high_by_mnth_year <- weather_data_impute %>% 
  select(TMAX,MONTH,YEAR) %>% 
  group_by(MONTH,YEAR) %>% 
  summarize(round(median(TMAX),0))

# calculate and store the median low for month/year of the imputed data
med_low_by_mnth_year <- weather_data_impute %>% 
  select(TMIN,MONTH,YEAR) %>% 
  group_by(MONTH,YEAR) %>% 
  summarize(round(median(TMIN),0))

# create a median high for the month/year and add it to weather_data_mnth_year-rename column
weather_data_mnth_year <- weather_data_mnth_year %>% inner_join(med_high_by_mnth_year)
names(weather_data_mnth_year)[3] <- "MED_HIGH_FOR_MONTH"

# create a median low for the month/year and add it to weather_data_mnth_year-rename column
weather_data_mnth_year <- weather_data_mnth_year %>% inner_join(med_low_by_mnth_year)
names(weather_data_mnth_year)[4] <- "MED_LOW_FOR_MONTH"

# create a column in weather_data_mnth_year to identify months by text value
# rather than by numeric value
weather_data_mnth_year <- weather_data_mnth_year %>% 
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
# Define UI (fluid page) for Shiny application that generates dotPlots(geom_point)
ui <- fluidPage(

    # Application title
    titlePanel(p("Sonoma Valley Historical Temperature Visualizations(data from 1/1/1953 through 12/31/2019)",
                 windowTitle = "Sonoma Temp Visualizations",
                 style={'color:maroon;font-size:20px;'})),

    # Sidebar with select inputs and slider inputs
       selectInput(inputId = "month", 
                label = "Choose Month for All Plots",
                choices = unique(weather_data_all$MONTHTEXT), 
                selected = 1
                 ),
    
       selectInput(inputId = "year", 
                label = "Choose Year for the Daily Plots",
                choices = unique(weather_data_all$YEAR), 
                selected = 1953
                 ),
        sliderInput("year_range",
                    "Choose Year Range for the Median Plots",
                min = min(weather_data_mnth_year$YEAR),
                max = max(weather_data_mnth_year$YEAR),
                value = c(min(weather_data_mnth_year$YEAR),max(weather_data_mnth_year$YEAR)),
                sep = "",
                ticks = FALSE
                 ),
    
     # Show Output
        mainPanel(
          h1(textOutput("Text"),style={'color:maroon;font-size: 20px;
                                 font-style: bold-italic;'}),
          plotOutput("dotPlot1"),
          plotOutput("dotPlot2"),
          plotOutput("dotPlot3"),
          plotOutput("dotPlot4")
         )
   )

# Define server logic required to draw a dot plot
server <- function(input, output) {
    
    weather_data_subset <- reactive({
        weather_data_all[weather_data_all$YEAR == input$year &
                         weather_data_all$MONTHTEXT == input$month,]
    })
    
    weather_data_mnthyr_subset <- reactive({
         weather_data_mnth_year[weather_data_mnth_year$MONTHTEXT == input$month &
                                weather_data_mnth_year$YEAR >= input$year_range[1]&
                                weather_data_mnth_year$YEAR <= input$year_range[2],]
    })
     
    # define plotting theme
    mygridtheme <- theme(aspect.ratio = .65, plot.title = element_text(family = "NimbusMon", color = "blue", face = "bold", size = (15)), 
    # no legend title:   legend.title = element_text(color = "steelblue",  face = "bold.italic", family = "Helvetica", size = (12)), 
                         legend.text = element_text(face = "italic", color = "steelblue4", family = "NimbusMon"),
                         axis.title = element_text(family = "NimbusMon", size = (12), colour = "steelblue4"),
                         axis.text = element_text(family = "NimbusMon", color = "cornflowerblue", size = (12)),
                         axis.text.x = element_text(angle = 20))
     # Plot 1
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
               labs(title = paste("Daily High Temperatures for",input$month,input$year), x= "Day of the Month", y = "Temp (F)")
        }
      })
 
     #Plot 2
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
            labs(title = paste("Daily Low Temperatures for",input$month,input$year), x= "Day of the Month", y = "Temp (F)")
        
      }
     })

     #Plot 3
     output$dotPlot3 <- renderPlot({
        ggplot(weather_data_mnthyr_subset(),
         aes(x=YEAR,y=MED_HIGH_FOR_MONTH))+
         geom_point(color="red") +
         geom_smooth(method = "lm", color = "maroon",se = FALSE)+
         labs(title = paste("Median High Temperatures for",input$month), x= "Year", y = "Temp (F)")
     })

     #Plot 4
     output$dotPlot4 <- renderPlot({
       ggplot(weather_data_mnthyr_subset(),
              aes(x=YEAR,y=MED_LOW_FOR_MONTH))+
         geom_point(color="blue") +
         geom_smooth(method = "lm", color = "maroon",se = FALSE)+
         labs(title = paste("Median Low Temperatures for",input$month), x= "Year", y = "Temp (F)")
     })
    
      output$Text <- renderText({
       checkfordata <- is.na(as.factor(weather_data_all %>% filter(MONTHTEXT == input$month, YEAR == input$year)))
       if (checkfordata == TRUE) { 
        paste('No Daily Data Recorded for:',input$month,input$year)
       }
      })
  }

# Run the application 
shinyApp(ui = ui, server = server)
