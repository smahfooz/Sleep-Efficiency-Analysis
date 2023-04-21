if(!require(shiny)){install.packages(c("shiny", "shinydashboard"))}
library(tidyverse)
library(shiny)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(ggcorrplot)
library(DT)
library(rsconnect)
library(reactable)
library(lubridate)
library(stringi)
library(ggthemes)
library(shinyWidgets)

load("sleep.RData")



orange_pal <- function(x) rgb(colorRamp(c("#FFFDC8", "#FFC145", "#FE8276"))(x), maxColorValue = 255)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Sleep Analysis",
                                    tags$li(class = "dropdown",
                                            tags$a(href="https//:www.linkedin.com/in/shafaqmahfooz/", 
                                                   target="_blank", 
                                                   icon("linkedin", "fa-1x", lib = "font-awesome")  # fa-3x mean 3 times bigger
                                            ))),                 # Title of the dashboard
                    dashboardSidebar(
                      sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("info", lib = "font-awesome")),
                      
                      menuItem("General Analysis", tabName = "analysis", icon = icon("magnifying-glass-chart", lib = "font-awesome")),
                      
                      menuItem("Sleep Pattern", tabName = "pattern", icon = icon("moon", lib = "font-awesome")),
                      
                      menuItem("Sleep Stages", tabName = "stage", icon = icon("stairs", lib = "font-awesome")),
                      
                      menuItem("Lifestyle Factors", tabName = "factor", icon = icon("dna", lib = "font-awesome")),
                      
                      menuItem("Inputs", tabName = "inputvalues", icon = icon("keyboard", lib = "font-awesome"),                              # Element 1: a simple title
                      sliderInput("Age", h6("Age"),                        # Element 1: year selection
                                  min = 10, max = 70,                      
                                  value = c(10, 70), step = 10 
                      ), 
                      sliderInput("alcohol_consumption", h6("Alcohol Consumption (oz)"),                        # Element 1: year selection
                                  min = 0, max = 5,                      
                                  value = c(0, 5), step = 1
                      ),
                      sliderInput("exercise_freq", h6("Exercise Frequency (weekly)"),                        # Element 1: year selection
                                  min = 0, max = 5,                      
                                  value = c(0, 5), step = 1
                      ),
                      selectInput("gender", h6("Gender"),                    # Element 2: country selection
                                  choices = sleep$Gender %>% unique(),
                                  selected = c("Female", "Male"),
                                  multiple = TRUE
                      

                    
                 )
                      )
                    )
                 ),
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName ="about",
                                tags$div(style="text-align:center;",
                                         tags$strong(style='font-size:24px;',"Analysis of Sleep Patterns and its impact on Sleep Efficiency")
                                ),
                                fluidRow(box(h5(textOutput("text")), width = 12)),
                                tags$br(),
                                fluidRow(h4(HTML("<b> A few facts about the data </b>")),align="center", width =12),
                                tags$br(),
                                fluidRow(
                                  column(7,
                                         reactableOutput("table_1")
                                  ),
                                  column(5,
                                         valueBoxOutput("box1", width = 12),
                                         valueBoxOutput("box2", width = 12),
                                         valueBoxOutput("box3", width = 12)
                                  )
                                  
                                )
                        ),
                        
                        
                      
                          tabItem(tabName ="analysis",
                            tabsetPanel(
                             tabPanel("General Overview",
                                 fluidRow(
                                   column(6,
                                         
                                          plotOutput("plot1", height = 300),
                                          tags$br(),
                                          plotOutput("plot2", height = 300)
                                          
                                          ),
                                   column(6,
                                          box(h6(HTML("<ul> <em>
                                          <li>There is a high co-relation between Sleep efficiency and Deep sleep stage followed by the impact of Exercise on sleep efficiency</li>
                                          <li>Alcohol consumptions leads to an interrupted sleep cycle.</li>
                                          <li>The REM sleep stage (more on it under Sleep stages) is slightly impacted by caffeine consumption</li>
                                           </em> </ul> ")), width = 14),
                                          plotOutput("plot", height = 480))
                                          
                                   
                                 )
                                 
                        ),
                        tabPanel("Behavioral",
                                 fluidRow(
                                   column(6,
                                          valueBoxOutput("bbox1", width = 12)),
                                   column(6,
                                          valueBoxOutput("bbox2", width = 12))
                                 ),
                                 tags$div(style="text-align:center;",
                                 tags$strong("Consumption Habits")
                                 ),
                                 tags$br(),
                                 fluidRow(
                                   column(4,
                                          plotOutput("bplot1", height=300)
                                   ),
                                   column(4,
                                          plotOutput("bplot2", height = 300)
                                   ),
                                   column(4,
                                          plotOutput("bplot3"), height = 300)
                                   
                                 ),
                                 tags$em("*Interactive Plots")
                        )
                                 
                            )        
                        
                      
                      
                    ),
                    
                    tabItem(tabName ="pattern", 
                            fluidRow(
                              column(10,
                              box(h6(HTML("<p>Bedtime can have a significant impact on sleep efficiency and sleep interruption. 
                                                 Going to bed at consistent times each night can help regulate the body's internal clock and improve sleep quality.</p>
                                                 <p>From our observations, we can conclude the following:</p
                                                 <ul>
                                                 <li>Going to bed too early or too late can impact the sleep efficiency</li>
                                                 <li>The same can be said for sleep interruptions especially for people in the range of 50-70</li>
                                                 </ul>
                                                 <br>
                                                 <p>Therefore, it is recommended to aim for a regular bedtime that allows for 7-9 hours of sleep per night for adults.</p>"
                                                 )
                                                 
                                                 ), width =12)),
                              column(2,
                                     img(src = "Bedtime.png",align = "right", height = "141", width = 180)
                              )
                              ),
                            tags$div(style="text-align:center;",
                                     tags$strong("Effect of Bedtime on Sleep")
                            ),
      
                            fluidRow(
                              column(6,
                              plotOutput("plot4", height = 450)
                              ),
                              column(6,
                              plotOutput("plot5", height = 450)
                              
                            )
                    )
                    
                    
              ),
              
              tabItem(tabName ="stage",
                      tabsetPanel(
                        tabPanel("Overview",
                                 fluidRow(
                                   column(11,
                                   plotOutput("plot6", height = 300)
                                   )),
                                 tags$br(),
                                 tags$div(style="text-align:center;",
                                          tags$strong("Overview of Sleep Stages in Participants")),
                                 fluidRow(
                                   column(11,
                                   DT::dataTableOutput("table_2")
                                   )
                                   ),
                                 tags$em("*Interactive Table")
                                  
                                   
                                   
                                 ),
                                 
                        
                        tabPanel("REM Sleep",
                                 fluidRow(h4(HTML("<b> Rapid Eye Movement (REM) stage </b>")), width =12
                                          ),
                                 
                                 fluidRow(box(h5(textOutput("text1")), width = 12)
                                          ),
                                 fluidRow(
                                 plotOutput("plot7", height = 500)
                                 )
                        
                        
                      ),
                      tabPanel("Deep Sleep",
                               fluidRow(h4(HTML("<b> Deep Sleep stage </b>")), width =12
                               ),
                               
                               fluidRow(box(h5(textOutput("text2")), width = 12)
                               ),
                               fluidRow(
                                 plotOutput("plot8")
                               ),
                               tags$em("*Interactive Plot")
                      )
                      )
              ),
              tabItem(tabName ="factor",
                      tabsetPanel(
                        tabPanel("Negative Factors",
                                 fluidRow(box(h5(textOutput("text3")), width = 12)),
                                 fluidRow(
                                   column(8,
                                          plotOutput("plot9", height = 300),
                                          tags$br(),
                                          plotOutput("plot10", height = 300)
                                   ),
                                   column(4,
                                          valueBoxOutput("box4", width = 12),
                                          valueBoxOutput("box5", width = 12),
                                          tags$em("*Interactive"),
                                          box(h6(HTML("<p>From the analysis, we can conclude:</p>
                                                        <ul>
                                                        <li>Smoking can reduce sleep efficiency, especially in women</li>
                                                        <li>High Alcohol consumption can cause an irratic sleep cycle.</li>
                                                        <li>Caffeine can disrupt sleep by increasing alertness and delaying the onset of sleep.</li>
                                                        <ul>")
                                          ), width = 12
                                          ),
                                          box(h6(HTML("
                                                      <p>Exercise can help counteract these effects by strengthening the respiratory and cardiovascular systems. 
                                                      It can also help to reduce stress and anxiety, which are two common triggers for caffeine consumption.</p>
                                                      <p>Regular exercise is a beneficial lifestyle habit that can have a positive impact on sleep patterns and 
                                                      help alleviate the negative impacts of other lifestyle factors such as alcohol consumption, smoking, and caffeine.</p>"
                                          )
                                          
                                          ), width =12)
                                          
                                          )
                                 )
                        ),
                        tabPanel("Positive Factors",
                                 fluidRow(h4(HTML("<b> Impact of Exercise on Sleep </b>")), width =12
                                 ),
                                 fluidRow(box(h5(textOutput("text4")), width = 12)),
                                 fluidRow(plotlyOutput("plot11", height = 500)
                                 )
                        )
                      )
              )
                        
                                   
              
)
))


server <- function(input, output){ 
  data <- reactive({                 # Creates the dynamic data
    sleep |>                  # Filter years, countries, population & source
      filter(Age >= input$Age[1], 
             Age <= input$Age[2],
             Alcohol_consumption >= input$alcohol_consumption[1],
             Alcohol_consumption >= input$alcohol_consumption[2],
             Exercise_frequency >= input$exercise_freq[1],
             Exercise_frequency >= input$exercise_freq[1],
             Gender %in% input$gender
             ) 
  })
 
  #about tab o/p
  output$text <- renderText("Sleep efficiency is a measure of how much time a person
spends asleep relative to the time they spend in bed, and it can provide valuable insights into the quality and duration of sleep.\n The dataset contains information about a group of test subjects and their sleep patterns. With sleep being a critical component of overall
health and well-being, the information gained from studying a sleep efficiency dataset can help in the development of evidence-based interventions and treatments for sleep disorders and improve overall sleep health.")
  
  output$box1 <- renderValueBox({
    valueBox(
      value = sleep |> nrow(), 
      subtitle =  "Total Participants", 
      icon = icon("people-group", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  output$box2 <- renderValueBox({
    valueBox(
      value = 
        paste0(round((sum(sleep$Gender == "Female")/nrow(sleep))*100, 1), "%"), 
      subtitle =  "Female", 
      icon = icon("person-dress", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  output$box3 <- renderValueBox({
    valueBox(
      value = 
        paste0(round((sum(sleep$Gender == "Male")/nrow(sleep))*100, 1), "%"), 
      subtitle =  "Male", 
      icon = icon("person", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  
  age_counts <- table(sleep$age_range)
  age_counts_df <- data.frame(Age_Range = names(age_counts), Participants = as.numeric(age_counts))
   
  output$table_1 <- renderReactable({
    tbl <- reactable(age_counts_df,
                     height = "350px",
                     pagination = FALSE,
                     defaultSorted = "Age_Range",     # By which column the data is sorted
                     defaultSortOrder = "asc",    # Sorting order
                     defaultColDef = colDef(headerClass = "header", align = "left"),
                     columns = list(               # Giving the options for EACH column
                       Age_range = colDef(minWidth = 120,
                                        maxWidth = 130,
                                        name = "Age Range",
                                        cell = function(value){
                                          div(style = list(fontFamily = "font-awesome", fontWeight = 600),
                                              value)
                                        }),
                       Participants = colDef(
                         name = "Total participants",
                         maxWidth = 230,
                         cell = function(value) {                # A lot of the work below is coding the color
                           width <- paste0(value * 100 / max(age_counts_df$Participants), "%")
                           lev <- (value-min(age_counts_df$Participants)) / (max(age_counts_df$Participants)-min(age_counts_df$Participants)) / 2
                           #value <- format(value, width = 9, justify = "right")
                           value <- div(style = list(fontFamily = "font-awesome", color = "#000000", fontWeight = 700), value)
                           bar <- div(class = "bar", 
                                      style = list(width = width,              # The line below codes the color
                                                   backgroundColor = orange_pal(lev), 
                                                   height = 20))
                           space <- div(style = list(width = 10, backgroundColor = "#FFFFFF", height = 20))
                           div(style = list(display = "flex"), bar, space, span(class = "GtCO2", value) )
                         }
                       ),
                       stylesheet = "
                       .rt-tr-group {
                         margin-bottom: 40px;
                       }"
                       
                     )
    )
  }
  )
  
 #overview tab o/p 

  
  corr_matrix <- cor(sleep %>% select(-age_range, -Gender, -Bedtime,-Smoking_status))
  
  output$plot <- renderPlot({
    ggcorrplot(corr_matrix, 
               hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 2,
               colors = c("#6D9EC1", "white", "#E46726"))+
      theme(axis.text.x = element_text(size = 10), 
            axis.text.y = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, vjust = 6, size = 12, face = "bold"),
            plot.title.position = "plot") +
      ggtitle("Correlation Matrix")
      }, height = 505)
  
  output$plot1 <- renderPlot({
    sleep %>%
      filter(Age>20) %>%
      ggplot(aes(x = Gender, y = Sleep_duration, fill = age_range))+
      geom_violin() +theme_minimal() + labs(x = "", y = "Sleep Duration", fill = "Age Group") +
      theme(axis.text.x = element_text(size = 10), 
            axis.text.y = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
            plot.title.position = "plot")+
      scale_fill_manual(values = c("#E46726", "#6D9EC1", "#FFE5B4", "#FF8C00", "#87CEEB", "#D55E00")) +
      ggtitle("Distribution of Sleep Duration by Age")
  }, height = 300)
  
 
    output$plot2 <- renderPlot({
      sleep %>%
        filter(Age>20) %>%
        ggplot(aes(x= Age, y= Sleep_Efficiency, color = Gender)) +geom_point()+
        geom_smooth(se= F) + theme_minimal() + ggtitle("Sleep Efficiency by Age")+
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
              plot.title.position = "plot")+ 
        labs(x = "Age", y = "Sleep Efficiency", fill = "Gender") +
        scale_color_manual(values = c("#E46726", "#6D9EC1"))
    }, height = 300)
    
    output$bbox1 <- renderValueBox({
      smokers_pct <- sum(data()$Smoking_status == "Yes")/nrow(data()) * 100
      valueBox(
        value = paste0(round(smokers_pct), "%"), 
        subtitle = "Smokers", 
        icon = icon("smoking", lib = "font-awesome"),
        color = "light-blue"
      )
    })
    
    output$bbox2 <- renderValueBox({
      non_smokers_pct <- sum(data()$Smoking_status == "No")/nrow(data()) * 100
      valueBox(
        value = paste0(round(non_smokers_pct), "%"), 
        subtitle = "Non-Smokers", 
        icon = icon("ban-smoking", lib = "font-awesome"),
        color = "light-blue"
      )
    })
    
    
    output$bplot1 <- renderPlot({data() %>%
        filter(Age>20) %>%
        group_by(age_range) %>%
        summarise(avg_alcohol = mean(Alcohol_consumption, na.rm= T)) %>%
        ggplot(aes(x= age_range, y = avg_alcohol, fill = age_range)) + 
        geom_col(fill= c("#FFE5B4","#FFA07A","#FF8C00", "#E46726", "#e44626" )) + 
        labs(x = "Age", y = "Average Alcohol Consumption")+ theme_minimal() +
        coord_flip()
    }, height = 300
    ) 
    
    output$bplot2 <- renderPlot({data() %>%
        filter(Age>20) %>%
        group_by(age_range) %>%
        summarise(avg_caffeine = mean(Caffeine_consumption, na.rm= T)) %>%
        ggplot(aes(x= age_range, y = avg_caffeine)) + 
        geom_col(fill= c("#FFE5B4","#FFA07A","#FF8C00", "#E46726", "#e44626" ))+
        labs(x = "Age", y = "Average Caffeine Consumption") + theme_minimal() +
        coord_flip()
    }, height = 300
    )
    
    
    output$bplot3 <- renderPlot({data() %>%
        filter(Age>20) %>%
        group_by(age_range) %>%
        summarise(avg_exercise = mean(Exercise_frequency, na.rm= T)) %>%
        ggplot(aes(x= age_range, y = avg_exercise)) + 
        geom_col(fill= c("#FFE5B4","#FFA07A","#FF8C00", "#E46726", "#e44626" )) +
        labs(x = "Age", y = "Average Exercise Frequency")+ theme_minimal() +coord_flip()
    }, height = 300
    )
    
    
    
    
    output$plot4 <- renderPlot({
        sleep%>%
        mutate(Bedtime = recode_factor(Bedtime, "21:00:00" = "21:00", "21:30:00" = "21:30", 
                                       "22:00:00" = "22:00", "22:30:00" = "22:30",
                                       "23:00:00" = "23:00", "23:30:00" = "23:30",
                                       "00:00:00" = "00:00", "00:30:00" = "00:30",
                                       "01:00:00" = "01:00", "01:30:00" = "01:30",
                                       "02:00:00" = "02:00", "02:30:00" = "02:30")) %>%
        
        ggplot(aes(x= Bedtime, y= Sleep_Efficiency, fill = Gender))+ 
        labs(x = "Bedtime", y = "Sleep Efficiency", fill = "Gender")+
        scale_fill_manual(values= c("#E69F00", "#56B4E9"))+ geom_col()
    }, height = 450)
    
    output$plot5 <- renderPlot({
        sleep%>%
        mutate(Bedtime = recode_factor(Bedtime, "21:00:00" = "21:00", "21:30:00" = "21:30", 
                                       "22:00:00" = "22:00", "22:30:00" = "22:30",
                                       "23:00:00" = "23:00", "23:30:00" = "23:30",
                                       "00:00:00" = "00:00", "00:30:00" = "00:30",
                                       "01:00:00" = "01:00", "01:30:00" = "01:30",
                                       "02:00:00" = "02:00", "02:30:00" = "02:30")) %>%
        
        ggplot(aes(x = Bedtime, y = Awakenings, fill = Gender)) + 
        geom_col() + facet_wrap(vars(age_range), ncol = 2) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
        labs(x = "Bedtime", y = "Awakenings", fill = "Gender")+
        theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }, height = 450)
    
    output$plot6 <- renderPlot({
      # Calculate the proportion of time spent in each sleep stage for all subjects
      dataset_proportion <- sleep %>%
        group_by(age_range) %>%
        summarize(REM = mean(`REM_sleep%`),
                  Deep = mean(`Deep_sleep%`)
        )
      
      # Reshape the data into long format for plotting
      dataset_long <- dataset_proportion %>%
        pivot_longer(cols = c(REM, Deep), names_to = "Sleep_stage", values_to = "Proportion")
      
      # Create a stacked bar chart of sleep stages proportion
      ggplot(data = dataset_long, aes(x = age_range, y = Proportion, fill = Sleep_stage)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
        labs(x = "Age", y = "Proportion", title = "Proportion of Time Spent in Each Sleep Stage", fill= "Sleep Stage" ) +
        coord_flip()+ theme_minimal() +
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
              plot.title.position = "plot")
    }, height = 300)
    
    
      
    
    output$table_2 <- DT::renderDataTable(data()%>%
                                            select("Age", "Gender", "Sleep_duration",
                                                   "REM_sleep%", "Deep_sleep%") %>%
                                              datatable(options = list(
                                                lengthMenu = list(c(1, 2, 3, 6, 7), c('5 lines', '12 rows', '19 items')),
                                                pageLength = 5  
                                              )) %>%
                                                formatStyle("Gender",
                                                            backgroundColor = styleEqual(c("Female", "Male"), c("#FFB6C1", "#87CEEB")))
                                            )
    
    output$text1 <- renderText("Also known as the dreaming stage, is a stage of sleep characterized by rapid eye movements, 
                               low muscle tone and vivid dreams. During this stage, brain activity is high and resembles the 
                               level of brain activity during wakefulness. REM sleep is an important stage of sleep as it is
                               associated with various cognitive processes such as memory consolidation, emotional regulation, 
                               and creativity. REM sleep typically occurs cyclically throughout the night, with the first REM period 
                               starting about 90 minutes after falling asleep, and each cycle lasting longer as the night progresses.")
                                          
    
    output$plot7 <- renderPlot({
      sleep %>%
        ggplot(aes(x = `REM_sleep%`, y = Sleep_duration, fill = Gender)) + 
        geom_col() + facet_wrap(vars(age_range), ncol = 2) + theme_minimal()+
        scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
              plot.title.position = "plot")+
        labs(x = "REM%", y = "Sleep Duration", title = "Percentage of REM sleep by Sleep duration across Gender")
    }, height = 500)
    
    output$text2 <- renderText("Deep sleep stage is the stage of sleep characterized by the slowest and largest brain waves known as delta waves. This stage is also known as slow-wave sleep (SWS) or delta sleep. During this stage, the body repairs and regenerates tissues, builds bone and muscle, and strengthens the immune system.

Deep sleep is important for physical health, as it promotes recovery, helps boost the immune system, and allows for the consolidation of memories. Lack of deep sleep can result in feeling physically exhausted and fatigued, having difficulty concentrating, and experiencing mood disturbances.

During deep sleep, it can be difficult to wake up, and it is common to feel groggy and disoriented upon waking from this stage. Deep sleep usually makes up about 20-25% of a person's total sleep time, and it typically occurs earlier in the night.")
    
    output$plot8 <- renderPlot({
      data() %>%
        ggplot(aes(x= `Deep_sleep%`, y = Awakenings)) + 
        geom_point() +geom_smooth(color= "#56B4E9") + theme_minimal() + 
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
              plot.title.position = "plot") +
        labs(x = "Deep Sleep%", y = "Awakenings", title = "Relationship between Deep Sleep and Awakenings")
      
    })
    
    output$text3 <- renderText("As per the heatmap in the General Analysis between
                               variable, we see that Alcohol consumption and 
                               Smoking has the most impact on the sleeping pattern. Alcohol consumption and smoking can disrupt sleep by causing breathing problems, increasing heart rate, and increasing the likelihood of waking up during the night.")
    
    output$plot9 <- renderPlot({
      sleep %>%
        ggplot(aes(x = Gender, y = Sleep_Efficiency, fill = Smoking_status))+ 
        geom_boxplot() +theme_minimal() + 
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
              plot.title.position = "plot")+
        labs(x = "", y = "Sleep Efficiency", title = "Distribution of Sleep Efficiency by Gender", fill = "Smoking Status") +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"))
      
    }, height = 300)
    
    output$plot10 <- renderPlot({
      sleep %>%
        group_by(Alcohol_consumption) %>%
        summarise(avg_awakening = mean(Awakenings)) %>%
        ggplot(aes(x= Alcohol_consumption, y= avg_awakening )) + geom_point()+ 
        geom_smooth(color = "#56B4E9") + theme_minimal() + 
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
              plot.title.position = "plot")+
        labs(x = "Alcohol Consumption", y = "Average Awakening", 
             title = "Impact of Alcohol on Sleep Interruption")
        
    })
    
    output$box4 <- renderValueBox({
      avg_sleep_eff <- data() |> summarise(mean(Sleep_Efficiency, na.rm = T)) |> round(2)
      if(avg_sleep_eff <0.85 ){colorbox <- "red"}
      if(avg_sleep_eff >0.90 ){colorbox <- "green"}
      if(avg_sleep_eff >= 0.85 & avg_sleep_eff <= 0.90){colorbox <- "yellow"}
      valueBox(
        value = avg_sleep_eff, 
        subtitle =  "Average sleep efficiency",
        icon = icon("moon", lib = "font-awesome"),
        color = colorbox,
        width = 4  # Width in Bootstrap mode: needs a column()!
      )
    })
    
    
    output$box5 <- renderValueBox({
      valueBox(
        value = data() |> summarise(mean(Awakenings, na.rm = T)) |> round(2),
        subtitle =  "Average Awakening (Interrupted Sleep)", 
        icon = icon("face-frown", lib = "font-awesome"),
        color = "light-blue"
      )
    })
    
    
    output$text4 <- renderText("Regular exercise has been shown to have a positive impact on sleep patterns.
                               It is a beneficial lifestyle habit that can have a positive impact on sleep 
                               patterns and help alleviate the negative impacts of other lifestyle factors such as alcohol consumption, smoking, and caffeine.")
    
    output$plot11 <- renderPlotly({
      g <-  sleep %>% 
        ggplot(aes(x = Sleep_Efficiency, y = Exercise_frequency, label = age_range, fill = age_range)) +
        geom_col() + theme(axis.text.x = element_text(size = 10), 
                           axis.text.y = element_text(size = 10),
                           plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
                           plot.title.position = "plot")+
        labs(x = "Sleep Efficiency", y = "Exercise Frequency", 
             title = "Impact of Exercise on Sleep Efficiency", fill = "Age Range")
      ggplotly(g)
    })
    
  
   }


  
  
  
  


# Run the app ----
shinyApp(ui = ui, server = server)
