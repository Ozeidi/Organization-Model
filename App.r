#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#PearsonDS is used to generate custom distribution with defined skewness and kurtosis
list.of.packages <- c("tidyverse", "readxl", "lubridate", "naniar",
                      "prettyR", "magrittr", "reshape2","xlsx", "RODBC", "ImportExport", "RSQLite", "PearsonDS",
                      "shinythemes", "shinydashboard")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)



w <- seq(1,4)
my_theme <-   theme(plot.title = element_text(hjust = 0, size = 18, face="bold", vjust=0),
                    axis.title.x =element_blank(),
                    axis.title.y =element_blank(),
                    axis.ticks.x = element_blank(),  
                    axis.ticks.y = element_blank(),
                    axis.text.x = element_blank(),  
                    axis.text.y =element_blank(),
                    axis.line=element_blank(),
                    plot.margin = margin(1,1,1,6, "cm"),
                    panel.background=element_blank(),
                    panel.border=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank()) 

ui <- dashboardPage(
  dashboardHeader(title = "The Generations Evolution"),
  dashboardSidebar(
    sidebarMenu(

            menuItem("Simualtion Settings",icon = icon("th"), tabName = 'SimSettings',startExpanded=TRUE,
                     menuItem("Organization Profile",tabName = 'orgprofile',
                                 selectInput("Age_Profile", "Dominant Age Group:",
                                             c("Youngsters" = "Youngsters",
                                               "Mid Age" = "Mid_Age",
                                               "Grey Hair" = "Grey_Hair"),selected = "Mid_Age"),
                                 
                                 selectInput("Intake_Profile", "Intake Profile:",
                                             c("Youngsters" = "Youngsters",
                                               "Mid Age" = "Mid_Age",
                                               "Grey Hair" = "Grey_Hair")),
                                 
                                 selectInput("Attrition_Profile", "Attrition Profile:",
                                             c("Youngsters" = "Youngsters",
                                               "Mid Age" = "Mid_Age",
                                               "Grey Hair" = "Grey_Hair"), selected = "Grey_Hair")
                                 ),
                     
                numericInput("Headcount", "Organization Headcount", 9000),
                sliderInput("year_limit",
                            "End Year for Simulation",
                            min = 2021,
                            max = 2070,
                            value = 2030),
                sliderInput("Attrition",
                            "Attrition Rate",
                            min = 0.0,
                            max = 0.1,
                            value = .02),
                sliderInput("Intake",
                            "Annual Intake Headcount",
                            min = 0.0,
                            max = 1000,
                            value = 550),
                uiOutput("slider"),
                actionButton("Export", "Export Population RDS")
                ),
                  menuItem("Dashboard", tabName = "dashboard")
                )),
  
  
            dashboardBody(
              tabItems( 
                tabItem("dashboard",
                fluidRow( 
                  box(
                    width = 4, status = "info", solidHeader = TRUE,
                    title = "Age Profile",
                    plotOutput("Age_Profile", width = "100%", height = "150")
                  ),
                  box(
                    width = 4, status = "info", solidHeader = TRUE,
                    title = "Intake Profile",
                    plotOutput("Intake_Profile", width = "100%", height = "150")
                  ),
                  box(
                    width = 4, status = "info", solidHeader = TRUE,
                    title = "Attrition Profile",
                    plotOutput("Attrition_Profile", width = "100%", height = "150")
                  ),
                  
                  
                  box(
                    width = 6, status = "info", solidHeader = TRUE,
                    title = "Organization Age Profile",
                    plotOutput("Plot1", width = "100%", height = "300")
                  ),
                  box(
                    width = 6, status = "info", solidHeader = TRUE,
                    title = "Organization Age Profile",
                    plotOutput("Plot2", width = "100%", height = "300")
                  )
                  )
                )

              )
            
            )
            
            

)



          


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  df_Age_Profile <-reactive({
    Profile_Generator(input$Age_Profile)
  })
  
  df_Intake_Profile <-reactive({
    Profile_Generator(input$Intake_Profile)
  })
  
  df_Attrition_Profile <-reactive({
    Profile_Generator(input$Attrition_Profile)
  })
  
  
  
  output$Age_Profile <- renderPlot({
  res <- df_Age_Profile()
  res%>%  ggplot(aes(x= Age, y =value)) + geom_bar( stat="identity")+
    ylab("Frequency") + xlab("Age") + theme_minimal()+
    geom_text(aes(label = paste0("Total Headcount: ", sum(value)),
                                 x =40, y = .3
                  )) 
  })
  
  output$Intake_Profile <- renderPlot({
    res <- df_Intake_Profile()
    res%>%  ggplot(aes(x= Age, y =value)) + geom_bar( stat="identity")+
      ylab("Frequency") + xlab("Age") + theme_minimal()+
      geom_text(aes(label = paste0("Total Headcount: ", sum(value)),
                    x =40, y = .3
      )) 
  })

  output$Attrition_Profile <- renderPlot({
    res <- df_Attrition_Profile()
    res%>%  ggplot(aes(x= Age, y =value)) + geom_bar( stat="identity")+
      ylab("Frequency") + xlab("Age") + theme_minimal()+
      geom_text(aes(label = paste0("Total Headcount: ", sum(value)),
                    x =40, y = .3
      )) 
  })
  

  output$slider <- renderUI({
    sliderInput("Year_filter", "Year on Display", min = 2020,
                max = max(input$year_limit),
                value = 2020,
                step = 1,
                animate =
                  animationOptions(interval = 300, loop = TRUE))
  })
  
  
  df <- reactive({
    age_mat<- df_Age_Profile() * input$Headcount
    age_mat <- as.matrix(age_mat$value)
    
    arr_dis <- df_Intake_Profile() 
    arr_dis <- as.matrix(arr_dis$value)
    
    att_dis <- df_Attrition_Profile() 
    att_dis <- as.matrix(att_dis$value)
    
    Simulate(Pop_mat = age_mat, steps = input$year_limit - 2020+1, intake = input$Intake,arr_prob = arr_dis, att_prob = att_dis, attrition = input$Attrition)
    
  })
        
  output$Plot1 <- renderPlot({
    res <-df()
    res%>% filter(Year == input$Year_filter) %>%  ggplot(aes(x= Age, y =value)) + geom_bar( stat="identity")+
       ylab("Headcount") + xlab("Age") + theme_minimal()+
      geom_text(aes(label = paste0("Total Headcount: ", sum(value)), 
                    x =  30, 
                    y = 600)) #+ ylim(0,700)
    


  },    width = "auto", height = 300)    
  output$Plot2<-renderPlot({
    res <-df()
    
    res %>%  filter(Year ==input$Year_filter) %>%  group_by(Generation, .drop =F) %>% 
      summarise(value=sum(value)) %>% mutate(pct = round(value/sum(value)*100,0)) %>% 
      ggplot( aes(x =w, color = Generation))+
      geom_linerange( aes(ymin = -value, ymax = value), size = 25, alpha = 0.8)+
      scale_color_brewer(palette="Spectral")+
      geom_label(aes(x = w, y = 0, label = paste0(Generation, ": ", pct, "%")), 
                 inherit.aes = F,
                 size = 5, label.padding = unit(0.0, "lines"), label.size = 0,
                 label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.1, color = "#5D646F")+
      geom_label(aes(x = w, y = 0, label = paste0("\n\n ", value)), 
                 inherit.aes = F,
                 size = 4, label.padding = unit(0.0, "lines"), label.size = 0,
                 label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.1, color = "#5D646F")+
      geom_text(aes(x = w, y = 0, label = paste0("Total Headcount: ", sum(value))), 
                 inherit.aes = F,
                 size = 5, label.padding = unit(0.0, "lines"), label.size = 0,
                 label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.1, color = "#5D646F")+
      labs(title = paste0("Gneration Distribution: ", input$Year_filter))+
      xlim(0.5,4.5)+
      coord_flip()+ my_theme +
      guides(color = FALSE, fill = FALSE) 
  })
  
  observeEvent(input$Export, {
    res <-df()
    res %<>%  group_by(Year,Generation, .drop =F) %>% 
      summarise(value=sum(value))
    write_rds(res, "Generation.rds")
    session$sendCustomMessage(type = 'testmessage',
                              message = 'RDS Exported With name of: Generation.rds')
  })
    
  
}



Simulate <- function (steps = 21, Pop_mat = age_mat,arr_prob= arr_dis, att_prob = att_dis,  intake =200, attrition = 0.02 ){
  full_matrix <- matrix(nrow =nrow(Pop_mat) , ncol = steps )
  print(c("Full Matrix", full_matrix[1,2]))
  print(c("steps:", steps))
  full_matrix[,1] <- Pop_mat
  for (i in seq(2, steps)){
    print (i)
    #Age offset
    full_matrix[,i] <- lag(full_matrix[, i-1], default = 0)
    

    
    
    
    #Attrition
    total_headcount <- sum(full_matrix[,i])
    print(total_headcount)
    att_vector <- att_prob * (total_headcount * attrition)
    att_vector <- round(att_vector, 0)
    
    full_matrix[,i] <- full_matrix[,i] - att_vector
    
    #Recruitment
    print(c("Dimension: ", dim(full_matrix) ))
    total_headcount <- sum(full_matrix[,i])
    print(total_headcount)
    arr_vector <- arr_prob * intake
    arr_vector <- round(arr_vector, 0)
    
    full_matrix[,i] <- full_matrix[,i] + arr_vector
    #full_matrix[1,i] <- intake

  }
  full_matrix
  pop.df <- as.data.frame(full_matrix, row.names =  seq(20, 60))
  colnames(pop.df) <- as.character(c(2020:(2020+steps-1)))
  pop.df %<>%  mutate(Age = seq(20, 60)) %>%  select(Age , everything())
  print ("Simulate Completed")
  
  pop.df <- melt((pop.df), id.vars = c("Age"), variable.name = "Year")
  
  pop.df %<>% mutate(Birth_Year = as.numeric(levels(Year))[Year]-Age)
  pop.df %<>%  mutate(Generation =Generation_Cut(Birth_Year), value = ifelse(value<0, 0, value) )
  pop.df 
  
}

Generation_Cut <- function(x){
  print("Generation Cut")
  cut(x, breaks=c(1945, 1964, 1979, 1995, 2020),labels = c("Baby Boomers","Generation X","Generation Y", "Generation Z" ), include.lowest=TRUE)
}
# This function taks the population table from simulation and convert it to Generation Headcount

# Generation <- function(pop.df){
#   res <- melt((pop.df), id.vars = c("Age"), variable.name = "Year")
#   
#   res %<>% mutate(Birth_Year = as.numeric(levels(Year))[Year]-Age)
#   res %<>%  mutate(Generation =Generation_Cut(Birth_Year) )
#   res
# }
# 
# res

Profile_Generator <- function(option = "Youngester"){
  
  if (option == "Youngsters"){
    moments <- c(mean = 0,variance = 1,skewness = 1, kurtosis = 3)
  }else if(option == "Mid_Age"){
    moments <- c(mean = 0,variance = 1,skewness = 0, kurtosis = 3)
  }else if (option == "Grey_Hair"){
    moments <- c(mean = 0,variance = 1,skewness = -0.5, kurtosis = 3)
  }
  
  dt <- rpearson(1000, moments = moments)
  hst <- table(cut(dt, breaks = 41))
  vec <- as.vector(hst/sum(hst))
  vec_df <- as.data.frame(vec)
  vec_df %<>% mutate(Age = seq(20,60))
  colnames(vec_df) <-  c('value', 'Age')
  
  vec_df
}

# Run the application 
shinyApp(ui = ui, server = server)


