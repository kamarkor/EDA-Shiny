# MAIN
#install.packages('shinycssloaders')
library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(data.table)
library(shinycssloaders)
library(rsconnect)
#install.packages('rsconnect')
#install.packages("devtools")
#devtools::install_github("hadley/emo")
#devtools::install_github('rstudio/rsconnect')
#rsconnect.max.bundle.files

#saveRDS(df, "C:/Users/kkoraibi/Desktop/projet R/movies/df.rds")
#setwd("C:/Users/kkoraibi/Desktop/projet R/shinymovies")

#df <- read.csv2(text = readLines("movies.csv", warn = FALSE),header=T)
df <- read.csv("movies.csv")

df$duration <- as.numeric(df$duration,na.rm=T)
typeof(df$duration)
head(df)
df$duration

genres <- unlist(stringr::str_split(paste0(df$genre,collapse = ",",sep=","),","))
genres <- stringr::str_trim(genres) #supprimer les espaces
unique_genres <- unique(genres)
unique_genres <- unique_genres[unique_genres!=""]

df <- df[which(rowMeans(!is.na(df)) > 0.7), which(colMeans(!is.na(df)) > 0.7)]
df$duration <- as.numeric(df$duration, na.rm=TRUE)
df$duration[is.na(df$duration)] <- median(df$duration, na.rm=TRUE)
keys = list(list(1890,1899,"Between 1890 and 1899"),
            list(1900,1909,"Between 1900 and 1909"),
            list(1910,1919,"Between 1910 and 1919"),
            list(1920,1929,"Between 1920 and 1929"),
            list(1930,1939,"Between 1930 and 1939"),
            list(1940,1949,"Between 1940 and 1949"),
            list(1950,1959,"Between 1950 and 1959"),
            list(1960,1969,"Between 1960 and 1969"),
            list(1970,1979,"Between 1970 and 1979"),
            list(1980,1989,"Between 1980 and 1989"),
            list(1990,1999,"Between 1990 and 1999"),
            list(2000,2009,"Between 2000 and 2009"),
            list(2010,2019,"Between 2010 and 2019"),
            list(2020,2029,"From 2020 till now"))
df$decade = NA
for(k in keys){
    df$decade[df$year >= k[[1]] & df$year <= k[[2]]]=k[[3]]
}


#Data à ajouter pour créer le df top10
setDT(df)
top_ten_mov_genre <- purrr::map_dfr(.x=unique_genres,
                                    .f=function(x){
                                        genre_movies <- df[stringr::str_detect(genre,x),.(title,avg_vote)]
                                        setorderv(genre_movies,cols=c("avg_vote"),order=c(-1))
                                        top_movies <- genre_movies[1:min(nrow(genre_movies),10)]
                                        top_movies[,genre:=x]
                                        return(top_movies)
                                    })
top_ten_mov_genre



################### SERVER

server = function(input, output, session){
    library(ggplot2)
    library(DT)
    output$plot <- renderPlot({
        Sys.sleep(3)
        ggplot(df,aes(duration,avg_vote))+geom_point()
    })
    
    
    dat <- reactive({
        user_brush <- input$user_brush
        sel <- brushedPoints(df,user_brush)
        return(sel)
    })
    
    
    output$table <- DT::renderDataTable(DT::datatable(dat()))
    
    output$mydownload = downloadHandler(
        filename = "selected_data.csv",
        content = function(file) {
            write.csv(dat(), file)})
    
    
    output$graph <- renderPlot({
        ggplot(data = df%>% select(genre,decade) %>% filter(str_detect(genre, input$genre))%>%filter(!is.na(decade)), aes(x = decade)) +
            geom_bar(aes(y = (..count..)), stat = "count", width=0.8, color='#3CB5BE', fill="#3CB5BE")+
            theme(axis.text.x = element_text(angle = 90)) + ylab(paste('Number of', input$genre,'movies')) +
            ggtitle(paste('Graphical representation of', input$genre,'movies by decade'))+
            theme(plot.title = element_text(size = 12,hjust = 0.5,face="bold")) +
            geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                          y= ((..count..)/sum(..count..))), stat = "count", vjust=1, colour = "black", fontface='bold',size=3)+
            geom_text(aes(label = ..count..), stat = "count", vjust=-0.2, colour = "black", fontface='bold',size=3)
    })
    
    output$topten <- DT::renderDataTable(top_ten_mov_genre %>% filter(genre == input$genre) %>% select(title, avg_vote))
    
    output$range <- renderPrint({ paste("Number of movies with a duration in this range : ", 
                                        dim(df %>% filter(duration>=input$slider[1] & duration<=input$slider[2]))[1]) })
    
    
}

################### UI

ui = navbarPage(theme = shinytheme("sandstone"), title = "Welcome to our Movies application!",
                
                tabPanel("Graphs",
                         sidebarPanel(
                             selectInput("genre",
                                         "Please select a genre",
                                         choices = unique_genres)
                         ),
                         mainPanel(plotOutput("graph")),
                         sidebarPanel(
                             #a ajouter pr crÃ©er le df top10
                             dataTableOutput("topten")),
                         sliderInput(inputId = "slider", label = "Movie duration", value = c(0,max(df$duration)), 
                                     min = min(df$duration), max = max(df$duration)),
                         verbatimTextOutput("range")
                ),
                
                tabPanel("DataTable",strong("Use the mouse to drag a rectangle around some points!",emo::ji("smile")),
                         p("The graph below is sensitive to a brush maneuver and shows the correlation between the variables duration and average vote. All you need is to select a box or a field on the plot and all the observations that are within this field are then displayed in the table below the plot.
It is a way for the user to select the observations of interest. There are too many observations in this dataset and thus too many points in the plot. However, one could filter observations thanks to the", span("Search", style = "color:blue"), "option on the top right of the table.
You can also select the number of observations you want to show in the table thanks to the ", span("Show Entries", style = "color:blue"), " block on the top left of the table.
Then, you can download the csv data file by clicking", span("Download Table", style = "color:blue"), "in the bottom left hand corner."), 
                         withSpinner(plotOutput("plot",brush="user_brush")),
                         dataTableOutput("table"),
                         downloadButton(outputId = "mydownload", label = "Download Table")
                ),
                
                tabPanel("Suggestion",
                         h4("20 Best Movies Of 2021 - Embedded from Youtube"),
                         tags$iframe(style="height:700px; width:100%",
                                     src="https://www.youtube.com/embed/imM7Mn1PFWw")
                )
)

shinyApp(ui = ui, server = server)



