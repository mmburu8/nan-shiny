# install package
# import libraries
library(tidyverse)
library(shiny)
library(readxl)
library(shinythemes)
library(scales)
library(hrbrthemes)
library(stringi)
library(gridExtra)
library(DBI)
library(RPostgres)
library(reactable)

# user interface
ui <- fluidPage(theme=shinytheme("cyborg"),
                # use bootstrap to my advantage
                tags$div(class="jumbotron text-center", 
                         style="margin-bottom:0px;margin-top:0px;height:130px",
                         tags$h2(class="jumbotron-heading",
                                 style="margin-bottom:0px;margin-top:0px",
                                 "Album Analytics"),
                         p("What song stole the show?")
                         ),
                fluidRow(
                  column(5,
                  selectInput("artist", "Artist",
                              list(
                                "Burna Boy" = "Burna Boy",
                                "WSTRN" = "WSTRN",
                                "Fireboy DML" = "Fireboy DML",
                                "Victony" = "Victony",
                                "Bnxn TYE" = "Bnxn TYE",
                                "Tion Wayne" = "Tion Wayne",
                                "Gunna" =  "Gunna",
                                "ASAKE" = "ASAKE",
                                "Quavo & Takeoff" = "Quavo Huncho",
                                "Kendrick Lamar" = "Kendrick Lamar",
                                "Migos ATL" = "Migos ATL",
                                "Buruklynboyz" = "Buruklynboyz",
                                "Ruger" = "RugerVEVO"
                              )),
                  imageOutput("albums")),
                column(7,
                  # output is a plot
                  plotOutput("plot", click="plot_click"),
                  tableOutput("dracarys"))
)
                )
# server function
server <- function(input, output, session){
  # read from database
  conn <- dbConnect(RPostgres::Postgres(), dbname = "GIVEON", 
                    host="localhost",  user="postgres", port=5432,
                    password="#Juanmata8")
  youtube <- as.data.frame(dbGetQuery(conn, "select * from vidAnalytics"))
  # summarise data
  data <- reactive({
    youtube <- youtube %>%
      mutate(channeltitle = fct_recode(channeltitle, 
                                       "Buruklynboyz" = "Buruklynboyz "))
    df <- youtube %>%
      filter(channeltitle == input$artist) %>%
      group_by(Songs = stringi::stri_trans_totitle(song)) %>%
      summarise(viewers = max(viewcount),
                likes = max(likecount),
                comments = max(commentcount)) %>%
      mutate(Songs = fct_reorder(Songs, viewers))
  })
  # get name of title
  artistique <- c("Burna Boy", "WSTRN", "Fireboy DML", "Victony", 
                  "Bnxn TYE", "Tion Wayne", "Gunna", "ASAKE", "Quavo Huncho",
                  "Kendrick Lamar", "Migos ATL", "Buruklynboyz", "RugerVEVO")
  album_head <- c("Love Damini", "WSTRN Season 3", "Playboy", "Outlaw",
                  "Bad Since '97", "Green With Envy", "DS4EVER",
                  "Mr Money With The Vibe", "Only Built For Infinity Links",
                  "Mr Morale And The Big Steppers", "Culture 3", 
                  "East Mpaka London", "The Second Wave")
  output$plot <- renderPlot({
    ggplot(data(), aes(x = viewers, y = Songs))+
      geom_bar(fill="red2", color="mintcream", stat="identity")+
      scale_x_continuous(labels = comma)+
      theme_ipsum()+
        theme(
          axis.text.x = element_text(size=12, color="gray10"),
          axis.text.y = element_text(size=10, color="gray10"),
          axis.title.x = element_text(size=15, hjust=0.5),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size=16, face="bold", hjust=0.4)
        )+
      labs(x = "views", y="", title = album_head[[match(input$artist, artistique)]])
    
  })
  output$dracarys <- renderTable({
    req(input$plot_click)
    tristan <- nearPoints(data(), input$plot_click, threshold = 10)
    tristan %>%
      mutate(`Percent Likes` = paste(round((likes / viewers) * 100, 3), "%", sep=""),
        views = comma(viewers),
             likes = comma(likes),
             comments = comma(as.integer(comments)))%>%
      select(Songs, views, likes, comments, `Percent Likes`)
  })
  output$albums <-  renderImage({
    picFilePath <- paste(input$artist, ".jpg", sep="")
    return(list(
      src = picFilePath,
      contentType = "image/jpg",
      alt = "Album Cover",
      deleteFile=FALSE
    ))
  })
}

# create Shiny App
shinyApp(ui = ui, server = server)

