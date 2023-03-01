library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

choices <- c("Entire Country","North Central","North East","North West",
             "South East","South South","South West")

candidates <- c("Atiku Abubakar", "Bola Ahmed Tinubu", "Peter Obi", "Others")

linkedin <- a("Elijah Akwijoro", href="https://www.linkedin.com/in/elijah-akwijoro/")
github <- a("elijahrona", href="https://github.com/elijahrona")
twitter <- a("@Elijah_Rona", href="https://twitter.com/Elijah_Rona")
kaggle <- a("Elijah Rona", href="https://www.kaggle.com/elijahrona")

ui <- dashboardPage(
  dashboardHeader(title = "2019 Nigerian Election Dashboard",titleWidth = 350),
  dashboardSidebar(sidebarMenu(
    menuItem("Election Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Voter Statistics", tabName = "stats", icon = icon("pie-chart")),
    menuItem("Candidate Popularity", tabName = "popularity", icon = icon("sort-amount-asc")),
    div(
      class ="about_me",
      menuItem("Source code", icon = icon("file-code"),
               href = "https://github.com/rstudio/shinydashboard/"),
      style="color: white;
      position: absolute; bottom: 0; font-size: 16px !important;")
    
  )),
  dashboardBody(
    tags$head(tags$style(HTML('
                              /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                              
                              .light-blue-box .info-box-icon {
                                background-color: #5cc3e7 !important;
                                }
                              .red-box .info-box-icon {
                                background-color: #ed3237 !important;
                                }
                              .green-box .info-box-icon {
                                background-color: #018427 !important;
                                }
                              .green-box .box-header {
                                background-color: #018427 !important;
                                }
                              /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #018427;
                                }
                              /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #018427;
                                }
                              /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #018427;
                                }
                              /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #018427;
                                }
                              /*Select input*/
                              .selectize-input.items.full.has-options.has-items {
                              background-color: #ed3237 !important; border: 2px solid black !important;
                              }
                              /*Select input words*/
                              .item {
                              color: #FFFFFF !important; font-size: 16px !important;
                              }
                              '))), 
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Election Result Dashboard"),
              fluidRow( #row for KPIS
                div(
                  class ="light-blue-box",
                  infoBoxOutput("election_winner")),
                div(
                  class ="red-box",
                  infoBoxOutput("first_runner_up")),
                div(
                  class ="green-box",
                  infoBoxOutput("voter_number")) #018427
              ),
              fluidRow(
                div(
                  class ="green-box",
                  box(title = "Winner by States", #First box
                      status = "success", solidHeader = TRUE,
                      height = 580, width = 12,
                      p(strong(icon("hand-pointer"),"Click on a state to get some figures and stats",icon("hand-pointer")),
                        style="background-color: #ed3237; color: white;"),
                      leafletOutput("p1", height = 490)))),
              fluidRow(
                box(title = "Nigeria Total", #third box
                    status = "success", width = 12, height = 200,
                    plotOutput("p3", height = 50, width = "100%"),
                    selectizeInput("region", "Select Any Region for the Bar Above", choices,
                                   width = "50%"))
              ),
              fluidRow(
                box(status = "success", width = 12,
                    h3("About the Dashboard"),
                    p("You can get a lot of insights from the 2023 Nigerian presidential 
    elections by viewing the interactive charts and tables in the three tabs 
    of this dashboard. The charts and tables were created with reliable 
    data from INEC, so you can trust every information you get from this dashboard. 
    However, if you have concerns, corrections or recommendations, do contact me."),
                    h3("Contact Me"),
                    p("You can find me via:"),
                    tags$ul(
                      tags$li(icon("envelope"),"elijahakwijoro@gmail.com"),
                      tags$li(icon("github"),github),
                      tags$li(icon("twitter"),twitter),
                      tags$li(icon("linkedin"),linkedin),
                      tags$li("Kaggle: ",kaggle)
                    )
                    ))
      ),
      tabItem(tabName = "stats",
              h2("Voter Statistics"),
              fluidRow(
                box(title = "How Regions Voted (Plot)", #third box
                    status = "success", width = 7,
                    plotOutput("p4")),
                box(title = "How Regions Voted (Table)", #third box
                    status = "success", width = 5,
                    tableOutput("t1"))),
              fluidRow(
                box(title = "Voter Turnout for States", #third box
                    status = "success",
                    height = 580, width = 12,
                    p(strong(icon("hand-pointer"),"Click on a state to get some figures and stats",icon("hand-pointer")),
                      style="background-color: #ed3237; color: white;"),
                    leafletOutput("p5", height = 490))
              ),
              fluidRow(
                box(title = "Voter Turnout Growth from 2019 to 2023 (%)",
                    status = "success",
                    height = 700, width = 6,
                    plotOutput("p7")
                ),
                box(title = "Voter Eligibility Growth from 2019 to 2023 (%)",
                    status = "success",
                    height = 700, width = 6,
                    plotOutput("p8")
              )),
              fluidRow(
                box(
                  status = "success",
                  tableOutput("t_turnout")
                ),
                box(
                  status = "success",
                  tableOutput("t_eligibility")
                )
              ),
              fluidRow(
                box(status = "success", width = 12,
                    h3("About the Dashboard"),
                    p("You can get a lot of insights from the 2023 Nigerian presidential 
    elections by viewing the interactive charts and tables in the three tabs 
    of this dashboard. The charts and tables were created with reliable 
    data from INEC, so you can trust every information you get from this dashboard. 
    However, if you have concerns, corrections or recommendations, do contact me."),
                    h3("Contact Me"),
                    p("You can find me via:"),
                    tags$ul(
                      tags$li(icon("envelope"),"elijahakwijoro@gmail.com"),
                      tags$li(icon("github"),github),
                      tags$li(icon("twitter"),twitter),
                      tags$li(icon("linkedin"),linkedin),
                      tags$li("Kaggle: ",kaggle)
                    )))),
      tabItem(tabName = "popularity",
              h2("Popularity of Candidates for Every State"),
              fluidRow(
                box(
                  status = "success",
                  selectizeInput("candidate", "Select Any Candidate of Your Choice", candidates)
                ),
                box(
                  title = "Popularity by Candidate", #third box
                  status = "success",
                  height = 580, width = 12,
                  p(strong(icon("hand-pointer"),"Click on a state to get some figures and stats",icon("hand-pointer")),
                    style="background-color: #ed3237; color: white;"),
                  leafletOutput("p6", height = 490)
                )
              ),
              fluidRow(
                box(
                  width = 8,
                  status = "success",
                  tableOutput("t_popularity")
                ),
                box(
                  width = 4, height = 260,
                  status = "success",
                  div(
                    class ="candidate-photo",
                    imageOutput("photo",
                                width = "100%",
                                height = 235,
                                inline = FALSE))
                )
              ),
              fluidRow(
                box(status = "success", width = 12,
                    h3("About the Dashboard"),
                    p("You can get a lot of insights from the 2023 Nigerian presidential 
    elections by viewing the interactive charts and tables in the three tabs 
    of this dashboard. The charts and tables were created with reliable 
    data from INEC, so you can trust every information you get from this dashboard. 
    However, if you have concerns, corrections or recommendations, do contact me."),
                    h3("Contact Me"),
                    p("You can find me via:"),
                    tags$ul(
                      tags$li(icon("envelope"),"elijahakwijoro@gmail.com"),
                      tags$li(icon("github"),github),
                      tags$li(icon("twitter"),twitter),
                      tags$li(icon("linkedin"),linkedin),
                      tags$li("Kaggle: ",kaggle)
                    )))
      )
    )
  ))


server <- function(input, output) { 
  library(readxl)
  library(tidyverse)
  library(leaflet)
  library(reshape2)
  aa <- read_excel("Election Result.xlsx",
                   sheet = "2023")
  aa <- aa[-38,]
  
  aa$State[aa$State == "FCT"] <- "Federal Capital Territory"
  
  aa <- aa %>% 
    mutate(Abr = factor(ifelse(Region == "South East","SE",
                               ifelse(Region == "North East","NE",
                                      ifelse(Region == "South South","SS",
                                             ifelse(Region == "North Central","NC",
                                                    ifelse(Region == "South West","SW",
                                                           "NW")))))))
  
  states <- geodata::gadm("Nigeria",
                          level = 1,
                          path = tempdir()) %>%
    sf::st_as_sf()
  
  #Rename column
  states <- states %>% 
    rename(State = NAME_1)
  
  #Merge both datasets
  df = merge(x=states,y=aa,by="State", all = FALSE)
  
  sample_data <- aa[,c("Region","Winner","PDP","APC","LP","2023 Casted Votes")]
  
  melt_for_stack <- melt(sample_data, id.vars = c("Region","Winner","2023 Casted Votes"), #aa as in original dataset
                         variable.name = "Party", 
                         value.name = "Votes")
  
  grouped_for_stack <- melt_for_stack %>%
    group_by(Party) %>%
    summarize(`Total Votes` = sum(Votes),
              `2023 Casted Votes` = sum(`2023 Casted Votes`))
  
  #Each party gets votes from a state whether they win it or not, so the sum of "Casted vote" is the same for the two parties in every state. In this case, use just a row from the casted votes columns.
  grouped_for_stack <- grouped_for_stack %>% 
    add_row(Party = "Others", `Total Votes` = grouped_for_stack$`2023 Casted Votes`[1]-sum(grouped_for_stack$`Total Votes`),
            `2023 Casted Votes` = grouped_for_stack$`2023 Casted Votes`[1])
  
  winner_df <- grouped_for_stack %>% filter(`Total Votes` == max(grouped_for_stack$`Total Votes`))
  first_r_u <- grouped_for_stack %>% filter(`Total Votes` == max(grouped_for_stack$`Total Votes`[grouped_for_stack$`Total Votes` != max(grouped_for_stack$`Total Votes`)]))
  
  
  grouped_aa <- aa %>%
    group_by(Region,Abr) %>%
    summarise(APC = round(sum(APC),0),
              PDP = round(sum(PDP),0),
              LP = round(sum(LP),0))
  
  aa <- aa %>%
    mutate(`2019 Turnout` = (`2019 Casted Votes`/`2019 Eligible Voters`)*100) %>%
    mutate(`2023 Turnout` = (`2023 Casted Votes`/`2023 Eligible Voters`)*100)
  
  
  
  output$election_winner <- renderInfoBox({
    infoBox(
      "Winner of the Election",
      paste0(winner_df$Party,":\n",round((winner_df$`Total Votes`/winner_df$`2023 Casted Votes`)*100,2),"%"),
      icon = icon("trophy"), color = "light-blue"
    )
  })
  output$first_runner_up <- renderInfoBox({
    infoBox(
      "First Runner Up",
      paste0(first_r_u$Party,":\n",round((first_r_u$`Total Votes`/first_r_u$`2023 Casted Votes`)*100,2),"%"),
      icon = icon("umbrella"), color = "red"
    )
  })
  
  output$voter_number <- renderInfoBox({
    infoBox(
      "Total Number of Voters",
      paste(scales::comma(sum(df$`2023 Casted Votes`))),
      icon = icon("bar-chart"), color = "green"
    )
  })
  
  output$p1 <- renderLeaflet({
    df <- df %>%
      mutate(`Winner percent` = if_else(Winner == "APC",paste0("APC: ",round((APC/`2023 Casted Votes`)*100,2),"% (",scales::comma(APC)," votes)"),
                                        if_else(Winner == "PDP",paste0("PDP: ",round((PDP/`2023 Casted Votes`)*100,2),"% (",scales::comma(PDP)," votes)"),
                                                paste0("LP: ",round((LP/`2023 Casted Votes`)*100,2),"% (",scales::comma(LP)," votes)")))) %>%
      mutate(`First Runner Up percent` = if_else(`First Runner Up` == "APC",paste0("APC: ",round((APC/`2023 Casted Votes`)*100,2),"% (",scales::comma(APC)," votes)"),
                                                 if_else(`First Runner Up` == "PDP",paste0("PDP: ",round((PDP/`2023 Casted Votes`)*100,2),"% (",scales::comma(PDP)," votes)"),
                                                         paste0("LP: ",round((LP/`2023 Casted Votes`)*100,2),"% (",scales::comma(LP)," votes)")))) %>%
      mutate(`Second Runner Up percent` = if_else(`Second Runner Up` == "APC",paste0("APC: ",round((APC/`2023 Casted Votes`)*100,2),"% (",scales::comma(APC)," votes)"),
                                                  if_else(`Second Runner Up` == "PDP",paste0("PDP: ",round((PDP/`2023 Casted Votes`)*100,2),"% (",scales::comma(PDP)," votes)"),
                                                          paste0("LP: ",round((LP/`2023 Casted Votes`)*100,2),"% (",scales::comma(LP)," votes)"))))
    
    df$Winner[df$State=="Kano"] <- "NNPP"
    
    colpal <- colorFactor(palette = c("#5cc3e7", "#018427", "#1943fa", "#ed3237"), df$Winner, na.color = "#808080")
    
    map_by_party_label <- glue::glue("{df$State} ({df$Winner})")
    
    map_by_party_popup <- glue::glue("<strong>{df$State}: {scales::comma(df$`2023 Casted Votes`)} Votes</strong><br />
                      Winner: {df$Winner}<br />
                      {df$`Winner percent`}<br />
                      {df$`First Runner Up percent`}<br />
                      {df$`Second Runner Up percent`}<br />") %>%
      lapply(htmltools::HTML)
    
    #When dealing with PDP vs APC in 2019, use SRU instead of FRU because I used "MEDIAN()," adding casted vote in Excel to replace LP.
    #For the 2023 dataset, do it as winner, FRU, then SRU. It will be correct
    leaflet(df) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(minZoom = 5, maxZoom = 6)) %>%
      addPolygons(
        # fill
        fillColor   = ~colpal(Winner),
        fillOpacity = 1,
        # line
        dashArray   = "1",
        weight      = 2,
        color       = "black",
        opacity     = 0.7,
        # interaction or selected
        highlight = highlightOptions(
          weight = 5,
          color = "black",
          dashArray = "1",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = map_by_party_label,
        popup = map_by_party_popup,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend( #for legend
        pal = colpal, values = ~Winner, opacity = 1, title = htmltools::HTML("Winner of State"),
        position = "bottomright") %>%
      setMaxBounds(lng1 = -1.5, lng2 = 19, lat1 = -3, lat2 = 16)
  })
  
  
  output$p3 <- renderPlot({
    melt_for_stack <- if (input$region == "Entire Country") {
      melt_for_stack
    } else {
      melt_for_stack %>%
        filter(Region == input$region)
    }
    
    grouped_for_stack <- melt_for_stack %>%
      group_by(Party) %>%
      summarize(`Total Votes` = sum(Votes),
                `2023 Casted Votes` = sum(`2023 Casted Votes`))
    
    #Each party gets votes from a state whether they win it or not, so the sum of "Casted vote" is the same for the two parties in every state. In this case, use just a row from the casted votes columns.
    grouped_for_stack <- grouped_for_stack %>% 
      add_row(Party = "Others", `Total Votes` = grouped_for_stack$`2023 Casted Votes`[1]-sum(grouped_for_stack$`Total Votes`),
              `2023 Casted Votes` = grouped_for_stack$`2023 Casted Votes`[1])
    
    grouped_for_stack$percent <- paste0(grouped_for_stack$Party,":\n",round((grouped_for_stack$`Total Votes`/grouped_for_stack$`2023 Casted Votes`) * 100,2),"%")
    grouped_for_stack$Party <- factor(grouped_for_stack$Party, levels = c('Others','LP','PDP','APC')) #Opposite direction
    
    
    ggplot(grouped_for_stack, aes(x = "", fill = Party, y = `Total Votes`)) + 
      geom_bar(position="stack", stat="identity", color = "black", linewidth = 1) +
      theme_void() +
      theme(legend.position = "none",
            axis.line=element_blank(),
            axis.text = element_blank(),
            axis.ticks=element_blank(),
            axis.title = element_blank(),
            panel.border=element_blank(), 
            panel.spacing = unit(0, "cm"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank()) +
      coord_flip() +
      geom_text(aes(label = percent), 
                position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values=c("#787374","#018427","#ed3237","#5cc3e7")) #Opposite direction
  }, height = 50)
  
  
  output$p4 <- renderPlot({
    grouped_aa %>% 
      pivot_longer(
        cols = c("APC","PDP","LP")
      ) %>%
      rename(`Number of Votes` = value, Party = name) %>% 
      mutate(Party = factor(Party, levels = c("APC","PDP","LP"))) %>%
      ggplot(aes(x = Abr, y = `Number of Votes`, fill=Party)) + 
      geom_bar(position="dodge", stat="identity", colour="black", linewidth = 1) +
      theme(
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid")) +
      scale_fill_manual(values=c("#5cc3e7", "#ed3237", "#018427"))+
      scale_y_continuous(name="Number of Votes", labels = scales::comma) +
      labs(x = "Region")
  })
  
  output$t1 <- renderTable({
    grouped_aa <- grouped_aa %>%
      mutate(APC = scales::comma(APC)) %>%
      mutate(PDP = scales::comma(PDP)) %>%
      mutate(LP = scales::comma(LP))
    
    grouped_aa[,-2] 
  })
  
  output$p5 <- renderLeaflet({
    df <- df %>%
      mutate(Turnout = (`2023 Casted Votes`/`2023 Eligible Voters`)*100)
    
    turnout_pal <- colorBin("Set3", domain = df$Turnout, bins = 6) #Change number for the legend
    
    turnout_label <- glue::glue("{df$State} ({round(df$Turnout,2)}%)")
    
    turnout_popup <- glue::glue("<strong>{df$State} ({df$Winner})</strong><br />
                      Eligible Voters: {scales::comma(df$`2023 Eligible Voters`)}<br />
                      Number of Votes: {scales::comma(df$`2023 Casted Votes`)}<br />
                      Turnout Percent: {round(df$Turnout,2)}%") %>%
      lapply(htmltools::HTML)
    
    leaflet(df) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(minZoom = 5, maxZoom = 6)) %>%
      addPolygons(
        # fill
        fillColor   = ~turnout_pal(Turnout),
        fillOpacity = 1,
        # line
        dashArray   = "1",
        weight      = 2,
        color       = "black",
        opacity     = 0.7,
        # interaction or selected
        highlight = highlightOptions(
          weight = 5,
          color = "black",
          dashArray = "1",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = turnout_label,
        popup = turnout_popup,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend( #for legend
        pal = turnout_pal, values = ~Turnout, opacity = 1, title = htmltools::HTML("Voter Turnout for States (%)"),
        position = "bottomright") %>%
      setMaxBounds(lng1 = -1.5, lng2 = 19, lat1 = -3, lat2 = 16)
  })
  
  output$p6 <- renderLeaflet({
    df <- df %>%
      mutate(Others = `2023 Casted Votes`-(APC+PDP+LP))
    
    party <- if (input$candidate == "Atiku Abubakar") {
      df$PDP
    } else if (input$candidate == "Bola Ahmed Tinubu") {
      df$APC
    } else if (input$candidate == "Peter Obi") {
      df$LP
    } else {
      df$Others
    }
    
    df <- df %>%
      mutate(Popularity = round((!!party/`2023 Casted Votes`)*100,2))
    
    candidate_pal <- colorBin("Set3", domain = df$Popularity, bins = 6) #Change number for the legend
    
    candidate_label <- glue::glue("{df$State}: {df$Popularity}%")
    
    candidate_popup <- glue::glue("<strong>{df$State}: {scales::comma(df$`2023 Casted Votes`)} Votes</strong><br />
                      Voted for {input$candidate}: {scales::comma(party)}<br />
                      Popularity of {input$candidate}: {df$Popularity}%") %>%
      lapply(htmltools::HTML)
    
    legend_title <- paste0("Popularity of ",input$candidate," (%)")
    
    leaflet(df) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(minZoom = 5, maxZoom = 6)) %>%
      addPolygons(
        # fill
        fillColor   = ~candidate_pal(Popularity),
        fillOpacity = 1,
        # line
        dashArray   = "1",
        weight      = 2,
        color       = "black",
        opacity     = 0.7,
        # interaction or selected
        highlight = highlightOptions(
          weight = 5,
          color = "black",
          dashArray = "1",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = candidate_label,
        popup = candidate_popup,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend( #for legend
        pal = candidate_pal, values = ~Popularity, opacity = 1, title = lapply(legend_title, htmltools::HTML),
        position = "bottomright") %>%
      setMaxBounds(lng1 = -1.5, lng2 = 19, lat1 = -3, lat2 = 16)
    
  })
  
  output$t_popularity <- renderTable({
    aa <- aa %>%
      mutate(Others = `2023 Casted Votes`-(APC+PDP+LP))
    
    party <- if (input$candidate == "Atiku Abubakar") {
      aa$PDP
    } else if (input$candidate == "Bola Ahmed Tinubu") {
      aa$APC
    } else if (input$candidate == "Peter Obi") {
      aa$LP
    } else {
      aa$Others
    }
    
    aa$Party <- party
    aa <- aa %>%
      group_by(Region) %>%
      summarize(`2023 Casted Votes` = sum(`2023 Casted Votes`),
                col1 = sum(Party)) %>%
      mutate(Popularity = round((col1/`2023 Casted Votes`)*100,2))
    
    aa <- aa %>%
      mutate(`2023 Casted Votes` = scales::comma(`2023 Casted Votes`)) %>%
      mutate(Popularity = paste(Popularity)) %>%
      mutate(col1 = scales::comma(col1))
    
    colnames(aa)[3] <- paste("Voted",input$candidate)
    colnames(aa)[4] <- paste0("Popularity of ",input$candidate," (%)")
    
    aa
  })
  
  output$photo <- renderImage({
    list(
      src = file.path("candidate photos", paste0(input$candidate, ".jpg")),
      contentType = "image/jpeg",
      width = "100%",
      height = "100%"
    ) 
  }, deleteFile = FALSE)
  
  output$p7 <- renderPlot({
    aa <- aa %>%
      mutate(`Turnout Increase` = round((((`2023 Turnout`-`2019 Turnout`)/`2019 Turnout`)*100),2))
    
    aa <- aa %>%
      mutate(cond = case_when(
        `Turnout Increase` > 0 ~ '#018427',
        `Turnout Increase` < 0 ~ '#ed3237',
        TRUE ~ '#ed3237'))
    
    ggplot(data = aa, aes(x = State, y =`Turnout Increase`)) +
      geom_bar(stat = "identity", aes(fill = cond), colour="black", linewidth = 1) +
      scale_fill_identity() +
      geom_text(aes(label = `Turnout Increase`, hjust = -0.1), size = 4) +
      coord_flip() + 
      theme(panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            linewidth = 0.5, linetype = "solid"),
            axis.text = element_text(color="black", size = 15),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_discrete(limits = rev(levels(as.factor(aa$State)))) +
      xlab("") + 
      ylab("") +
      ylim(min(aa$`Turnout Increase`)-10,max(aa$`Turnout Increase`)+10)
    
  }, height = 680)
  
  output$p8 <- renderPlot({
    
    aa <- aa %>%
      mutate(`Eligibility Increase` = round((((`2023 Eligible Voters`-`2019 Eligible Voters`)/`2019 Eligible Voters`)*100),2))
    ggplot(data = aa, aes(x = State, y =`Eligibility Increase`)) +
      geom_bar(stat = "identity", aes(fill = "#018427"), colour="black", linewidth = 1) +
      scale_fill_identity() +
      geom_text(aes(label = `Eligibility Increase`, hjust = -0.1), size = 4) +
      coord_flip() + 
      theme(panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            linewidth = 0.5, linetype = "solid"),
            axis.text = element_text(color="black", size = 15),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_discrete(limits = rev(levels(as.factor(aa$State)))) +
      xlab("") + 
      ylab("") +
      ylim(0,max(aa$`Eligibility Increase`)+10)
  }, height = 680)
  
  output$t_turnout <- renderTable({
    aa_turnout <- aa %>%
      group_by(Region) %>%
      summarise(`2019 Casted Votes` = sum(`2019 Casted Votes`),
                `2023 Casted Votes` = sum(`2023 Casted Votes`),
                `2019 Eligible Voters` = sum(`2019 Eligible Voters`),
                `2023 Eligible Voters` = sum(`2023 Eligible Voters`))
    
    aa_turnout <- aa_turnout %>%
      mutate(`2019 Turnout (%)` = (`2019 Casted Votes`/`2019 Eligible Voters`)*100) %>%
      mutate(`2023 Turnout (%)` = (`2023 Casted Votes`/`2023 Eligible Voters`)*100)

    aa_turnout <- aa_turnout %>%
      mutate(`Increase (%)` = round((((`2023 Turnout (%)`-`2019 Turnout (%)`)/`2019 Turnout (%)`)*100),2))
    
    aa_turnout[c("Region","2019 Turnout (%)","2023 Turnout (%)","Increase (%)")]
  })
  output$t_eligibility <- renderTable({
    aa_eligibility <- aa %>%
      group_by(Region) %>%
      summarise(`2019 Eligible` = sum(`2019 Eligible Voters`),
                `2023 Eligible` = sum(`2023 Eligible Voters`))

    
    aa_eligibility <- aa_eligibility %>%
      mutate(`Increase (%)` = round((((`2023 Eligible`-`2019 Eligible`)/`2019 Eligible`)*100),2)) %>%
      mutate(`2019 Eligible` = scales::comma(`2019 Eligible`)) %>%
      mutate(`2023 Eligible` = scales::comma(`2023 Eligible`))
    
    aa_eligibility
  })

}

shinyApp(ui, server)