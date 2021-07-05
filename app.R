library(shiny)
library(shinyWidgets)
library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
# library(magrittr)
library(scales)
library(stats)
library(ggpubr)


options(scipen = 80) # loading file size less than 50MB

file_pre <- "./" # same directory as shiny file

# loading data for geomap
brain_sf <- st_read(dsn = paste(file_pre, "brain.geojson", sep = "/"), 
                    stringsAsFactors = FALSE, quiet = TRUE)

# loading Metabolites data
demo_df <- read_csv(file = paste(file_pre, "longdata.csv", sep = "/"), 
                    col_names = TRUE) 

Meta_v <- sort(unique(demo_df$Metabolite))

# create Mice brain region data with right order
Region_df <- data.frame(x = 1:10, stringsAsFactors = FALSE,
                        Region = c("Cerebral cortex", "Olfactory bulb", "Hippocampus",
                                   "Hypothalamus", "Basal ganglia", "Thalamus", "Midbrain", 
                                   "Pons", "Medulla", "Cerebellum"))

ui <- fluidPage(
    # Page title
    title="The Metabolome Atlas of the Aging Mouse Brain",

    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    # Application title
    fluidRow(
        column(
            width = 12,
            tags$a(href="https://fiehnlab.ucdavis.edu", img(src = "wcmc.png", height = "auto"), target = "_blank"),
            h2("The Metabolome Atlas of the Aging Mouse Brain")
        ),
        id = "titleBar"
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            h1("Parameter Setting", align = "center",
               style = "font-family:arial; font-size:25pt; color:#000000"),
            hr(),
            pickerInput(
                inputId = "meta",
                label = "Metabolite",
                choices = Meta_v,
                multiple = FALSE,
                options = list(
                    `actions-box` = TRUE,
                    size = 10),
                selected = c("Adenosine")
            ),
            br(),
            hr(),
            # self-adaption in image size 
            p(img(src = "brainfigure.jpg", height = "auto", width = "100%"), align = "left"),
            br(),
            awesomeCheckboxGroup(
                inputId = "region",
                label = "Brain Region (Multiple options)",
                choices = c("1.Cerebral cortex" = "Cerebral cortex",# label on left and variable on right
                            "2.Olfactory bulb" = "Olfactory bulb",
                            "3.Hippocampus" = "Hippocampus",
                            "4.Hypothalamus" = "Hypothalamus",
                            "5.Basal ganglia" = "Basal ganglia",
                            "6.Thalamus" = "Thalamus", # label on left and variable on right
                            "7.Midbrain" = "Midbrain",
                            "8.Pons" = "Pons", 
                            "9.Medulla" = "Medulla", 
                            "10.Cerebellum" = "Cerebellum"),
                selected = Region_df$Region
                # status = "danger"
            ),
            br(),
            hr(),
            awesomeCheckboxGroup(
                inputId = "gender",
                label = "Gender (Multiple options)",
                choices = c("Female", "Male"),
                selected = c("Female", "Male"),
                inline = TRUE
            ),
            hr()
        ),

        mainPanel(
            h1(textOutput("text1"), align = "center",
               style = "font-family:arial; font-size:30pt; color:#000000"), 
            hr(),
            p(img(src = "mylegend2.png", width = "20%", height = "auto"), align = "right"),
            fluidRow(
                column(width = 3, height = 200,
                       plotOutput("plot_w3",  width = "auto", height = 200)),
                column(width = 3, height = 200,
                       plotOutput("plot_w16", width = "auto", height = 200)),
                column(width = 3, height = 200,
                       plotOutput("plot_w59", width = "auto", height = 200)),
                column(width = 3, height = 200,
                       plotOutput("plot_w92", width = "auto", height = 200))
            ),
            hr(),
            fluidRow(
               column(width = 3, height = 60,  
                      p("3 weeks", 
                        style = "font-family:arial;font-size:18pt; color:#000000", align = "center"),
                      p(img(src = "3.png", height = 50, width = "auot"), align = "center")),
               column(width = 3, height = 60,
                      p("16 weeks", 
                        style = "font-family:arial;font-size:18pt; color:#000000", align = "center"),
                      p(img(src = "16.png", height = 50, width = "auto"), align = "center")),
               column(width = 3, height = 60,
                      p("59 weeks", 
                        style = "font-family:arial;font-size:18pt; color:#000000", align = "center"),
                      p(img(src = "59.png", height = 50, width = "auto"), align = "center")),
               column(width = 3, height = 60,
                      p("92 weeks", 
                        style = "font-family:arial;font-size:18pt; color:#000000", align = "center"),
                      p(img(src = "92.png", height = 50, width = "auto"), align = "center"))
            ), 
            hr(),
            
            fluidRow(
               column(width = 12, height = 550,
                      plotOutput("plot_bar", width = "auto", height = 450),
                      br(), 
                      hr(), 
                      # add comments
                      p("1. Bar graphs represent arithmetic means and their corresponding standard errors.", 
                        style = "font-family:arial;font-size:12pt; color:#000000"),
                      p("2. One-way ANOVA was used for significance analysis. 
                        We use the following convention for symbols indicating statistical significance, ns: p > 0.05; *: p <= 0.05; **: p <= 0.01; ***: p <= 0.001; ****: p <= 0.0001", 
                        style = "font-family:arial;font-size:12pt; color:#000000")
                )
               
            )
        )
    )
)


fun1 <- function(df){
  mypal <- colorRampPalette(c("#91D1C1","#FFEB84","#E74B35"))(101) 
  df1 <- df %>% select(Region, Age, Intensity) %>% 
    group_by(Region, Age) %>% 
    summarise(meanInten = mean(Intensity)) %>% ungroup() 
  minInten <- min(df1$meanInten)
  maxInten <- max(df1$meanInten)
  df_A <- df1 %>% mutate(normInten = round((meanInten - minInten)/(maxInten-minInten)*100)) %>% 
    mutate(color = mypal[normInten + 1]) %>% 
    select(Region, Age, color) %>% 
    replace_na(list(color = "#91D1C1"))
  
    
  df_B <- df %>% select(Region, Age, Intensity) %>% 
    right_join(Region_df, by = "Region") %>% 
    mutate(A2 = factor(x = Age, levels = c("3 weeks", "16 weeks", "59 weeks"))) %>% 
    select(x, Intensity, Age = A2) 
  
  y_p <- df %>% select(Region, Age, Intensity) %>% 
    group_by(Region, Age) %>% 
    summarise(y_mean = mean(Intensity),
              y_diff = sd(Intensity, na.rm = TRUE)/sqrt(sum(!is.na(Intensity)))) %>% 
    ungroup() %>% 
    transmute(y_up = y_mean + y_diff) %>% pull(y_up) %>% 
    max() %>% `*`(1.1) 
  
  
  return(list(df_A, df_B, y_p))
    
}



server <- function(input, output) {
    reaction1 <- reactive({
      req(input$meta, input$region, input$gender) # the trick
      options(scipen = 80)
      demo_df %>%
            filter(Region %in% input$region, 
                   Metabolite == input$meta, 
                   Gender %in% input$gender) %>% 
            fun1()
            
    })
    
    output$text1 <- renderText({
      input$meta
    })
    
    
    output$plot_w3 <- renderPlot({
      sf_w3 <- brain_sf %>% left_join(filter(reaction1()[[1]], Age == "3 weeks"), by = "Region") %>% 
        select(-Age)
      ggplot(sf_w3) + 
        geom_sf(aes(fill = Region), color = "black", show.legend = FALSE) + 
        coord_sf() + 
        scale_fill_manual(values = sf_w3$color, 
                          labels = sf_w3$Region) + 
        theme_void()
    })
    
    output$plot_w16 <- renderPlot({
      sf_w16 <- brain_sf %>% left_join(filter(reaction1()[[1]], Age == "16 weeks"), by = "Region") %>% 
        select(-Age)
      ggplot(sf_w16) + 
        geom_sf(aes(fill = Region), color = "black", show.legend = FALSE) + 
        coord_sf() + 
        scale_fill_manual(values = sf_w16$color, 
                          labels = sf_w16$Region) + 
        theme_void()
    })
    
    output$plot_w59 <- renderPlot({
      sf_w59 <- brain_sf %>% left_join(filter(reaction1()[[1]], Age == "59 weeks"), by = "Region") %>% 
        select(-Age)
      ggplot(sf_w59) + 
        geom_sf(aes(fill = Region), color = "black", show.legend = FALSE) + 
        coord_sf() + 
        scale_fill_manual(values = sf_w59$color, 
                          labels = sf_w59$Region) + 
        theme_void()
    })
    
    output$plot_w92 <- renderPlot({
      sf_w92 <- brain_sf %>% left_join(filter(reaction1()[[1]], Age == "92 weeks"), by = "Region") %>% 
        select(-Age)
      ggplot(sf_w92) + 
        geom_sf(aes(fill = Region), color = "black", show.legend = FALSE) + 
        coord_sf() + 
        scale_fill_manual(values = sf_w92$color, 
                          labels = sf_w92$Region) + 
        theme_void()
    })
    
    output$plot_bar <- renderPlot({
      
      # reaction1()[[2]] %>%
      # group_by(x, Age) %>%
      #   summarise(mean = mean(Intensity),
      #             sd = sd(Intensity),
      #             se = sd/sqrt(length(Intensity))) %>%
      #   ggplot(.,aes(x, mean, fill = Age)) +
      #   geom_bar(stat = "identity", position = position_dodge(0.8), color = "black", width = 0.7,size = 0.25) +
      #   geom_errorbar(aes(ymin= mean - se, ymax = mean + se), width=0.3, size = .25,
      #                 position=position_dodge(.8)) +
      #   ggpubr::theme_pubr()+
      #   theme(legend.position = " ")+
      #   labs(x = "",y = "percentages", fill = "")+
      #   scale_fill_manual(breaks = c("3 weeks", "16 weeks", "59 weeks","92 weeks"),
      #                     values = c("#F39B7E","#91D1C1","#8491B3","#7F6149")) +
      #   ggpubr::rotate_x_text(angle = 60)+
      #   ggpubr::stat_compare_means(data = reaction1()[[2]],aes(x, Intensity, group = Age),label = "p.signif",
      #                              label.y = reaction1()[[3]])
      
      

      ggbarplot(reaction1()[[2]], x = "x", y = "Intensity", fill = "Age", legend = "none",
                add = "mean_se", add.params = list(width = 0.3, size = 0.25),
                position = position_dodge(0.8), size = 0.25) +
        stat_compare_means(aes(group = Age), size = 6, method = "anova",  label = "p.signif",
                           label.y = reaction1()[[3]]) +
        scale_x_continuous(breaks = 1:10,
                           labels = Region_df$Region,
                           limits = c(0.5, 10.5),
                           expand = c(0,0)) + # no gap
        scale_y_continuous(labels = scientific) +
        # scale_fill_manual(breaks = c("3 weeks", "16 weeks", "59 weeks","92 weeks"),
        #                   values = c("#F39B7E","#91D1C1","#8491B3","#7F6149")) +
        labs(x = "Brain Regions",
             y = "Peak Intensity") +
        theme(axis.text.y  = element_text(size = 18, color = "black"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"),
              axis.ticks.length = unit(2, "mm"),
              axis.ticks = element_line(size = 0.5),
              axis.line = element_line(size = 0.5),
              axis.title = element_text(size = 20, color = "black"),
              panel.grid = element_blank(),
              panel.background = element_blank())
    })
    
    
}


shinyApp(ui = ui, server = server)
