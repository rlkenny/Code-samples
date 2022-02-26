# Load libraries

pkgs <- c("tidyverse", "shiny", "shinythemes", "sf", "rgeos", "rmapshaper", "janitor", "datasets", "RColorBrewer", "plotly", "png", "imager")
for (pkg in pkgs) {
    do.call("library", list(pkg))
}

# Read in data

scatterplot_df1 <- read_csv("clean_scatterplot.csv")

bio <- read_csv("bio.csv")

bio$Country <- as.character(bio$Country)

country_vector <- bio$Country

PopulationPredictions <- read_csv("PopulationPredictions.csv", 
                                  col_types = cols(Total2000 = col_factor(levels = c("2", 
                                                                                     "3", "4", "5", "6", "7", "8", "9", 
                                                                                     "10")), Total2010 = col_factor(levels = c("2", 
                                                                                                                               "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                               "10")), Total2020 = col_factor(levels = c("2", 
                                                                                                                                                                         "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                         "10")), Total2030 = col_factor(levels = c("2", 
                                                                                                                                                                                                                   "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                                                                   "10")), Total2040 = col_factor(levels = c("2", 
                                                                                                                                                                                                                                                             "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                                                                                                             "10")), Total2050 = col_factor(levels = c("2", 
                                                                                                                                                                                                                                                                                                       "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                                                                                                                                                       "10"))))
world_outline <- read_sf(dsn = ".", layer = "countries")
world_df <- full_join(world_outline, PopulationPredictions, by = "COUNTRY")
cols <- c("2" = "#65182f", "3" = "#9c2530", "4" = "#c75c57", "5" = "#f28882", "6" = "#fcd2ce", "7" = "#c9e3e5", "8" = "#87c4cf", "9" = "#3b8eab", "10" = "#053761")



# Define plots
Graph2000 <- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2000), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_fill_brewer(palette = "RdBu") +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = " ")

Graph2010<- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2010), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = " ")

Graph2020 <- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2020), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = " ")


Graph2030 <- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2030), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = " ")

Graph2040<- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2040), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = " ")

Graph2050<- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2050), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = " ")


# User interface 
ui <- navbarPage("Exploring Population Growth and the Global Distribution of IUCN Red List Species",
                 theme = shinytheme("flatly"),
                 
                 # Home page panel               
                 tabPanel("About",
                          
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(tags$img(src = "gorilla-baby.jpg", align = "center", width="100%")
                              ),
                              mainPanel(
                                br(),
                                h2("Examine the relationship between human population growth and the global distribution of IUCN Red List species (Critically Endangered, Endangered and Vulnerable)."), 
                                h3("The goal of this app is to help visualize how many and what class of listed species are located in each country compared to historical and projected population growth."),
                                br(),
                                br(),
                                strong("Our app will utilize three datasets:"),
                                p(tags$ol("1) International Union for the Conservation of Nature (IUCN) - Number of  species listed as threatened on the IUCN Red List for each country. Species counts within country are further subdivided by family (IUCN Red List of Threatened Species 2018).")),
                                p(tags$ol("2) United Nations (UN) - Total population (including both sexes) by region, subregion, and country, annually for 1950-2100 (United Nations 2017).")),
                                p(tags$ol("3) United Nations (UN) - Average annual rates of population change (percent change) by region, subregion, and country for 1950-2100 (United Nations 2017).")),
                                br(),
                                br(),
                                p(em("Creator: Rachel Kenny")
                                ))
                              
                            )
                          )
                 ),
                 
                 # Map panel
                 tabPanel("Population over time",
                          titlePanel("Distribution of Worldwide Population"),
                          h5("Move the slider to see how the distribution of global population has changed since 2000 and is projected to change by 2050."),
                    
                          sidebarLayout(
                            sidebarPanel(sliderInput("year", "Year",
                                                     min = 2000, max = 2050,
                                                     value = 2000, step = 10)),
                            mainPanel(
                              imageOutput(outputId = "map")
                            )
                          )
                          
                 ),
                 
                 # Bar graph panel
                 tabPanel("Threatened species by country",
                          titlePanel("Number of IUCN Threatened Species by Country"),
                          h5("Select two countries to compare the number of IUCN listed threatened species in each. It's important to note that the number of listed species depends on the availability of species distribution data."),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country_1", "Country 1",
                                          choices = c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia", "Western Sahara", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, The Democratic Republic of the", "Djibouti", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mayotte", "Mozambique", "Namibia", "Niger", "Nigeria", "Réunion", "Rwanda", "Saint Helena, Ascension and Tristan da Cunha", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania, United Republic of", "Togo", "Uganda", "Zambia", "Zimbabwe", "Antarctica", "Bouvet Island", "French Southern Territories", "Heard Island and McDonald Islands", "South Georgia and the South Sandwich Islands", "China", "Hong Kong", "Japan", "Korea, Republic of", "Macao", "Mongolia", "Taiwan, Province of China", "Belarus", "Moldova", "Russian Federation", "Ukraine", "Bangladesh", "Bhutan", "British Indian Ocean Territory", "Brunei Darussalam", "Cambodia", "Disputed Territory", "India", "Indonesia", "Malaysia", "Maldives", "Myanmar", "Nepal", "Philippines", "Singapore", "Sri Lanka", "Thailand", "Timor-Leste", "Viet Nam", "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Cyprus", "Georgia", "Iran, Islamic Republic of", "Iraq", "Israel", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Lebanon", "Oman", "Pakistan", "Palestine, State of", "Qatar", "Saudi Arabia", "Syrian Arab Republic", "Tajikistan", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Yemen", "Albania", "Andorra", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "Germany", "Gibraltar", "Greece", "Greenland", "Guernsey", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jersey", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia, the former Yugoslav Republic of", "Malta", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Svalbard and Jan Mayen", "Sweden", "Switzerland", "United Kingdom", "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Bermuda", "Bonaire, Sint Eustatius and Saba", "Cayman Islands", "Cuba", "Curaçao", "Dominica", "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Montserrat", "Puerto Rico", "Saint Barthélemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands", "Virgin Islands, British", "Virgin Islands, U.S.", "Canada", "Saint Pierre and Miquelon", "United States", "Argentina", "Bolivia, Plurinational States of", "Brazil", "Chile", "Colombia", "Ecuador", "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela, Bolivarian Republic of", "American Samoa", "Australia", "Christmas Island", "Cook Islands", "Fiji", "French Polynesia", "Guam", "Kiribati", "Marshall Islands", "Micronesia, Federated States of", "Nauru", "New Caledonia", "New Zealand", "Niue", "Norfolk Island", "Northern Mariana Islands", "Palau", "Papua New Guinea", "Pitcairn", "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "United States Minor Outlying Islands", "Vanuatu", "Wallis and Futuna"),
                                          selected = "China",
                                          multiple = FALSE),
                              selectInput("country_2", "Country 2",
                                          choices = c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia", "Western Sahara", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, The Democratic Republic of the", "Djibouti", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mayotte", "Mozambique", "Namibia", "Niger", "Nigeria", "Réunion", "Rwanda", "Saint Helena, Ascension and Tristan da Cunha", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania, United Republic of", "Togo", "Uganda", "Zambia", "Zimbabwe", "Antarctica", "Bouvet Island", "French Southern Territories", "Heard Island and McDonald Islands", "South Georgia and the South Sandwich Islands", "China", "Hong Kong", "Japan", "Korea, Republic of", "Macao", "Mongolia", "Taiwan, Province of China", "Belarus", "Moldova", "Russian Federation", "Ukraine", "Bangladesh", "Bhutan", "British Indian Ocean Territory", "Brunei Darussalam", "Cambodia", "Disputed Territory", "India", "Indonesia", "Malaysia", "Maldives", "Myanmar", "Nepal", "Philippines", "Singapore", "Sri Lanka", "Thailand", "Timor-Leste", "Viet Nam", "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Cyprus", "Georgia", "Iran, Islamic Republic of", "Iraq", "Israel", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Lebanon", "Oman", "Pakistan", "Palestine, State of", "Qatar", "Saudi Arabia", "Syrian Arab Republic", "Tajikistan", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Yemen", "Albania", "Andorra", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "Germany", "Gibraltar", "Greece", "Greenland", "Guernsey", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jersey", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia, the former Yugoslav Republic of", "Malta", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Svalbard and Jan Mayen", "Sweden", "Switzerland", "United Kingdom", "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Bermuda", "Bonaire, Sint Eustatius and Saba", "Cayman Islands", "Cuba", "Curaçao", "Dominica", "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Montserrat", "Puerto Rico", "Saint Barthélemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands", "Virgin Islands, British", "Virgin Islands, U.S.", "Canada", "Saint Pierre and Miquelon", "United States", "Argentina", "Bolivia, Plurinational States of", "Brazil", "Chile", "Colombia", "Ecuador", "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela, Bolivarian Republic of", "American Samoa", "Australia", "Christmas Island", "Cook Islands", "Fiji", "French Polynesia", "Guam", "Kiribati", "Marshall Islands", "Micronesia, Federated States of", "Nauru", "New Caledonia", "New Zealand", "Niue", "Norfolk Island", "Northern Mariana Islands", "Palau", "Papua New Guinea", "Pitcairn", "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "United States Minor Outlying Islands", "Vanuatu", "Wallis and Futuna"),
                                          selected = "Ghana",
                                          multiple = FALSE)
                            ),
                            mainPanel(
                              plotOutput(outputId = "bargraph")
                            )
                          )
                          
                 ),
                 
                 
                 # Scatterplot panel
                 tabPanel("Exploring countries at risk",
                          titlePanel("Countries At Risk"),
                          h5("First select a continent you want to explore and a species class that you're interested in. Finally select one of three variants (high, low, medium) of population growth rate. Hover over each point to see details for each country."),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              
                              #Drop-down menu to choose continent of interest
                              radioButtons("continent",
                                           label = "Select a Continent:",
                                           choices = c(Africa = "Africa", Asia = "Asia", Europe = "Europe", 'North America' = "North America", Oceania = "Oceania", 'South America' = "South America"),
                                           selected = "Africa"),
                              
                              #Drop-down menu to choose species class (X axis)
                              selectInput("x", "Species Class",
                                          choices = c("Mammals", "Birds", "Reptiles", "Amphibians", "Fishes", "Molluscs", "All"),
                                          selected = "All",
                                          multiple = FALSE),
                              
                              #Drop-down menu to choose growth rate variant (Y axis)
                              selectInput("y", "Growth Rate Variant",
                                          choices = c("Low", "Medium", "High"),
                                          selected = "High",
                                          multiple = FALSE)
                            ),
                            mainPanel(
                              plotlyOutput(outputId = "scatterplot"),
                              verbatimTextOutput("hover")
                            )
                          )
                          
                 )
                 
                 
                 )



# Define server logic
server <- function(input, output) {
  
  scatterplot_filtered <- reactive({
    scatterplot_df1 %>%
      filter(Continent == input$continent)
  })
  
  bio_df <- reactive({
    bio %>%
      filter(Country == input$country_1 | Country == input$country_2)
  })
  
  # Generate plotly scatterplot of requested variables
  output$scatterplot <- renderPlotly({
    
    
    
    plot <- ggplot(data = scatterplot_df1, 
                   aes_string(x = input$x, y = input$y)) +
      geom_point(aes(text = paste("Country:", Country)), show.legend = TRUE,
                 size = 4, alpha = 0.7) +
      geom_point(data=scatterplot_filtered(), aes(text = paste("Country:", Country)), colour="turquoise1", alpha = .7, size=3)+
      labs(x = "Number of IUCN Listed Threatened Species",
           y = "2050 Projected Rate of Population Increase (%)") +
      scale_fill_brewer(palette="Paired") +
      theme(panel.grid.major = element_line(color = gray(0.5), 
                                            linetype = "blank", 
                                            size = 0.5), 
            panel.background = element_rect(fill = "white"),
            axis.title = element_text(face="bold"), title = element_text(face="bold"))+
      theme_classic()
    
    require(plotly)
    ggplotly(plot, tooltip = c("text", "x", "y"))
    
  })
  
  # Generate  bar graph of requested variables
  output$bargraph <- renderPlot({
    ggplot(data = bio_df(), 
           aes(x = bio_df()$species_type, y = bio_df()$thr_count, fill = bio_df()$Country)) +
      geom_bar(stat="identity", position="dodge") +
      labs(x = "Species Type", y = "Count") +
      theme(panel.grid.major = element_line(color = gray(0.5), 
                                            linetype = "blank", size = 0.5), 
            panel.background = element_rect(fill = "white"))+
      theme_classic()+
      theme(axis.title = element_text(face="bold"), title = element_text(face="bold"))+
      scale_fill_manual(values=c("turquoise3","turquoise4"))+
      scale_x_discrete(expand=c(0.15,0))+
      scale_y_continuous(expand=c(0,0))+
      labs(fill="Country")
  })
  
  output$map <- renderImage({
    if (input$year == 2000) 
    {
      return(list(
        src = "plot2000.png",
        contentType = "image/png",
        alt = "2000"
      ))
    } 
    if (input$year == 2010)
    {
      return(list(
        src = "plot2010.png",
        contentType = "image/png",
        alt = "2010"
      ))
    } 
    if (input$year == 2020)
    {
      return(list(
        src = "plot2020.png",
        contentType = "image/png",
        alt = "2020"
      ))
    } 
    if (input$year == 2030)
    {
      return(list(
        src = "plot2030.png",
        contentType = "image/png",
        alt = "2030"
      ))
    } 
    if (input$year == 2040)
    {
      return(list(
        src = "plot2040.png",
        contentType = "image/png",
        alt = "2040"
      ))
    } 
    else if(input$year == 2050)
    {
      return(list(
        src = "plot2050.png",
        contentType = "image/png",
        alt = "2050"
      ))
    } 
  } 
  , deleteFile = FALSE)
  
}





# Run the application 
shinyApp(ui = ui, server = server)



