library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(treemap)
library(tidyverse)
library(maps)
library(plotly)

# Load the data
shark_tank_data <- read.csv("./Shark Tank US dataset.csv")
data <- rename(shark_tank_data, Pitchers_State = `Pitchers.State`)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Shark Tank Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sharks' Analysis", tabName = "Sharkcomprehensive", icon = icon("dashboard")),
      menuItem("Pitchers' Analysis", tabName = "pitchercomprehensive", icon = icon("map")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Sharkcomprehensive",
              titlePanel("Sharks' Analysis"),
              tabsetPanel(
                tabPanel("Industry Analysis",
                         fluidRow(
                           column(12, 
                                  sliderInput("seasonRangeIndustry", "Select Season Range:",
                                              min = min(data$Season.Number, na.rm = TRUE), 
                                              max = max(data$Season.Number, na.rm = TRUE), 
                                              value = c(min(data$Season.Number, na.rm = TRUE), max(data$Season.Number, na.rm = TRUE)),
                                              step = 1)
                           )
                         ),
                         fluidRow(
                           column(6, 
                                  selectInput("shark1", "Select Shark 1:",
                                              choices = c("Overall", "Mark Cuban", "Barbara Corcoran", "Lori Greiner", "Robert Herjavec", "Daymond John", "Kevin O'Leary")),
                                  plotOutput("plot1")
                           ),
                           column(6,
                                  selectInput("shark2", "Select Shark 2:",
                                              choices = c("Overall", "Mark Cuban", "Barbara Corcoran", "Lori Greiner", "Robert Herjavec", "Daymond John", "Kevin O'Leary")),
                                  plotOutput("plot2")
                           )
                         )
                ),
                tabPanel("Gender Analysis",
                         fluidRow(
                           column(6, 
                                  selectInput("investorSelect1", "Choose Shark 1 ",
                                              choices = c("Mark Cuban", "Lori Greiner", "Barbara Corcoran", "Robert Herjavec", "Daymond John", "Kevin O'Leary")),
                                  plotOutput("investmentPlot1")
                           ),
                           column(6,
                                  selectInput("investorSelect2", "Choose Shark 2",
                                              choices = c("Mark Cuban", "Lori Greiner", "Barbara Corcoran", "Robert Herjavec", "Daymond John", "Kevin O'Leary")),
                                  plotOutput("investmentPlot2")
                           )
                         )
                ),
                tabPanel("Shark Tank Investments by State",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("sharkSelect", "Select Shark:",
                                         choices = c("Mark Cuban", "Barbara Corcoran", "Lori Greiner", "Robert Herjavec", "Daymond John", "Kevin O Leary"),
                                         width = "100%"
                             )
                           ),
                           mainPanel(
                             plotlyOutput("choroplethMap"),
                             width = 12
                           )
                         )),
                tabPanel("Viewership Analysis",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput("investors", "Choose Investors:",
                                                choices = c("Overall", "Mark Cuban", "Lori Greiner", "Barbara Corcoran", "Robert Herjavec", "Daymond John", "Kevin O'Leary"),
                                                selected = "Overall")
                           ),
                           mainPanel(
                             plotOutput("boxPlot")
                           )
                         )
                ),
                tabPanel("Deal Closure Rates Analysis",
                         fluidRow(
                           column(6, 
                                  selectInput("sharkInput2", "Choose Shark:",
                                              choices = c("Overall","Mark Cuban", "Barbara Corcoran", "Lori Greiner", "Robert Herjavec", "Daymond John", "Kevin O Leary")
                                  ),
                                  plotOutput("dealPlot2")
                           )
                         )),
                tabPanel("Investment Deals by Shark",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxInput("mark", "Mark Cuban", value = TRUE),
                             checkboxInput("kevin", "Kevin O'Leary", value = TRUE),
                             checkboxInput("daymond", "Daymond John", value = TRUE),
                             checkboxInput("robert", "Robert Herjavec", value = TRUE),
                             checkboxInput("lori", "Lori Greiner", value = TRUE),
                             checkboxInput("barbara", "Barbara Corcoran", value = TRUE)
                           ),
                           mainPanel(
                             plotOutput("violinPlot")
                           )
                         ))
              )
      ),
      tabItem(tabName = "pitchercomprehensive",
              titlePanel("Pitchers' Analysis "),
              tabsetPanel(
                tabPanel("All",
                plotOutput("barPlot", width = "100%"),  # Bar plot with full width
                checkboxInput("showPlots", "Scatterplots", value = FALSE),  # Checkbox below the bar plot
                checkboxInput("showHistogram", "Viewership Histogram", value = FALSE),  # Checkbox for Histogram
                plotOutput("scatterPlot1"),
                plotOutput("scatterPlot2"),
                plotOutput("histogramPlot")  # Output for Histogram
              ),
                
                tabPanel("Great Deal",
                         plotOutput("barPlotFiltered"),  # This will show only two specific bars
                         checkboxInput("showTreeMap", "Industry-TreeMap", value = FALSE),  # Checkbox for TreeMap
                         checkboxInput("showTreeMap2", "Sharks-TreeMap", value = FALSE),  # Checkbox for second TreeMap
                         plotOutput("treeMapPlot"),
                         plotOutput("treeMapPlot2"))
              
      )),
      tabItem(tabName = "data_table",
              titlePanel("Data Table"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("seasonInput", "Select Season:",
                              choices = unique(shark_tank_data$`Season.Number`)),
                  selectInput("episodeInput", "Select Episode:",
                              choices = NULL),  # Updated dynamically
                  checkboxGroupInput("sharkInput", "Select Sharks:",
                                     choices = c("Barbara Corcoran" = "Barbara.Corcoran.Investment.Amount",
                                                 "Mark Cuban" = "Mark.Cuban.Investment.Amount",
                                                 "Lori Greiner" = "Lori.Greiner.Investment.Amount",
                                                 "Robert Herjavec" = "Robert.Herjavec.Investment.Amount",
                                                 "Daymond John" = "Daymond.John.Investment.Amount",
                                                 "Kevin O'Leary" = "Kevin.O.Leary.Investment.Amount")),
                  width = 4
                ),
                mainPanel(
                  DTOutput("table")
                )
              ))
    )
  )
)



# Define server logic
server <- function(input, output,session) {
  observe({
    updateSelectInput(session, "episodeInput", "Select Episode:",
                      choices = unique(shark_tank_data$`Episode.Number`[shark_tank_data$`Season.Number` == input$seasonInput]))
  })
  
  # Render table based on selections
  output$table <- renderDT({
    data <- shark_tank_data %>%
      filter(`Season.Number` == input$seasonInput,
             `Episode.Number` == input$episodeInput) %>%
      filter(if (length(input$sharkInput) > 0) rowSums(.[,input$sharkInput, drop = FALSE] > 0) > 0 else TRUE)
    datatable(data, options = list(scrollX = TRUE))
  })
  
  # Server code from app1
  # Prepare barplot data
  data_bar <- shark_tank_data %>%
    filter(Got.Deal == 1) %>%
    mutate(
      AskDealAmountDifference = Original.Ask.Amount - Total.Deal.Amount,
      OfferedDealEquityDifference = Original.Offered.Equity - Total.Deal.Equity,
      Category = case_when(
        AskDealAmountDifference < 0 & OfferedDealEquityDifference < 0 ~ "More Money and More Equity",
        AskDealAmountDifference == 0 & OfferedDealEquityDifference < 0 ~ "Same Money but More Equity",
        AskDealAmountDifference > 0 & OfferedDealEquityDifference == 0 ~ "Less Money but Same Equity",
        AskDealAmountDifference == 0 & OfferedDealEquityDifference == 0 ~ "Same Money but Same Equity",
        AskDealAmountDifference == 0 & OfferedDealEquityDifference > 0 ~ "Same Money but Less Equity",
        AskDealAmountDifference > 0 & OfferedDealEquityDifference > 0 ~ "Less Money but Less Equity",
        AskDealAmountDifference < 0 & OfferedDealEquityDifference == 0 ~ "More Money but Same Equity",
        AskDealAmountDifference > 0 & OfferedDealEquityDifference < 0 ~ "Less Money but More Equity",
        TRUE ~ "Other Combinations"
      )
    ) %>%
    group_by(Category) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  # Prepare scatterplot data
  deal_data_both <- shark_tank_data[shark_tank_data$Got.Deal == 1, ]
  
  # Calculate the correlation coefficients for use in plot titles
  cor_amount <- cor(deal_data_both$Original.Ask.Amount, deal_data_both$Total.Deal.Amount, use = "complete.obs")
  cor_equity <- cor(deal_data_both$Original.Offered.Equity, deal_data_both$Total.Deal.Equity, use = "complete.obs")
  
  output$barPlot <- renderPlot({
    ggplot(data_bar, aes(x=reorder(Category, -Count), y=Count, fill=Category)) +
      geom_bar(stat="identity", color="black") +
      geom_text(aes(label=Count), vjust=-0.3, size=3.5) +
      theme_minimal() +
      labs(title="Sorted Distribution of Negotiation Outcomes in Shark Tank Pitches (Deals Only)",
           x="Categories", y="Number of Instances") +
      theme(axis.text.x = element_text(angle=65, hjust=1, size=12),  # Increased angle and adjusted size
            axis.title.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.y = element_text(size=14),
            plot.margin = unit(c(1,1,1,1), "cm"))+ 
      scale_fill_manual(values = c(
        "More Money and More Equity" = "yellow",
        "Same Money but More Equity" = "red",
        "Less Money but Same Equity" = "red",
        "Same Money but Same Equity" = "green",
        "Same Money but Less Equity" = "green",
        "Less Money but Less Equity" = "yellow",
        "More Money but Same Equity" = "green",
        "Less Money but More Equity" = "red",
        "Other Combinations" = "white"
      ))+
      annotate("text", x = "Same Money but Less Equity", y = max(data_bar$Count) * 1.05, label = "★", size = 8, color = "#FFA500") +
      annotate("text", x = "More Money but Same Equity", y = max(data_bar$Count) * 1.05, label = "★", size = 8, color = "#FFA500")+
      annotate("text", x = "Same Money but Less Equity", y = max(data_bar$Count) * 1.25, label = "Great deal", size = 5, color = "#FFA500")+
      annotate("text", x = "More Money but Same Equity", y = max(data_bar$Count) * 1.25, label = "Great deal", size = 5, color = "#FFA500")
  })
  
  # filtered barplot for great deal 
  output$barPlotFiltered <- renderPlot({
    data_filtered <- data_bar %>%
      filter(Category %in% c("Less Money but More Equity", "More Money but Same Equity"))
    
    ggplot(data_filtered, aes(x=reorder(Category, -Count), y=Count, fill=Category)) +
      geom_bar(stat="identity", color="black") +
      geom_text(aes(label=Count), vjust=-0.3, size=5.5) +  # Increase size here for bar labels
      theme_minimal() +
      labs(title="Focused Analysis on Specific Deal Types",
           x="Categories", y="Number of Instances") +
      scale_fill_manual(values = c("Less Money but More Equity" = "green", "More Money but Same Equity" = "green")) +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title font size
        axis.title.x = element_text(size = 16),  # Increase x-axis title font size
        axis.title.y = element_text(size = 16),  # Increase y-axis title font size
        axis.text.x = element_text(size = 14),  # Increase x-axis text font size
        axis.text.y = element_text(size = 14)   # Increase y-axis text font size
      )
  })
  
  output$scatterPlot1 <- renderPlot({
    if(input$showPlots) {  # Conditional rendering based on single checkbox
      ggplot(deal_data_both, aes(x = Original.Ask.Amount, y = Total.Deal.Amount)) +
        geom_point(color = 'purple') +
        geom_smooth(method = 'lm', formula = y ~ x, color = 'black', linetype = 'dashed') +
        ggtitle(paste("Original Ask Amount vs Total Deal Amount for Deals Made\nCorrelation:", round(cor_amount, 2))) +
        xlab('Original Ask Amount') +
        ylab('Total Deal Amount') +
        theme_minimal()
    }
  })
  
  output$scatterPlot2 <- renderPlot({
    if(input$showPlots) {  # Conditional rendering based on single checkbox
      ggplot(deal_data_both, aes(x = Original.Offered.Equity, y = Total.Deal.Equity)) +
        geom_point(color = 'blue') +
        geom_smooth(method = 'lm', formula = y ~ x, color = 'black', linetype = 'dashed') +
        ggtitle(paste("Original Offered Equity vs Total Deal Equity for Deals Made\nCorrelation:", round(cor_equity, 2))) +
        xlab('Original Offered Equity') +
        ylab('Total Deal Equity') +
        theme_minimal()
    }
  })
  
  output$treeMapPlot <- renderPlot({
    if(input$showTreeMap) {  # Conditional rendering based on the new checkbox
      # Define the conditions for "Same Money but Less Equity" and "More Money but Same Equity"
      condition_same_money_less_equity <- shark_tank_data %>%
        filter((Original.Ask.Amount - Total.Deal.Amount == 0) & 
                 (Original.Offered.Equity - Total.Deal.Equity > 0))
      
      condition_more_money_same_equity <- shark_tank_data %>%
        filter((Original.Ask.Amount - Total.Deal.Amount < 0) & 
                 (Original.Offered.Equity - Total.Deal.Equity == 0))
      
      # Extract the industry information for the filtered lists
      industry_same_money_less_equity <- condition_same_money_less_equity %>%
        select(Startup.Name, Industry)
      
      industry_more_money_same_equity <- condition_more_money_same_equity %>%
        select(Startup.Name, Industry)
      
      # Combine the industry data into one data frame
      combined_industry_data <- bind_rows(industry_same_money_less_equity, industry_more_money_same_equity)
      
      # Count the occurrences of each industry
      industry_counts <- combined_industry_data %>%
        count(Industry)
      
      # Calculate the percentage
      industry_counts <- industry_counts %>%
        mutate(Percentage = n / sum(n) * 100,
               Label = paste(Industry, "\n", n, " (", round(Percentage, 2), "%)", sep = ""))
      
      # Create treemap with number of instances and percentage
      treemap(industry_counts,
              index = "Label",
              vSize = "n",
              title = "Treemap of Industries for Filtered Pitches",
              palette = "Set3",
              fontsize.labels = 12,
              border.col = "white",
              bg.labels = "transparent",
              position.legend = "none")
    }
  })
  
  output$treeMapPlot2 <- renderPlot({
    if(input$showTreeMap2) {  # Conditional rendering based on the new checkbox for the second treemap
      # Create a long format data frame with shark names
      
      
      # Define the conditions for "Same Money but Less Equity" and "More Money but Same Equity"
      condition_same_money_less_equity <- shark_tank_data %>%
        filter((Original.Ask.Amount - Total.Deal.Amount == 0) & 
                 (Original.Offered.Equity - Total.Deal.Equity > 0))
      
      condition_more_money_same_equity <- shark_tank_data %>%
        filter((Original.Ask.Amount - Total.Deal.Amount < 0) & 
                 (Original.Offered.Equity - Total.Deal.Equity == 0))
      
      # Extract the industry information for the filtered lists
      investment_columns <- c("Startup.Name", "Industry", 
                              "Barbara.Corcoran.Investment.Amount", 
                              "Mark.Cuban.Investment.Amount", 
                              "Lori.Greiner.Investment.Amount", 
                              "Robert.Herjavec.Investment.Amount", 
                              "Daymond.John.Investment.Amount", 
                              "Kevin.O.Leary.Investment.Amount")
      
      industry_same_money_less_equity <- condition_same_money_less_equity %>%
        select(all_of(investment_columns))
      
      industry_more_money_same_equity <- condition_more_money_same_equity %>%
        select(all_of(investment_columns))
      
      # Combine the industry data into one data frame
      combined_shark_data <- bind_rows(industry_same_money_less_equity, industry_more_money_same_equity)
      
      shark_long_data <- combined_shark_data %>%
        pivot_longer(cols = starts_with("Barbara.Corcoran.Investment.Amount"):starts_with("Kevin.O.Leary.Investment.Amount"),
                     names_to = "Shark",
                     values_to = "Investment.Amount") %>%
        filter(Investment.Amount > 0) %>%
        count(Shark)
      
      # Rename the shark names to be more readable
      shark_long_data$Shark <- recode(shark_long_data$Shark, 
                                      `Barbara.Corcoran.Investment.Amount` = "Barbara Corcoran",
                                      `Mark.Cuban.Investment.Amount` = "Mark Cuban",
                                      `Lori.Greiner.Investment.Amount` = "Lori Greiner",
                                      `Robert.Herjavec.Investment.Amount` = "Robert Herjavec",
                                      `Daymond.John.Investment.Amount` = "Daymond John",
                                      `Kevin.O.Leary.Investment.Amount` = "Kevin O'Leary")
      
      # Create labels with the number of deals
      shark_long_data <- shark_long_data %>%
        mutate(Label = paste(Shark, "\n", n, " deals", sep = ""))
      
    # Create treemap with the number of deals each shark invested in
      treemap(shark_long_data,
              index = "Label",
              vSize = "n",
              title = "Treemap of Number of Deals Each Shark Invested In",
              palette = "Set3",
              fontsize.labels = 12,
              border.col = "white",
              bg.labels = "transparent",
              position.legend = "none",
              align.labels = list(c("center", "center")),
              fontcolor.labels = "black",
              fontface.labels = 2,
              fontfamily.labels = "sans",
              draw.labels = TRUE)
    }
  })
  
  output$histogramPlot <- renderPlot({
    if(input$showHistogram) {  # Conditional rendering based on the new checkbox for the histogram
      # Extract relevant columns: "Got Deal" and "US Viewership"
      viewership_data <- shark_tank_data %>% select(Got.Deal, US.Viewership)
      
      # Drop rows with missing values in the relevant columns
      viewership_data <- viewership_data %>% drop_na(Got.Deal, US.Viewership)
      
      # Convert the 'Got Deal' column to integer type
      viewership_data$Got.Deal <- as.integer(viewership_data$Got.Deal)
      
      # Separate the data into two categories based on the 'Got Deal' column
      got_deal <- viewership_data %>% filter(Got.Deal == 1)
      no_deal <- viewership_data %>% filter(Got.Deal == 0)
      
      # Calculate statistics for got_deal
      stats_deal <- got_deal %>% 
        summarise(Mean = mean(US.Viewership),
                  Median = median(US.Viewership),
                  Max = max(US.Viewership),
                  Min = min(US.Viewership))
      
      # Calculate statistics for no_deal
      stats_no_deal <- no_deal %>% 
        summarise(Mean = mean(US.Viewership),
                  Median = median(US.Viewership),
                  Max = max(US.Viewership),
                  Min = min(US.Viewership))
      
      # Create histogram plot
      p <- ggplot() +
        geom_histogram(data = got_deal, aes(x = US.Viewership), bins = 20, alpha = 0.7, fill = 'lightblue') +
        geom_histogram(data = no_deal, aes(x = US.Viewership), bins = 20, alpha = 0.7, fill = 'orange') +
        labs(title = 'US Viewership for Got Deal vs No Deal', x = 'US Viewership', y = 'Count') +
        theme_minimal() +
        theme(legend.position = 'right') +
        expand_limits(y = c(0, max(c(stats_deal$Max, stats_no_deal$Max)) * 1.2))  # Ensure there is space for annotations
      
      # Annotate statistics for got_deal
      p <- p + annotate("text", x = Inf, y = Inf, label = paste("Deal:\nMean:", round(stats_deal$Mean, 2),
                                                                "\nMedian:", round(stats_deal$Median, 2),
                                                                "\nMax:", stats_deal$Max,
                                                                "\nMin:", stats_deal$Min), vjust = 1.5, hjust = 1.5, color = "lightblue", size = 4)
      
      # Annotate statistics for no_deal
      p <- p + annotate("text", x = -Inf, y = Inf, label = paste("No Deal:\nMean:", round(stats_no_deal$Mean, 2),
                                                                 "\nMedian:", round(stats_no_deal$Median, 2),
                                                                 "\nMax:", stats_no_deal$Max,
                                                                 "\nMin:", stats_no_deal$Min), vjust = 4, hjust = -0.5, color = "orange", size = 4)
      
      return(p)
    }
  })
  
  
  
  # Violin plot for Investment Deals by Shark
  output$violinPlot <- renderPlot({
    sharks <- c("mark", "kevin", "daymond", "robert", "lori", "barbara")
    shark_names <- c("Mark Cuban", "Kevin O'Leary", "Daymond John", "Robert Herjavec", "Lori Greiner", "Barbara Corcoran")
    shark_columns <- c("Mark.Cuban", "Kevin.O.Leary", "Daymond.John", "Robert.Herjavec", "Lori.Greiner", "Barbara.Corcoran")
    colors <- c("lightblue", "orange", "purple", "lightpink", "lightyellow", "lightgreen")
    
    data_list <- lapply(1:6, function(i) {
      if (input[[sharks[i]]]) {
        deals <- shark_tank_data[shark_tank_data[[paste0(shark_columns[i], ".Present")]] == 1 & shark_tank_data$Got.Deal == 1,]
        deals$Shark <- shark_names[i]
        deals$Investment.Amount <- deals[[paste0(shark_columns[i], ".Investment.Amount")]]
        deals$Color <- colors[i]
        return(deals)
      }
    })
    
    combined_data <- bind_rows(data_list)
    ggplot(combined_data, aes(x = Shark, y = Investment.Amount, fill = Shark)) +
      geom_violin(trim = FALSE, aes(color = Shark)) +
      scale_fill_manual(values = setNames(colors, shark_names)) +
      scale_color_manual(values = setNames(colors, shark_names)) +
      theme_minimal() +
      labs(title = "Violin Plot of Deal Amounts by Shark",
           x = "",
           y = "Deal Amount ($)") +
      scale_y_continuous(labels = scales::dollar_format())
  })
  
  # Deal closure analysis
  
 plot_deal_closure_rate <- function(shark_name, data) {
  if (shark_name != "Overall" && nzchar(shark_name)) {
    # Format the shark name to match column names in the dataset
    shark_name_formatted <- gsub(" ", ".", shark_name)
    shark_present_field <- paste0(shark_name_formatted, ".Present")
    shark_investment_field <- paste0(shark_name_formatted, ".Investment.Amount")

    # Check if the dynamically created column names exist in the dataset
    if (shark_present_field %in% names(data) && shark_investment_field %in% names(data)) {
      total_pitches_present <- sum(data[[shark_present_field]], na.rm = TRUE)
      deals_closed <- sum(data$Got.Deal == 1 & data[[shark_present_field]] == 1, na.rm = TRUE)
    } else {
      total_pitches_present <- 0
      deals_closed <- 0
    }
  } else {
    # For "Overall" - aggregate across all sharks
    sharks_present_cols <- grep(".Present$", names(data), value = TRUE)
    total_pitches_present <- sum(rowSums(data[, sharks_present_cols, drop = FALSE] == 1, na.rm = TRUE) > 0)
    deals_closed <- sum(data$Got.Deal == 1 & rowSums(data[, sharks_present_cols, drop = FALSE] == 1, na.rm = TRUE) > 0)
  }

  plot_data <- data.frame(
    Category = c("Total Pitches", "Deals Closed"),
    Count = c(total_pitches_present, deals_closed)
  )

  percentage_closure <- ifelse(total_pitches_present > 0, sprintf("%.f%%", deals_closed / total_pitches_present * 100), "N/A")

  ggplot(plot_data, aes(x = Category, y = Count, fill = Category)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = paste(Count, "(", percentage_closure, ")", sep = "")), vjust = -0.5) +
    ggtitle(paste(shark_name, "Deal Closure on Shark Tank")) +
    ylab("Number of Pitches") +
    theme_minimal()
}
  
  
  # Output for the second selected shark
  output$dealPlot2 <- renderPlot({
    plot_deal_closure_rate(input$sharkInput2, shark_tank_data)
  })
  
  
  
  
  
  # List of investors for gender analysis 
  investors <- list(
    list("Mark Cuban", "Mark.Cuban.Present", "Mark.Cuban.Investment.Amount"),
    list("Lori Greiner", "Lori.Greiner.Present", "Lori.Greiner.Investment.Amount"),
    list("Barbara Corcoran", "Barbara.Corcoran.Present", "Barbara.Corcoran.Investment.Amount"),
    list("Robert Herjavec", "Robert.Herjavec.Present", "Robert.Herjavec.Investment.Amount"),
    list("Daymond John", "Daymond.John.Present", "Daymond.John.Investment.Amount"),
    list("Kevin O'Leary", "Kevin.O.Leary.Present", "Kevin.O.Leary.Investment.Amount")
  )
  
  # Function to plot investments by gender for a specific investor
  plot_investments_by_gender <- function(data, investor_column_present, investment_amount_column) {
    investments <- data %>%
      filter(!!sym(investor_column_present) == 1, !is.na(!!sym(investment_amount_column))) %>%
      group_by(Pitchers.Gender) %>%
      summarise(Total.Investment = sum(!!sym(investment_amount_column), na.rm = TRUE))
    
    ggplot(investments, aes(x = Pitchers.Gender, y = Total.Investment, fill = Pitchers.Gender)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Gender", y = "Investment Amount ($)", title = paste("Investment Amount by Gender")) +
      scale_y_continuous(labels = scales::comma)  # Format y-axis labels
  }
  
  output$investmentPlot1 <- renderPlot({
    investor_info <- investors[[match(input$investorSelect1, sapply(investors, function(x) x[[1]]))]]
    plot_investments_by_gender(shark_tank_data, investor_info[[2]], investor_info[[3]])
  })
  
  
  output$investmentPlot2 <- renderPlot({
    investor_info <- investors[[match(input$investorSelect2, sapply(investors, function(x) x[[1]]))]]
    plot_investments_by_gender(shark_tank_data, investor_info[[2]], investor_info[[3]])
  })


  # Server logic for plotting investments by industry
  output$plot1 <- renderPlot({
    data_to_plot <- shark_tank_data %>%
      filter(Season.Number >= input$seasonRangeIndustry[1] & Season.Number <= input$seasonRangeIndustry[2]) %>%
      switch(input$shark1,
             "Overall" = .,
             "Mark Cuban" = filter(., Mark.Cuban.Present == 1),
             "Barbara Corcoran" = filter(., Barbara.Corcoran.Present == 1),
             "Lori Greiner" = filter(., Lori.Greiner.Present == 1),
             "Robert Herjavec" = filter(., Robert.Herjavec.Present == 1),
             "Daymond John" = filter(., Daymond.John.Present == 1),
             "Kevin O'Leary" = filter(., Kevin.O.Leary.Present == 1))
    
    ggplot(data_to_plot, aes(x=reorder(Industry, -Total.Deal.Amount), y=Total.Deal.Amount, fill=Industry)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      labs(title = paste(input$shark1, "Investment Amount by Industry"),
           x = "Industry",
           y = "Investment Amount ($)") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.y = element_text(size = 14))  # Adjust font size here
  })
  
  output$plot2 <- renderPlot({
    data_to_plot <- shark_tank_data %>%
      filter(Season.Number >= input$seasonRangeIndustry[1] & Season.Number <= input$seasonRangeIndustry[2]) %>%
      switch(input$shark2,
             "Overall" = .,
             "Mark Cuban" = filter(., Mark.Cuban.Present == 1),
             "Barbara Corcoran" = filter(., Barbara.Corcoran.Present == 1),
             "Lori Greiner" = filter(., Lori.Greiner.Present == 1),
             "Robert Herjavec" = filter(., Robert.Herjavec.Present == 1),
             "Daymond John" = filter(., Daymond.John.Present == 1),
             "Kevin O'Leary" = filter(., Kevin.O.Leary.Present == 1))
    
    ggplot(data_to_plot, aes(x=reorder(Industry, -Total.Deal.Amount), y=Total.Deal.Amount, fill=Industry)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      labs(title = paste(input$shark2, "Investment Amount by Industry"),
           x = "Industry",
           y = "Investment Amount ($)") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.y = element_text(size = 14))  # Adjust font size here
  })
  
  # Function to create and return a combined box plot for selected investors
  plot_investments <- function(data, selected_investors) {
    if ("Overall" %in% selected_investors) {
      # If "Overall" is selected, include all investors
      selected_investors <- c("Mark Cuban", "Lori Greiner", "Barbara Corcoran", "Robert Herjavec", "Daymond John", "Kevin O'Leary")
    }
    
    combined_data <- data.frame()
    for (investor_name in selected_investors) {
      investor_info <- investors[[which(sapply(investors, function(x) x[[1]]) == investor_name)]]
      investments <- data %>%
        filter(!!as.name(investor_info[[2]]) == 1, !!as.name(investor_info[[3]]) > 0) %>%
        mutate(Investor = investor_name)
      combined_data <- rbind(combined_data, investments)
    }
    
    plot <- ggplot(combined_data, aes(x = Investor, y = US.Viewership, fill = Investor)) +
      geom_boxplot() +
      labs(title = "Box Plot of US Viewership for Selected Investors' Invested Pitches", x = "Investor", y = "US Viewership (millions)") +
      theme_minimal()
    
    return(plot)
  }
  
  output$boxPlot <- renderPlot({
    selected_investors <- input$investors
    if (length(selected_investors) > 0) {
      plot_investments(shark_tank_data, selected_investors)
    }
  })
  
  
  # Server code for map
  output$choroplethMap <- renderPlotly({
    # Determine the selected shark and corresponding columns
    shark <- gsub(" ", ".", input$sharkSelect)  # Replace spaces with dots to match column names in the data frame
    if (shark == "Overall") {
      filtered_data <- data %>%
        filter(rowSums(select(., ends_with("Present"))) > 0) %>%
        mutate(Total_Investment = rowSums(select(., ends_with("Investment.Amount")), na.rm = TRUE))
    } else {
      shark_present_col <- paste(shark, "Present", sep = ".")
      shark_investment_col <- paste(shark, "Investment.Amount", sep = ".")
      filtered_data <- data %>%
        filter(!!as.name(shark_present_col) == 1, !!as.name(shark_investment_col) > 0) %>%
        mutate(Total_Investment = !!as.name(shark_investment_col))
    }
    
    # Summarize the investment amounts by state
    state_investments <- filtered_data %>%
      group_by(Pitchers_State) %>%
      summarise(Total_Investment = sum(Total_Investment))
    
    # Determine global min and max investment amounts for consistent color scaling
    global_min <- min(data %>% mutate(Total_Investment = rowSums(select(., ends_with("Investment.Amount")), na.rm = TRUE)) %>% pull(Total_Investment), na.rm = TRUE)
    global_max <- max(data %>% mutate(Total_Investment = rowSums(select(., ends_with("Investment.Amount")), na.rm = TRUE)) %>% pull(Total_Investment), na.rm = TRUE)
    
    # Load USA states data
    states <- map_data("state")
    
    # Create a manual mapping of state names to abbreviations if not already present
    if (!"abbr" %in% names(states)) {
      states$abbr <- state.abb[match(states$region, tolower(state.name))]
    }
    
    # Ensure that the regions are in the correct case, if necessary
    states$region <- tolower(states$region)
    
    # Merge state investments with the state data, ensure columns match
    merged_data <- merge(state_investments, states, by.x = 'Pitchers_State', by.y = 'abbr', all.x = TRUE)
    
    # Create a choropleth map
    fig <- plot_geo(merged_data, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Total_Investment,
        locations = ~Pitchers_State,
        text = ~paste('State:', Pitchers_State, '<br>Total Investment:', Total_Investment),
        colors = 'Reds',
        zmin = global_min,  # Set minimum value for color scale
        zmax = global_max   # Set maximum value for color scale
      ) %>%
      layout(
        title = paste(gsub("\\.", " ", shark), 'Investments by State'),
        geo = list(scope = 'usa', projection = list(type = 'albers usa'))
      )
    fig
  }) 
}

# Run the application
shinyApp(ui = ui, server = server)
