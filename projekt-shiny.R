#install.packages("pacman")

pacman::p_load(shiny, shinydashboard, MASS, DT, moments, dplyr, corrplot, 
               rpart, rpart.plot)

dane <- subset(Pima.tr, select = -c(npreg, skin, ped))
colnames(dane) <- c("Glukoza", "Ciśnienie", "BMI", "Wiek", "Cukrzyca")
dane2 <- dane %>%
  mutate(Cukrzyca = ifelse(Cukrzyca == "Yes", 1, 0))
model <- rpart(Cukrzyca~., dane)

ui <- fluidPage(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "Projekt zaliczeniowy"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Strona startowa", tabName = "strona", icon = icon("jedi-order")),
        menuItem("Dane", tabName = "dane", icon = icon("book-journal-whills"), startExpanded = FALSE,
          menuSubItem("Tabela", tabName = "tabela", icon = icon("table")),
          menuSubItem("Statystyki", tabName = "statystyki", icon = icon("book")),
          menuSubItem("Wykresy", tabName = "wykresy", icon = icon("chart-simple"))),
        menuItem("Wyniki", tabName = "wyniki", icon = icon("flag-checkered"), startExpanded = FALSE,
          menuSubItem("Drzewo", tabName = "drzewo", icon = icon("tree")),
          menuSubItem("Prognoza", tabName = "prognoza", icon = icon("dice")))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "strona",
                h1("Programowanie w R"),
                h3("Projekt: Drzewo klasyfikacyjne"),
                h5("Dariusz Różycki")),
        tabItem(tabName = "tabela",
                box(width = 10, status = "success", solidHeader = TRUE,
                    collapsible = TRUE, title = "Tabela z danymi",
                    DTOutput("table"))),
        tabItem(tabName = "statystyki",
                box(title = "Statystyki opisowe", solidHeader = TRUE,
                    collapsible = TRUE, status = "success",
                    column(12, align="center", tableOutput("statistics"))),
                box(title = "Dostępne opcje", solidHeader = TRUE,
                    collapsible = TRUE, status = "warning",
                    selectInput("variable", "Wybierz zmienną:", 
                    choices = setdiff(names(dane), "Cukrzyca")))),
        tabItem(tabName = "wykresy",
                box(title = "Wykres", status = "success", solidHeader = TRUE,
                    collapsible = TRUE, plotOutput("plot")),
                box(title = "Dostępne opcje", solidHeader = TRUE,
                    collapsible = TRUE, status = "warning",
                    selectInput("variable2", "Wybierz zmienną:", 
                                choices = setdiff(names(dane), "Cukrzyca")),
                    selectInput("type", "Wybierz typ wykresu:",
                                choices = c("Histogram", "Wykres pudełkowy", 
                                            "Wykres gęstości", "Macierz korelacji")),
                    conditionalPanel("input.type == 'Histogram'",
                                     sliderInput("bins", "Liczba przedziałów histogramu:",
                                                 min = 5, max = 20, value = 10)),
                    conditionalPanel("input.type == 'Wykres pudełkowy'",
                                     checkboxInput("horizontal", "Obróć wykres pudełkowy", TRUE)),
                    conditionalPanel("input.type == 'Wykres gęstości'",
                                     checkboxInput("fill", "Wypełnij pole pod krzywą gęstości", FALSE)),
                    conditionalPanel("input.type == 'Macierz korelacji'",
                                     radioButtons("method", "Wybierz metodę wyświetlania macierzy korelacji:", 
                                                  c("circle", "square", "ellipse", "number", "shade", "color", "pie"))))),
        tabItem(tabName = "drzewo",
                box(width = 10, title = "Drzewo klasyfikacyjne dla zmiennej Cukrzyca",
                    solidHeader = TRUE, collapsible = TRUE, status = "success",
                    plotOutput("plot2", height = "80vh"),
                    height = "90vh")),
        tabItem(tabName = "prognoza",
                box(title = "Ustawianie wartości parametrów", status = "warning",
                    solidHeader = TRUE, collapsible = TRUE,
                    sliderInput("p_glu", "Glukoza:", min = min(dane$Glukoza),
                                max = max(dane$Glukoza), value = median(dane$Glukoza),
                                step = 1),
                    sliderInput("p_bp", "Ciśnienie:", min = min(dane$Ciśnienie),
                                max = max(dane$Ciśnienie), value = median(dane$Ciśnienie),
                                step = 1),
                    sliderInput("p_bmi", "BMI:", min = round(min(dane$BMI)),
                                max = round(max(dane$BMI)), value = median(dane$BMI),
                                step = 1),
                    sliderInput("p_age", "Wiek:", min = min(dane$Wiek),
                                max = max(dane$Wiek), value = median(dane$Wiek),
                                step = 1),
                    column(12, actionButton("button", "Prognozuj", 
                                            class = "btn btn-info"), align = "center")),
                box(title = "Prawdopodobieństwo dla zmiennej Cukrzyca", status = "success", 
                    solidHeader = TRUE, collapsible = TRUE,
                    column(12, align = "center", tableOutput("table2"))))
      )
    )
  )
)

server <- function(input, output) {
  
  output$table <- renderDT({
    datatable(dane, options = list(
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ))
  })
  
  output$statistics <- renderTable({
    data.frame(cbind("Statystyka" = c("Minimum", "Kwartyl dolny", "Mediana", 
                                      "Kwartyl górny","Maksimum", "Średnia",
                                      "Odchylenie standardowe", "Skośność", "Kurtoza"),
                     "Wartość" = c(round(min(dane[[input$variable]]), 2),
                                   round(unname(quantile(dane[[input$variable]], 0.25)), 2),
                                   round(median(dane[[input$variable]]), 2),
                                   round(unname(quantile(dane[[input$variable]], 0.75)), 2),
                                   round(max(dane[[input$variable]]), 2),
                                   round(mean(dane[[input$variable]]), 2),
                                   round(sd(dane[[input$variable]]), 2),
                                   round(skewness(dane[[input$variable]]), 2),
                                   round(kurtosis(dane[[input$variable]]), 2))))
  }, align = "lr")
  
  output$plot <- renderPlot({
    if (input$type == "Histogram") {
      hist(dane[[input$variable2]], main = paste("Histogram zmiennej", input$variable2), 
           xlab = input$variable2, ylab = "Częstość", col = "gold", border = "gold4",
           breaks = input$bins)
    } else if (input$type == "Wykres pudełkowy") {
      boxplot(dane[[input$variable2]], main = paste("Wykres pudełkowy zmiennej", input$variable2), 
              xlab = input$variable2, col = "royalblue", border = "royalblue4", 
              horizontal = input$horizontal)
    } else if (input$type == "Wykres gęstości") {
      plot(density(dane[[input$variable2]]), main = paste("Wykres gęstości zmiennej", input$variable2), 
           xlab = input$variable2, ylab = "Gęstość")
      if (input$fill) {
        polygon(density(dane[[input$variable2]]), col = "seagreen4")
      }
    } else {
      cor = cor(dane2, method = "spearman")
      corrplot(cor, method = input$method, type = "upper", tl.srt = 45)
    }
  })
  
  output$plot2 <- renderPlot({
    rpart.plot(model, yesno = 2)
  })
  
  output$table2 <- renderTable({
    req(input$button)
    isolate(df <- data.frame("Glukoza" = input$p_glu, "Ciśnienie" = input$p_bp,
                     "BMI" = input$p_bmi, "Wiek" = input$p_age))
    predict(model, df)
  })
}

shinyApp(ui, server)
