# Kelly Strategy Trading Shiny App
# Elegant, minimalistic portfolio optimization tool

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(httr)
library(jsonlite)
library(xts)
library(ravapi)
library(trader)
library(optport)
library(findistr)

# Utility Functions
get_latest_quote <- function(symbol) {
  api_key <- Sys.getenv("finnhub_api_key")
  if (api_key == "") stop("API key not found. Set environment variable 'finnhub_api_key'.")
  url <- paste0("https://finnhub.io/api/v1/quote?symbol=", symbol, "&token=", api_key)
  resp <- GET(url)
  if (resp$status_code != 200) stop("Finnhub API error: ", resp$status_code)
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  if (is.null(data$c)) stop("No price returned for ", symbol)
  data$c
}

update_xts_with_quote <- function(price_xts) {
  today <- Sys.Date()
  if (any(as.Date(index(price_xts)) == today)) {
    warning("Date already present; skipping update.")
    return(price_xts)
  }
  symbols <- colnames(price_xts)
  prices  <- sapply(symbols, get_latest_quote, USE.NAMES = TRUE)
  new_row <- xts(t(prices), order.by = today)
  colnames(new_row) <- symbols
  rbind(price_xts, new_row)
}

in_trading_hours <- function() {
  now        <- as.POSIXlt(Sys.time(), tz = "America/New_York")
  is_weekday <- now$wday %in% 1:5
  after_open <- now$hour  >  9 || (now$hour == 9 && now$min >= 30)
  before_close<- now$hour < 16
  is_weekday && after_open && before_close
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Kelly Strategy Optimizer", titleWidth = 300),

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Portfolio Analysis", tabName = "analysis", icon = icon("chart-line"))
    ),

    br(),

    # Input Controls
    div(style = "padding: 15px;",
        h4("Portfolio Settings", style = "color: #fff; margin-bottom: 15px;"),

        textInput("tickers",
                  "Stock Tickers",
                  value = "tsla, gme, ko, nvda, aapl, goog",
                  placeholder = "Enter comma-separated tickers"),

        numericInput("cash_to_risk",
                     "Cash at Risk ($)",
                     value = 250,
                     min = 1,
                     step = 50),

        numericInput("ema_filter",
                     "EMA Filter (λ)",
                     value = 0.1,
                     min = 0.01,
                     max = 1,
                     step = 0.01),

        br(),

        actionButton("analyze",
                     "Run Analysis",
                     class = "btn-primary btn-block",
                     style = "margin-bottom: 10px;"),

        checkboxInput("use_realtime",
                      "Use Real-time Quotes",
                      value = TRUE),

        br(),

        div(id = "trading_status",
            style = "padding: 10px; border-radius: 5px; text-align: center;")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .info-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .btn-primary {
          background-color: #007bff;
          border-color: #007bff;
        }
        .small-box h3 {
          font-size: 2.2rem;
        }
      "))
    ),

    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                # Key Metrics
                valueBoxOutput("best_asset", width = 4),
                valueBoxOutput("allocation", width = 4),
                valueBoxOutput("investment", width = 4)
              ),

              fluidRow(
                # Portfolio Allocations
                box(
                  title = "Kelly Optimal Allocations",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("allocation_plot")
                ),

                # Risk Metrics
                box(
                  title = "Risk Analysis",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("risk_table")
                )
              ),

              fluidRow(
                # Detailed Results
                box(
                  title = "Detailed Analysis",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  verbatimTextOutput("detailed_output")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  analysis_results <- reactiveValues(
    strategy = NULL,
    assets = NULL,
    best_asset = NULL,
    allocation = NULL,
    investment = NULL,
    stable_var = NULL,
    returns = NULL
  )

  # Update trading status
  observe({
    if (in_trading_hours()) {
      updateDiv <- paste0(
        '<div style="background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb;">',
        '<strong>Market Open</strong><br>Real-time quotes available</div>'
      )
    } else {
      updateDiv <- paste0(
        '<div style="background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb;">',
        '<strong>Market Closed</strong><br>Using historical data</div>'
      )
    }

    shinyjs::html("trading_status", updateDiv)
  })

  # Main analysis
  observeEvent(input$analyze, {

    withProgress(message = 'Running Kelly Analysis...', value = 0, {

      # Parse tickers
      tickers_clean <- toupper(trimws(strsplit(input$tickers, ",")[[1]]))
      incProgress(0.2, detail = "Fetching asset data...")

      # Get assets
      tryCatch({
        assets <- getAssets(tickers_clean)

        # Update with real-time quotes if requested and market is open
        if (input$use_realtime && in_trading_hours()) {
          incProgress(0.2, detail = "Fetching real-time quotes...")
          assets <- update_xts_with_quote(assets)
        }

        incProgress(0.3, detail = "Computing returns...")

        # Compute returns
        x <- trader::stockReturns(assets)

        incProgress(0.2, detail = "Optimizing portfolio...")

        # Kelly strategy
        strategy <- optport::kelly_gbm(x, lambda = input$ema_filter)
        allocs <- strategy$allocations
        max_asset <- rownames(allocs)[which.max(allocs)]
        x_asset <- x[, max_asset]

        incProgress(0.2, detail = "Computing risk metrics...")

        # Stable VaR
        stable_fit <- findistr::fitDTFM(x_asset, "stable")
        stable_var <- findistr::stableVAR(0.99, stable_fit)
        cash_invested <- input$cash_to_risk / abs(stable_var)

        # Store results
        analysis_results$strategy <- strategy
        analysis_results$assets <- assets
        analysis_results$best_asset <- max_asset
        analysis_results$allocation <- max(allocs) * 100
        analysis_results$investment <- cash_invested
        analysis_results$stable_var <- stable_var
        analysis_results$returns <- x

        incProgress(0.1, detail = "Complete!")

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
  })

  # Value boxes
  output$best_asset <- renderValueBox({
    valueBox(
      value = ifelse(is.null(analysis_results$best_asset), "---", analysis_results$best_asset),
      subtitle = "Top Asset",
      icon = icon("trophy"),
      color = "green"
    )
  })

  output$allocation <- renderValueBox({
    valueBox(
      value = ifelse(is.null(analysis_results$allocation), "---",
                     paste0(round(analysis_results$allocation, 1), "%")),
      subtitle = "Optimal Allocation",
      icon = icon("percentage"),
      color = "blue"
    )
  })

  output$investment <- renderValueBox({
    valueBox(
      value = ifelse(is.null(analysis_results$investment), "---",
                     paste0("$", formatC(analysis_results$investment, format = "f", digits = 0, big.mark = ","))),
      subtitle = "Max Safe Investment",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })

  # Allocation plot
  output$allocation_plot <- renderPlotly({
    if (is.null(analysis_results$strategy)) {
      return(plotly_empty())
    }

    allocs <- analysis_results$strategy$allocations
    df <- data.frame(
      Asset = rownames(allocs),
      Allocation = as.numeric(allocs) * 100
    )

    p <- plot_ly(df, x = ~Asset, y = ~Allocation, type = 'bar',
                 marker = list(color = '#007bff')) %>%
      layout(title = "",
             xaxis = list(title = "Assets"),
             yaxis = list(title = "Allocation (%)"),
             showlegend = FALSE)

    p
  })

  # Risk table
  output$risk_table <- renderTable({
    if (is.null(analysis_results$strategy)) {
      return(data.frame())
    }

    data.frame(
      Metric = c("99% VaR", "Expected Return", "Volatility"),
      Value = c(
        paste0(round(analysis_results$stable_var * 100, 2), "%"),
        paste0(round(analysis_results$strategy$expected_return * 100, 2), "%"),
        paste0(round(analysis_results$strategy$volatility * 100, 2), "%")
      )
    )
  }, striped = TRUE, hover = TRUE)

  # Detailed output
  output$detailed_output <- renderText({
    if (is.null(analysis_results$strategy)) {
      return("Run analysis to see detailed results...")
    }

    paste(
      "Kelly Strategy Results:",
      paste(capture.output(print(analysis_results$strategy)), collapse = "\n"),
      "",
      paste("Best Asset:", analysis_results$best_asset),
      paste("99% VaR:", round(analysis_results$stable_var, 4)),
      paste("Maximum Safe Investment: $", round(analysis_results$investment, 2)),
      sep = "\n"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
