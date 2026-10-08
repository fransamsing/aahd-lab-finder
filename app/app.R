library(shiny)
library(bslib)
library(leaflet)
library(htmltools)

# ---- Data ------------------------------------------------------------------
# On the website the CSV sits next to app.R; when run locally it's in ../data
csv_path <- if (file.exists("service_providers.csv")) "service_providers.csv" else "../data/service_providers.csv"
raw <- read.csv(csv_path, stringsAsFactors = FALSE,
                check.names = FALSE, encoding = "UTF-8")

split_list <- function(x) {
  lapply(strsplit(x, ",\\s*"), function(v) {
    v <- trimws(v)
    v[!(v %in% c("N/A", "", NA))]
  })
}

web <- raw$Website
web[web %in% c("N/A", "")] <- NA
needs_scheme <- !is.na(web) & !grepl("^https?://", web)
web[needs_scheme] <- paste0("https://", web[needs_scheme])

labs <- data.frame(
  id     = seq_len(nrow(raw)),
  name   = raw$Provider,
  state  = raw$State,
  sector = raw$Sector,
  lat    = raw$Latitude,
  lng    = raw$Longitude,
  web    = web,
  stringsAsFactors = FALSE
)
labs$focus     <- split_list(raw$Tab)
labs$toxins    <- split_list(raw[["Notable toxins"]])
labs$services  <- lapply(split_list(raw$Service),
                              function(v) unique(sub("^Parisitology$", "Parasitology", v)))
labs$pathogens <- split_list(raw[["Notable pathogens"]])
labs$group <- vapply(labs$focus, function(v)
  if (length(v) > 1) "Both" else v[1], character(1))

choices_of <- function(lst) sort(unique(unlist(lst)))
toxin_choices    <- choices_of(labs$toxins)
pathogen_choices <- choices_of(labs$pathogens)
service_choices  <- choices_of(labs$services)
state_choices    <- sort(unique(labs$state))
sector_choices   <- sort(unique(labs$sector))

pal <- colorFactor(c("#2a9d8f", "#e76f51", "#6a4c93"),
                   levels = c("Environmental", "Infectious disease", "Both"))

# Search index: everything about a provider in one lowercase string
labs$haystack <- tolower(paste(
  labs$name, labs$state, labs$sector,
  vapply(labs$toxins, paste, "", collapse = " "),
  vapply(labs$services, paste, "", collapse = " "),
  vapply(labs$pathogens, paste, "", collapse = " ")
))

has_any <- function(lst, sel) {
  if (length(sel) == 0) return(rep(TRUE, length(lst)))
  vapply(lst, function(v) any(v %in% sel), logical(1))
}

popup_html <- function(d) {
  vapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    line <- function(label, v) {
      v <- v[[1]]
      if (length(v) == 0) "" else
        sprintf("<div style='margin-top:4px'><b>%s:</b> %s</div>",
                label, htmlEscape(paste(v, collapse = ", ")))
    }
    link <- if (is.na(row$web)) "" else
      sprintf("<div style='margin-top:6px'><a href='%s' target='_blank' rel='noopener'>Visit website &#8599;</a></div>",
              htmlEscape(row$web, attribute = TRUE))
    paste0(
      "<div style='max-width:280px'>",
      "<b style='font-size:1.05em'>", htmlEscape(row$name), "</b>",
      "<div style='color:#666'>", htmlEscape(row$sector), " &middot; ", htmlEscape(row$state),
      " &middot; ", htmlEscape(row$group), "</div>",
      line("Toxins", row$toxins),
      line("Services", row$services),
      line("Pathogens", row$pathogens),
      link, "</div>"
    )
  }, character(1))
}

# ---- UI --------------------------------------------------------------------
ui <- page_sidebar(
  title = "Find a lab or service",
  theme = bs_theme(version = 5, preset = "flatly"),
  sidebar = sidebar(
    width = 320,
    textInput("q", NULL, placeholder = "Search name, toxin, pathogen..."),
    radioButtons("focus", "Area", inline = FALSE,
                 choices = c("All", "Environmental", "Infectious disease")),
    conditionalPanel(
      "input.focus != 'Infectious disease'",
      selectizeInput("toxins", "Tests for toxins", toxin_choices, multiple = TRUE,
                     options = list(placeholder = "Any toxin"))
    ),
    conditionalPanel(
      "input.focus != 'Environmental'",
      selectizeInput("pathogens", "Tests for pathogens", pathogen_choices, multiple = TRUE,
                     options = list(placeholder = "Any pathogen")),
      selectizeInput("services", "Services offered", service_choices, multiple = TRUE,
                     options = list(placeholder = "Any service"))
    ),
    checkboxGroupInput("state", "State / territory", state_choices,
                       selected = state_choices, inline = TRUE),
    checkboxGroupInput("sector", "Sector", sector_choices,
                       selected = sector_choices, inline = TRUE),
    actionButton("reset", "Reset filters", class = "btn-outline-secondary btn-sm")
  ),
  card(
    full_screen = TRUE,
    card_header(textOutput("count", inline = TRUE)),
    leafletOutput("map", height = 480)
  ),
  card(
    card_header("Results (click a name to show it on the map)"),
    uiOutput("results")
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    d <- labs
    if (input$focus != "All") {
      d <- d[vapply(d$focus, function(v) input$focus %in% v, logical(1)), ]
    }
    if (input$focus != "Infectious disease") d <- d[has_any(d$toxins, input$toxins), ]
    if (input$focus != "Environmental") {
      d <- d[has_any(d$pathogens, input$pathogens), ]
      d <- d[has_any(d$services, input$services), ]
    }
    d <- d[d$state %in% input$state & d$sector %in% input$sector, ]
    q <- trimws(tolower(input$q))
    if (nzchar(q)) d <- d[grepl(q, d$haystack, fixed = TRUE), ]
    d
  })

  output$count <- renderText({
    n <- nrow(filtered())
    sprintf("%d of %d providers", n, nrow(labs))
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      fitBounds(112, -44, 154, -10) |>
      addLegend("bottomleft", pal = pal, values = c("Environmental", "Infectious disease", "Both"),
                title = "Area", opacity = 1)
  })

  # Wait until the map has drawn once before adding markers to it
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_zoom, map_ready(TRUE), once = TRUE)

  observe({
    req(map_ready())
    d <- filtered()
    proxy <- leafletProxy("map") |> clearMarkers() |> clearPopups()
    if (nrow(d) == 0) return()
    proxy |>
      addCircleMarkers(
        data = d, lng = ~lng, lat = ~lat, layerId = ~id,
        radius = 8, stroke = TRUE, color = "white", weight = 1.5,
        fillColor = ~pal(group), fillOpacity = 0.9,
        label = ~name, popup = popup_html(d)
      )
  })

  output$results <- renderUI({
    d <- filtered()
    if (nrow(d) == 0) {
      return(p(class = "text-muted", "No providers match these filters."))
    }
    d <- d[order(d$name), ]
    tags$div(
      style = "max-height: 360px; overflow-y: auto;",
      tags$table(
        class = "table table-sm table-hover",
        tags$thead(tags$tr(tags$th("Provider"), tags$th("Area"),
                           tags$th("State"), tags$th("Sector"), tags$th("Website"))),
        tags$tbody(lapply(seq_len(nrow(d)), function(i) {
          r <- d[i, ]
          tags$tr(
            tags$td(tags$a(
              href = "#", r$name,
              onclick = sprintf("Shiny.setInputValue('zoom_to', %d, {priority: 'event'}); return false;", r$id)
            )),
            tags$td(r$group), tags$td(r$state), tags$td(r$sector),
            tags$td(if (is.na(r$web)) "" else
              tags$a(href = r$web, target = "_blank", rel = "noopener", "Open"))
          )
        }))
      )
    )
  })

  observeEvent(input$zoom_to, {
    r <- labs[labs$id == input$zoom_to, ]
    leafletProxy("map") |>
      clearPopups() |>
      setView(r$lng, r$lat, zoom = 11) |>
      addPopups(r$lng, r$lat, popup = popup_html(r))
  })

  observeEvent(input$reset, {
    updateTextInput(session, "q", value = "")
    updateRadioButtons(session, "focus", selected = "All")
    updateSelectizeInput(session, "toxins", selected = character(0))
    updateSelectizeInput(session, "pathogens", selected = character(0))
    updateSelectizeInput(session, "services", selected = character(0))
    updateCheckboxGroupInput(session, "state", selected = state_choices)
    updateCheckboxGroupInput(session, "sector", selected = sector_choices)
    leafletProxy("map") |> clearPopups() |> fitBounds(112, -44, 154, -10)
  })
}

shinyApp(ui, server)
