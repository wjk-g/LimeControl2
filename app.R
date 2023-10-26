library(shiny)
library(limer)
library(tibble)
library(dplyr)
library(glue)
library(MASS)
library(plotly)
library(sass)
library(DT)
library(openxlsx)
library(shinyjs)
library(lubridate)
library(bslib)
library(htmltools)
library(showtext)

# ----------------------- JavaScript -----------------------
# JavaScript for handling button pressing in the login module
# Button id has to include the name of the module.
jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#moduleLogin-loginButton").click();
}});'

# ----------------------- Loading fonts via showtext ----------------------- 

font_add_google("Roboto", "Roboto")
showtext_auto()

# ----------------------- Sourcing modules -----------------------
source("modules/module_login.R")

# ----------------------- EDITABLE SECTION -----------------------

# LOGIN DATA
# Table with user login data
user_base_module_tbl <- tibble(
  user_name = c("user1", "user2"),
  password  = c("pass", "pass"),
  lime_username = c(Sys.getenv("DUMMY_USER_1"), Sys.getenv("DUMMY_USER_2")),
  lime_password = c(Sys.getenv("PASSWORD_1"), Sys.getenv("PASSWORD_2"))
)

# SURVEY SETTINGS
# LS url

url <- Sys.getenv("URL_TO_RC2_API") # url to LimeSurvey server

# Title
title <- "Change this study title"


# Survey ID numbers
sid1 <- Sys.getenv("SURVEY_1_ID")
sid2 <- Sys.getenv("SURVEY_2_ID")

# A list of survey ids supplied to the survey selection dropdown
# Add more ids to display multiple surveys
surveyIDs <- c("Example survey 1 (n = 1000)" = sid1,
               "Example survey 2 (n = 500)" = sid2)

# Actual names of columns in the table
actual_colnames <- c("tid", "token", "emailstatus", 
                     "participant_info.firstname", "participant_info.lastname",
                     "sent", "remindersent", "completed",
                     "participant_info.email",
                     "lastpage",
                     "attribute_1", "attribute_2", 
                     "attribute_3",
                     "attribute_4",
                     "attribute_5", "attribute_6",
                     "attribute_7")

# Human-readable names
displayed_colnames <- c("id", "token", "status",
                        "miejscowość", "typ",
                        "wysłano", "przypomniano", "wypełnione",
                        "e-mail",
                        "ost. str.",
                        "woj.", "powiat", 
                        "attribute_3", 
                        "telefon",
                        "notatki", "modyfikacja",
                        "attribute_7")

# Names of the columns you want to hide
hide <- c("status", "modyfikacja", "attribute_3", "attribute_7", "token", "powiat")

# Names of editable columns
editable <- c("e-mail", "notatki")

# ----------------------------------------------

# columns that are displayed in the table
visible_actual <- actual_colnames[!(displayed_colnames %in% hide)]
visible_displayed <- displayed_colnames[!(displayed_colnames %in% hide)]

# columns that are not editable
noneditable <- !(visible_displayed %in% editable)
disable <- (1:length(visible_displayed)-1)[noneditable]

# ----------------------------------------------


# Modified version of the call_limer() function from the limer package.
# I replaced the original call_limer() function because get_session_key()
# attempts to store the session key in a separate envir.
# When get_session_key() is wrapped inside a reactive expression
# this approach fails.

call_limer_mod <- function(method, session_key, params = list(), ...) {
  
  key.list <- list(sSessionKey = session_key)
  params.full <- c(key.list, params)
  
  body.json <- list(method = method,
                    # This seems to not matter, but the API call breaks without it,
                    # so just pass nothing. ¯\_(ツ)_/¯
                    id = " ",
                    params = params.full)
  
  r <- httr::POST(getOption('lime_api'), httr::content_type_json(),
                  body = jsonlite::toJSON(body.json, auto_unbox = TRUE), ...)
  
  return(jsonlite::fromJSON(httr::content(r, as='text', encoding="utf-8"))$result)   # incorporated fix by petrbouchal
}

# ----------------------- Key functions and resources -----------------------

# Connect to LimeSurvey and provide login data
# username and password are defined in the login module

connect2LS <- function(username, password) {
  options(lime_api = url)
  options(lime_username = username )
  options(lime_password = password )
  get_session_key() # Log in
}

# Stwórz stronę z tytułem, podtytułem i główną zawartością 
# Szczególnie przydatne jeżeli aplikacja składa się z kilku zakładek -- do wykorzystania z shiny.router
# *** NOT SURE WHAT TO DO WITH THIS YET

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "mainTitle",
    span(title, style="margin-top: 15px"),
    span(subtitle)
  ),
  contents)
}

# Chart colors
kolory <- c("#529EE0", "#9BC7E4", "#D9A6C2", "#F9B665", "#F59B00")

# ----------------------- Main page UI elements -----------------------

# Cards
card_indicators <-
  card(
    full_screen = FALSE,
    card_header("Podsumowanie realizacji"),
    card_body(uiOutput("indicators")),
    style = "background-color: #ffeff1;"
  )

card_timeline <-
  card(
    full_screen = TRUE,
    card_header("Przyrost liczby odpowiedzi w czasie"),
    card_body_fill(class = "p-0",
                   plotlyOutput("timeline", height="100%"))
  )

card_answer_time <- 
  card(
    full_screen = TRUE,
    card_header("Czas wypełniania ankiety"),
    card_body_fill(class = "p-0",
                   plotlyOutput("answerTime", height="100%"))
  )

card_table <- 
  card(
    full_screen=TRUE,
    div(card_title("Tabela respondentów", style="width: 300px"),
        card_body(uiOutput("participantSummary")), 
        style = "display: flex; align-items: center; justify-content: space-between; height: 55px;"
        ),
        # padding: 0px; margin: 0px -15px -5px -15px;
    card_body_fill(div(DT::DTOutput("tabela"), style = "font-size: 75%; padding-top: 5px;"))
  )

# Main page
main_page <- makePage(
  title = h4(title),
  subtitle = h6("Podgląd stanu realizacji sondaży", style = "color: gray;"),
  contents =
    div( 
      selectInput("dropdown", label=NULL, surveyIDs, width = "100%") %>%
        tagAppendAttributes(style = "height: 32px;"),
      plotOutput("responseRate",height = 25, width = "100%") %>%
        tagAppendAttributes(style = "margin-bottom: 10px; margin-top: 0px;"),
      layout_column_wrap( # top 3 cards
        heights_equal = "row",
        height = 360,
        width = NULL, # this is required for setting width with css
        style = css(grid_template_columns = "2fr 3fr 3fr"), # cards' width
        card_indicators, card_timeline, card_answer_time # cards
      ),
      br(),
      card_table # user table
    )
)

# ----------------------- layout -----------------------
# Header
header <- div(
  class = "header",
  a(img(src = "logo2.png", class = "logo"), href = "http://stocznia.org.pl"),
  div(
    uiOutput("daytime", class = "daytime"),
    actionButton("reloadData", label = "Aktualizuj dane", class = "btn"),
    downloadButton("downloadData", label = "Pobierz dane w formacie .xlsx", 
                   class = "btn btn-download", icon = icon("file-excel", style = "color:#1D6F42;margin-top:7px;"))
  )
)

# Footer
footer <- div(
  class = "footer",
  p("Autor: Fundacja Stocznia"),
  p("Możesz skontaktować się z nami pod adresem stocznia@stocznia.org.pl"),
  p("Wszystkie prawa zastrzeżone")
)

# Layout
layout <- function(mainUI){
  div(class = "grid-container",
      header,
      div(class = "main", mainUI),
      footer
  )
}

# ----------------------- UI -----------------------

ui <- fluidPage(
  
  tags$head(tags$script(HTML(jscode))), # this snipped is necessary to associate loginButton with Enter
  theme = bs_theme(#bootswatch = "minty",
    base_font = font_google("Roboto",wght = 300),
    heading_font = font_google("Raleway",wght = 500)),
  
  # This function must be called from a Shiny app's UI in order for all other 
  # shinyjs functions to work.
  # You can call useShinyjs() from anywhere inside the UI, 
  # as long as the final app UI contains the result of useShinyjs().
  useShinyjs(),
  
  login_ui(id = "moduleLogin"), # login_ui function is sourced from the module 
  uiOutput(outputId = "displayContent") # displays the main screen of the app
  
)

# ----------------------- Server -----------------------

server <- function(input, output, session) {
  
  # ---- establish connection with LS ----
  
  session_key <- reactive({
    connect2LS(username = ls_username(),
               password = ls_password())
  })
  
  # ---- User data ----
  
  ls_username <- callModule(
    module   = lime_username,
    id       = "moduleLogin",
    data     = user_base_module_tbl,
    user_col = user_name
  )
  
  ls_password <- callModule(
    module   = lime_password,
    id       = "moduleLogin",
    data     = user_base_module_tbl,
    user_col = user_name
  )
  
  # callModule() function accesses the server side of the module
  # passes information on where to search for the password
  # (via the password parameter) <== This could be a df with users and pswds instead 
  
  validate_password_module <- callModule(
    module   = validate_pwd, # validate_pwd is the name of the "server function" inside the module
    # callModule() function accesses it and provides it with arguments
    id       = "moduleLogin", # the id corresponds with the ID assigned to the login_ui() function
    data     = user_base_module_tbl, 
    user_col = user_name, # the name of the user_name column in user_base_module_tbl
    pwd_col  = password # this is the name of the password column in user_base_module_tbl
  )
  
  # show app 
  output$displayContent <- renderUI({
    
    req(validate_password_module())
    
    # ---- uwaga! Tutaj generuje się nasza strona główna
    div(
      layout(main_page),
      tags$head(
        tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
      )
    )
  })
  
  # Currently selected survey
  surveyID <- reactive({ as.numeric(input$dropdown) })
  
  # ---- Downloading and processing data from LimeSurvey Server -----
  
  formData <- 
    eventReactive(c(input$reloadData,
                    input$dropdown), {
                      
                      odpowiedzi <- call_limer_mod(session_key = session_key(),
                                                   method = "export_responses",
                                                   params = list(iSurveyID = surveyID(),
                                                                 sDocumentType = "csv", 
                                                                 sLanguageCode = "pl", 
                                                                 sCompletionStatus = "all",
                                                                 sHeadingType = "code", 
                                                                 sResponseType = "short",
                                                                 iFromResponseID = 0, 
                                                                 iToResponseID = 9999,
                                                                 aFields = list("id",
                                                                                "token",
                                                                                "lastpage", 
                                                                                "interviewtime") ))
                      odpowiedzi <- base64_to_df(unlist(odpowiedzi))
                      
                      odpowiedzi <- odpowiedzi %>%
                        group_by(id) %>%
                        # Certain records in the answers table are duplicated 
                        mutate(max_lastpage = max(lastpage)) %>%
                        # I keep only the most complete answer (based on the page number)
                        filter(max_lastpage == lastpage) %>%
                        ungroup() %>%
                        distinct(token, .keep_all = TRUE)
                      
                      odpowiedzi$id <- as.character(odpowiedzi$id)
                      
                      respondenci <- call_limer_mod(session_key = session_key(),
                                                    method = "list_participants", # get participants' attribs
                                                    params = list(iSurveyID = surveyID(), iStart = 1, iLimit = 9999, bUnused = FALSE,
                                                                  aAttributes = list(actual_colnames)[[1]]))
                      
                      respondenci <- do.call("data.frame", respondenci)
                      
                      # I join answer data and respondent data based on the "token" column
                      data <- left_join(respondenci, odpowiedzi, by = "token") %>%
                        mutate(sent = as.POSIXct(sent, format = "%Y-%m-%d %H:%M", tz = "CET"),
                               completed = as.POSIXct(ifelse(completed == "N", NA_character_, completed),
                                                      format = "%Y-%m-%d %H:%M", tz = "CET"),
                               remindersent = as.POSIXct(ifelse(remindersent == "N", NA_character_, remindersent),
                                                         format = "%Y-%m-%d %H:%M", tz = "CET"))
                      
                    })
  
  # Tworzę dodatkowy reactive -- displayData() -- żeby móc elastyczniej sterować wyświetlaniem kolumn
  displayData <- reactive({
    
    formData()  %>%
      dplyr::select(all_of(c(actual_colnames))) %>%
      select(visible_actual) %>%
      mutate(across(is.POSIXct, ~format(.x, "%Y-%m-%d %H:%M") )) # I convert posixct here because otherwise R cannot compute times correctly
    
  })
  
  printer <- reactive({
    print(colnames(displayData()))
  })
  
  # Tworzę czytelną etykietę czasu
  humanTime <- eventReactive(c(input$reloadData,
                               input$dropdown), {
                                 
                                 daytime <- format(Sys.time(), "%H:%M:%S")
                                 
                               })
  
  # https://stackoverflow.com/questions/70184312/select-a-dt-row-and-then-change-the-value-of-one-cell-of-this-row-based-on-widge
  # https://stackoverflow.com/questions/69344974/dt-dynamically-change-column-values-based-on-selectinput-from-another-column-in
  # ---- definiuję właściwości tabeli użytkowników
  output$tabela <- DT::renderDataTable(displayData(),
                                       options = list(
                                         language = list(url = 'Polish.json'),
                                         scrollX = TRUE,
                                         scrollY = "500px",
                                         lengthMenu = c(25, 50, 100), 
                                         pageLength = 50),
                                       rownames = FALSE,
                                       selection = "single",
                                       colnames = visible_displayed,
                                       fillContainer = TRUE,
                                       #disable is defined above
                                       editable = list(target = "cell",
                                                       server = FALSE,
                                                       disable = list(columns = disable)))
  
  
  # Tworzę obserwera, który wypatruje zaznaczenie respondenta w tabeli
  observeEvent(input$tabela_cell_edit, {
    info <- input$tabela_cell_edit # info o zaznaczonym respondencie
    
    newVal <- info$value # new value in the notes column
    
    token <- formData()$token[info$row]
    
    # which() identifies the position of the column with notes
    if (info$col == which("notatki"==visible_displayed)-1) {
      
      # This is how you assign variables as names to named lists...  
      notes_column <- visible_actual[which("notatki"==visible_displayed)]
      print(notes_column)
      new_note <- list(newVal)
      print(new_note)
      names(new_note)[1] <- notes_column
      print(names(new_note)[1])
      
      print(actual_colnames[which("modyfikacja"==displayed_colnames)])
      notes_datestamp_column <- actual_colnames[which("modyfikacja"==displayed_colnames)] # ************ DO ZMIANY!!!
      print(paste0("notes_date_stamp_column: ", notes_datestamp_column))
      new_datestamp <- list( format(Sys.time(), "%m/%d %H:%M:%S") )
      print(new_datestamp)
      names(new_datestamp)[1] <- notes_datestamp_column
      
      call_limer_mod(session_key = session_key(),
                     method = "set_participant_properties",
                     params = list(iSurveyID = surveyID(), 
                                   aTokenQueryProperties=list(token=token),
                                   aTokenData=new_note ))
      
      
      call_limer_mod(session_key = session_key(),
                     method = "set_participant_properties",
                     params = list(iSurveyID = surveyID(), 
                                   aTokenQueryProperties=list(token=token),
                                   aTokenData=new_datestamp ))
      
      
      # which() identifies the position of the column with emails
    } else if (info$col == which("e-mail"==visible_displayed)-1) {
      
      call_limer_mod(session_key = session_key(),
                     method = "set_participant_properties",
                     params = list(iSurveyID = surveyID(), 
                                   aTokenQueryProperties=list(token=token),
                                   aTokenData = list(email = newVal) ))
      
    }
    
  })
  
  # Basic info on selected participant 
  
  participantSelected <- reactive({
    
    print(input$tabela_rows_selected)
    
    participant <- formData()[input$tabela_rows_selected, ]
    
    print(participant)
    
    token <- participant$token
    completed <- participant$completed
    real_tid <- participant$tid
    
    list("token" = token, 
         "completed" = completed,
         "real_tid" = real_tid)
  })
  
  output$participantSummary <- renderUI({
    
    validate(
      need(input$tabela_rows_selected != "", " "),
      errorClass = "myError"
    )
    
    if (is.na(participantSelected()$completed)) {
      
      div(
        class = "participantSummary",
        div(strong("Token:"), participantSelected()$token),
        div(strong("Wypełnił/a ankietę?"), "Nie"),
        div(actionButton("sendReminderInitiate", label = "Wyślij przypomnienie"))
      )
      
    } else {
      
      div(
        class = "participantSummary",
        div(strong("ID:"), participantSelected()$token),
        div(strong("Wypełnił/a ankietę?"), "Tak")
      )
    }
    
  })
  
  
  # ----- Sending reminders -----
  
  # Listen for the sendReminderInitiate button to be pressed ("Wyślij przypomnienie")
  observeEvent(input$sendReminderInitiate, {
    showModal(
      modalDialog(
        title = "Wyślij przypomnienie",
        "Czy na pewno chcesz wysłać przypomnienie do tego respondenta?",
        # adding a footer to replace the default Dismiss button
        footer = tagList( 
          actionButton("confirmSendReminder", "Wyślij przypomnienie") %>%
            tagAppendAttributes(style = paste0("background-color:", kolory[3])),
          modalButton("Nie wysyłaj") # closes the modal
        ),
        easyClose = TRUE
      )
    )
  })
  
  # Closes the modal after confirmSendReminder is pressed
  observeEvent(input$confirmSendReminder, {
    removeModal()
  })
  
  # Connect to LimeSurvey and send a reminder
  observeEvent(input$confirmSendReminder, {
    
    selected <- list(participantSelected()$token)
    names(selected) <- "token"
    real_tid <- participantSelected()$real_tid
    print(paste0("real_tid: ", real_tid))
    print(paste0("selected: ",selected))
    
    # We set remindersent to "N" and remindercount to 0.
    # This part is necessary because Lime API doesn't allow you to send reminders
    # to a user if remindersent != "N" or remindercount > 0.
    # Remember that this operation modifies participant data on LimeSurvey.
    call_limer_mod(session_key = session_key(),
                   method = "set_participant_properties", 
                   params = list(iSurveyID = surveyID(),
                                 aTokenQueryProperties=selected,
                                 aTokenData = list(remindersent = "N", remindercount = 0) ))
    
    # After modifying remindersent and remindercount we can send the reminder.
    call_limer_mod(session_key = session_key(),
                   method = "remind_participants",
                   params = list(iSurveyID = surveyID(),
                                 aTokenIds=list(real_tid), # this is in fact a tid number, not a row number
                                 iMaxReminders=1
                   ))
    
  })
  
  
  
  # ----- Generate basic statistics -----
  
  calculations <- reactive({
    
    l_respondentow <- nrow(formData()) # Liczba respondentów
    
    l_wyslanych <- formData() %>% # Liczba wysłanych wiadomości
      filter(!is.na(sent))%>%
      nrow()
    
    l_wypelnionych <- formData() %>% # Liczba wypełnionych ankiet
      filter(!is.na(completed)) %>%
      nrow()
    
    stopa_zwrotu <- round(l_wypelnionych / l_wyslanych, 4) * 100 # Stopa zwrotu
    
    l_blednych_maili <- formData() %>% # Liczba błędnych maili
      filter(emailstatus != "OK") %>%
      nrow()
    
    proc_blednych_maili <- round(l_blednych_maili / l_wyslanych, 2) * 100 # Procent błędnych maili
    
    time_diff <- difftime( as.POSIXct(formData()$completed), as.POSIXct(formData()$sent), units = "hours" ) %>% as.numeric()
    
    mean_time_diff <- round(median(time_diff, na.rm = TRUE),2)
    
    median_answer_time <- round(median(formData() %>% 
                                         filter(!is.na(completed) & interviewtime < (45*60) & interviewtime > 0) %>% 
                                         pull(interviewtime), na.rm = TRUE) / 60, 2)
    
    l_zaczetych_nieskonczonych <- formData() %>%
      mutate(zaczete_nieskonczone = ifelse( is.na(completed) & !is.na(lastpage), "Tak", "Nie" ) ) %>%
      count(zaczete_nieskonczone) %>%
      filter(zaczete_nieskonczone == "Tak") %>%
      pull(n)
    print(l_zaczetych_nieskonczonych)
    l_zaczetych_nieskonczonych <- ifelse(identical(l_zaczetych_nieskonczonych, integer(0)), 0, l_zaczetych_nieskonczonych)
    
    proc_zaczetych_nieskonczonych <- formData() %>%
      mutate(zaczete_nieskonczone = ifelse( is.na(completed) & !is.na(lastpage), "Tak", "Nie" ) ) %>%
      count(zaczete_nieskonczone) %>%
      summarise( prop = n / sum(n), zaczete_nieskonczone = zaczete_nieskonczone) %>%
      filter(zaczete_nieskonczone == "Tak") %>%
      pull( prop )
    
    proc_zaczetych_nieskonczonych <- ifelse(identical(proc_zaczetych_nieskonczonych, numeric(0)), 0, proc_zaczetych_nieskonczonych)
    
    list("l_respondentow" = l_respondentow, 
         "l_wyslanych" = l_wyslanych,
         "l_wypelnionych" = l_wypelnionych,
         "stopa_zwrotu" = stopa_zwrotu,
         "l_blednych_maili" = l_blednych_maili,
         "proc_blednych_maili" = proc_blednych_maili,
         "time_diff" = time_diff,
         "mean_time_diff" = mean_time_diff,
         "median_answer_time" = median_answer_time,
         "l_zaczetych_nieskonczonych" = l_zaczetych_nieskonczonych,
         "proc_zaczetych_nieskonczonych" = round(proc_zaczetych_nieskonczonych, 2)*100)
    
  })
  
  output$daytime <- renderUI({
    div(
      p(paste0("Godzina ostatniej aktualizacji: ", humanTime()))
    )
  })
  
  output$indicators <- renderUI({
    div(
      class = "ppanel",
      p(strong("Liczba respondentów: "), as.character(calculations()["l_respondentow"]) ),
      p(strong("Liczba wysłanych zaproszeń: "), as.character(calculations()["l_wyslanych"]) ),
      p(strong("Liczba kompletnych odpowiedzi: "), as.character(calculations()["l_wypelnionych"]) ),
      p(strong("Stopa zwrotu: "), as.character(calculations()["stopa_zwrotu"]), "%" ),
      p(strong("Mediana czasu odpowiedzi: "), as.character(calculations()["median_answer_time"]), " min." ),
      p(strong("Liczba błędnych adresów e-mail: "), as.character(calculations()["l_blednych_maili"]) ),
      p(strong("% błędnych adresów e-mail: "), as.character(calculations()["proc_blednych_maili"]) ),
      p(strong("Liczba zaczętych nieskończonych: "), as.character(calculations()["l_zaczetych_nieskonczonych"]) ),
      p(strong("% zaczętych nieskończonych: "), as.character(calculations()["proc_zaczetych_nieskonczonych"]) )
    )
  })
  
  
  # ----- Rendering plots -----
  
  output$responseRate <- renderPlot({
    
    rr <- data.frame(
      "l_wyslanych" = c(0, as.numeric(calculations()["l_wyslanych"]) ),
      "l_wypelnionych" = c(0, as.numeric(calculations()["l_wypelnionych"]) )
    )
    
    ggplot(data = rr) +
      geom_col(aes(x = l_wyslanych, y = "y"), fill = kolory[2]) +
      geom_col(aes(x = l_wypelnionych, y = "y"), fill = kolory[3]) +
      annotate(geom = "text", x = max(rr$l_wypelnionych/2), y = "y", 
               family = "Roboto",
               label = paste0(#max(rr$l_wypelnionych), #"/",
                 #max(rr$l_wyslanych),
                 #" (",
                 round(max(rr$l_wypelnionych)/max(rr$l_wyslanych)*100, 0),
                 "%")) +#")) +
      theme_void() +
      scale_x_continuous(expand = c(0, 0)) +
      theme(plot.background = element_rect(fill = "#faf9f8", color = "#faf9f8" ), # region outside plot
            panel.background = element_rect(fill = "#faf9f8", color = "#faf9f8"),
            plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "cm"))
  })
  
  observeEvent( input$reloadData, {
    
    floor <- formData() %>%
      filter(!is.na(completed)) %>%
      mutate(completed_floor = floor_date(completed, "day")) %>%
      count(completed_floor)
    
    print(floor)
    
  })
  
  output$timeline <- renderPlotly({
    
    ceiling <- formData() %>%
      filter(!is.na(completed)) %>%
      mutate(completed_floor = floor_date(completed, "day")) %>%
      count(completed_floor)
    
    p <- formData() %>% 
      filter(!is.na(completed)) %>%
      arrange(completed) %>%
      mutate(rank = row_number()) %>%
      ggplot() + 
      #geom_segment(aes(x = sent, xend = completed, y = rank, yend = rank)) +
      geom_point(aes(x = sent, y = rank), alpha = .1, color = kolory[1]) +
      geom_point(aes(x = remindersent, y = rank), alpha = .1, color = kolory[2]) +
      geom_point(aes(x = completed, y = rank), alpha = .25, color = kolory[3]) +
      geom_line(aes(x = completed, y = rank, group = 1), alpha = .2, color = kolory[3]) +
      geom_col(data = ceiling, aes(completed_floor, n),
               # position_nudge(x = 43200) moves the bar by 43000 seconds (~half a day)
               alpha = .4, fill = kolory[3], position = position_nudge(x = 43200)) +
      scale_x_datetime(timezone = "CET") +
      #scale_y_continuous(breaks = ~round(unique(pretty(.)))) +
      #xlab("czas") +
      #ylab("kolejność || l. odp. / dzień") +
      theme_minimal()
    
    ggplotly(p) %>% # , height = 350) %>% 
      config(displayModeBar = FALSE) %>%
      plotly::layout(yaxis = list(title = list(text=' ',
                                               font=list(size=1)),
                                  tickfont = list(family = "Roboto", size = 12, color = "gray")),
                     xaxis = list(showgrid = FALSE,
                                  title = list(text = 'Czas', 
                                               font=list(family = "Roboto", size = 13, color = "gray")), 
                                  tickfont = list(family = "Roboto", size = 12, color = "gray")))
  })
  
  output$answerTime <- renderPlotly({
    
    p <- formData() %>% 
      filter(!is.na(completed) & interviewtime < (45*60) & interviewtime > 0) %>%
      ggplot(aes(x = interviewtime/60)) +
      geom_histogram(fill = kolory[3], bins = 25) +
      geom_vline(aes(xintercept = calculations()["median_answer_time"][[1]]), color = "#404040", size = .2) +
      #scale_x_continuous(limits = c(0, 45)) +
      #scale_y_continuous(breaks = ~round(unique(pretty(.)))) +
      #xlab("minuty") +
      #ylab("liczba respondentów") +
      theme_minimal()
    
    ggplotly(p) %>% 
      config(displayModeBar = FALSE) %>% 
      plotly::layout(yaxis = list(title = list(text=' ',
                                               font=list(size=1)),
                                  tickfont = list(family = "Roboto", size = 12, color = "gray")),
                     xaxis = list(showgrid = FALSE,
                                  title = list(text = 'Minuty', 
                                               font=list(family = "Roboto", size = 13, color = "gray")), 
                                  tickfont = list(family = "Roboto", size = 12, color = "gray")))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("wyniki_ankiety_", format(Sys.time(), "%Y_%m_%d-%H_%M"), ".xlsx", sep="") # To w zasadzie powinna być data aktualizacji, nie pobrania
    },
    content = function(file) {
      write.xlsx(formData(), file)
    }
  )
  
}

shinyApp(ui, server)