# UI

# I'm defining the login_ui function for displaying the login window.
# It is called in the ui section of app.R.

login_ui <- function(id, title){
  
  ns <- NS(id) # namespaced id
  
  # The login window is wrapped inside a div, 
  # so that it can be dynamically hidden (with shinyJS) after pwd validation
  div( 
    
    id = ns("login"),
    
    br(), # creating some top padding
    br(),
    br(),
    img(src = "login_logo.png", # Shipyard's logo
        style = "height: 150px; display: block;margin-left: auto;margin-right: auto;"),
    
    layout_column_wrap(
      width = 1/3,
      div(), # presence of this div centers the login card
      
      # the login card
      card(
        height = 300,
        full_screen = FALSE,
        card_header("Logowanie") %>% tagAppendAttributes(style="font-family:Raleway;font-weight:bold;"),
        
        # adding username and password fields
        # appending tags so that it is possible to log in by pressing enter
        # For this to work, I also include a js code snippet in app.R
        tagList(
          tagAppendAttributes(
            textInput(ns("username"), "Użytkownik", placeholder = "Wprowadź nazwę użytkownika"),
                              `data-proxy-click` = "moduleLogin-loginButton"), # include the name of the module in the button id
          tagAppendAttributes(
            passwordInput(ns("password"), "Hasło", placeholder = "Wprowadź hasło"),
                              `data-proxy-click` = "moduleLogin-loginButton"),
          actionButton(ns("loginButton"), label = "Zaloguj się")
          )
        )
      )
  )
}

# SERVER
# The function checks user credentials on loginButton press.
# Providing correct user id and password triggers a renderUI function
# which renders the main screen of the app.

validate_pwd <- function(input, output, session, 
                         data, user_col, pwd_col) {
  
  eventReactive(input$loginButton, { #establishing a reactive on loginButton press
    
    # get user and pwd from data/ user_col/ pwd_col information
    user <- data %>% filter({{ user_col }} == input$username ) %>% pull({{ user_col }}) 
    pwd  <- data %>% filter({{ user_col }} == input$username ) %>% pull({{ pwd_col }})
    
    validate <- FALSE
    
    if (input$password == pwd &&
        input$username == user)
    {validate <- TRUE}
    
    # hide login form when user is confirmed
    if (validate) {
      shinyjs::hide(id = "login")
    }
    
    validate
    
  })

}

# These functions pull LimeSurvey username and password assigned to each user of the app.
# These are --different-- from the username and password used to access the app!

# LimeSurvey username
lime_username <- function(input, output, session,
                          data, user_col) {
  
  eventReactive(input$loginButton, {
    data %>%
      filter({{ user_col }} == input$username) %>% 
      pull(lime_username)
  })
  
}

# LimeSurvey passwords
lime_password <- function(input, output, session,
                          data, user_col) {
  
  eventReactive(input$loginButton, {
    data %>%
      filter({{ user_col }} == input$username) %>% 
      pull(lime_password)
  })
  
}


# Login username
login_username <- function(input, output, session) {
  
  eventReactive(input$loginButton, {
      input$username
    })
  }
