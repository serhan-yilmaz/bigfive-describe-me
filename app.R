library(shiny)
library(htm2txt)
library(rjson)
library(stringr)
library(shinyjs)
library(colorspace)

json_file <- "bigfive_adjectives.json"
adjective_data <- fromJSON(paste(readLines(json_file, warn=FALSE), collapse=""))

json_file <- "alignment_classes.json"
alignment_data <- fromJSON(paste(readLines(json_file, warn=FALSE), collapse=""))

scales = c(
  c("Gregariousness", "Assertiveness", "Activity Level", "Excitement-Seeking", "Cheerfulness", "Friendliness"), 
  c("Self-Efficacy", "Achievement-Striving", "Self-Discipline", "Orderliness", "Dutifulness", "Cautiousness"),
  c("Trust", "Morality", "Altruism", "Cooperation", "Modesty", "Sympathy"),
  c("Anxiety", "Anger", "Depression", "Self-Consciousness", "Immoderation", "Vulnerability"), 
  c("Imagination", "Artistic Interests", "Emotionality", "Adventurousness", "Intellect", "Liberalism")
)

traits = c("Extraversion", "Conscientiousness", "Agreeableness", "Neuroticism", "Openness to Experience")
traits_title = c(
  "Extraversion vs Intraversion", 
  "Conscientiousness vs. lack of direction", 
  "Agreeableness vs. antagonism", 
  "Neuroticism vs. emotional stability", 
  "Openness vs. closedness to experience"
)

ordering = c(1, 2, 3, 4, 5)
traits = traits[ordering]
traits_title = traits_title[ordering]
scale_ordering = rep(ordering, each=6)*6-6+(1:6)
scales = scales[scale_ordering]

alignment_index_dim1 = which(traits == "Agreeableness")
alignment_index_dim2 = which(traits == "Conscientiousness")

high_score_cutoff = 4.01

foTraitScoreToIndex <- function(trait_score, invert = T){
  index = NA
  if(trait_score >= high_score_cutoff){
    index = 5
  }
  if((trait_score > 1.5) && (trait_score < high_score_cutoff)){
    index = 4
  }
  if((trait_score >= -1.5) && (trait_score <= 1.5)){
    index = 3
  }
  if((trait_score > (-1 * high_score_cutoff)) && (trait_score < -1.5)){
    index = 2
  }
  if(trait_score <= (-1 * high_score_cutoff)){
    index = 1
  }
  if(invert){
    index = 6 - index;
  }
  
  return(index)
}

new_line = "<br>"

on_ready <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "Shiny.setInputValue('initialized', 1);",
  "Shiny.setInputValue('pathname', window.location.href);", 
  "});",
  "",
  "});",
  sep = "\n"
)

ui <- fluidPage(
    useShinyjs(),
    tags$head(
      tags$title("Big 5 Describe Me!"), 
      tags$meta(name = "title", content="Big 5 Describe Me!"), 
      tags$link(rel="shortcut icon", href="favicon.png"),
      includeCSS("www/style.css"),
      tags$script(src = "script.js"),
      tags$script(on_ready),
    ),

    tags$div(
      style = "
        display: grid;
        place-items: center;
        padding-top: 20vh;
        padding-left: calc(33vw - 130px);
        padding-right: calc(33vw - 130px);
      ", 
      tags$div(
        style = "
          display: grid; 
          place-items: center; 
          border: solid;
          border-color: #eaeaef;
          border-width: 3px;
          padding: calc(8px + 1vw);
          border-radius: 5%;", 
        # tags$h3(style = "font-weight:bold;", "Welcome to Big 5 Describe Me!"),
        tags$p(style = "font-weight:bold;font-size:170%;", "Welcome to Big 5 Describe Me!"),
        tags$hr(style = "margin-top: min(12px, 2vw); margin-bottom: min(12px, 2vw);"), 
        tags$div(
          textInput("bigfive_link", "Enter Link or ID:", value = "", width = NULL, placeholder = "http://bigfive-test.com/result/abcdefgh"),
          uiOutput("error_message")
        ), 
        tags$div(
          actionLink("example_button", "Example", style = "border-color: #88c; margin:6px;", icon = icon("book")),
          actionButton("run_button", "Let's Go!", style = "background-color: #CCFECC; font-size:120%; border-color: #686;"),
          tags$a("Take the test", 
                     href = "https://bigfive-test.com/",
                     target = "_blank", 
                     icon("link"), style = "border-color: #88c; margin:6px;")
        ), 
      ),
      
      conditionalPanel(condition="!$('html').hasClass('shiny-busy')", 
        tags$div(
          style = "margin-top:2vh;", 
          uiOutput("overview_panel", inline = F),
          uiOutput("highlights_panel", inline = F),
          uiOutput("primary_traits_panel", inline = F),
          uiOutput("secondary_traits_panel", inline = F),
          tags$div(style = "margin-bottom:24px;"), 
          tags$hr(), 
          uiOutput("details_panel", inline = F),
          uiOutput("share_results", inline = F)
        )
      ), 
      conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
        tags$div("", tags$span(class = "loadingbar-custom"), id="loadmessage")    
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  started = reactiveVal(FALSE);
  
  current_link = reactiveVal(""); 
  current_scores = reactiveVal(c())
  current_error = reactiveVal(""); 
  
  observeEvent(input$initialized, {
    if(!started()){
      query <- parseQueryString(session$clientData$url_search)
      query_s = paste(names(query), query, sep = "=", collapse=", ")
      query_names = names(query)
      # message(input$pathname)
      # browser()
      # message(query_s)
      # token = query[["token"]]
      if(!is.null(query_s) && query_s != "" && length(query_names) >= 1){
        url = paste0("http://bigfive-test.com/result/", query_names[[1]])
        updateTextInput(session, "bigfive_link", value = url)
        delay(100, foRunButton())
        # foRestoreConfiguration(token)
      }
      started(TRUE)
    }
  })
  
  observeEvent(input$example_button, {
    current_error("");
    url_list = c(
      "http://bigfive-test.com/result/62bf4338a2518a00091d979d", 
      "http://bigfive-test.com/result/62e0482f7070e4000981e7d8", 
      "http://bigfive-test.com/result/62e0ecc95b43470009572b96", 
      "http://bigfive-test.com/result/62e075260281f400090d0d66", 
      "http://bigfive-test.com/result/62a1e6f1b7e2050009730677", 
      "http://bigfive-test.com/result/65215a3834aefd0008b6a846")
    url = url_list[sample(1:length(url_list), 1, replace = TRUE)]
    # current_link("")
    # current_scores(c())
    updateTextInput(session, "bigfive_link", value = url)
    delay(100, foRunButton())
  })
  
  foRunButton <- function(){
    tryCatch({
      library(htm2txt)
      url = input$bigfive_link
      url = gsub("https://", "http://", url)
      if(!startsWith(url, "http://") ){
        url = paste0("http://bigfive-test.com/result/", url)
      }
      text <- gettxt(url)
      if(text == ""){
        current_error("Please enter a valid link")
        return();
      }
      scores = rep(NA, length(scales))
      for(iScale in 1:length(scales)){
        pattern <- paste0(scales[iScale], "\\n\\nscore: (\\d+)")
        score <- str_extract(text, pattern)
        if (!is.na(score)) {
          score <- str_match(text, pattern)[, 2]
          score = as.integer(score)
        }
        scores[iScale] = score
      }
      if(is.na(sum(scores))){
        current_error("Please enter a valid link")
        return(); 
      }
      current_link(url)
      current_scores(scores)
      current_error("")
    }, 
    error=function(e) {
      current_error("Please enter a valid link");
      message(paste("in err handler2\n"),e)
    })
  }
  
  observeEvent(input$run_button, {
    foRunButton()
  })
  
  output$error_message <- renderUI({
    req(current_error())
    tags$p(
      style = "text-align: left; font-weight:bold; color: red; font-size:85%; margin-left:0px; margin-top:-10px; margin-bottom:10px;", 
      "Please enter a valid link"
    )
  })
  
  is_ready <- reactive({
    validate(
      need(length(current_scores()) > 0, ""),
      need(length(current_link()) > 0, "")
    )
    return(T)
  })
  
  current_trait_scores <- reactive({
    req(is_ready())
    scores <- current_scores()
    trait_scores = rep(0, length(traits_title))
    for(iTrait in 1:length(traits_title)){
      scale_start = (iTrait - 1) * 6 + 1
      for(iScale in scale_start:length(scales)){
        score = scores[iScale] - 12
        trait_scores[iTrait] = trait_scores[iTrait] + score
        if(iScale >= (6*iTrait)){
          break;
        }
      }
    }
    trait_scores = trait_scores / 6
    return(trait_scores)
  })
  
  output$overview_panel <- renderUI({
    req(is_ready())
    scores <- current_scores()
    trait_scores <- current_trait_scores()
    
    overview = ""
    for(it in 1:3){
      for(iTrait in 1:length(traits_title)){
        trait = traits[iTrait]
        trait_title = traits_title[iTrait]
        trait_score = trait_scores[iTrait]
        
        target = ""
        valid = F
        if(it == 1){
          if(trait_score >= high_score_cutoff){
            target = "Long Positive"
            valid = T
          }
          if(trait_score <= (-1 * high_score_cutoff)){
            target = "Long Negative"
            valid = T
          }
        }
        if(it == 2){
          if((trait_score > 1.5) && (trait_score < high_score_cutoff)){
            target = "Slight Positive"
            valid = T
          }
          if((trait_score < -1.5) && (trait_score > (-1 * high_score_cutoff))){
            target = "Slight Negative"
            valid = T
          }
        }
        if(it == 3){
          if((trait_score <= 1.5) && (trait_score >= -1.5)){
            target = "Neutral"
            valid = T
          }
        }
        
        if(valid){
          trait_value = adjective_data[[trait]][[target]]
          trait_score_txt = sprintf(" (%+.1f %s)", trait_score, tolower(trait))
          overview = paste0(overview, "* ", trait_value, trait_score_txt, new_line)
        }
      }
    }
    
    index1 = foTraitScoreToIndex(trait_scores[alignment_index_dim1]);
    index2 = foTraitScoreToIndex(trait_scores[alignment_index_dim2]);
    alignment_id = paste0(index1, "-", index2)
    aligment_txt = alignment_data[[alignment_id]][["Alignment"]]
    class_txt = alignment_data[[alignment_id]][["Name"]]
    description_txt = alignment_data[[alignment_id]][["Description"]]
    color_html = alignment_data[[alignment_id]][["Color"]]
    
    tooltip_html = sprintf("<span style = 'border-color:%s'><b>%s</b><br>%s<hr style ='margin:7px;'><text style = 'color:%s'>%s<text></span>", darken(color_html, 0.15), class_txt, description_txt, color_html, aligment_txt)
    
    alignment_line = paste0("Alignment: ",  sprintf("<text class = 'alignment_txt'>"), aligment_txt, "</text>")
    class_line = paste0("Class: ", "<text class = 'alignment_txt' href='#'", ">", class_txt, "  </text>")
    alignment_html_txt = paste0("<a class = 'custom_tooltip'>", alignment_line, ", ", class_line, tooltip_html, "</a>")
    
    tags$div(
      style = "
        margin-top: 14px;
        font-size: 105%; 
        text-align: center; 
        display: grid;
        place-items: center;", 
      tags$b(style = "display:block; font-size: 110%; color: #d96402;", "Overview "), 
      tags$div(
        style = paste(
          "margin-top: 2px;", 
          "padding: 12px;",
          "border-color: #ffb094;", 
          "border-style: solid;", 
          "border-width: 3px;", 
          "border-radius: 15% 15% 15% 15%;", 
          "width: fit-content;", 
          "text-align: left;", 
          "font-size: 105%;"
        ), 
        HTML(overview)
      ), 
      tags$div(
        style = "margin-top: 2px;", 
        HTML(alignment_html_txt)
      )
    )
  })
  
  output$highlights_panel <- renderUI({
    req(is_ready())
    scores <- current_scores()
    
    output = ""
    for(iScale in 1:length(scales)){
      scale = scales[iScale]
      score = scores[iScale] - 12
      valid = F
      if(score >= 5){
        target = "Positive"
        valid = T
      }
      if(score <= -5){
        target = "Negative"
        valid = T
      }
      if(valid){
        value = adjective_data[[scale]][[target]]
        if(output == ""){
          output = value; 
        } else {
          output = paste(output, value, sep = ", ")
        }
      }
    }
    
    tags$div(
      style = "
          margin-top: 10px;
          margin-bottom: 14px; 
          font-size: 105%; 
          text-align: center; 
          display: grid;
          place-items: center;", 
      tags$b(style = "display:block; font-size: 110%; color: #0020c8;", "Highlights "), 
      tags$div(
        style = paste(
          "margin-top: 2px;", 
          "padding: 12px;",
          "border-color: #9a94ff;", 
          "border-style: solid;", 
          "border-width: 3px;", 
          "border-radius: 15%;", 
          "width: fit-content;", 
          "text-align: center;", 
          "max-width: 85%;", 
          "font-size:105%"
        ), 
        HTML(output)
      )
    )
  })
  
  output$primary_traits_panel <- renderUI({
    req(is_ready())
    scores <- current_scores()
    
    detailed_output = ""
    for(iScale in 1:length(scales)){
      scale = scales[iScale]
      score = scores[iScale] - 12
      valid = F
      if(score >= 5){
        target = "Long Positive"
        valid = T
      }
      if(score <= -5){
        target = "Long Negative"
        valid = T
      }
      if(valid){
        value = adjective_data[[scale]][[target]]
        valuex = paste0(
          "- ", value, sprintf(" (%+0d %s)", score, tolower(scale))
        )
        detailed_output = paste0(detailed_output, valuex, new_line)
      }
    }
    
    tags$div(
      style = "
          margin-top: 20px;
          margin-bottom: 16px; 
          font-size: 105%; 
          text-align: center; 
          display: grid;
          place-items: center;", 
      tags$b(style = "display:block; font-size: 110%; color: #319400;", "Defining characteristics"), 
      tags$div(
        style = paste(
          "margin-top: 2px;", 
          "padding: 12px;",
          "border-color: #7fc600;", 
          "border-style: solid;", 
          "border-width: 3px;", 
          "border-radius: 10%;", 
          "width: fit-content;", 
          "text-align: left;", 
          "max-width: 85%;", 
          "font-size:100%"
        ), 
        HTML(detailed_output)
      )
    )
  })
  
  output$secondary_traits_panel <- renderUI({
    req(is_ready())
    scores <- current_scores()
    
    detailed_output = ""
    for(iScale in 1:length(scales)){
      scale = scales[iScale]
      score = scores[iScale] - 12
      valid = F
      if((score >= 2) && (score < 5)){
        target = "Slight Positive"
        valid = T
      }
      if((score <= -2) && (score > -5)){
        target = "Slight Negative"
        valid = T
      }
      if(valid){
        value = adjective_data[[scale]][[target]]
        valuex = paste0(
          "- ", value, sprintf(" (%+0d %s)", score, tolower(scale))
        )
        detailed_output = paste0(detailed_output, valuex, new_line)
      }
    }
    
    tags$div(
      style = "
          margin-top: 16px;
          margin-bottom: 16px; 
          font-size: 105%; 
          text-align: center; 
          display: grid;
          place-items: center;", 
      tags$b(style = "display:block; font-size: 105%; color: #333;", "Less prominent characteristics"), 
      tags$div(
        style = paste(
          "margin-top: 2px;", 
          "padding: 12px;",
          "border-color: #cfcfcf;", 
          "border-style: solid;", 
          "border-width: 3px;", 
          "border-radius: 10%;", 
          "width: fit-content;", 
          "text-align: left;", 
          "max-width: 85%;", 
          "font-size:95%"
        ), 
        HTML(detailed_output)
      )
    )
  })
  
  
  output$details_panel <- renderUI({
    req(is_ready())
    scores <- current_scores()
    trait_scores <- current_trait_scores()
    
    traits_output = ""
    for(iTrait in 1:length(traits_title)){
      scale_start = (iTrait - 1) * 6 + 1
      
      trait = traits[iTrait]
      trait_title = traits_title[iTrait]
      traits_output = paste0(traits_output, "<b>", trait_title, ": ", "</b>", new_line)
      trait_score = trait_scores[iTrait]
      
      target = ""
      if(trait_score >= high_score_cutoff){
        target = "Long Positive"
      }
      if(trait_score <= (-1 * high_score_cutoff)){
        target = "Long Negative"
      }
      if((trait_score > 1.5) && (trait_score < high_score_cutoff)){
        target = "Slight Positive"
      }
      if((trait_score < -1.5) && (trait_score > (-1 * high_score_cutoff))){
        target = "Slight Negative"
      }
      if((trait_score <= 1.5) && (trait_score >= -1.5)){
        target = "Neutral"
      }
      trait_value = adjective_data[[trait]][[target]]
      # trait_value = ""
      trait_score_txt = sprintf(" (%+.1f %s)", trait_score, tolower(trait))
      traits_output = paste0(traits_output, "* ", trait_value, trait_score_txt, new_line)
      for(iScale in scale_start:length(scales)){
        scale = scales[iScale]
        score = scores[iScale] - 12
        target = ""
        if(score >= 5){
          target = "Long Positive"
        }
        if(score <= -5){
          target = "Long Negative"
        }
        if((score >= 2) && (score < 5)){
          target = "Slight Positive"
        }
        if((score <= -2) && (score > -5)){
          target = "Slight Negative"
        }
        if((score <= 1) && (score >= -1)){
          target = "Neutral"
        }
        value = adjective_data[[scale]][[target]]
        valuex = paste0(
          "- ", value, sprintf(" (%+0d %s)", score, tolower(scale))
        )
        traits_output = paste0(traits_output, valuex, new_line)
        if(iScale >= (6*iTrait)){
          break;
        }
      }
      if(iTrait < length(traits_title)){
        traits_output = paste0(traits_output, new_line)
      }
    }
    
    tags$div(
      style = "
          margin-top: 20px;
          margin-bottom: 16px; 
          font-size: 105%; 
          text-align: center; 
          display: grid;
          place-items: center;", 
      tags$b("Complete List", 
             style = "display:block; font-size: 110%; color: #333;"), 
      tags$div(
        style = paste(
          "margin-top: 2px;", 
          "padding: 16px;",
          "border-color: #cfcfcf;", 
          "border-style: dashed;", 
          "border-width: 3px;", 
          "border-radius: 3%;", 
          "width: fit-content;", 
          "text-align: left;", 
          "max-width: 90%;", 
          "font-size:95%"
        ), 
        HTML(traits_output)
      )
    )
  })
  
  output$share_results <- renderUI({
    req(is_ready())
    link = current_link()
    pathname = input$pathname
    pathname = str_extract(pathname, "^[^?]*")
    identifier = str_extract(link, "(?<=/)[^/]*$")
    
    tags$div(
      style = "
        font-size: max(120%, 16px); 
        margin-right: 4px;
        display: grid;
        place-items: center;", 
      tags$span(
        id = "snackbar", 
        "Successfully Copied"
      ), 
      tags$span(
        tags$a(
          id = "config_link_element", 
          target = "_blank", 
          "Share your results", 
          icon("share-from-square"), 
          href = paste0(pathname, "?", identifier), 
        ),
        tags$a( 
          style = "font-size:110%;", 
          id = "copy_clipboard_element", 
          title = "Copy link", 
          " ", 
          target = "_blank", 
          icon("copy"), 
          # value = paste0(pathname, "?", identifier), 
          onclick = sprintf("fox('%s')", paste0(pathname, "?", identifier))
        ),
      )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
