#---------------------------------------------------------------------------------#
#Fines and Fees Calculator Shiny App----
#created:       2021/10/19 by Adam S. Cohen (Hawaii State Judiciary)
#Last modified: 2024/09/09 by Adam S. Cohen (Hawaii State Judiciary)
#---------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------#
# load packages ----
#---------------------------------------------------------------------------------#
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)
library(shinyBS)
library(stringi)
library(shinyjs)
# remotes::install_github("dreamRs/capture")
library(capture)
library(bsplus)
library(digest)
# devtools::install_github("karthik/rdrop2")
# remotes::install_version("rdrop2", "0.8.2.1")
library(rdrop2)
library(shinylogs)
# library(googledrive)
# library(googlesheets4)
# library(shinyCleave)
# library(rebus)
library(shinyvalidate)
library(gfonts)

#---------------------------------------------------------------------------------#
#                     ---START DROPBOX PERSISTENT STORAGE----                                       
#---------------------------------------------------------------------------------#
#h/t: https://deanattali.com/blog/shiny-persistent-data-storage/#dropbox
##updated method to address issue with short term authentication
##https://github.com/karthik/rdrop2/issues/201
##https://stackoverflow.com/questions/71393752/get-a-refresh-token-for-dropbox-api-using-rdrop2-and-drop-auth
##https://github.com/karthik/rdrop2/issues/206
#---------------------------------------------------------------------------------#
# token<-drop_auth(new_user = TRUE)
# saveRDS(token,'H:\\p11_LFO_calculator\\lfo_calculator\\droptoken.rds')
token<-readRDS("droptoken.rds")
# token<-readRDS("H:\\p11_LFO_calculator\\lfo_calculator\\droptoken.rds")
token$refresh()

TABLE_NAME <- "responsesLFO" #Fine and fee info
TABLE_NAME_2 <- "shinylogs" #user and widget info

# get a formatted string of the timestamp
get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

fields_all <- c("role", "chargeDate", "charges")

file_linker <- capture.output(cat(runif(6,0,9)%/%1, sep = "")) #unique identifer so we can link the two output files

save_data_dropbox <- function(data) {
  # Create a temporary file to hold the data
  # data <- t(data) #comment out if using data in reactives, comment in if using inputs 
 
  file_name <- paste0(
    paste(
      get_time_human(),
      file_linker,
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
  
  file_path <- file.path(tempdir(), file_name)
  write.csv(data ,file_path, row.names = FALSE, quote = TRUE)
  
  # Upload the file to dropbox
  drop_upload(file_path, path = TABLE_NAME)
}

#attempt to use shinylogs with dropbox
#use data argument (form_data()) to create filename so that it matches LFO data filename
#use shinylog argument to pass data to save
save_shinylogs_dropbox <- function(data) {
  # Create a temporary file to hold the data
  # data <- t(data) #comment out if using data in reactives, comment in if using inputs

  file_name <- paste0(
    paste(
      get_time_human(),
      file_linker,
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
  
  file_path <- file.path(tempdir(), file_name)
  write.csv(data ,file_path, row.names = FALSE, quote = TRUE)

  # Upload the file to dropbox
  drop_upload(file_path, path = TABLE_NAME_2)
  # drop_upload(file_path, path = TABLE_NAME, dtoken = token)
}

#odd behavior: file not saved to dropbox unless the load data function is also run
#testing shows that the drop_read_csv in the lapply is critical to also triggering the saving of data (2022/12/09)
load_data_dropbox <- function() {
  files_info <- drop_dir(TABLE_NAME)
  file_paths <- files_info$path_display
  # Only take the last 20 because each file takes ~1 second to download
  file_paths <- tail(file_paths, 5)
  data <-
    lapply(file_paths, drop_read_csv, stringsAsFactors = FALSE) %>%
    do.call(rbind, .)
  
  data
}

#---------------------------------------------------------------------------------#
#                      ---END DROPBOX PERSISTENT STORAGE----                                       
#---------------------------------------------------------------------------------#

#-----------------------------------#
#read in files----
#-----------------------------------#
#calc_inputs contains two lists
#charge list (for step 1 of calc) that’s been filtered to include 1st Circuit, traffic, non-felonies and appended with seat belt offenses and cell phone offenses not in top 100 and pedestrian cross walk offenses
#fines and fees list (for step 3 of calc)
calc_inputs <- readRDS("calc_inputs_2023-06-21_1405.rds")

lfoTableJIMS_chargeList_non_felony <- calc_inputs[[2]] %>% 
  filter(!(grepl("HRS 431:10C-104",CHARGE_CODE) & CHARGE_LEVEL == "PM")) %>%  #remove to avoid confusion with VL and MD, wait until we decide on how we want to include it with VL and MD
  filter(!(grepl("^HRS 291C-102\\(a\\)\\(1\\)$",CHARGE_CODE) & is.na(CHARGE_QUALIFIER))) %>%   #remove so people don't select it by accident
  filter(!(grepl("^HRS 286-102",CHARGE_CODE) & !is.na(CHARGE_QUALIFIER)))  #remove JUV version, added checkbox at step 3 so user can select Juvenile

lfoTableJIMS_non_felony <- calc_inputs[[1]]

#-----------------------------------#
#set up HTML/CSS----
#-----------------------------------#
HTML('<head>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
        </head>')

#-----------------------------------#
#set up JS print function for summary page
#h/t: https://stackoverflow.com/questions/41459361/rshiny-print-current-page
#custom date in filename, and b/c it changes the title of the webpage/tab, change back after printing
#https://stackoverflow.com/questions/10189225/how-to-create-a-custom-file-name-for-a-file-while-saving-the-print-page
#custom date in filename, add 1 to month since getMonth() goes from 0-11
#h/t: https://stackoverflow.com/questions/2013255/how-to-get-year-month-day-from-a-date-object/60609524
#add leading zeros to custom date and add +1 one to month without concatenating month and 1
#h/t: https://stackoverflow.com/questions/3605214/javascript-add-leading-zeroes-to-date
#-----------------------------------#
#custom code to display HST i.e. Pacific/Honolulu time
#h/t (custom time zone function): https://stackoverflow.com/questions/15141762/how-to-initialize-a-javascript-date-to-a-particular-time-zone
#test code from jsfiddle, shows how to print date to screen/console and some other neat tricks:
#h/t (how to test print to console: https://stackoverflow.com/a/18537115
jsCode <- "shinyjs.winprint = function(){
 var shinyappsTime = new Date();
 var invdate = new Date(shinyappsTime.toLocaleString('en-US', {
    timeZone: 'Pacific/Honolulu'
  }));

  var diff = shinyappsTime.getTime() - invdate.getTime();
  var hnl = new Date(shinyappsTime.getTime() - diff); // needs to substract

document.title = 'LFO_calculator_summary_'+hnl.getFullYear()+'-'+('0'+(hnl.getMonth()+1)).slice(-2)+'-'+('0'+hnl.getDate()).slice(-2);window.print();
document.title = 'Hawaii Judiciary Fines and Fees Calculator'
}"

#-----------------------------------#
# enhancement to bs_accordion----
# allow multiple panels to be open and to control which if any panels are initially open
#h/t: https://github.com/ijlyttle/bsplus/issues/112
#h/t: https://github.com/ijlyttle/bsplus/pull/113
#-----------------------------------#
bs_multi_open <- function(X, multi=TRUE, open=1) {
  for(i in 1:length(X$children)) 
  {
    if(multi)
      # Remove 'data-parent' attribute so multiple panels can be open at once
      X$children[[i]]$children[[1]]$attribs$`data-parent` <- NULL    
    
    # Remove 'in' class to prevent *any* panel from starting as open    
    classAttribs <- which(names(X$children[[i]]$children[[2]]$attribs) == "class")
    for(j in classAttribs)
    {
      if(X$children[[i]]$children[[2]]$attribs[j]=="in")
      {
        X$children[[i]]$children[[2]]$attribs[j] <- NULL
      }
    }
    
    # add 'in' class (back) to panels selected to start as open
    if(i %in% open)
      X$children[[i]]$children[[2]]$attribs <- append(X$children[[i]]$children[[2]]$attribs, list(class="in"))
  }
  X
}

#-----------------------------------#
# Define UI for application----
#-----------------------------------#

ui <- dashboardPage(
    
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      
        # Application title
      fluidRow(style = "background-image: url(windward-glory-kaneohe-bay-rainbowD.jpg); background-size: cover; margin:-25px;",
               column(width = 12, align = 'right',
                   #h/t: https://www.w3schools.com/css/css_font.asp
                   #h/t: https://stackoverflow.com/questions/48028990/what-is-the-shiny-default-font
                   titlePanel(title = div(HTML("Fines and Fees Calculator&nbsp"), 
                                          style = 'color:white',#font-family: Metropolis, "Times New Roman", Helvetica, Arial, sans-serif; 
                                          # img(src='hawaii seal _judiciary.png', href = "https://www.courts.state.hi.us/", height = '6%', width = '6%')
                                          a(href = "https://www.courts.state.hi.us/", img(src='hawaii seal _judiciary.png', height = '9%', width = '9%'), target = "_blank")),
                              windowTitle = 'Hawaii Judiciary Fines and Fees Calculator')
            )#column
        ),#fluidRow header - title
        br(),
        br(),
        
      use_font("montserrat", "www/css/montserrat.css", selector = c("div","h3","h4")),#default selector is "body" but wasn't selecting everything
      useSweetAlert(),#for modal at step 2, contest ticket
      useShinyjs(),
      extendShinyjs(text = paste(jsCode#,
                                    # 'document.title = LFO_calculator_summary.pdf;'
                                    ), functions = c("winprint")),
        
        #set default orientation to landscape for printing
        #h/t: https://stackoverflow.com/questions/138422/landscape-printing-from-html/
        #h/t: https://stackoverflow.com/questions/4249532/is-page-sizelandscape-obsolete
        # tags$style(type ='text/css', '@media print{@page {size: landscape}}'),
        tags$style(type ='text/css', '@media print{@page {size: portrait;
                                                          margin-left: 0.25in;
                                                          margin-right: 0.25in;
                   }}'),
      

        #validation error formatting
        tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight:bold;
      }
                        ")),
      
      #to customize color of shinyWidgets actionBttn
      #h/t: https://stackoverflow.com/questions/59150267/how-to-override-actionbttn-color-in-shinywidget-package
      tags$style(HTML(".buttonStartCalc .bttn-primary{background-color: #3c8dbc;}")),
      
      # tags$style(HTML('.box {font-family: "Times New Roman";}')),
      
        #for floating ATP and CSW boxes at step 3
        #h/t: https://stackoverflow.com/questions/75902762/getting-css-positionsticky-to-work-with-shinydashboard
        tags$head(
          tags$style(HTML(
            "#hhh {position: sticky; top: 0;}
        .wrapper {overflow: visible;}"
          ))
        ),
        
        tabsetPanel(
            id = "wizard",
            type = "hidden",
            
            #-----------------------------------#
            #tabPanel page_0----
            #-----------------------------------#
            tabPanel("page_0",
                     ##Blurb----
                     fluidRow(column(width = 6, offset = 3,
                                     # h3("Welcome to the Hawaii Judiciary's"),
                                     h3(HTML("Welcome to the Hawai&#699i Judiciary's<br><span style='font-size:22px;color:#2758a5;font-style:italic;'>&nbspFINES AND FEES CALCULATOR&nbsp</span>"), style = "display:inline-block;border-left:6px solid orange;border-bottom:6px solid orange;"),
                                     br(),br(),
                                     HTML("<span style='font-size:16px;'>This calculator is an online tool designed to help people calculate the fines and fees owed on a specific ticket, and if eligible, request either a reduction in their fines or a conversion to community service. This calculator can be used by defendants, attorneys, the court, and the general public.<br>
                                          <br>
                                          The calculator provides only an <u>estimate</u> of the fines and fees a person may owe. Please note that certain fines and fees are mandatory and may not be eligible for reduction or conversion to community service. 
                                          All requests will be decided by the judge.<br>
                                          <br>
                                          Some offenses may carry additional requirements/penalties, including but not limited to, driver license suspension, driver improvement class, child passenger restraint class, and/or jail.</span>")
                                     )),
                     br(),
                     ##Calc button----
                     fluidRow(
                         column(width = 6, offset = 3, align = 'center',
                                div(class = "buttonStartCalc",
                                actionBttn(inputId = 'startCalc',
                                           # label = 'Go to the calculator (no data storage and no API)',
                                           label = 'Go to the calculator',
                                           # width = '33%',
                                           style = 'jelly',
                                           color = 'primary')
                                )#div
                                )#column
                         ),#fluidRow #
                     br(),
                     
                     ##FAQs----
                     fluidRow(
                         column(width = 6, offset = 3, 
                                box(status = "primary", solidHeader = TRUE, width = 12,
                                    title = HTML('<span style="font-size:20px;">Frequently Asked Questions</span>'),
                                    bs_accordion(id = 'faqLFOcalc') %>%
                                      bs_append(title = "Do I have to pay to use this calculator?", content = HTML("<span style='font-size:16px'>No, there is no fee to use this tool.</span>")) %>%
                                      bs_append(title = "How do I look up the fines and fees on my ticket?", content = HTML('<span style="font-size:16px">Look for the charges/violations written on your ticket. For example, ROH 15-22.11(b) EXPIRED PRKG METER. Input the violation exactly how it is written on your ticket. If you don\'t have the ticket, you may look it up on  <a href="http://jimspss1.courts.state.hi.us:8080/eCourt/", target = "_blank">eCourt Kokua</a> (more info <a href = "https://www.courts.state.hi.us/legal_references/records/jims_system_availability", target = "_blank">here</a>) by your name (Party Search), license plate/VIN for parking tickets (Vehicle Search), or citation number (Case Search).</span>')) %>%
                                      bs_append(title = "What if I have more than one ticket?", content = HTML("<span style='font-size:16px'>You will need to look up each ticket. If your ticket has more than one charge/violation, you can input up to 11 charges from a single ticket into the calculator.</span>")) %>%
                                      bs_append(title = "What if I do not have enough money to pay for my ticket?", content = HTML('<span style="font-size:16px">If you don\'t have enough money to pay the monetary assessment due to financial hardship, you have options. <br><br> <ul> <li>You may ask the court to consider your financial circumstances in determining any fine and/or imposing community service in lieu of a fine. The court does not have access to your financial information, so it is your responsibility to provide such information.* </li><br><li>If your financial circumstances change (for example, if you lose your job after the judgment is entered), you may request to reduce your fine or convert it to community service.</li></ul> You can request a reduction in your fines or conversion to community service either in person or by written statement. Please see the back of your ticket for instructions on how to request a hearing or submit a written statement.<br><br>*The calculator is designed to help you collect this information and have it ready whether you make the request in person or by written statement. You will be required to provide information about your household income, your expenses, and how much you can pay per month.  You will also need to present copies of your paystubs or other income information. This does not guarantee the court will lower the amount on your fines or give you the chance to do community service.</span>')) %>%
                                      bs_append(title = "Can I pay for my ticket here?", content = HTML('<span style="font-size:16px">No, this tool is not for payment. if you want to pay for the ticket and it has not been longer than 21 days since receiving it, you can pay online at <a href="https://etraffic.ehawaii.gov/etraffic/home", target = "_blank">https://etraffic.ehawaii.gov/etraffic/home</a>. If more than 21 days has passed, then information on how to pay is on the back of the ticket.</span>')) %>%
                                      bs_append(title = "Can I contest my ticket here?", content = HTML("<span style='font-size:16px'>No, this site is not to challenge or dispute your ticket.  To do so, please read the instructions on the back of your ticket.</span>")) %>%
                                      # bs_append(title = "What if I do not have the ability to pay for my ticket?", content = HTML('If you cannot afford to pay for the fine and fees, you are welcome to submit a request to reduce the fees and/or set up a payment plan associated with the ticket. You will be required to submit a <a href="Declaration of Financial Status.docx", target = "_blank">Declaration of Financial Status</a> and provide relevant information for the consideration of the court, including but not limited to employment information, household income, current debt, and how much you can pay per month.  Please have copies of paystubs or other relevant income information ready to upload.  This does not guarantee a reduction in your ticket, but only your request.  Additionally, if eligible, you may request to convert the fines and fees to community service.')) %>%
                                      bs_append(title = "Does the calculator track my personal information?", content = HTML("<span style='font-size:16px'>The data you enter is saved for aggregate statistics only. We do not collect case or ticket numbers, so the information you enter is not linked to you or your case unless you bring copies to your court appearance and submit them to the court.</span>")) %>% 
                                      bs_append(title = "Who can I contact with questions, feedback, or suggestions relating to the calculator?", content = HTML("<span style='font-size:16px'>Please contact <a href = 'mailto:finescalculator@courts.hawaii.gov'>finescalculator@courts.hawaii.gov</a>. We will aim to respond to questions within 48 hours. Please note that <em>we cannot provide any legal advice</em>. For legal advice, please contact an attorney, or if you cannot afford one, please consider these <a href=' https://www.courts.state.hi.us/self-represented-litigants-srl', target = '_blank'>resources</a>.</span>")) %>% 
                                      bs_append(title = "Acknowledgements", content = HTML("<span style='font-size:16px'>This calculator was inspired by the <a href ='https://lfocalculator.org/', target = '_blank'>Washington State LFO Calculator</a>. We gratefully acknowledge their pioneering efforts and thank them for their time helping us get started.</span>")
                                                ) %>% 
                                      bs_multi_open(multi = FALSE,
                                                    open = c()
                                                    )
                                )#box
                                )#column
                         )#fluidRow
                     ),#tabPanel page_0
            
            #-----------------------------------#
            #tabPanel page_1----
            #-----------------------------------#
            tabPanel("page_1", 

                    # progress meter/line
                    fluidRow(
                        column(width = 6, offset = 3, align = 'center',
                               box(width = 12,
                                   title = HTML("<div><img src='number1.png' height = 20px width = 20px'/><span style='color:#0073b7;'><strong> Charges </strong></span><hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number2.png' height = 20px width = 20px> Ability to Pay <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number3.png' height = 20px width = 20px> Fines and Fees <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number4.png' height = 20px width = 20px> Summary</div>")
                               )#box
                               )#column
                        ),#fluidRow progress meter/line
                    
                    # box to select role, date of charges, and charges
                     fluidRow(
                         column(width = 6, offset = 3,
                                box(title = 'Charges', solidHeader = TRUE, status = 'primary', width = 12, align = 'center',
                                    
                                    ##select role----
                                    selectInput(input = 'role',
                                                label = 'Select your role',
                                                choices = c('Defendant','Public Defender/Defense Attorney','Prosecutor','Judge','Other'),
                                                selected = 'Defendant',
                                                width = '50%'),
                                    
                                    ##select date----
                                    #replaced dateInput with airDatepickerInput b/c unable to center align date value using dateInput
                                    airDatepickerInput(inputId = 'chargeDate',
                                              label = list('Select date of citation ',br(),
                                                           #h/t: https://stackoverflow.com/questions/40513153/shiny-extra-white-space-in-selectinput-choice-display-label
                                                           # stri_dup(intToUtf8(160), 3), # Replace 6 with the desired number,
                                                           actionLink(inputId = 'infoChargeDate',
                                                                        label = '(Why do I need to provide this information?)'
                                                                        #icon = icon('info-circle'),
                                                                        # class = 'btn-xs'
                                                           ),
                                                           tags$style(".popover{max-width:30%;}"),
                                                           bsPopover(id = 'infoChargeDate',
                                                                     title = 'Why do I need to provide this information?',
                                                                     content = paste('Fines and fees change over time (for example, when laws change). The calculator needs the date to select the fines and fees that applied at the time of your citation. Failing to enter the correct date may yield inaccurate fines and fees.',
                                                                                   em('What if I have citations with different dates?'),
                                                                                   'If you have tickets with different dates, please run the calculator separately for each citation.', sep = '<br>'),
                                                                     placement = 'right',
                                                                     # trigger = c('hover','focus'),
                                                                     trigger = c('focus'),
                                                                     options = list(container = 'body')
                                                           )#bsPopover
                                              ),#label list
                                              value = Sys.Date()+1, #appears to be a bug in airDatepickerInput, need to increment by 1 to get Sys.Date()
                                              minDate = '1960-01-01',
                                              maxDate = Sys.Date(),
                                              dateFormat = 'MMMM dd, yyyy',
                                              addon = 'none',
                                              width = '50%'),
                                    #center align date; dateInput, which is left aligned by default, doesn't center align using css, couldn't troubleshoot, so using airDatepickerInput
                                    tags$style(type="text/css", "#chargeDate {text-align: center;}"),
                                    
                                    ##select charges----
                                    #charge list is set up on server-side to speed up loading b/c list is large
                                    #OPTION 1: use css to position dropdown downward and use optionsCount to control how many options are visible in dropdown
                                    #dropdown open in downward direction, h/t: https://stackoverflow.com/questions/52270897/shiny-selectize-dropdown-menu-open-in-upward-direction
                                    # modify virtual select css https://github.com/sa-si-dev/virtual-select/blob/master/dist/virtual-select.min.css
                                    # tags$head(tags$style(type = "text/css", paste0(".vscomp-dropbox {
                                    #                     position: absolute !important;
                                    #                     bottom: auto !important;
                                    #                     top: 100% !important;
                                    #                  }}"))),
                                    #OPTION 2: use showDropboxAsPopup and popupDropboxBreakpoint to display dropdown as popup
                                    #see properties here: https://sa-si-dev.github.io/virtual-select/#/properties
                                    virtualSelectInput(inputId = 'charges',
                                                       label = list('Select charges',br(),
                                                                a(href="https://adamcohen3.github.io/courts/commonChargesReferenceSheet.html", target = "_blank", '(Help! I don\'t see my charge!)')),
                                                       choices = NULL,
                                                       multiple = TRUE,
                                                       search = TRUE,
                                                       placeholder = "Type one or more charges",
                                                       # hideClearButton = TRUE,
                                                       showSelectedOptionsFirst = TRUE,
                                                       showValueAsTags = TRUE,
                                                       showDropboxAsPopup = TRUE, #https://sa-si-dev.github.io/virtual-select/#/examples?id=show-dropbox-as-popup
                                                       popupDropboxBreakpoint = '3000px', #Maximum screen width that allowed to show dropbox as popup
                                                       dropboxWidth = "800px",
                                                       # popupPosition = "right",
                                                       optionsCount = 9,
                                                       # noOfDisplayValues = 3
                                                       disableSelectAll = TRUE,
                                                       html = TRUE,
                                                       hasOptionDescription = TRUE,
                                                       markSearchResults = TRUE
                                    ),
                                    
                                    ##select default judgment----
                                    radioGroupButtons(inputId = "default_judgment",
                                                 # label = HTML(paste0("Has a Default Judgment has been entered against you?", "<span style = 'color:red;'> *Required field</span>")),
                                                 label = list('Has a ',
                                                              actionLink(inputId = "default_judgment_info", label = "default judgment"),
                                                              # HTML('<span id="csw291D" style="color:blue;text-decoration:underline blue dotted;">community service</span>'),
                                                              'been entered against you?',
                                                              tags$style(".popover{max-width:30%;}"),
                                                              bsPopover(#session=session,
                                                                id="default_judgment_info",
                                                                title = "What is a default judgment?",
                                                                content = 'According to <a href= "https://www.courts.state.hi.us/docs/court_rules/rules/hctr.htm#Rule_3" target = "_blank">HCTR Rule 3(b)</a>, "<strong>Default Judgment.</strong> A judgment entered in favor of the State of Hawai‘i when a defendant fails to answer or respond, either in person or in writing, to the notice of infraction within twenty-one (21) calendar days from the date the notice of infraction was issued."<br><br> Please also refer to <a href= "https://www.courts.state.hi.us/docs/court_rules/rules/hctr.htm#Rule_15" target = "_blank">HCTR Rule 15</a> "DEFAULT JUDGMENT AND STOPPER," <a href= "https://www.courts.state.hi.us/docs/court_rules/rules/hctr.htm#Rule_18" target = "_blank">HCTR Rule 18</a> "POST-JUDGMENT RELIEF," and <a href= "https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291D/HRS_0291D-0007.htm" target = "_blank">HRS 291D-7</a> subsections (d) and (e).',
                                                                placement = 'right',
                                                                trigger = c('focus'),
                                                                options = list(container = 'body'))),
                                                 choices = c("Yes", "No"),
                                                 selected = character(0),
                                                 width = "81%",
                                                 justified = TRUE,
                                                 individual = TRUE),
                                    fluidRow(column(width = 12, align = 'center',
                                                    actionButton("page_10", "back"),
                                                    actionButton("page_12", "next"))
                                             )#fluidRow buttons
                                    )#box
                                )#column
                         )#fluidRow
            ), #tabPanel page_1

            #-----------------------------------#
            #tabPanel page_2----
            #-----------------------------------#
            tabPanel("page_2", 

                     # progress meter/line
                     fluidRow(
                         column(width = 6, offset = 3, align = 'center',
                                box(width = 12,
                                    title = HTML("<div><img src='number1.png' height = 20px width = 20px'/> Charges <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number2.png' height = 20px width = 20px><span style='color:#0073b7;'><strong> Ability to Pay </strong></span><hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number3.png' height = 20px width = 20px> Fines and Fees <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number4.png' height = 20px width = 20px> Summary</div>")
                                    )#box
                                )#column
                         ),#fluidRow header - title
                     
                     fluidRow(id = "print_page2_box",
                         column(width = 6, offset = 3,
                                box(title = 'Ability to Pay', solidHeader = TRUE, status = 'primary', width = 12,
                                    ##box to select requests----
                                    checkboxGroupInput(inputId = 'atp_requests',
                                                       label = HTML('If you are unable to pay your ticket, would you like the Court to consider any of the following requests? <br><em>Note: There may be additional administrative fees associated with setting up a community service work plan.</em>'), 
                                                       choices = c('Reduce fines',
                                                                   # 'Set up a payment plan', #2023/10/05 - disable payment plan option per discussion with Judge May and Angela
                                                                   'Convert fines to community service',
                                                                   "Contest ticket",
                                                                   # "Skip this step to preview fines and fees first*",
                                                                   "None of the above*"
                                                                   # "None of the above (I just want to view the fines and fees)" = "None of the above"
                                                                   ),
                                                       selected = NULL),
                                    em("*Select \"None of the above\" to skip to the next step if you would like to first preview 
                                    your fines and fees. After previewing, if you decide you would like to make a request to the 
                                    court, please return to this step to indicate which request(s) you would like to make and complete 
                                    the ability to pay form."),
                                    br(),
                                    br(),
                                    
                                    #conditionally show ATP input fields
                                    conditionalPanel("input.atp_requests[0] == 'Reduce fines' || input.atp_requests[0] == 'Convert fines to community service' || input.atp_requests[0] == 'Set up a payment plan' && input.atp_requests!='None of the above'",
                                                     strong('We will walk you through a few questions to help the court determine whether you are eligible for this request. 
                                                       Please be prepared to share information about your monthly income, monthly expenses, and any public benefits you currently receive.'),
                                                     br(),br(),
                                                     strong(style = "color:red;", "Please be advised that your financial information will not be submitted through this app."),
                                                     HTML("<ul style = 'color:red'><li>You will need to print a hard copy to bring to court or to submit with a written statement  (see the back of your ticket for instructions). You can print a copy of this form at the bottom of the page. <strong>WARNING:</strong> The public kiosk computers at the courthouses do not allow printing.</li>
                                                          <li>You will need to bring to court or submit with a written statement any supporting documents (original pay stubs, bills, other documentation of your income, monthly expenses, and public benefits you currently receive). </li></ul>"),
                                                     br(),
                                                     ##income----
                                                     strong(em('a. What is your monthly gross income (before taxes)?')),
                                                     br(),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Employment'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'income1',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Social Security'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'income2',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Unemployment Income'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'income3',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Child and/or spousal support as income'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'income4',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Other Income (rental, disability/workers compensation, etc.)'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'income5',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'right', strong('Monthly Gross Income')),
                                                              column(width = 6, align = 'center',
                                                                     strong(textOutput('incomeSum')))),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 6, align = 'left', br(), p(span('b. What is your monthly',style = 'font-weight:bold;font-style:italic;'),span('household',style = 'font-weight:bold;'),span(' gross income (before taxes)? This is any amount NOT included above but earned by your spouse/live-in partner and other adults in household.',style = 'font-weight:bold;font-style:italic;'))),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'householdIncome',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     br(),
                                                     
                                                     ##employed?----
                                                     wellPanel(#style = "background-color:light-gray;",
                                                       radioButtons(inputId = 'workingYN',
                                                                    label = 'Are you currently employed?',
                                                                    choices = c('Yes','No'),
                                                                    selected = character(0),
                                                                    # selected = 'No',
                                                                    width = '100%'),
                                                       #conditionally show past employment info
                                                       conditionalPanel("input.workingYN=='No'",
                                                                        fluidRow(column(width = 12, em('If not working, when was the last time you were employed and what was your income?'))),
                                                                        br(),
                                                                        fluidRow(column(width = 6, align = 'right', br(), 'Last day of employment'),
                                                                                 column(width = 6,
                                                                                        dateInput(inputId = 'prevEmployedDate',
                                                                                                  label = ''))),
                                                                        # fluidRow(column(width = 6, align = 'right', br(), 'Income when last employed'),
                                                                        #          column(width = 6,
                                                                        #                 autonumericInput('prevEmployIncome',
                                                                        #                                  label = '',
                                                                        #                                  value = 0,
                                                                        #                                  currencySymbol = '$'))),
                                                                        fluidRow(column(width = 6, align = 'right', br(), 'What was your monthly gross income (before taxes)?'),
                                                                                 column(width = 6,
                                                                                        autonumericInput('prevEmployMonthlyGross',
                                                                                                         label = '',
                                                                                                         value = 0,
                                                                                                         currencySymbol = '$')))
                                                       ),#conditionalPanel  
                                                     ),
                                                     
                                                     ##household size----
                                                     #disabled, instead using adults, seniors, infants, preschoolers, and school-age numericInputs that can also be used for ALICE guideline
                                                     # fluidRow(column(width = 6, align = 'left', br(), strong(em('c. How many people are in your household? List adults and children, including yourself.'))),
                                                     #          column(width = 6,
                                                     #                 numericInput(inputId = 'householdSize',
                                                     #                              label = '',
                                                     #                              value = 0))),
                                                     # br(),
                                                     
                                                     fluidRow(column(width = 12, align = 'left', br(), strong(em('c. How many people are in your household, including yourself?'))),
                                                              ),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 9, align = 'right', br(), 'Number of adults (18-64 years old):'),
                                                              column(width = 3,
                                                                     numericInput(inputId = 'adultNum',
                                                                                  label = '',
                                                                                  value = 0))),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 9, align = 'right', br(), 'Number of seniors (65+):'),
                                                              column(width = 3,
                                                                     numericInput(inputId = 'seniorNum',
                                                                                  label = '',
                                                                                  value = 0))),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 9, align = 'right', br(), 'Number of infants (0-2)'),
                                                              column(width = 3,
                                                                     numericInput(inputId = 'infantNum',
                                                                                  label = '',
                                                                                  value = 0))),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 9, align = 'right', br(), 'Number of preschoolers (3-5)'),
                                                              column(width = 3,
                                                                     numericInput(inputId = 'preschoolerNum',
                                                                                  label = '',
                                                                                  value = 0))),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 9, align = 'right', br(), 'Number of school-age children (5-17)'),
                                                              column(width = 3,
                                                                     numericInput(inputId = 'childNum',
                                                                                  label = '',
                                                                                  value = 0))),
                                                     br(),
                                                     
                                                     ##poverty guideline----
                                                     #poverty guideline output generated on server-side, involves formatted text
                                                     fluidRow(column(width = 6, align = 'left', br(),
                                                                     actionButton('povertyGuidelineCheck',
                                                                                  label = 'Check poverty guideline')),
                                                              column(width = 6,
                                                                     uiOutput('povertyGuidelineEstimate'))),
                                                     br(),
                                                     
                                                     ##ALICE guideline----
                                                     #ALICE guideline output generated on server-side, involves formatted text
                                                     fluidRow(column(width = 6, align = 'left', br(),
                                                                     div(actionButton('aliceGuidelineCheck',
                                                                                  label = 'Check ALICE guideline'),
                                                                     br(),
                                                                     div(actionLink(inputId = 'whatIsAlice',
                                                                                label = '(What is ALICE?)'
                                                                                #icon = icon('info-circle'),
                                                                                # class = 'btn-xs'
                                                                     ),#actionLink
                                                                     style = 'position:relative;left:30px;')#inner div on actionLink
                                                                     ),#outer div on actionButton
                                                                     tags$style(".popover{max-width:30%;}"),
                                                                     bsPopover(id = 'whatIsAlice',
                                                                               title = 'What is ALICE?',
                                                                               #use paste0 with HTML syntax to add formatting to popover, and add link to ALICE Report
                                                                               content = paste0('<p>ALICE stands for <strong>A</strong>sset <strong>L</strong>imited, <strong>I</strong>ncome <strong>C</strong>onstrained, <strong>E</strong>mployed, and refers to households not earning enough to afford basic household necessities. The difference between the Federal poverty guideline and the ALICE guideline is that, "Traditional economic measures systematically underestimate the actual cost of basic needs and their rate of increase over time, concealing important aspects of the local and national economy. To better capture the reality of how much income households need to live and work," the ALICE Household Survival Budget for Hawaii provides a "more accurate estimate of the cost of living and a clearer way to track changes over time" (<a href="https://www.unitedforalice.org/Attachments/AllReports/2020ALICEReport_HI_FINAL.pdf" target="_blank">ALICE Report, 2020</a>).</p>',
                                                                                           "<br>",
                                                                                           '<em>What is the ALICE Household Survival Budget?</em><br>',
                                                                                           'The ALICE Household Survival Budget is an estimate of the minimal total cost of household essentials - housing, child care, food, transportation, health care, and a smartphone plan, plus taxes and a miscellaneous contingency fund equal to 10% of the budget. It does not include savings, auto repairs, cable service, travel, laundry costs, or amenities such as holiday gifts or dinner at a restaurant that many families take for granted.',
                                                                                           '<br><br>',
                                                                                           '<em>Why do the Household Survival Budget estimates differ slightly from those in the ALICE Report tables?</em><br>',
                                                                                           '<p>The estimates here are similar but not identical to those in the ALICE Report because the LFO calculator configures a budget starting with a base of 1 adult and then uses a marginal cost formula to determine the budget for each additional adult or child whereas the <a href="https://www.unitedforalice.org/household-budgets/hawaii" _target="blank">ALICE Report table</a> determines the budgets for the standard six households and computes the budget for other configurations using the marginal cost formula. Despite this difference, the estimates should differ by no more than 5%.</p>'),
                                                                               placement = 'right',
                                                                               # trigger = c('hover','focus'),
                                                                               trigger = c('focus'),
                                                                               options = list(container = 'body')
                                                                     )),#bsPopover),
                                                              column(width = 6,
                                                                     uiOutput('aliceGuidelineEstimate'))),
                                                     br(),
                                                     
                                                     ##benefits----
                                                     strong(em('d. Are you currently receiving any of the following benefits?')),
                                                     br(),
                                                     fluidRow(column(width = 6, align = 'left', 
                                                                     checkboxInput(inputId = 'benefitsButtonSNAP',
                                                                                        # label = '',
                                                                                        # choices = c("Supplement Nutrition Assistance Program (SNAP)/Food Stamps" = "SNAP")
                                                                                   label = 'Supplement Nutrition Assistance Program (SNAP)/Food Stamps'
                                                                                   
                                                                     )),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'benefitsAmountSNAP',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'left', 
                                                                     checkboxInput(inputId = 'benefitsButtonTANF',
                                                                                        # label = '',
                                                                                        # choices = c("Temporary Assistance for Needy Families (TANF)" = "TANF")
                                                                                   label = 'Temporary Assistance for Needy Families (TANF)'
                                                                     )),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'benefitsAmountTANF',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'left', br(),
                                                                     checkboxInput(inputId = 'benefitsButtonSSI',
                                                                                        # label = '',
                                                                                        # choices = c('Supplemental Security Income (SSI)' = "SSI")
                                                                                   label = 'Supplemental Security Income (SSI)'
                                                                     )),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'benefitsAmountSSI',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     
                                                     fluidRow(column(width = 6, align = 'left', 
                                                                     checkboxInput(inputId = 'benefitsButtonGA',
                                                                                        # label = '',
                                                                                        # choices = c("General Assistance (GA)" = "GA")
                                                                                   label = 'General Assistance (GA)'
                                                                     )),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'benefitsAmountGA',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'left', 
                                                                     checkboxInput(inputId = 'benefitsButtonOther',
                                                                                        # label = '',
                                                                                        # choices = c("Other" = "OTHER")
                                                                                   label = 'Other'
                                                                     )),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'benefitsAmountOther',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     br(),
                                                     
                                                     ##expenses and debt----
                                                     strong(em('e. What are your monthly expenses?')),
                                                     br(),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Rent/Mortgage'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'expenses1',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Utilities'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'expenses2',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Food'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'expenses3',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Insurance'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'expenses4',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Child/Spousal Support'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'expenses5',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     fluidRow(column(width = 6, align = 'right', br(), 'Transportation'),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'expenses6',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),

                                                     fluidRow(column(width = 6, align = 'right', strong('Total Expenses')),
                                                              column(width = 6, align = 'center',
                                                                     strong(textOutput('expenseSum')))),
                                                     br(),
                                                     
                                                     fluidRow(column(width = 6, align = 'left', br(), strong(em('f. What is your current debt?'))),
                                                              column(width = 6,
                                                                     autonumericInput(inputId = 'debt',
                                                                                      label = '',
                                                                                      value = 0,
                                                                                      currencySymbol = '$'))),
                                                     br(),
                                                     
                                                     ##additional info----
                                                     fluidRow(column(width = 12,
                                                                     textAreaInput(inputId = 'additionalInfo',
                                                                               label = strong(em('g. Is there anything else that the court should know about that would impact your ability to pay your legal financial obligations?')),
                                                                               placeholder = 'additional information...'))),
                                                     
                                                     #2023/03/30: discussed with AM, for now disable this section
                                                     # fluidRow(column(width = 12,
                                                     #                 strong(em('i. Declaration of financial status')),
                                                     #                 br(),
                                                     #                 div('Step 1.',downloadLink('downloadDoFS','Download Declaration of Financial Status')),
                                                     #                 fileInput(inputId = 'myFile',
                                                     #                           label = div('Step 2. Upload completed Declaration of Financial Status:',style='font-weight:normal;'),
                                                     #                           multiple = TRUE,
                                                     #                           accept = 'application/pdf',
                                                     #                           buttonLabel = 'upload')))
                                                     
                                                     p('(Optional) Please include a valid e-mail and phone number in case the court needs to communicate with you about your request.'),
                                                     textInput(inputId = 'email', label = 'email', placeholder = 'jdoe@mail.com'),
                                                     textInput(inputId = 'phone', label = 'phone', placeholder = '(XXX) XXX-XXXX'),
                                                     # phoneInput(inputId = 'phone', label = 'phone', placeholder = '(###)###-####', country = "us")
                                                     uiOutput("phone_check"),
                                                     
                                                     ##print----
                                                     wellPanel(fluidRow(column(width = 12, align = 'center',
                                                                               "Print your ability to pay information for your court appearance or written statement:   "),
                                                                        
                                                                        ),
                                                     fluidRow(
                                                       column(width = 12, align = 'center',
                                                              #disable capture_pdf, inconsistent behavior, use JS print for now
                                                              # capture_pdf(selector = '#print_page2_box',#hack for now b/c not working when using "body" to print whole page
                                                              #             # filename = paste0('LFO_calculator_summary_',Sys.Date(),'.pdf'),
                                                              #             filename = paste0('ATP_summary_',format(Sys.time()-10*60*60,'%Y-%m-%d'),'.pdf'),
                                                              #             icon("fas fa-download"),strong("Download"),
                                                              #             loading = loading(text = 'Generating PDF, please wait...')
                                                              # ),
                                                              actionButton('print_page_2',strong('Print'))
                                                       )
                                                              ), #fluidRow buttons
                                                     br(),
                                                     fluidRow(column(width = 12, align ="left",
                                                                     HTML("<p style='color:red;font-weight:bold;'>Please be advised that your financial information will not be submitted through this app. </p>")
                                                                     ),
                                                                     HTML("<ul style = 'color:red'><li>You will need to print a hard copy to bring to court or to submit with a written statement  (see the back of your ticket for instructions). <strong>WARNING:</strong> The public kiosk computers at the courthouses do not allow printing.</li>
                                                          <li>In addition to the information provided on this page, when you come to court or submit a written statement, please be prepared to show original pay stubs, bills, and other documentation of your income, monthly expenses, and any public benefits you currently receive. </li></ul>"),
                                                              
                                                              )
                                                     ),#wellPanel
                                                     
                                                     
                                                     ),#conditionalPanel

                                    fluidRow(column(width = 12, align = 'center',
                                                    actionButton("page_21", "back"),
                                                    actionButton("page_23", "next"))
                                             ) #fluidRow buttons
                                    )#box
                                )#column
                         )#fluidRow
            ),#tabPanel page_2
            
            #-----------------------------------#
            #tabPanel page_3----
            #-----------------------------------#
            tabPanel("page_3", 

                     # progress meter/line
                     fluidRow(
                       column(width = 6, offset = 3, align = 'center',
                              box(width = 12,
                                  title = HTML("<div><img src='number1.png' height = 20px width = 20px'/> Charges <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number2.png' height = 20px width = 20px> Ability to Pay <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number3.png' height = 20px width = 20px><span style='color:#0073b7;'><strong> Fines and Fees </strong></span><hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number4.png' height = 20px width = 20px> Summary</div>")
                              )#box
                              )#column
                       ),#fluidRow progress meter/line
                     
                     ##Disclaimer (if role selected is defendant or other; logic on server-side)----
                     uiOutput("srl_warning1"),

                     #fines and fees box
                     fluidRow(
                         column(width = 6, offset = 3, 
                                ##Fines and fees----
                                box(title = 'Fines and Fees', solidHeader = TRUE, status = 'primary', width = 12,
                                    #if no charges selected, send message, otherwise sends a blank message (logic on server-side)
                                    fluidRow(column(width = 12, align = 'center', uiOutput('noChargesSelected_p3'))),
                                    
                                    #if N > 1 charges selected, conditionally show N boxes, generated on the server-side
                                    conditionalPanel(condition = "output.chargeCounter",
                                                     uiOutput('chargeBox')),
                                    
                                    fluidRow(column(width = 12, align = 'center',
                                                    actionButton("page_32", "back"),
                                                    actionButton("page_34", "next"))
                                             ) #fluidRow buttons
                                    ),#box fines and fees
                                ),#column
                         
                         ##Ability to pay----
                         column(id = "hhh", width = 3, 
                                box(title = 'Ability to Pay Determination', solidHeader = TRUE, status = 'primary', width = 12,
                                    #if N > 1 charges selected, conditionally show N lines, generated on the server-side
                                    conditionalPanel(condition = "output.chargeCounter",
                                                       uiOutput('summaryATP1'),
                                                       uiOutput('csPopover1')
                                                       ),#conditionalPanel for summaryATP1
                                    ),#box ATP
                                
                                ##Payment plan (disabled)---- #2023/10/05 - disable payment plan option per discussion with Judge May and Angela
                                # conditionalPanel(condition = "input.addRequests[0] == 'Payment Plan' || input.addRequests[1] == 'Payment Plan'",
                                #                  box(title = "Payment Plan Planner",
                                #                      solidHeader = TRUE,
                                #                      status = 'warning',
                                #                      width = 12,
                                #                      conditionalPanel(condition = "output.chargeCounter",
                                #                                       fluidRow(column(width = 6, align = "right", 
                                #                                                       HTML("<strong>Total Fines and Fees: </strong>"),
                                #                                       ),#column
                                #                                       column(width = 6, align = "left", 
                                #                                              uiOutput('summaryPPTotalFinesFees')
                                #                                       )#column
                                #                                       ),#fluidRow Total Fines
                                #                                       br(),
                                #                                       #move label to side of widget
                                #                                       #h/t: https://stackoverflow.com/questions/39230356/how-to-make-label-and-box-align-next-to-each-other-in-shinynumericinput
                                #                                       fluidRow(column(width = 6, align ="right",
                                #                                                       HTML("<strong>How much can you pay per month?</strong>")
                                #                                                       ),
                                #                                                column(width = 6, align ="left",
                                #                                                       autonumericInput(inputId = "payplan",
                                #                                                                        label = NULL,
                                #                                                                        value = 0,
                                #                                                                        width = "100%",
                                #                                                                        align = "left",
                                #                                                                        currencySymbol = '$'
                                #                                                                        )
                                #                                                       )#column
                                #                                                ),#fluidRow conversation rate
                                #                                       br(),
                                #                                       fluidRow(column(width = 6, align ="right",
                                #                                                       HTML("<strong>Months needed to pay in full: </strong>")
                                #                                                       ),
                                #                                                column(width = 6, align ="left",
                                #                                                       uiOutput('pay_plan_monthly')
                                #                                                       )#column
                                #                                                )#fluidRow
                                #                                       )#conditionalPanel
                                #                      )#box for Payment Plan
                                #                  ),#conditionalPanel for Payment Plan
                                
                                ##Community Service----
                                conditionalPanel(condition = "input.atp_requests[0] == 'Convert fines to community service' || input.atp_requests[1] == 'Convert fines to community service' || input.atp_requests[2] == 'Convert fines to community service'",
                                                   box(title = "Convert Fines to Community Service (CS)?",
                                                       solidHeader = TRUE,
                                                       status = 'warning',
                                                       width = 12,
                                                       fluidRow(column(width = 10, align ="left",
                                                                       checkboxInput(inputId = "convertCSW",
                                                                                     label = "Convert fines to CS hours",
                                                                                     value = FALSE)
                                                       ),#column
                                                       ),#fluidRow checkbox
                                                       fluidRow(style="font-style:italic;",
                                                                column(width = 12, align = 'left',
                                                                       "Please note that certain fines and fees are mandatory and may not be eligible for conversion to community service.",
                                                                       br(),br())),
                                                       conditionalPanel(condition = "output.chargeCounter && input.convertCSW==1",
                                                                        fluidRow(column(width = 6, align = "right", 
                                                                                        # HTML("<strong>Total Fines: </strong>"),
                                                                                        actionLink(inputId = 'cswFinesInfo',
                                                                                                   label = HTML("<strong>Total Fines: </strong>")
                                                                                                   #icon = icon('info-circle'),
                                                                                                   # class = 'btn-xs'
                                                                                        ),#actionLink
                                                                                        tags$style(".popover{max-width:30%;}"),
                                                                                        bsPopover(id = 'cswFinesInfo',
                                                                                                  title = 'Exclusion of fees and court costs',
                                                                                                  content = paste0('<p>According to <a href = "https://www.courts.state.hi.us/docs/court_rules/rules/hctr.htm#Rule_17" target = "_blank">HCTR Rule 17</a>, community service may not be ordered in lieu of payment of costs and fees.</p>'),
                                                                                                  placement = 'left',
                                                                                                  # trigger = c('hover','focus'),
                                                                                                  trigger = c('focus'),
                                                                                                  options = list(container = 'body')
                                                                                        )#bsPopover
                                                                        ),#column
                                                                        column(width = 6, align = "left", 
                                                                               uiOutput('summaryCSWTotalFines')
                                                                        )#column
                                                                        ),#fluidRow Total Fines
                                                                        br(),
                                                                        #move label to side of widget
                                                                        #h/t: https://stackoverflow.com/questions/39230356/how-to-make-label-and-box-align-next-to-each-other-in-shinynumericinput
                                                                        fluidRow(column(width = 6, align ="right",
                                                                                        HTML("<strong>Conversion Rate: </strong>")
                                                                        ),#column
                                                                        column(width = 6, align ="left",
                                                                               autonumericInput(inputId = "cswRate",
                                                                                                label = NULL,
                                                                                                value = 15,
                                                                                                width = "100%",
                                                                                                align = "left",
                                                                                                currencySymbol = '$',
                                                                                                currencySymbolPlacement = 'p',
                                                                                                suffixText = '/hour')
                                                                        )#column
                                                                        ),#fluidRow conversation rate
                                                                        fluidRow(column(width = 6, align = "right", 
                                                                                        # HTML("<strong>Total Hours: </strong>")
                                                                                        actionLink(inputId = 'cswHoursInfo',
                                                                                                   label = HTML("<strong>Total Hours: </strong>")
                                                                                                   #icon = icon('info-circle'),
                                                                                                   # class = 'btn-xs'
                                                                                        ),#actionLink
                                                                                        tags$style(".popover{max-width:30%;}"),
                                                                                        bsPopover(id = 'cswHoursInfo',
                                                                                                  title = 'Calculation',
                                                                                                  content = paste0('<p>Rounds down to the nearest hour.</p>'),
                                                                                                  placement = 'left',
                                                                                                  # trigger = c('hover','focus'),
                                                                                                  trigger = c('focus'),
                                                                                                  options = list(container = 'body')
                                                                                        )#bsPopover
                                                                        ),#column
                                                                        column(width = 6, align = "left", 
                                                                               uiOutput('summaryCSWTotalHours')
                                                                        )#column
                                                                        )#fluidRow Total CSW hours
                                                                        # fluidRow(column(width = 6, align = "left",
                                                                        #                 actionBttn("csWConvert","convert!")),
                                                                        # )#fluidRow
                                                       ) #conditionalPanel summaryCSW1
                                                       
                                                   )#box for CSW
                                  )#conditionalPanel for CSW
                                )#column
                         ),#fluidRow

            ),#tabPanel page_3
            
            #-----------------------------------#
            #tabPanel page_4----
            #-----------------------------------#
            tabPanel("page_4", 
                     
                     # progress meter/line
                     fluidRow(
                         column(width = 6, offset = 3, align = 'center',
                                box(width = 12,
                                    title = HTML("<div><img src='number1.png' height = 20px width = 20px'/> Charges <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number2.png' height = 20px width = 20px> Ability to Pay <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number3.png' height = 20px width = 20px> Fines and Fees <hr style='display:inline-block; width:30px;border-color:black;margin-bottom:1%;' /> <img src='number4.png' height = 20px width = 20px><span style='color:#0073b7;'><strong> Summary</strong></span></div>")
                                )#box
                         )#column
                     ),#fluidRow progress meter/line
                     
                     
                     #box for summary
                     fluidRow(id="test1", 
                              
                              ##Disclaimer (if role selected is defendant or other; logic on server-side)----
                              uiOutput("srl_warning2"),
                              
                              ##summary table----
                              column(width = 6, offset = 3,
                                     box(title = 'Summary', solidHeader = TRUE, status = 'primary', width = 12,
                                    
                                    conditionalPanel("input.atp_requests[0] == 'Reduce fines' || input.atp_requests[0] == 'Convert fines to community service' || input.atp_requests[0] == 'Set up a payment plan'",
                                                     fluidRow(column(width = 6, align = 'left',
                                                                     HTML(paste0("<b>Contact Information</b>")),
                                                                     uiOutput("email_info"),
                                                                     uiOutput("phone_info"),
                                                                     br()
                                                                     )#column
                                                              )#fluidRow
                                                     ),#conditionalPanel
                                    
                                    #if no charges selected, send message, otherwise sends a blank message; logic on server-side
                                    fluidRow(column(width = 12, align = 'center', uiOutput('noChargesSelected_p4'))),
                                    
                                    #if N > 1 charges selected, conditionally show N lines, generated on the server-side
                                    conditionalPanel(condition = "output.chargeCounter",
                                                     uiOutput('summaryTable')),

                                    fluidRow(column(width = 4, align = 'left',
                                                    paste(format(Sys.time()-10*60*60,'%x %I:%M:%S %p'),'HST')), #convert to HST
                                             column(width = 4, align = 'center',
                                                    actionButton("page_43", "back"),
                                                    actionButton("page_41", "return to step 1")),
                                             ##print----
                                             column(id = "firstpage", width = 4, align = 'right',
                                                    capture_pdf(
                                                      selector = '#test1',#hack for now b/c not working when using "body" to print whole page
                                                      # filename = paste0('LFO_calculator_summary_',Sys.Date(),'.pdf'),
                                                      filename = paste0('LFO_calculator_summary_',format(Sys.time()-10*60*60,'%Y-%m-%d'),'.pdf'),
                                                      icon("fas fa-download"),strong("Download/Print"),
                                                      loading = loading(text = 'Generating PDF, please wait...')
                                                    )#,
                                                    # actionButton('print_page_4',strong('Print'))
                                                    )#column
                                             ) #fluidRow buttons
                                    )#box
                                ),#column
                         
                              ##Ability to pay----
                              column(width = 3, 
                                box(title = 'Ability to Pay Determination', solidHeader = TRUE, status = 'primary', width = 12,
                                    #if N > 1 charges selected, conditionally show N lines, generated on the server-side
                                    conditionalPanel(condition = "output.chargeCounter",
                                                     uiOutput('summaryATP2'),
                                                     uiOutput('csPopover2')),
                                    )#box
                                )#column
                         ),#fluidRow
                     )#tabPanel page_4
            
        )#tabsetPanel
        
    )#dashboardBody

)#dashboardPage

# Define server logic
server <- function(input, output, session) {
    
  #---------------------------------------------------------------------------------#
  #                     ---START DROPBOX PERSISTENT STORAGE----                                       
  #---------------------------------------------------------------------------------#

  # Gather all the form inputs
  form_data <- reactive({
    #Method A: if using inputs
    # sapply(fields_all, function(x) x = input[[x]])
    
    #Method B: if using reactives, but it's very important to update save_data_dropbox by removing the transpose on the data
    ui_charges <- ui_charges()
    
    
  })
  
  observeEvent(input$page_34, {
    # Save the data (show an error message in case of error)
    tryCatch({
      save_data_dropbox(form_data())
    },
    error = function(err) {
      shinyjs::html("errorMsg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
      shinyjs::logjs(err)
    })
  })
  
  # Update the responses whenever a new submission is made or the storage type is changed
  responses_data <- reactive({
    input$page_34
    load_data_dropbox()
  })
  
  #---------------------------------------------------------------------------------#
  #                     ---END DROPBOX PERSISTENT STORAGE----                                       
  #---------------------------------------------------------------------------------#
  
  #-----------------------------------#
  #shinylogs----
  #dropbox - writes to dropbox after session closes
  track_usage(storage_mode = store_custom(FUN = function(logs) {
    save_shinylogs_dropbox(logs)
    })
  )
  
  #-----------------------------------#
  
  #required/mandatory/validated fields----
  iv <- InputValidator$new()
  iv$add_rule("default_judgment", sv_required())#require an input to the default judgment radiobutton on page 1, otherwise prevent from going to page 2
  iv$add_rule("charges", sv_optional())#charges are optional
  
  #Note: AC (2023/04/14): set max no of charges that can be selected to 11 based on an analysis of district court cases in 2023; well over 99% have 11 or fewer charges
  iv$add_rule("charges", function(value){
    if(length(value) > 11) {
      paste0("Maximum of 11 charges allowed. You have selected ", length(input$charges), " charges. Please remove charges.")
    }
  })#require less than or equal to 11 charge selections
  
  iv2 <- InputValidator$new()
  iv2$add_rule("atp_requests", sv_required())#require an input to the atp request checkBoxGroupInput on page 2, otherwise prevent from going to page 3
  
    #-----------------------------------#
    #switch pages----
    #-----------------------------------#
    switch_page <- function(i) {
        updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
    }
    
    observeEvent(input$startCalc, switch_page(1))
    observeEvent(input$page_10, switch_page(0))
    #check default judgement yes/no selected; check # of charges <= 11
    observeEvent(input$page_12, {
      if(iv$is_valid()){
        switch_page(2)
      }else{
        iv$enable()
        }
    })
    observeEvent(input$page_21, switch_page(1))
    #check atp request selected
    observeEvent(input$page_23, {
      if(iv2$is_valid()){
        switch_page(3)
        }else{
          iv2$enable()
        }
      })
    observeEvent(input$page_32, switch_page(2))
    observeEvent(input$page_34, switch_page(4))
    observeEvent(input$page_43, switch_page(3))
    observeEvent(input$page_41, switch_page(1))
    
    #-----------------------------------#
    #page 1 renderUI for selectizeInput for charge codes----
    #-----------------------------------#
    #filter charge selection dropdown based on date of charge/citation/offense
    chargeList <- reactive({
      lfoTableJIMS_chargeList_non_felony %>%
        filter(CHARGE_VALID_FROM2 <= input$chargeDate) %>% #exclude charges valid after date of offense (has to be on books before offense occurred)
        # group_by(CHARGE_CODE,CHARGE_LEVEL,CHARGE_QUALIFIER) %>% #group CHARGE_CODExCHARGE_QUALIFIER with multiple valid_froms; include charge_level b/c some charge codes have multiple charge levels under same code
        group_by(CHARGE_CODE,CHARGE_QUALIFIER) %>% #group CHARGE_CODExCHARGE_QUALIFIER with multiple valid_froms; exclude charge_level at this step but make an option at step 3
        slice_max(CHARGE_VALID_FROM2, with_ties = FALSE) %>%  #get most recent version of charge code that is valid before date of offense
        #add arrange to sort charges so they appear in numerical section order and alphabetical law-source (HAR vs HRS vs ROH, etc.) order
        #!is.na is so that 291 appears before 291C b/c 291 has an NA for the suffix and this keeps the NAs sorted before the rest
        arrange(LAW_CODE, CHARGE_TITLE_numeric, CHARGE_CHAPTER_3NUM, !is.na(CHARGE_CHAPTER_SUFFIX), CHARGE_CHAPTER_SUFFIX, CHARGE_SECTION_numeric, CHARGE_CODE, CHARGE_QUALIFIER_SPEED)
      
    })
    
    #update SelectizeInput on server-side to speed up processing since list is large
    observeEvent(input$chargeDate,{
        #show a descriptive list to user, but use charge ver id as the value under the hood
        #h/t: https://stackoverflow.com/questions/33105044/make-list-for-shiny-dropdown-selectinput?rq=1
        mylist <- as.list(chargeList()$CHARGE_VER_ID)
        names(mylist) <- paste0(if_else(is.na(chargeList()$commonDescription),'',paste0('\"',chargeList()$commonDescription,'\"/')),
                                chargeList()$DESCRIPTION, 
                                if_else(is.na(chargeList()$CHARGE_QUALIFIER),'',paste0(' ',chargeList()$CHARGE_QUALIFIER)),
                                # br(),
                                ' [',chargeList()$CHARGE_CODE,']'
                                # div(' [',chargeList()$CHARGE_CODE,']',style = 'color:blue;')
                                )

        chargeList<-chargeList() %>%
          mutate(commonTmp = case_when(is.na(commonDescription) ~ '',
                                       !is.na(commonDescription) ~ paste0('\"',commonDescription,'\"/')
                                       ),
                 qualifierTmp = case_when(is.na(CHARGE_QUALIFIER) ~ '',
                                          !is.na(CHARGE_QUALIFIER) ~ paste0(' ',CHARGE_QUALIFIER)
                                          ),
                 value = CHARGE_VER_ID, #this is the underlying value attached to named items in the list
                 label=paste0(commonTmp,DESCRIPTION,qualifierTmp,': ',CHARGE_CODE), #this is what is analyzed when typing in the search box and what shows up in the selection box
                 name=paste0(commonTmp,DESCRIPTION,qualifierTmp), #extra?
                 dropdown = paste0(commonTmp,DESCRIPTION,qualifierTmp), #this is for custom HTML if using selectizeInput; use as label for virtualSelect use as label for virtualSelect/updateVirtualSelect if using description in prepare_choices and hasOptionDescription = TRUE
                 # dropdown = paste0(commonTmp,DESCRIPTION,qualifierTmp,"<br><strong style='color:blue;font-size:0.9em;'>", CHARGE_CODE, "</strong>"), #use as label for virtualSelect if not using description in prepare_choices and using html = TRUE
                 html = paste0("<strong style='color:blue;font-size:0.9em;'>", CHARGE_CODE, "</strong>") #this is for custom HTML
                 )
        
        #replaced updateSelectizeInput with updateVirtualSelect; keep in case need to revert back
        # updateSelectizeInput(session,
        #                      inputId = 'charges',
        #                      # label = list('Type charges',br(),
        #                      #        actionLink(inputId = 'helpTypeCharge',
        #                      #                   label = 'Help! I don\'t see my charge!',
        #                      #                   #icon = icon('info-circle'),
        #                      #                   # class = 'btn-xs'
        #                      #                   ),#actionLink
        #                      #       bsPopover(id = 'helpTypeCharge',
        #                      #                  title = '',
        #                      #                  content = paste('The legal descriptions for many offenses sometimes differ from the everday terms. Please type in the charge code or legal description to select the correct charge.'),
        #                      #                  options = list(container = 'body')
        #                      #                  )#bsPopover
        #                      #        ),
        #                      label = NULL,
        #                      choices = chargeList,
        #                      # options = list(
        #                      #     placeholder = "Select/Type one or more charges"),
        #                      #h/t: https://shiny.rstudio.com/articles/selectize.html#server-side-selectize
        #                      #h/t: https://stackoverflow.com/questions/54522970/shiny-selectizeinput-with-value-and-label-fields
        #                      #h/t: https://github.com/rstudio/shiny/issues/929
        #                      server = TRUE, 
        #                      options = list(render = I(
        #                        '{
        #                                             option: function(item, escape) {
        #                                                 return "<div>" + item.dropdown + "<br>" + item.html + "<br>" + "</div>"; }
        #                                             }'
        #                      ))
        # )
        
        updateVirtualSelect(#session,
                             inputId = 'charges',
                             label = NULL,
                             choices = prepare_choices(chargeList, label = dropdown, value = value, description = html)
                            )
    })
    
    #-----------------------------------#
    #page 2 requests section - modal, and clear if none of the above----
    #-----------------------------------#
    atp_request_values <- reactiveValues(selection = NULL)
    atp_request_counter <- reactiveValues(counter = 0)
    
    #h/t: https://community.rstudio.com/t/logical-test-in-observeevent/37152/2
    observeEvent(input$atp_requests,{
      
      #keep track of order of selections on atp_requests, this allows us to to clear selections when "None of the above" is chosen after other selections and to clear "None of the above" if chosen before other selections
      atp_request_counter$counter <- atp_request_counter$counter + 1
      atp_request_values$selection[atp_request_counter$counter] <- data.frame(values=input$atp_requests)
      
      current_selections_not_contest <- input$atp_requests[input$atp_requests != "Contest ticket"]
      
      if("Contest ticket" %in% input$atp_requests){
        sendSweetAlert(
          session = session,
          title = "",
          text = " Your ticket cannot be challenged or disputed here. To do so, please read the instructions on the back of your ticket.",
          type = "warning"
        )
        
        updateCheckboxGroupInput(session, 
                                 inputId = "atp_requests",
                                 selected = current_selections_not_contest) #update selections so that it remembers selections but removes "Contest ticket"; important to remove "Contest ticket" other it will trigger the modal when any option is selected
        }
      
      current_selections_not_none <- input$atp_requests[input$atp_requests != "None of the above*"]
      
      #if none of the above in the current set of selections but not in previous set, then clear out other selections
      if("None of the above*" %in% input$atp_requests &&
         length(atp_request_values$selection) > 1 &&
         !"None of the above*" %in% atp_request_values$selection[[length(atp_request_values$selection)-1]]){
        updateCheckboxGroupInput(session, 
                                 inputId = "atp_requests",
                                 selected = "None of the above*")#doesn't accept other selections as long as "None of the above" is selected
        current_selections_not_none <- character(0)#need to update this to reflect current state of input$atp_requests so that next if is not triggered if other choices had been selected prior to None of the above selection
        }
      
      #if anything is selected that is not "None of the above," make sure "None of the above" is deselected
      if(length(current_selections_not_none) > 0 & !rlang::is_empty(current_selections_not_none) & "None of the above*" %in% input$atp_requests){
        updateCheckboxGroupInput(session, 
                                 inputId = "atp_requests",
                                 selected = current_selections_not_none)#unselect "None of the above" if other choices selected
        }
    })

    #-----------------------------------#
    #page 2 benefits section - enable/disable inputs----
    #-----------------------------------#    
    #reset benefits to zero if disabled
    observe({
      if(input$benefitsButtonSNAP == TRUE){
        enable("benefitsAmountSNAP")
      }else{
        updateAutonumericInput(
          session = session,
          inputId = 'benefitsAmountSNAP',
          label = '',
          value = 0, 
          #currencySymbol = '$'
          options = list(
            currencySymbol = '$'
          )
        )
        disable("benefitsAmountSNAP")
      }
    })
    
    observe({
      if(input$benefitsButtonTANF == TRUE){
        enable("benefitsAmountTANF")
      }else{
        updateAutonumericInput(
          session = session,
          inputId = 'benefitsAmountTANF',
          label = '',
          value = 0,
          #currencySymbol = '$'
          options = list(
            currencySymbol = '$'
          )
        )
        disable("benefitsAmountTANF")
      }
    })
    
    observe({
      if(input$benefitsButtonSSI == TRUE){
        enable("benefitsAmountSSI")
      }else{
        updateAutonumericInput(
          session = session,
          inputId = 'benefitsAmountSSI',
          label = '',
          value = 0,
          #currencySymbol = '$'
          options = list(
            currencySymbol = '$'
          )
        )
        disable("benefitsAmountSSI")
      }
    })
    
    observe({
      if(input$benefitsButtonGA == TRUE){
        enable("benefitsAmountGA")
      }else{
        updateAutonumericInput(
          session = session,
          inputId = 'benefitsAmountGA',
          label = '',
          value = 0,
          #currencySymbol = '$'
          options = list(
            currencySymbol = '$'
          )
        )
        disable("benefitsAmountGA")
      }
    })
    
    observe({
      if(input$benefitsButtonOther == TRUE){
        enable("benefitsAmountOther")
      }else{
        updateAutonumericInput(
          session = session,
          inputId = 'benefitsAmountOther',
          label = '',
          value = 0,
          #currencySymbol = '$'
          options = list(
            currencySymbol = '$'
          )
        )
        disable("benefitsAmountOther")
      }
    })
    
    #-----------------------------------#
    #page 2 income calculation----
    #-----------------------------------#
    incomeSumTmp <- reactive(
        # input$income1 + input$income2 + input$income3 + input$income4 + input$income5 + input$income6
        sum(input$income1,input$income2,input$income3,input$income4,input$income5)
    )

    output$incomeSum <- renderText(dollar(incomeSumTmp()))

    #-----------------------------------#
    #page 2 Federal poverty scale/guideline calculation----
    #https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines
    #-----------------------------------# 
    # povGuideline <- reactive(9600 + (input$householdSize * 5220)) #2021
    # povGuideline <- reactive(10200 + (input$householdSize * 5430)) #2022
    # povGuideline <- reactive(10200 + (householdSize() * 5430)) #2022
    #Starting in 2022, use ALICE household size broken down by adult, senior, infant, etc.
    householdSize <- reactive(sum(input$adultNum, input$seniorNum, input$infantNum, input$preschoolerNum, input$childNum))
    # povGuideline <- reactive(10860 + (householdSize() * 5910)) #2023 
    povGuideline <- reactive(11120 + (householdSize() * 6190)) #2024
    yearlyIncome <- reactive(12*(input$householdIncome +incomeSumTmp()))

    povertyGuidelineEstimateTmp <- eventReactive(input$povertyGuidelineCheck,{
        
        p('For a',strong(householdSize()),'person household, the',
        a('2024 poverty guideline for Hawaii', href = 'https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines', target="_blank"),
          'is',strong(dollar(povGuideline()),.noWS = 'after'),
          '. The annual gross income reported for this household is',
          strong(dollar(yearlyIncome()),.noWS = 'after'),
          ' (12 months x',dollar(input$householdIncome +incomeSumTmp()),' monthly gross income including other adults in household), which is',
          strong(percent(yearlyIncome()/povGuideline())),'of the poverty guideline.',
        em("Your request will be considered by the court whether or not it is above or below the poverty guideline."))
        })#eventReactive
    
    output$povertyGuidelineEstimate <- renderUI({
        if(TRUE){
            povertyGuidelineEstimateTmp()
        }else{
            ''
        }
        })
    
    #-----------------------------------#
    #page 2 ALICE guideline calculation----
    #-----------------------------------# 
    aliceGuideline <- reactive({
      # ALICE 2020 Report
      if(input$adultNum > 0){
        adultNumTmp <- input$adultNum - 1 #a single adult is captured by the constant at the front of the statement, so use adultNumTmp for any additional adults
        aliceGuidelineTmp <- 31056 + (adultNumTmp * 19040) + (input$seniorNum * 20575) + (input$infantNum * 21597) + (input$preschoolerNum * 21432) + (input$childNum * 14407)
      }else if (input$adultNum == 0 & input$seniorNum > 0){
        seniorNumTmp <- input$seniorNum - 1 #a single senior is captured by the constant at the front of the statement, so use seniorNumTmp for any additional seniors
        aliceGuidelineTmp <- 34308 + (0 * 19040) + (seniorNumTmp * 20575) + (input$infantNum * 21597) + (input$preschoolerNum * 21432) + (input$childNum * 14407)
      }
      
      #ALICE 2023 Report - ***Honolulu County figures -> calculator is for 1DC for now
      # if(input$adultNum > 0){
      #   adultNumTmp <- input$adultNum - 1 #a single adult is captured by the constant at the front of the statement, so use adultNumTmp for any additional adults
      #   #statewide figures -> underconstruction!
      #   aliceGuidelineTmp <- 36912 + (adultNumTmp * 16776) + (input$seniorNum * 19824) + (input$infantNum * 17100) + (input$preschoolerNum * 14340) + (input$childNum * 14407)
      #   #Honolulu County figures -> under construction!
      #   aliceGuidelineTmp <- 34632 + (adultNumTmp * 15264) + (input$seniorNum * 19824) + (input$infantNum * 17100) + (input$preschoolerNum * 14340) + (input$childNum * 14407)
      # }else if (input$adultNum == 0 & input$seniorNum > 0){
      #   seniorNumTmp <- input$seniorNum - 1 #a single senior is captured by the constant at the front of the statement, so use seniorNumTmp for any additional seniors
      #   aliceGuidelineTmp <- 34308 + (0 * 19040) + (seniorNumTmp * 20575) + (input$infantNum * 21597) + (input$preschoolerNum * 21432) + (input$childNum * 14407)
      # }
      }) 
    
    aliceGuidelineEstimateTmp <- eventReactive(input$aliceGuidelineCheck,{
      #h/t: https://shiny.rstudio.com/articles/validation.html
      validate(
        need(expr = as.numeric(aliceGuideline()),
             message = paste('Must have atleast one Adult or one Senior'))
      )
      p('For a',strong(householdSize()),'person household, the',
        a('2020 ALICE Household Survival Budget for Hawaii', href = 'https://www.unitedforalice.org/household-budgets/hawaii', target="_blank"),
        'is',strong(dollar(aliceGuideline()),.noWS = 'after'),
        '. The annual gross income reported for this household is',
        strong(dollar(yearlyIncome()),.noWS = 'after'),
        ' (12 months x',dollar(input$householdIncome +incomeSumTmp()),
        ' monthly gross income including other adults in household), which is',
        strong(percent(yearlyIncome()/aliceGuideline())),'of the ALICE Household Survival Budget.',
        em("Your request will be considered by the court whether or not it is above or below the survival budget."))
    })#eventReactive
    
    output$aliceGuidelineEstimate <- renderUI({
      if(TRUE){
        aliceGuidelineEstimateTmp()
      }else{
        ''
      }
    })
    #-----------------------------------#
    #page 2 expense calculation----
    #-----------------------------------#
    expenseSumTmp <- reactive(
        dollar(sum(input$expenses1,input$expenses2,input$expenses3,input$expenses4,input$expenses5,input$expenses6))
    )
    
    output$expenseSum <- renderText(expenseSumTmp())
    
    #-----------------------------------#
    #page 2 download declaration of financial status----
    #2023/03/30: discussed with AM, for now disable this section
    #-----------------------------------#
    # output$downloadDoFS <- downloadHandler(
    #   filename = "Declaration of Financial Status.docx",
    #   content = function(file) {
    #     file.copy("www/Declaration of Financial Status.docx", file)
    #   }
    # )
    
    #-----------------------------------#
    #page 2 upload declaration of financial status----
    #2023/03/30: discussed with AM, for now disable this section
    #-----------------------------------#
    # observeEvent(input$myFile, {
    #     inFile <- input$myFile
    #     if (is.null(inFile))
    #         return()
    #     file.copy(inFile$datapath, file.path("H:\\p11_LFO_calculator\\", inFile$name) )
    # })
    
    #-----------------------------------#
    #page 2 format phone number----
    #-----------------------------------#
    observeEvent(input$phone, {

    #https://stackoverflow.com/questions/34616466/formatting-phone-numbers-in-r
    phone_clean <- trimws(gsub("[[:punct:][:alpha:][:space:]]", "", input$phone))
    if(!is.na(as.numeric(phone_clean)) & nchar(phone_clean) == 10){
      #option 1
      phone_formatted <- paste0("(",substr(phone_clean, start = 1, stop = 3),") ",substr(phone_clean, start = 4, stop = 6),"-",substr(phone_clean, start = 7, stop = 10))
      #option 2
      # phone_formatted <- gsub("(^\\d{3})(\\d{3})(\\d{4}$)", 
      #      "(\\1)\\2-\\3",
      #      phone_clean)
      
      updateTextInput(session = session, 
                      inputId = 'phone', 
                      label = 'phone', 
                      value = phone_formatted
                      )      
      }else
        updateTextInput(session = session, 
                        inputId = 'phone', 
                        label = 'phone', 
                        value = input$phone
        )  
      })
        
    output$phone_check <- renderUI({
      
      phone_clean <- trimws(gsub("[[:punct:][:space:]]", "", input$phone))
      
      if(input$phone == ""){
        div("")
      }else if(!is.na(as.numeric(phone_clean)) & nchar(phone_clean) == 10){
        # div("valid phone number")
      }else if(nchar(phone_clean) != 10){
        HTML("<div style = color:red;><strong>Warning!</strong> Phone number must be 10 digits</div>")
      }else if(is.na(as.numeric(input$phone))){
        # div("phone number must be all digits")
        HTML("<div style = color:red;><strong>Warning!</strong> Phone number must be all digits</div>")
      }else
        # div("Phone number error! Phone numbers must be 10 digits long and only numbers.")
        HTML("<div style = color:red;>Phone number error! Phone numbers must be 10 digits long and only numbers.</div>")
    })
    
    #Page 2 print page----
    observeEvent(input$print_page_2,{
      js$winprint()
    })
    
    #-----------------------------------#
    #page 3 fines and fees table----
    #-----------------------------------#

    #return this to conditionalPanels in UI to determine whether to show fine and fee box on page 3 and summary box on page 4
    output$chargeCounter <- reactive({length(input$charges)>=1})
    outputOptions(output, "chargeCounter", suspendWhenHidden = FALSE)
    
    #Disclaimer box----
    #warning at top of steps 3 and 4 if role selected is defendant or other
    output$srl_warning1 <- output$srl_warning2 <- renderUI({
      if(!input$role %in% c('Public Defender/Defense Attorney','Prosecutor','Judge')){
        fluidRow(
          column(width = 6, offset = 3, 
                 #background opacity h/t: https://www.w3schools.com/css/css3_colors.asp
                 div(style = "border: 3px solid red; background-color:rgba(255, 0, 0, 0.1);",
                     HTML("<h3 style='color:red;font-size:16px; line-height: 1.3;padding:6px;'>
                     DISCLAIMER: The following is only an <u>estimate</u> of the fines and fees you may owe.
                     Please note that certain fines and fees are mandatory and may not be eligible for reduction or conversion to community service. 
                     All requests will be decided by the judge.<br>
                     <br>
                     Some offenses may carry additional requirements/penalties, including but not limited to, driver license suspension, driver improvement class, child passenger restraint class, and/or jail.")
                 ),#box
                 br(),
          )#column
        )#fluidRow
        }
    })
    #-----------------------------------#
    #reactive -> extract fines and fees for charges selected----
    #-----------------------------------#
    fee_codes <- lfoTableJIMS_non_felony %>% 
      distinct(DETC_CODE) %>% 
      filter(!is.na(DETC_CODE),!grepl('^F',DETC_CODE)) #remove charges with no default LFOs and remove fines which start with "F"
    
    ffTableTmp1 <- reactive({
        #simple function to extract rows from LFO table for a single charge and label as fine or fee
        extractCharge <- function(charges,table){
            chargeInfo <- table %>%
              filter(CHARGE_VER_ID == charges) %>%
              #LFO_TYPE = {default_fine, no_default}; no_default converted to other values below
              mutate(LFO_TYPE = case_when(grepl('^F',DETC_CODE) ~ "default_fine",
                                            is.na(DETC_CODE) ~ "no_default",
                                            DETC_CODE %in% fee_codes$DETC_CODE ~ "fee", #make sure to collapse fee_codes into a vector by calling the column!
                                            TRUE ~ "unknown"),
                     mandatory_fine = case_when(grepl('HRS 291-11\\.6|HRS 291C-73\\(c\\)|ROH 15-13\\.[358]|ROH 15-14\\.1\\(a\\)\\([1-7]\\)|ROH 15-14\\.[2356]|ROH 15-15\\.1\\([ab]\\)|ROH 15-22\\.11|ROH 15-23\\.6', 
                                                      CHARGE_CODE) | (grepl('HRS 291C-137', CHARGE_CODE) & grepl('SCH', CHARGE_QUALIFIER)) ~ "yes",
                                                grepl("default", LFO_TYPE) ~ "no",
                                                  grepl("fee", LFO_TYPE) ~ "fee"))
            
            #custom fees for charges without default fees listed in JIMS
            #must include else clause otherwise chargeInfo doesn't return for anything but 291-11.5
            if(grepl('HRS 291-11.5',chargeInfo$CHARGE_CODE[1])){
              feeDF <- data.frame(DETC_CODE = c(NA,'DE','N010','TRAU'), #the NA is a placeholder needed when setting the statutory fine range
                                  FEE_AMOUNT = c(0,50,10,10)) #the NA is a placeholder needed when setting the statutory fine range
              
              chargeInfo <- chargeInfo %>%
                bind_rows(chargeInfo, chargeInfo, chargeInfo) %>%
                mutate(DETC_CODE = feeDF$DETC_CODE,
                       FEE_AMOUNT = feeDF$FEE_AMOUNT,
                       LFO_TYPE = c("no_default", rep('fee',3))) #change to "statutory_range" when setting min/max below
            }else{
              chargeInfo 
              }
            }#extractCharge function

        #run extractCharge and send rows for each charge to its own list
        df_list <- lapply(input$charges,extractCharge,table=lfoTableJIMS_non_felony)
        
        # #extract lists and assign to their own dfs
        # #h/t: https://stackoverflow.com/questions/30516325/converting-a-list-of-data-frames-into-individual-data-frames-in-r
        # for (i in 1:length(df_list)) {
        #     assign(paste0("charge", i), as.data.frame(df_list[[i]]))
        #     }
        })#reactive

    #-----------------------------------#
    #render NO BOXES ----
    #-----------------------------------#   
    #for page 3
    output$noChargesSelected_p3 <- renderUI({
        if(length(input$charges)==0){
            h3('No charges selected. Please use the back button to return to step 1.')
        }
    })
    
    #for page 4
    output$noChargesSelected_p4 <- renderUI({
        if(length(input$charges)==0){
            h3('No charges selected. Please use the back button to return to step 1.')
        }
    })
    
    #-----------------------------------#
    #render CHARGE BOXES ----
    #-----------------------------------#
    #Attempt #1 to create boxes dynamically at step 3
    #description: reactiveVal and lapply -> doesn't work b/c chargesNum is reactive but is being used outside a reactive context
    # chargesNum <- reactiveVal(length(input$charges))
    
    # lapply(1:chargesNum, function(i){
    
    
    #Attempt #3 to create boxes dynamically at step 3 (2023/04/14)
    #description: observeEvent with output[[]] -> try this in the future
    #h/t: https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/
    #psuedocode:
    #observeEvent([when next is clicked to go to step 3], {
    #loop over length of input$charges
    #output[[paste0(name_of_output,i)]] <- renderUI({code here})
    #})
    
    #Attempt #2 to create boxes dynamically at step 3 
    #description: lapply outside a reactive context using a fixed upper limit on # of charges -> this is a non-dynamic solution that works 
    #Note: the lapply needs to be outside a reactive element; using chargesNum in a reactive element will not work b/c outputs, which are also reactive, would need to be embedded inside the reactive elements, which is not a good idea
    #Note: AC (2023/04/14): set max no of charges that can be selected to 11 based on an analysis of district court cases in 2023 so far; well over 99% have 11 or fewer charges

    lapply(1:11, function(i){
        
        fineTmp <- reactive({
          
            #simplify call
            ffTableTmp1 <- ffTableTmp1()
            
            #isolate row with fine
            fineTmp <- ffTableTmp1[[i]] %>%
                filter(LFO_TYPE %in% c("default_fine", "no_default"))
            
            #set custom fines
            #LFO_TYPE = no_default gets converted to one of these three: {statutory_range, missing_fine, no_fine}
            #no_fine = {291C-137(a) and (c) but not in sch/const; HAR 3-30-19(a) and (b)}
            if(nrow(fineTmp)==0){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(DETC_CODE = NA,
                       FEE_AMOUNT = 0,
                       authorizingLaw = NA,
                       LFO_TYPE = 'no_fine')
            }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS286.102',i)]]) || input[[paste0('priorsHRS286.102',i)]]==FALSE) & (is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==FALSE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 0,
                       FEE_AMOUNT_MAX = 1000,
                       LFO_TYPE = "statutory_range")
            }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS286.102',i)]]) && input[[paste0('priorsHRS286.102',i)]]==TRUE) & (is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==FALSE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 500,
                       FEE_AMOUNT_MAX = 1000,
                       LFO_TYPE = "statutory_range")
            }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==TRUE) & input$chargeDate >= '2003-05-20' & (is.null(input[[paste0('juvHRS286.102',i)]]) || input[[paste0('juvHRS286.102',i)]]==FALSE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT = 500,
                       LFO_TYPE = 'default_fine')
            }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==TRUE) & input$chargeDate >= '2003-05-20' & (!is.null(input[[paste0('juvHRS286.102',i)]]) || input[[paste0('juvHRS286.102',i)]]==TRUE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT = 0,
                       LFO_TYPE = 'default_fine')
            }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==FALSE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT = 500,
                       LFO_TYPE = 'default_fine',
                       mandatory_fine = "yes")
            }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==TRUE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 1500,
                       FEE_AMOUNT_MAX = 5000,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS291.11.5',i)]]) || input[[paste0('priorsHRS291.11.5',i)]]==1)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 0,
                       FEE_AMOUNT_MAX = 100,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==2) & input$chargeDate >= '2022-06-27'){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 250, #FEE_AMOUNT_MIN = 100,
                       FEE_AMOUNT_MAX = 500, #FEE_AMOUNT_MAX = 200,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==3) & input$chargeDate >= '2022-06-27'){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 500, #FEE_AMOUNT_MIN = 200,
                       FEE_AMOUNT_MAX = 800, #FEE_AMOUNT_MAX = 500,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==2) & input$chargeDate < '2022-06-27'){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 100,
                       FEE_AMOUNT_MAX = 200,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==3) & input$chargeDate < '2022-06-27'){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 200,
                       FEE_AMOUNT_MAX = 500,
                       LFO_TYPE = 'statutory_range')
              #this block replaced by no_fine above
            # }else if(grepl('HRS 291C-137',fineTmp$CHARGE_CODE) & !grepl('SCH', fineTmp$CHARGE_QUALIFIER)){
            #   fineTmp <- ffTableTmp1[[i]] %>%
            #     slice_head(n=1) %>%
            #     mutate(FEE_AMOUNT = 0,
            #            LFO_TYPE = 'default_fine')
            }else if(grepl('HRS 291-21.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('personRoleHRS291.21.5',i)]]) && input[[paste0('personRoleHRS291.21.5',i)]]=="owner")){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 250,
                       FEE_AMOUNT_MAX = 500,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-21.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('personRoleHRS291.21.5',i)]]) && input[[paste0('personRoleHRS291.21.5',i)]]=="installer")){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 500*input[[paste0("countInstallHRS291.21.5", i)]],
                       FEE_AMOUNT_MAX = 1000*input[[paste0("countInstallHRS291.21.5", i)]],
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-57',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('failDisplayHRS291.57',i)]]) && input[[paste0('failDisplayHRS291.57',i)]]==FALSE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 250,
                       FEE_AMOUNT_MAX = 500,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291-57',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('failDisplayHRS291.57',i)]]) && input[[paste0('failDisplayHRS291.57',i)]]==TRUE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 25,
                       FEE_AMOUNT_MAX = 100,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS291C.105',i)]]) || input[[paste0('priorsHRS291C.105',i)]]==1)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 500,
                       FEE_AMOUNT_MAX = 1000,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291C.105',i)]]) && input[[paste0('priorsHRS291C.105',i)]]==2)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = 750, 
                       FEE_AMOUNT_MAX = 1000,
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291C.105',i)]]) && input[[paste0('priorsHRS291C.105',i)]]==3)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT = 1000, 
                       LFO_TYPE = 'default_fine',
                       mandatory_fine = "yes")
            }else if(grepl('HRS 291C-222\\(c\\)',fineTmp$CHARGE_CODE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT = case_when(!is.null(input[[paste0('priorsHRS291C.222',i)]]) && input[[paste0('priorsHRS291C.222',i)]]== 1 ~ 75,
                                              !is.null(input[[paste0('priorsHRS291C.222',i)]]) && input[[paste0('priorsHRS291C.222',i)]]== 2 ~ 150,
                                              !is.null(input[[paste0('priorsHRS291C.222',i)]]) && input[[paste0('priorsHRS291C.222',i)]]== 3 ~ 200),
                       LFO_TYPE = 'default_fine',
                       mandatory_fine = "yes")
            }else if(grepl('HRS 291E-61',fineTmp$CHARGE_CODE) & input$chargeDate >= '2019-07-01'){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                                    (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 250,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 1000,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 750,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1500),
                       FEE_AMOUNT_MAX = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                                    (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 1000,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 3000,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1500,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 3500),
                       LFO_TYPE = 'statutory_range')
            }else if(grepl('HRS 291E-61',fineTmp$CHARGE_CODE) & input$chargeDate < '2019-07-01'){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT_MIN = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                                    (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 150,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 500,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 500,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 650,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1000,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1000),
                       FEE_AMOUNT_MAX = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                                    (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 1000,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 1500,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 2500,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1500,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 2000,
                                                  !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                                    input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 3000),
                       LFO_TYPE = 'statutory_range')
              #From Judge May, mandatory fine differs from default fine in JIMS, use mandatory fine according to 1DC fine schedule
            }else if(grepl("ROH 15-13\\.[35]|ROH 15-14\\.1\\(a\\)\\([1-7]\\)|ROH 15-14\\.[235]|ROH 15-15\\.1\\([ab]\\)|ROH 15-23\\.6",fineTmp$CHARGE_CODE)){
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(FEE_AMOUNT = 15, 
                       LFO_TYPE = 'default_fine')
            }else if (fineTmp$LFO_TYPE == "no_default"){#all of the remaining charges not yet entered into calculator
              fineTmp <- ffTableTmp1[[i]] %>%
                slice_head(n=1) %>%
                mutate(DETC_CODE = NA,
                       FEE_AMOUNT = NA,
                       authorizingLaw = NA,
                       LFO_TYPE = 'missing_fine')
            }else
              fineTmp
        })

    #fineInitial renderUI----
    output[[paste0('fineInitial',i)]] <- renderUI({

        #simplify call
        fineTmp <- fineTmp()
        
        if (grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==FALSE)){
          p(strong('Fine'),'between $',span(fineTmp$FEE_AMOUNT_MIN,.noWS = c("inside", "outside")), br(), 'and $',span(fineTmp$FEE_AMOUNT_MAX, .noWS = c("inside", "outside")))
        }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==FALSE)){
          #custom code for 431:10C-104 mandatory fine -> popover for when fine can be suspended
          p(strong('Fine'),'$',span(fineTmp$FEE_AMOUNT, .noWS = c("inside", "outside")),
            br(), span(style = "color:red;", 
                       actionLink(inputId = "431_suspend_fine", label = "Mandatory fine"),
                   tags$style(".popover{max-width:30%;}"),
                   bsPopover(#session=session,
                     id="431_suspend_fine",
                     title = "Judicial discretion",
                     content = 'According to <a href= "https://www.capitol.hawaii.gov/hrscurrent/Vol09_Ch0431-0435H/HRS0431/HRS_0431-0010C-0117.htm" target = "_blank">HRS 431:10C-117(a)(2)(B)(i)</a>, "[the court] Shall have the discretion to suspend all or any portion of the fine if the defendant provides proof of having a current motor vehicle insurance policy."',
                     placement = 'bottom',
                     trigger = c('focus'),
                     options = list(container = 'body')))
              )
        }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==TRUE)){
          p(strong('Fine'),'between $',span(fineTmp$FEE_AMOUNT_MIN,.noWS = c("inside", "outside")), br(), 'and $',span(fineTmp$FEE_AMOUNT_MAX, .noWS = c("inside", "outside")))
        }else if((grepl('HRS 291-(11\\.5|21\\.5|57)|HRS 291C-105|HRS 291E-61',fineTmp$CHARGE_CODE)) & 
                 fineTmp$LFO_TYPE == 'statutory_range'){
          p(strong('Fine'),'between $',span(fineTmp$FEE_AMOUNT_MIN,.noWS = c("inside", "outside")), br(), 'and $',span(fineTmp$FEE_AMOUNT_MAX, .noWS = c("inside", "outside")))
        }else if(grepl('HRS 291C-105|HRS 291C-222',fineTmp$CHARGE_CODE)){
          if(fineTmp$LFO_TYPE == 'statutory_range'){
            p(strong('Fine'),'between $',span(fineTmp$FEE_AMOUNT_MIN,.noWS = c("inside", "outside")), br(), 'and $',span(fineTmp$FEE_AMOUNT_MAX, .noWS = c("inside", "outside")))
          }else if(fineTmp$LFO_TYPE == 'default_fine'){
            p(strong('Fine'),'$',span(fineTmp$FEE_AMOUNT, .noWS = c("inside", "outside")),
              div(style = "color:red;", "Mandatory fine"))
          }
        }else if(fineTmp$LFO_TYPE %in% c('default_fine', "no_fine")){
          p(strong('Fine'),'$',span(fineTmp$FEE_AMOUNT, .noWS = c("inside", "outside")))
        }else if(fineTmp$LFO_TYPE %in% c('missing_fine')){
          p(strong('Fine '), span(fineTmp$FEE_AMOUNT, .noWS = c("inside", "outside")))
        }
    })
    
    #-----------------------------------#
    #checkbox logic ----
    #flush the checkboxes/radiobuttons at step 3 if new charges are entered
    #-----------------------------------#

    #fineInitialCheckboxes renderUI----
    output[[paste0('fineInitialCheckboxes',i)]] <- renderUI({

      #simplify call
      ffTableTmp1 <- ffTableTmp1()
      
      #isolate row with fine
      fineTmp <- ffTableTmp1[[i]] %>%
        filter(LFO_TYPE %in% c('no_default','default_fine'))
      
      #create custom fineTmp for fineInitialCheckboxes that doesn't create dependency/loop/circular reference between them
      #If fine doesn't exist, create a dummy fine row, label as 'no_fine' so that if/else if below can test no_fine charges
      if(nrow(fineTmp)==0){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(LFO_TYPE = 'no_fine')

      }else
        fineTmp
      
      if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE)){
          p(checkboxInput(inputId = paste0('priorsHRS286.102',i),
                        label = '2+ priors within 5 years?',
                        value = FALSE),
            checkboxInput(inputId = paste0("minorHRS286.102",i),
                          label = "Minor < 18, under the jurisdiction of the Family Court?",
                          value = FALSE),
            checkboxInput(inputId = paste0('juvHRS286.102',i),
                          label = 'Lose the right to drive until age of 18?',
                          value = FALSE)
            )
        #another way to implement JUV charge code
        # }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & fineTmp$CHARGE_QUALIFIER == "JUV"){
        #   checkboxInput(inputId = paste0('juvHRS286.102',i),
        #                 label = 'Lose the right to drive until age of 18?',
        #                 value = FALSE)
      }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE)){
        p(
          # checkboxInput(inputId = paste0('reduceFine',i),
          #               label = 'Reduce fine?',
          #               value = FALSE),
          checkboxInput(inputId = paste0('priorsHRS431.10C.104',i),
                        label = '1+ priors within 5 years?',
                        value = FALSE))
      }else if(grepl('HRS 291-11\\.5',fineTmp$CHARGE_CODE)){
        p(radioButtons(inputId = paste0('priorsHRS291.11.5',i),
                        label = 'Offenses within 3 years?',
                       choices = c('1st offense'=1,'2nd offense'=2,'third+ offense'=3),
                       selected = 1))
      }else if(grepl('HRS 291-21\\.5',fineTmp$CHARGE_CODE)){
        p(radioButtons(inputId = paste0('personRoleHRS291.21.5',i),
                       label = 'Role?',
                       choices = c('Car Owner'="owner",'Installer'="installer"),
                       selected = "owner"),
          numericInput(inputId = paste0("countInstallHRS291.21.5", i),
                       label = "Number of installs:", 
                       value = 1,
                       min = 1,
                       max = 10000, 
                       step = 1))
      }else if(grepl('HRS 291-57',fineTmp$CHARGE_CODE)){
          p(checkboxInput(inputId = paste0('failDisplayHRS291.57',i),
                         label = 'Failure to display valid disability parking permit?',
                         value = FALSE))
      }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE)){
        p(radioButtons(inputId = paste0('priorsHRS291C.105',i),
                       label = 'Offenses within 5 years?',
                       choices = c('1st offense'=1,'2nd offense'=2,'third+ offense'=3),
                       selected = 1))
      }else if(grepl('HRS 291C-222',fineTmp$CHARGE_CODE)){
        p(radioButtons(inputId = paste0('priorsHRS291C.222',i),
                       label = 'Offenses within 1 year?',
                       choices = c('1st offense'=1,'2nd offense'=2,'third+ offense'=3),
                       selected = 1))
      }else if(grepl('HRS 291E-61',fineTmp$CHARGE_CODE) & input$chargeDate < '2019-07-01'){
        p(radioButtons(inputId = paste0('priorsHRS291E.61',i),
                       label = paste0('Offenses within 5 years?'),
                       choices = c('no priors'=1,'one prior'=2, "two or more priors" = 3),
                       selected = 1),
          checkboxInput(inputId = paste0('minorPassengerHRS291E.61',i),
                        label = 'With passenger < 15 years of age?',
                        value = FALSE))
      }else if(grepl('HRS 291E-61',fineTmp$CHARGE_CODE) & input$chargeDate >= '2019-07-01'){
        p(radioButtons(inputId = paste0('priorsHRS291E.61',i),
                       label = paste0('Offenses within 10 years?'),
                       choices = c('no priors'=1,'one prior'=2),
                       selected = 1),
          checkboxInput(inputId = paste0('minorPassengerHRS291E.61',i),
                        label = 'With passenger < 15 years of age?',
                        value = FALSE))
      }else if(fineTmp$mandatory_fine == "yes"){
        p(style = "color:red;", "Mandatory fine")
      }else if(fineTmp$LFO_TYPE == 'default_fine'){
        p(checkboxInput(inputId = paste0('reduceFine',i),
                        label = 'Reduce fine?',
                        value = FALSE))
      }
    })
    
    #2023/##/## changed reduced fine to mandatory fine based on feedback from Judge May
    # #disable and reset reduce fine checkboxes for 431:10C-104 depending on status of priors checkbox----
    # observe({
    #   if(is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==FALSE){
    #     enable(paste0('reduceFine',i))
    #   }else if(input[[paste0('priorsHRS431.10C.104',i)]]==TRUE){
    #    disable(paste0('reduceFine',i))
    #     updateCheckboxInput(
    #       session = session,
    #       inputId = paste0('reduceFine',i),
    #       label = 'Reduce fine?',
    #       value = FALSE)
    #   }
    # })
    
    #disable and reset reduce fine checkboxes for 286-102 depending on status of juvenile checkbox----
    observe({
      if(is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==FALSE){
        enable(paste0('priorsHRS286.102',i))
        hide(paste0('juvHRS286.102',i))
        }else if(input[[paste0("minorHRS286.102",i)]]==TRUE){
          disable(paste0('priorsHRS286.102',i))
          show(paste0('juvHRS286.102',i))
      }
    })
    
    #disable and reset # of installations for installer checkboxes for 291-21.5 depending on status of role checkbox----
    observe({
      if(is.null(input[[paste0('personRoleHRS291.21.5',i)]]) || input[[paste0('personRoleHRS291.21.5',i)]]=="installer"){
        enable(paste0("countInstallHRS291.21.5", i))
      }else if(input[[paste0('personRoleHRS291.21.5',i)]]!="installer"){
        disable(paste0("countInstallHRS291.21.5", i))
        updateCheckboxInput(
          session = session,
          inputId = paste0("countInstallHRS291.21.5", i),
          label = 'Number of installs:',
          value = 1)
      }
    })
    
    
    #fineSlider renderUI----
    output[[paste0('fineSlider',i)]] <- renderUI({
        
        #simplify call
        fineTmp <- fineTmp()
        
            if(fineTmp$LFO_TYPE %in% c('statutory_range')){
              p(sliderInput(inputId = paste0('fineRangeSlider',i),
                            label = 'Set fine amount',
                            min = fineTmp$FEE_AMOUNT_MIN,
                            max = fineTmp$FEE_AMOUNT_MAX,
                            value = fineTmp$FEE_AMOUNT_MIN,
                            step = 5,
                            pre = '$')) 
              #first check if checkbox NULL at loading, if NULL, don't show slider (this avoids error message when initially evaluating if(input[[paste0('reduceFine',i)]] == TRUE), which will yield "Warning: Error in if: argument is of length zero" because slider hasn't been created yet
              #if !NULL, then check if checkbox checked, if TRUE, show slider
            }else if(fineTmp$LFO_TYPE == 'default_fine' & !is.null(input[[paste0('reduceFine',i)]])){
                if(input[[paste0('reduceFine',i)]] == TRUE){
                    p(sliderInput(inputId = paste0('fineReduceSlider',i),
                                  label = 'Reduce fine amount',
                                  min = 0,
                                  max = fineTmp$FEE_AMOUNT,
                                  value = fineTmp$FEE_AMOUNT,
                                  step = 5,
                                  pre = '$'))}
            }else if(fineTmp$LFO_TYPE == 'default_fine' & fineTmp$mandatory_fine == "yes"){
              p(' ')
            }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & !is.null(input[[paste0('priorsHRS286.102',i)]])){
              p(' ')
            }else if(fineTmp$LFO_TYPE %in% c('no_fine')){
              p(' ')
            }else if(fineTmp$LFO_TYPE %in% c("missing_fine")){
              HTML(paste0('Sorry! This charge has not yet been added to the calculator.',
              ' To have it included, please send a request to <a href="mailto:finescalculator@courts.hawaii.gov">',
              'finescalculator@courts.hawaii.gov</a>, mahalo.'))
            #if not statutory_range, default_fine, no_fine, or missing_fine
            }else if(fineTmp$LFO_TYPE == 'default_fine' & is.null(input[[paste0('reduceFine',i)]])){
              p(' ')
            }else
              p('ERROR',style="color:red;")
    })
    
    #update HRS 286-102 slider if priors----
    # observeEvent(input[[paste0('priorsHRS286.102',i)]],{
    #   fineTmp <- fineTmp()
    #   
    #   if(input[[paste0('priorsHRS286.102',i)]]==TRUE){
    #   updateSliderInput(inputId = paste0('fineRangeSlider',i),
    #               label = 'Set fine amount',
    #               min = 500,
    #               max = fineTmp$FEE_AMOUNT_MAX,
    #               value = 500,
    #               step = 5)
    # }else
    #   updateSliderInput(inputId = paste0('fineRangeSlider',i),
    #                     label = 'Set fine amount',
    #                     min = fineTmp$FEE_AMOUNT_MIN,
    #                     max = fineTmp$FEE_AMOUNT_MAX,
    #                     value = fineTmp$FEE_AMOUNT_MIN,
    #                     step = 5)
    # })
    
    #update HRS 431:10C-104 slider if priors----
    # fineTmp <- eventReactive(input[[paste0('priorsHRS431.10C.104',i)]],{
    #   if(input[[paste0('priorsHRS431.10C.104',i)]]==TRUE){
    #   # fineTmp <- fineTmp()
    #     fineTmp <- ffTableTmp1[[i]] %>%
    #       slice_head(n=1) %>%
    #       mutate(FEE_AMOUNT_MIN = 1500,
    #              FEE_AMOUNT_MAX = 5000,
    #              LFO_TYPE = 'statutory_range')
    #   }else if(input[[paste0('priorsHRS431.10C.104',i)]]==FALSE){
    #     # fineTmp <- fineTmp()
    #     fineTmp <- ffTableTmp1[[i]] %>%
    #       slice_head(n=1) %>%
    #       mutate(FEE_AMOUNT = 500,
    #              LFO_TYPE = 'default_fine')}
    # })
    
    # observeEvent(input[[paste0('priorsHRS431.10C.104',i)]],{
    #   fineTmp <- fineTmp()
    # 
    #   if(input[[paste0('priorsHRS431.10C.104',i)]]==TRUE){
    #     updateSliderInput(inputId = paste0('fineRangeSlider',i),
    #                       label = 'Set fine amount',
    #                       min = fineTmp$FEE_AMOUNT_MIN,
    #                       max = fineTmp$FEE_AMOUNT_MAX,
    #                       value = fineTmp$FEE_AMOUNT_MIN,
    #                       step = 5)
    #   }else
    #     updateSliderInput(inputId = paste0('fineRangeSlider',i),
    #                       label = 'Set fine amount',
    #                       min = 0,
    #                       max = fineTmp$FEE_AMOUNT,
    #                       value = fineTmp$FEE_AMOUNT,
    #                       step = 5)
    # })
    
    #fineFinal renderUI----
    output[[paste0('fineFinal',i)]] <- renderUI({

        #simplify call
        fineTmp <- fineTmp()
        
            if(fineTmp$LFO_TYPE %in% c('statutory_range')){
                p('To pay: $', span(input[[paste0('fineRangeSlider',i)]], .noWS = c("outside")))
            }else if(!is.null(input[[paste0('reduceFine',i)]]) && input[[paste0('reduceFine',i)]] == TRUE){
              #include is.null(input[[paste0('reduceFine',i)]]) in disjunction to suppress "Warning: Error in if: argument is of length zero" which will appear if slider hasn't been created yet
              p('To pay: $', span(input[[paste0('fineReduceSlider',i)]], .noWS = c("outside")))
            }else if(fineTmp$LFO_TYPE %in% c('missing_fine')){
              p('To pay: ', span(fineTmp$FEE_AMOUNT, .noWS = c("outside")))
            }else{
              p('To pay: $', span(fineTmp$FEE_AMOUNT, .noWS = c("outside")))
            }    
    })
    
    #feeTable renderUI----
    output[[paste0('feeTable',i)]] <- renderUI({
        
        #filter fees and add column for fee authorizing statute
        ffTableTmp1 <- ffTableTmp1()
        
        feeTable1 <- ffTableTmp1[[i]] %>%
            filter(LFO_TYPE == 'fee') %>%
            mutate(
              fee_description = case_when(
                DETC_CODE %in% c('AFCE','AFCM','AFCP') ~ 'Administrative Fee - Computer System Special Fund', #AFCE/AFCM/AFCP I think refer to the 1/2 of admin fees that go to  judiciary computer system special fund; see HRS 607-4
                DETC_CODE %in% c('AFE','AFM','AFP') ~ 'Administrative Fee', #AFCE/AFCM/AFCP I think refer to the 1/2 of admin fees that go to  judiciary computer system special fund; see HRS 607-4
                DETC_CODE == 'DE' ~ 'Driver Education Fee',
                DETC_CODE == 'DOT2' ~ 'DOT Fines-Highways',
                # DETC_CODE == 'DOT3' ~ ''; #related to old Red-Light Safety Pilot, see http://jira.courts.state.hi.us:8080/browse/JPS-30495
                DETC_CODE == 'DOT4' ~ 'Safe Routes to School Program Surcharge',
                DETC_CODE == 'DOT5' ~ 'Safe Routes to School Program Surcharge-Spdng Sch Zn/Cnstrxn Area',
                DETC_CODE == 'DOT6' ~ 'Illegal Parking Upon Bikeway Fee; State Highway Fund',
                DETC_CODE == 'DOT7' ~ 'Mobile Electronic Devices Fee',
                DETC_CODE == 'DOT8' ~ 'Illegal Parking Upon Bikeway Fee; State Highway Fund',
                DETC_CODE == 'DOT9' ~ 'State Highway Enforcement Program Surcharge',
                DETC_CODE == 'DPK1' ~ 'DAGS State Parking Fee',
                DETC_CODE == 'DPK2' ~ 'DAGS State Parking Fee',
                DETC_CODE == 'DPK3' ~ 'State Parking Revolving Fund Fee',
                DETC_CODE == 'N010' ~ 'Neurotrauma Special Fund Fee',
                DETC_CODE == 'TRAU' ~ 'Trauma System Special Fund Surcharge',
                TRUE ~ 'under construction'),
              authorizingLaw = case_when(
                    DETC_CODE %in% c('AFCE','AFCM','AFCP') ~ 'HRS 607-4', #; also see HRS 601-3.7
                    DETC_CODE %in% c('AFE','AFM','AFP') ~ 'HRS 607-4',
                    DETC_CODE == 'DE' ~ 'HRS 286G-3',
                    DETC_CODE == 'DOT2' ~ 'under construction',
                    DETC_CODE == 'DOT4' ~ 'HRS 291C-4',#; also see HRS 291-16 or HRS 291C-5',
                    DETC_CODE == 'DOT5' ~ 'HRS 291C-4',#; also see HRS 291C-104, HRS 291-16 or HRS 291C-5',
                    DETC_CODE == 'DOT6' ~ 'HRS 291C-123',#; also see HRS 248-9',
                    DETC_CODE == 'DOT7' ~ 'HRS 291C-137',#; also see HRS 248-9 or 291C-171',
                    DETC_CODE == 'DOT8' ~ 'HRS 291C-123',#; also see HRS 248-9',
                    DETC_CODE == 'DOT9' ~ 'HRS 291C-111',#; also see HRS 248-9',
                    DETC_CODE == 'DPK1' ~ 'HAR 3-30-19',
                    DETC_CODE == 'DPK2' ~ 'HAR 3-30-19',
                    DETC_CODE == 'DPK3' ~ 'HRS 107-11',
                    DETC_CODE == 'N010' ~ 'HRS 321H-4',
                    DETC_CODE == 'TRAU' ~ 'HRS 321-22.5',#; also see HRS 291-15, HRS 291C-2, or HRS 291E-7',
                    TRUE ~ 'under construction'),
                authorizingLink = case_when(
                    DETC_CODE %in% c('AFCE','AFCM','AFCP') ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol13_Ch0601-0676/HRS0607/HRS_0607-0004.htm',
                    DETC_CODE %in% c('AFE','AFM','AFP') ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol13_Ch0601-0676/HRS0607/HRS_0607-0004.htm',
                    DETC_CODE == 'DE' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0286G/HRS_0286G-0003.htm',
                    DETC_CODE == 'DOT2' ~ 'under construction',
                    DETC_CODE == 'DOT4' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291C/HRS_0291C-0004.htm',
                    DETC_CODE == 'DOT5' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291C/HRS_0291C-0004.htm',
                    DETC_CODE == 'DOT6' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291C/HRS_0291C-0123.htm',
                    DETC_CODE == 'DOT7' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291C/HRS_0291C-0137.htm',
                    DETC_CODE == 'DOT8' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291C/HRS_0291C-0123.htm',
                    DETC_CODE == 'DOT9' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291C/HRS_0291C-0111.htm',
                    DETC_CODE == 'DPK1' ~ 'https://ags.hawaii.gov/wp-content/uploads/2012/10/chapter_30.pdf',
                    DETC_CODE == 'DPK2' ~ 'https://ags.hawaii.gov/wp-content/uploads/2012/10/chapter_30.pdf',
                    DETC_CODE == 'DPK3' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol02_Ch0046-0115/HRS0107/HRS_0107-0011.htm',
                    DETC_CODE == 'N010' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol06_Ch0321-0344/HRS0321H/HRS_0321H-0004.htm',
                    DETC_CODE == 'TRAU' ~ 'https://www.capitol.hawaii.gov/hrscurrent/Vol06_Ch0321-0344/HRS0321/HRS_0321-0022_0005.htm'),
                authorizingLaw2 = case_when(
                  DETC_CODE %in% c('AFCE','AFCM','AFCP') ~ 'also see HRS 601-3.7',
                  DETC_CODE == 'DOT4' ~ '; also see HRS 291-16 or HRS 291C-5',
                  DETC_CODE == 'DOT5' ~ '; also see HRS 291C-104, HRS 291-16 or HRS 291C-5',
                  DETC_CODE == 'DOT6' ~ '; also see HRS 248-9',
                  DETC_CODE == 'DOT7' ~ '; also see HRS 248-9 or 291C-171',
                  DETC_CODE == 'DOT8' ~ '; also see HRS 248-9',
                  DETC_CODE == 'DOT9' ~ '; also see HRS 248-9',
                  DETC_CODE == 'TRAU' ~ '; also see HRS 291-15, HRS 291C-2, or HRS 291E-7',
                  )
                )

        #h/t: https://stackoverflow.com/questions/57202330/create-fluidrows-in-a-loop-r-shiny 
        ui_fees <- c()
        if(nrow(feeTable1)>=1){
            for(i in 1:nrow(feeTable1)){
                ui_fees[[i]] <- fluidRow(
                    column(width = 9, align = 'left',
                           #DISPLAY DETC CODE and authorizing statutes
                           # HTML(paste0(strong('Fee - '),feeTable1$DETC_CODE[i],' (',tags$a(feeTable1$authorizingLaw[i],href=feeTable1$authorizingLink[i], target="_blank"),feeTable1$authorizingLaw2[i],')'))
                           #DISPLAY description and authorizing statutes
                           # HTML(paste0(strong('Fee - '),feeTable1$fee_description[i],' (',tags$a(feeTable1$authorizingLaw[i],href=feeTable1$authorizingLink[i], target="_blank"),')'))
                           #DISPLAY description/DETC Code and authorizing statutes
                           HTML(paste0(strong('Fee - '),feeTable1$fee_description[i],'/',feeTable1$DETC_CODE[i],' (',tags$a(feeTable1$authorizingLaw[i],href=feeTable1$authorizingLink[i], target="_blank"),')'))
                           ),
                    
                    # column(width = 1, ''),
                    column(width = 3, align = 'right',
                           p('To pay: $', span(feeTable1$FEE_AMOUNT[i], .noWS = c("outside"))))
                )
            }
        }else{
            ui_fees <- fluidRow(
                column(width = 9, align = 'left',
                       p(strong('Fee - set by court'))),
                # column(width = 1, ''),
                column(width = 3, align = 'right',
                       p('To pay: TBD'))
            )
        }
        
        ui_fees
    })
    
    #chargeTotalFF renderUI----
    output[[paste0('chargeTotalFF',i)]] <- renderUI({

        #simplify call
        fineTmp <- fineTmp()
        
        if(fineTmp$LFO_TYPE %in% c('statutory_range','blank_fine')){
            finalFine <- input[[paste0('fineRangeSlider',i)]]
        }else if(!is.null(input[[paste0('reduceFine',i)]]) && input[[paste0('reduceFine',i)]] == TRUE){
            #include is.null(input[[paste0('reduceFine',i)]]) in disjunction to suppress "Warning: Error in if: argument is of length zero" which will appear if slider hasn't been created yet
            finalFine <- input[[paste0('fineReduceSlider',i)]]
        }else{
            finalFine <- fineTmp$FEE_AMOUNT
        }
        
        ffTableTmp1 <- ffTableTmp1()

        feeTable1 <- ffTableTmp1[[i]] %>%
          filter(LFO_TYPE == 'fee')
        
        totalFineFees <- sum(finalFine,
                             feeTable1$FEE_AMOUNT,na.rm = TRUE)
        
        fluidRow(column(width = 9, align = 'left',''),
                 # column(width = 3, ''),
                 column(width = 3, align = 'right',
                        hr(style = 'border-color:black;'),
                        if(fineTmp$LFO_TYPE == "missing_fine"){
                          paste("Calculator cannot estimate at this time")
                        }else 
                          p(strong('Total: $', span(totalFineFees, .noWS = "outside")))
                 )
        )
    })
    
    })#lapply
    
    #chargeBox renderUI----
    output$chargeBox <- renderUI({
        
        lapply(1:length(input$charges), function(i){
        
            ffTableTmp1 <- ffTableTmp1()
            ffTableTmp2 <- ffTableTmp1[[i]] %>%
                slice_head(n = 1)
            
            list(box(title = strong(ffTableTmp2$CHARGE_CODE,' - ',ffTableTmp2$DESCRIPTION,
                                    if_else(is.na(ffTableTmp2$CHARGE_QUALIFIER),'',paste0(' ',ffTableTmp2$CHARGE_QUALIFIER))),
                     width = 12,
                     collapsible = TRUE,
                     fluidRow(
                         column(width = 3, align = 'left',
                                uiOutput(paste0('fineInitial',i)),
                                # if(input$role %in% c('Public Defender/Defense Attorney','Prosecutor','Judge')){
                                  uiOutput(paste0('fineInitialCheckboxes',i))
                                # }
                                ),
                         column(width = 6,
                                # if(input$role %in% c('Public Defender/Defense Attorney','Prosecutor','Judge')){
                                  uiOutput(paste0('fineSlider',i))
                                # }
                                
                                # if(ffTableTmp1()$fine_type[i] == 'statutory range'){
                                #     input[[paste0('fineRange',i)]]
                                # }else if(input[[paste0('reduceFine',i)]] == TRUE || is.null(input[[paste0('reduceFine',i)]])){
                                #     input[[paste0('fineRange',i)]]
                                # }
                                ),
                         column(width = 3, align = 'right',
                                uiOutput(paste0('fineFinal',i))
                         )
                     ),
                     uiOutput(paste0('feeTable',i)),
                     uiOutput(paste0('chargeTotalFF',i))
                     )#box
                 )#list
            })#lapply
        })#renderUI chargeBox
    
    #-----------------------------------#
    #page 4 summary----
    #-----------------------------------#
    #___print page 4----
    observeEvent(input$print_page_4,{
      js$winprint()
    })
    
    #___email and phone info----
    output$email_info <- renderUI({
      if(input$email == "" || is.null(input$email)){
        HTML(paste0("e: <em>no e-mail provided</em>"))
      }else
        paste0("e: ", input$email)
    })
    
    output$phone_info <- renderUI({
      if(input$phone == "" || is.null(input$phone)){
        HTML(paste0("p: <em>no phone number provided</em>"))
      }else
        paste0("p: ", input$phone)
    })
    
    #extract total LFO for each charge, save in list of dataframes called ui_charges, one dataframe per charge
    #___ui_charges----
    ui_charges <- reactive({
      lapply(1:length(input$charges), function(i){
      
      #simplify call
      ffTableTmp1 <- ffTableTmp1()
      
      #isolate row with fine
      fineTmp <- ffTableTmp1[[i]] %>%
        filter(LFO_TYPE %in% c('default_fine', "no_default"))
      
      #re-compute initial fines (copied from beginning of page 3)
      #LFO_TYPE = no_default gets converted to one of these three: {statutory_range, missing_fine, no_fine}
      #no_fine = {291C-137(a) and (c) but not in sch/const; HAR 3-30-19(a) and (b)}
      if(nrow(fineTmp)==0){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(DETC_CODE = NA,
                 FEE_AMOUNT = 0,
                 authorizingLaw = NA,
                 LFO_TYPE = 'no_fine')
      }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS286.102',i)]]) || input[[paste0('priorsHRS286.102',i)]]==FALSE) & (is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==FALSE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 0,
                 FEE_AMOUNT_MAX = 1000,
                 LFO_TYPE = "statutory_range")
      }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS286.102',i)]]) && input[[paste0('priorsHRS286.102',i)]]==TRUE) & (is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==FALSE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 500,
                 FEE_AMOUNT_MAX = 1000,
                 LFO_TYPE = "statutory_range")
      }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==TRUE) & input$chargeDate >= '2003-05-20' & (is.null(input[[paste0('juvHRS286.102',i)]]) || input[[paste0('juvHRS286.102',i)]]==FALSE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT = 500,
                 LFO_TYPE = 'default_fine')
      }else if(grepl('HRS 286-102([^.]|$)',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0("minorHRS286.102",i)]]) || input[[paste0("minorHRS286.102",i)]]==TRUE) & input$chargeDate >= '2003-05-20' & (!is.null(input[[paste0('juvHRS286.102',i)]]) || input[[paste0('juvHRS286.102',i)]]==TRUE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT = 0,
                 LFO_TYPE = 'default_fine')
        
      }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==FALSE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT = 500,
                 LFO_TYPE = 'default_fine',
                 mandatory_fine = "yes")
      }else if(grepl('HRS 431:10C-104',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS431.10C.104',i)]]) || input[[paste0('priorsHRS431.10C.104',i)]]==TRUE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 1500,
                 FEE_AMOUNT_MAX = 5000,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS291.11.5',i)]]) || input[[paste0('priorsHRS291.11.5',i)]]==1)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 0,
                 FEE_AMOUNT_MAX = 100,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==2) & input$chargeDate >= '2022-06-27'){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 250, #FEE_AMOUNT_MIN = 100,
                 FEE_AMOUNT_MAX = 500, #FEE_AMOUNT_MAX = 200,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==3) & input$chargeDate >= '2022-06-27'){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 500, #FEE_AMOUNT_MIN = 200,
                 FEE_AMOUNT_MAX = 800, #FEE_AMOUNT_MAX = 500,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==2) & input$chargeDate < '2022-06-27'){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 100,
                 FEE_AMOUNT_MAX = 200,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-11.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291.11.5',i)]]) && input[[paste0('priorsHRS291.11.5',i)]]==3) & input$chargeDate < '2022-06-27'){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 200,
                 FEE_AMOUNT_MAX = 500,
                 LFO_TYPE = 'statutory_range')
        #this block replaced by no_fine above
        # }else if(grepl('HRS 291C-137',fineTmp$CHARGE_CODE) & !grepl('SCH', fineTmp$CHARGE_QUALIFIER)){
        #   fineTmp <- ffTableTmp1[[i]] %>%
        #     slice_head(n=1) %>%
        #     mutate(FEE_AMOUNT = 0,
        #            LFO_TYPE = 'default_fine')
      }else if(grepl('HRS 291-21.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('personRoleHRS291.21.5',i)]]) && input[[paste0('personRoleHRS291.21.5',i)]]=="owner")){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 250,
                 FEE_AMOUNT_MAX = 500,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-21.5',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('personRoleHRS291.21.5',i)]]) && input[[paste0('personRoleHRS291.21.5',i)]]=="installer")){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 500*input[[paste0("countInstallHRS291.21.5", i)]],
                 FEE_AMOUNT_MAX = 1000*input[[paste0("countInstallHRS291.21.5", i)]],
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-57',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('failDisplayHRS291.57',i)]]) && input[[paste0('failDisplayHRS291.57',i)]]==FALSE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 250,
                 FEE_AMOUNT_MAX = 500,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291-57',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('failDisplayHRS291.57',i)]]) && input[[paste0('failDisplayHRS291.57',i)]]==TRUE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 25,
                 FEE_AMOUNT_MAX = 100,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE) & (is.null(input[[paste0('priorsHRS291C.105',i)]]) || input[[paste0('priorsHRS291C.105',i)]]==1)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 500,
                 FEE_AMOUNT_MAX = 1000,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291C.105',i)]]) && input[[paste0('priorsHRS291C.105',i)]]==2)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = 750, 
                 FEE_AMOUNT_MAX = 1000,
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291C-105',fineTmp$CHARGE_CODE) & (!is.null(input[[paste0('priorsHRS291C.105',i)]]) && input[[paste0('priorsHRS291C.105',i)]]==3)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT = 1000, 
                 LFO_TYPE = 'default_fine',
                 mandatory_fine = "yes")
      }else if(grepl('HRS 291C-222\\(c\\)',fineTmp$CHARGE_CODE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT = case_when(!is.null(input[[paste0('priorsHRS291C.222',i)]]) && input[[paste0('priorsHRS291C.222',i)]]== 1 ~ 75,
                                        !is.null(input[[paste0('priorsHRS291C.222',i)]]) && input[[paste0('priorsHRS291C.222',i)]]== 2 ~ 150,
                                        !is.null(input[[paste0('priorsHRS291C.222',i)]]) && input[[paste0('priorsHRS291C.222',i)]]== 3 ~ 200),
                 LFO_TYPE = 'default_fine',
                 mandatory_fine = "yes")
      }else if(grepl('HRS 291E-61',fineTmp$CHARGE_CODE) & input$chargeDate >= '2019-07-01'){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                              (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 250,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 1000,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 750,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1500),
                 FEE_AMOUNT_MAX = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                              (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 1000,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 3000,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1500,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 3500),
                 LFO_TYPE = 'statutory_range')
      }else if(grepl('HRS 291E-61',fineTmp$CHARGE_CODE) & input$chargeDate < '2019-07-01'){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT_MIN = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                              (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 150,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 500,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 500,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 650,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1000,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1000),
                 FEE_AMOUNT_MAX = case_when((is.null(input[[paste0('priorsHRS291E.61',i)]]) || input[[paste0('priorsHRS291E.61',i)]]== 1) &&
                                              (is.null(input[[paste0('minorPassengerHRS291E.61',i)]]) || input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE) ~ 1000,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 1500,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==FALSE ~ 2500,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 1 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 1500,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 2 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 2000,
                                            !is.null(input[[paste0('priorsHRS291E.61',i)]]) && input[[paste0('priorsHRS291E.61',i)]]== 3 &&
                                              input[[paste0('minorPassengerHRS291E.61',i)]]==TRUE ~ 3000),
                 LFO_TYPE = 'statutory_range')
        #From Judge May, mandatory fine differs from default fine in JIMS, use mandatory fine according to 1DC fine schedule
      }else if(grepl("ROH 15-13\\.[35]|ROH 15-14\\.1\\(a\\)\\([1-7]\\)|ROH 15-14\\.[235]|ROH 15-15\\.1\\([ab]\\)|ROH 15-23\\.6",fineTmp$CHARGE_CODE)){
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(FEE_AMOUNT = 15, 
                 LFO_TYPE = 'default_fine')
      }else if (fineTmp$LFO_TYPE == "no_default"){#all of the remaining charges not yet entered into calculator
        fineTmp <- ffTableTmp1[[i]] %>%
          slice_head(n=1) %>%
          mutate(DETC_CODE = NA,
                 FEE_AMOUNT = NA,
                 authorizingLaw = NA,
                 LFO_TYPE = 'missing_fine')
      }else
        fineTmp
      
      #final fine
      if(fineTmp$LFO_TYPE %in% c('statutory_range','no_fine')){
        if(grepl('HRS 286-102([^.]|$)|HRS 431:10C-104|HRS 291-11\\.5|HRS 291-21\\.5|HRS 291-57|HRS 291C-105|HRS 291E-61',fineTmp$CHARGE_CODE)){
          finalFine <- input[[paste0('fineRangeSlider',i)]]
        }else
          finalFine <- fineTmp$FEE_AMOUNT
      }else if(!is.null(input[[paste0('reduceFine',i)]]) && input[[paste0('reduceFine',i)]] == TRUE){
        finalFine <- input[[paste0('fineReduceSlider',i)]]
      }else{
        finalFine <- fineTmp$FEE_AMOUNT
      }
      
      # if(!input$role %in% c('Public Defender/Defense Attorney','Prosecutor','Judge')){
      #   finalFine <- fineTmp$FEE_AMOUNT
      #   }
      
      feeTable1 <- ffTableTmp1[[i]] %>%
        filter(LFO_TYPE == 'fee')
      
      totalFineFees <- sum(finalFine,
                           feeTable1$FEE_AMOUNT,na.rm = TRUE)
      #2023/06/07: added so that NA, instead of $0, shows up on summary table at step 4 for missing fines
      # if(fineTmp$LFO_TYPE == "missing_fine"){
      #   totalFineFees <- NA
      #   }
      
      initialFineFees <- sum(fineTmp$FEE_AMOUNT,
                             feeTable1$FEE_AMOUNT,na.rm = TRUE)
      
      mandatory_fees <- sum(feeTable1$FEE_AMOUNT, na.rm = TRUE)
      
      fee_status <- if_else(nrow(feeTable1) == 0, "set_by_court", "defaults_in_JIMS")
      
      # #get charge info
      chargeInfo <- paste(fineTmp$CHARGE_CODE,' - ', fineTmp$DESCRIPTION,
                          if_else(is.na(fineTmp$CHARGE_QUALIFIER),'',paste0(' ',fineTmp$CHARGE_QUALIFIER)))
      
      data.frame(chargeInfo=chargeInfo,totalFineFees=totalFineFees,initialFineFees=initialFineFees,
                 finalFine=finalFine, InitialFine=fineTmp$FEE_AMOUNT, mandatory_fees=mandatory_fees, 
                 LFO_TYPE=fineTmp$LFO_TYPE, fee_status=fee_status)
      
    })#lapply
    })
    
    #______summaryTable----
    output$summaryTable <- renderUI({
      
      ui_charges <- ui_charges()
      
      #convert ui_charges list into a df
      ui_charges_bind <- bind_rows(ui_charges)
      #check if there are any missing fines, use to indicate * for total fines
      any_missing_fines <- ui_charges_bind %>% 
        filter(LFO_TYPE == "missing_fine") %>% 
        nrow(.)
      
      #check if there are any missing fees, use to indicate * for total fees
      any_missing_fees <- ui_charges_bind %>% 
        filter(fee_status == "set_by_court") %>% 
        nrow(.)
      
      #setup for lapply
      rgbaBG <- c("81, 81, 81, 0.3", 
                  "222, 222, 222, 0.3",
                  "256, 256, 256, 0")
      
      summaryRows <- function(rowType, i, align1, w1, w2, w3, rgbaVal){
        fluidRow(column(width = 1,''),
                 column(width = 10,
                        fluidRow(
                          column(width = w1, align = align1, #style="border:2px solid blue;border-radius: 8px;",
                                 case_when(rowType == "title" ~ HTML(paste0('<strong>Charge - Description</strong>')),
                                           rowType == "fine" ~ HTML(paste0("<span style='font-size:12px;'>",ui_charges[[i]][1],"</span>")),
                                           rowType == "total" ~ HTML("<strong>Estimated Total Amount Due: </strong>"),
                                           rowType == "total_cs1" ~ HTML("Estimated Fine Amount Due: "),
                                           rowType == "total_cs2" ~ HTML("CS Conversion Rate: "),
                                           rowType == "total_cs3" ~ HTML("<strong>Estimated Total Amount Due: </strong>"),
                                           TRUE ~ HTML("")
                                           )
                                 ),
                          column(width = w2, align = "right", #style="border:2px solid red;border-radius: 8px;", 
                                 case_when(rowType == "title" ~ HTML(paste0('<strong>Requested Fine</strong>')),
                                           rowType == "fine" ~ if_else(ui_charges[[i]][7] != "missing_fine",
                                                                      HTML(paste0('$',ui_charges[[i]][4])),
                                                                      HTML(paste0("<span style='color:red;'>calc cannot est</span>"))
                                                                      ),
                                           rowType == "note" ~ HTML(paste0("<span style='font-size:0.875em;'>(fine reduced from $", ui_charges[[i]][5], ")</span>")),
                                           rowType == "total" ~ if_else(any_missing_fines == 0,
                                                                        HTML(paste0('<strong>$',sum(sapply(ui_charges,'[[',4),na.rm = TRUE),"</strong>")),
                                                                        HTML(paste0('<strong>$',sum(sapply(ui_charges,'[[',4),na.rm = TRUE),"<span style='color:red;'>*</span></strong>"))
                                                                        ),
                                           rowType == "total_cs1" ~ if_else(any_missing_fines == 0,
                                                                            HTML(paste0('$',sum(sapply(ui_charges,'[[',4),na.rm = TRUE))),
                                                                            HTML(paste0('$',sum(sapply(ui_charges,'[[',4),na.rm = TRUE),"<span style='color:red;'>*</span>"))
                                                                            ),
                                           rowType == "total_cs2" ~ HTML(paste0('$',input$cswRate,'/hour')),
                                           rowType == "total_cs3" ~ if_else(any_missing_fines == 0,
                                                                            HTML(paste0("<strong>", trunc(sum(sapply(ui_charges,'[[',4),na.rm = TRUE)/input$cswRate)," CS hours</strong>")),
                                                                            HTML(paste0("<strong>", trunc(sum(sapply(ui_charges,'[[',4),na.rm = TRUE)/input$cswRate)," CS hours<span style='color:red;'>*</span></strong>")),
                                                                            ),
                                           TRUE ~ HTML("")
                                           )
                                 ),
                          column(width = w3, align = "right", #style="border:2px solid orange;border-radius: 8px;", 
                                 case_when(rowType == "title" ~ HTML(paste0('<strong>Mandatory Fees</strong>')),
                                           rowType == "fine" ~ case_when(#case 1: print sum of default fees; case 2: print "set by court"; case 3: print * for missing fines
                                                                         ui_charges[[i]][8]=="defaults_in_JIMS" ~ HTML(paste0('$', ui_charges[[i]][6])),
                                                                         ui_charges[[i]][8]=="set_by_court" & ui_charges[[i]][7] != "missing_fine" ~ HTML(paste0("<span style='color:red;'>set by court</span>")),
                                                                         ui_charges[[i]][8]=="set_by_court" & ui_charges[[i]][7] == "missing_fine" ~ HTML("<span style='color:red;'>calc cannot est</span>")
                                                                         ),
                                           rowType %in% c("total", "total_cs3") ~ if_else(any_missing_fees == 0,
                                                                        HTML(paste0('<strong>$',sum(sapply(ui_charges,'[[',6),na.rm = TRUE),"</strong>")),
                                                                        HTML(paste0('<strong>$',sum(sapply(ui_charges,'[[',6),na.rm = TRUE),"<span style='color:red;'>**</span></strong>"))
                                                                        ),
                                           TRUE ~ HTML("")
                                           )
                                 ),
                          style = paste0("background-color:rgba(",rgbaVal,");","width:100%;margin:auto;"))
                 )
        )
      }
        
        #format summary table -> a) header, b) dynamic list of charges and fine+fee total, c) total amount due
        div(
          fluidRow(summaryRows("title", 1, align1 = "left", 6, 3, 3, rgbaBG[3])
                   ),
          fluidRow(column(width = 10, offset = 1, 
                            hr(style = 'border-color:black;'))
                   ),
          lapply(1:length(input$charges),function(i){
            #row striping: if odd row, color background dark
            if(i %% 2 != 0){
              fluidRow(summaryRows("fine", i, align1 = "left", 6, 3, 3, rgbaBG[1]),
                       #if fine was reduced, add another row noting the reduction
                       if(ui_charges[[i]][7] != "missing_fine" & ui_charges[[i]][4]<ui_charges[[i]][5]){
                         summaryRows("note", i, align1 = "left", 5, 4, 3, rgbaBG[1])
                       }#if fine reduction
              )#fluidRow
            #row striping: if even row, color background light
            }else{
              fluidRow(summaryRows("fine", i, align1 = "left", 6, 3, 3, rgbaBG[2]),
                       #if fine was reduced, add another row noting the reduction
                       if(ui_charges[[i]][7] != "missing_fine" & ui_charges[[i]][4]<ui_charges[[i]][5]){
                         summaryRows("note", i, align1 = "left", 5, 4, 3,rgbaBG[2])
                       }#if fine reduction
              )#fluidRow
            }#else
          }),#lapply
          fluidRow(column(width = 10, offset = 1, 
                          hr(style = 'border-color:black'))
          ),
          
          if(!("Convert fines to community service" %in% input$atp_requests) | input$convertCSW==0){
            div(
              fluidRow(summaryRows("total", 1, align1 = "right", 6, 3, 3, rgbaBG[3])
              ),
            )#div
          }else if("Convert fines to community service" %in% input$atp_requests & input$convertCSW==1){
            div(
              fluidRow(summaryRows("total_cs1", 1, align1 = "right", 6, 3, 3, rgbaBG[3])
                       ),
              fluidRow(summaryRows("total_cs2", 1, align1 = "right", 6, 3, 3, rgbaBG[3])
              ),
              fluidRow(summaryRows("total_cs3", 1, align1 = "right", 6, 3, 3, rgbaBG[3])
              ),
            )#div
          },#else if
          fluidRow(
            column(width = 8, offset = 2, align = "center",
                   if(input$default_judgment == "Yes"){
                     HTML(paste0("<em>Has a Default Judgment been entered against you?</em> ", span(input$default_judgment, style = "font-weight:bold; color:red;")))
                   }else
                     HTML(paste0("<em>Has a Default Judgment been entered against you?</em> ", span(input$default_judgment, style = "font-weight:bold;")))
            )
          ),#fluidRow default judgment
          br(),
          fluidRow(column(width = 10, offset = 1,
                          if(any_missing_fines > 0){
                            HTML("<span style='color:red;'>*</span> At least one fine could not be estimated by the calculator. The total is likely an underestimate.")
                            }
                          )),
          fluidRow(column(width = 10, offset = 1,
                          if(any_missing_fees > 0){
                            HTML("<span style='color:red;'>**</span> At least one fee is either set by the court or could not be estimated by the calculator. The total is likely an underestimate.")
                            }
                          )),
          br(),
          #_________What do I do next?----
          fluidRow(column(width = 10, offset = 1,
                          strong("What do I do next?"),
                          p("If you would like to make a request to the court to reduce your fines, 
                            or convert them to community service, then please do the following:"),
                          tags$ol(tags$li("If you haven't already, complete the \"ability to pay\" form at step 2 and print your information."),
                                  tags$li("Collect any supporting documents for step 2, including original pay stubs, bills, and other documentation of your income, monthly expenses, and any public benefits you currently receive."),
                                  tags$li("Print your summary information on this page (step 4)."),
                                  tags$li("Submit your forms so the court can make an ability to pay determination and review a summary of your request. You have two options for submitting your forms:",
                                      tags$ol(type = "A",
                                              tags$li("Bring your printouts and supporting documentation to your hearing."),
                                              tags$li("Submit them with a written statement.")
                                              ),
                                      "Please see the back of your citation for more information on how to request a hearing or submit a written statement.")
                              )
                          )
                   ),
          br()
        )#div
    })
    
    #-----------------------------------#
    #ATP box on LFO and Summary pages----
    #-----------------------------------#

    #h/t: https://stackoverflow.com/questions/30871407/how-to-use-the-same-output-binding-on-different-tab-panels-in-shiny
    #h/t: https://github.com/rstudio/shiny/issues/867
    output$summaryATP1 <- output$summaryATP2 <- renderUI({
        div('Was a reduction in monetary sanction requested?',
            if(!'Reduce fines' %in% input$atp_requests){
                HTML(paste(strong('No'),br(),br()))
                }else{
                    HTML(paste(strong('Yes'),br(),br()))
                    },
            
            if(input$povertyGuidelineCheck != 0 && !is.null(input$atp_requests)){
                HTML(paste('Below 100% of the poverty guideline?',strong(if_else(yearlyIncome() <= povGuideline(),'Yes','No')),br(),povertyGuidelineEstimateTmp()))
              }else if(input$povertyGuidelineCheck == 0 && !is.null(input$atp_requests)){
                HTML('Below 100% of the poverty guideline? <strong>Not enough information reported</strong> (Return to step 2 to determine whether income is above or below poverty guideline.)<br><br>')
                },

            if(input$aliceGuidelineCheck != 0 && !is.null(input$atp_requests)){
              HTML(paste('Below 100% of the ALICE Household Survival Budget?',strong(if_else(yearlyIncome() <= aliceGuideline(),'Yes','No')),br(),aliceGuidelineEstimateTmp()))
            }else if(input$aliceGuidelineCheck == 0 && !is.null(input$atp_requests)){
              HTML('Below 100% of the ALICE Household Survival Budget? <strong>Not enough information reported</strong> (Return to step 2 to determine whether income is above or below ALICE Household Survival Budget.)<br><br>')
            },
            
            # if(length(input$benefits)!=0 && input$atpYN == 'Yes'){
            #     # p(strong('Yes -'),input$benefits[1:length(input$benefits)])
            #     #h/t: https://stackoverflow.com/questions/12962984/r-prevent-repeated-items-while-using-paste-for-vectors
            #     #h/t: https://stackoverflow.com/questions/65342140/how-to-use-html-code-to-bold-or-possibly-increase-the-font-size-for-sprintf-with
            #     HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br> %s<br><br>',paste(input$benefits[1:length(input$benefits)],collapse = ', ')))
            # }else if(length(input$benefits)==0 && !is.null(input$atpYN) && input$atpYN == 'Yes'){
            #     HTML('Receiving public assistance? <strong>None reported </strong><br><br>')
            #     },
            
            # if((input$benefitsButtonSNAP=="SNAP" | input$benefitsButtonTANF=="TANF" | input$benefitsButtonSSI=="SSI" | input$benefitsButtonGA=="GA" | input$benefitsButtonOther=="OTHER") && input$atpYN == 'Yes'){
            if((input$benefitsButtonSNAP==TRUE | input$benefitsButtonTANF==TRUE | input$benefitsButtonSSI==TRUE | input$benefitsButtonGA==TRUE | input$benefitsButtonOther==TRUE) && !is.null(input$atp_requests)){
              # p(strong('Yes -'),input$benefits[1:length(input$benefits)])
              #h/t: https://stackoverflow.com/questions/12962984/r-prevent-repeated-items-while-using-paste-for-vectors
              #h/t: https://stackoverflow.com/questions/65342140/how-to-use-html-code-to-bold-or-possibly-increase-the-font-size-for-sprintf-with
              
              # buttonsTmp <- data.frame(benefit=unlist(list(input$benefitsButtonSNAP,input$benefitsButtonTANF,input$benefitsButtonSSI,input$benefitsButtonGA,input$benefitsButtonOther))) %>%
              #   mutate(box = 1)
          
              benefitsDF <- data.frame(#benefit = c('Supplement Nutrition Assistance Program (SNAP)/Food Stamps','Temporary Assistance for Needy Families (TANF)','Supplemental Security Income','General Assistance (GA)','Other'),
                                       benefit = c('SNAP','TANF','SSI','GA','Other'),
                                       box = c(input$benefitsButtonSNAP,input$benefitsButtonTANF,input$benefitsButtonSSI,input$benefitsButtonGA,input$benefitsButtonOther),
                                       amount = c(input$benefitsAmountSNAP,input$benefitsAmountTANF,input$benefitsAmountSSI,input$benefitsAmountGA,input$benefitsAmountOther))
              
              #keep anything with a checked box, even if amount is $0; some users might not know how much $ in benefits they receive but still want to indicate they are receiving benefits
              benefitsDF1 <- benefitsDF %>%
                # left_join(buttonsTmp) %>%
                # filter(box == 1)
                filter(box==TRUE)
              
              div(
                HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br>')),
                HTML(sprintf('%s = $%d<br>',benefitsDF1$benefit,benefitsDF1$amount)),
                HTML("<br>")
              )
              # HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br> 
              #              %s = $%d<br>',benefitsDF1$benefit,benefitsDF1$amount))
              # HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br> %s<br><br>',paste(input$benefits[1:length(input$benefits)],collapse = ', ')))
              # HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br> %s<br><br>',paste(input$benefits[1:length(input$benefits)],collapse = ', ')))
              # HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br> %s<br><br>',paste(input$benefits[1:length(input$benefits)],collapse = ', ')))
              # HTML(sprintf('Receiving public assistance? <strong>Yes</strong><br> %s<br><br>',paste(input$benefits[1:length(input$benefits)],collapse = ', ')))
            }else if(!is.null(input$atp_requests)){
              HTML('Receiving public assistance? <strong>None reported </strong><br><br>')
            }
            
            #2023/10/05 - remove payment plan option per discussion with Judge May and Angela
            # 'Was a payment plan requested?',
            # if('Set up a payment plan' %in% input$atp_requests){
            #     HTML(paste(strong('Yes'),br(),br()))
            # }else{
            #     HTML(paste(strong('No'),br(),br()))
            # }
        )#div
    })

    #-----------------------------------#
    #Payment Plan box on LFO page----
    #2023/10/05 - disable payment plan option per discussion with Judge May and Angela
    #-----------------------------------#
    # total_LFO <- reactive({
    #   #see comments below in summaryCSWTotalFines
    #   res <- try(ui_charges(),silent = TRUE) 
    #   validate(
    #     need(class(res)!="try-error", message = FALSE)
    #   )
    #   ui_charges <- ui_charges()
    #   # total_fines <- sum(sapply(ui_charges,'[[',4),na.rm = TRUE)
    #   total_LFO <- sum(sapply(ui_charges,'[[',2),na.rm = TRUE)
    # })
    # 
    # output$summaryPPTotalFinesFees <- renderUI({
    #   div(HTML(paste0("$",total_LFO()))
    #   )#div
    # })
    # 
    # output$pay_plan_monthly <- renderUI({
    #   #anticipating same issue with CSW -> CSW total fees flickers with invalid input until all charges load, need to temporarily freeze to prevent invalid input
    #   #fix invalidation error when reduce fine checkbox is selected; see 3 options above
    #   res <- try(ui_charges(),silent = TRUE) 
    #   validate(
    #     need(class(res)!="try-error", message = FALSE)
    #   )
    #   # ui_charges <- ui_charges()
    #   total_LFO <- total_LFO()
    #   
    #   #email with Angela 2022/11/21, round down to nearest hour -> since CS is always >= 0, can use either floor or trunc
    #   # div(HTML(paste0(round(sum(sapply(ui_charges,'[[',4),na.rm = TRUE)/input$cswRate,digits = 1)," hours"))
    #   if(input$payplan == 0){
    #     div(HTML(paste0("---")))#div
    # 
    #   }else{
    #     div(HTML(paste0(ceiling(total_LFO/input$payplan)," months")))#div
    #   }
    #   })
        
    #-----------------------------------#
    #CSW box on LFO page----
    #-----------------------------------#
    output$csPopover1 <- renderUI({
      div(list('Was',
               actionLink(inputId = "csw291D", label = "community service"),
               # HTML('<span id="csw291D" style="color:blue;text-decoration:underline blue dotted;">community service</span>'),
               'requested?',
               tags$style(".popover{max-width:30%;}"),
               bsPopover(#session=session,
                 id="csw291D",
                 title = "Community service",
                 content = 'According to <a href= "https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291D/HRS_0291D-0009.htm" target = "_blank">HRS 291D-9(d)</a>, "Upon request of a person claiming inability to pay a monetary assessment, the court may grant an extension of the period in which the monetary assessment shall be paid or may impose community service in lieu thereof."',
                 placement = 'left',
                 trigger = c('focus'),
                 options = list(container = 'body'))),
          if('Convert fines to community service' %in% input$atp_requests){
            HTML(paste(strong('Yes')))
          }else{
            HTML(paste(strong('No')))
          }
      )#div
    })
    
    output$csPopover2 <- renderUI({
      div(list('Was',
               actionLink(inputId = "csw291Dpop2", label = "community service"),
               # HTML('<span id="csw291D" style="color:blue;text-decoration:underline blue dotted;">community service</span>'),
               'requested?',
               tags$style(".popover{max-width:30%;}"),
               bsPopover(#session=session,
                 id="csw291Dpop2",
                 title = "Community service",
                 content = 'According to <a href= "https://www.capitol.hawaii.gov/hrscurrent/Vol05_Ch0261-0319/HRS0291D/HRS_0291D-0009.htm" target = "_blank">HRS 291D-9(d)</a>, "Upon request of a person claiming inability to pay a monetary assessment, the court may grant an extension of the period in which the monetary assessment shall be paid or may impose community service in lieu thereof."',
                 placement = 'left',
                 trigger = c('focus'),
                 options = list(container = 'body'))),
          if('Convert fines to community service' %in% input$atp_requests){
            HTML(paste(strong('Yes')))
          }else{
            HTML(paste(strong('No')))
          }
      )#div
    })
    
    #h/t: https://stackoverflow.com/questions/30871407/how-to-use-the-same-output-binding-on-different-tab-panels-in-shiny
    #h/t: https://github.com/rstudio/shiny/issues/867
    output$summaryCSWTotalFines <- renderUI({
      #CSW total fees flickers with invalid input until all charges load, need to temporarily freeze to prevent invalid input
      #h/t: https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs
      # freezeReactiveValue(input,"charges")
      # req(ui_charges())
      # ui_charges <- ui_charges()
      
      #fix invalidation error when reduce fine checkbox is selected; 3 options:
      #Option A: hide all error messages -> put code at top in text/css style section
      #h/t: https://groups.google.com/g/shiny-discuss/c/FyMGa2R_Mgs
      # tags$style(type="text/css", 
      #            ".shiny-output-error { visibility: hidden; }",
      #            ".shiny-output-error:before { visibility: hidden; }"  ),
      
      #Option B: use try with validate and need
      #h/t: https://stackoverflow.com/questions/11316609/how-can-i-determine-if-try-returned-an-error-or-not
      #h/t: https://stackoverflow.com/questions/42210786/shiny-r-hide-error-message-when-no-value-in-selectizeinput
      res <- try(ui_charges(),silent = TRUE) 
      validate(
        need(class(res)!="try-error", message = FALSE)
      )
      ui_charges <- ui_charges()
      div(HTML(paste0("$",sum(sapply(ui_charges,'[[',4),na.rm = TRUE)))
          )#div

      #Option C: use default value with try
      #h/t: http://adv-r.had.co.nz/Exceptions-Debugging.html
      # ui_charges <- 0 
      # try(ui_charges <- sum(sapply(ui_charges(),'[[',4),na.rm = TRUE),silent=TRUE)
      # 
      # div(HTML(paste0("$",ui_charges)))
    })
    
    output$summaryCSWTotalHours <- renderUI({
      #CSW total fees flickers with invalid input until all charges load, need to temporarily freeze to prevent invalid input
      #fix invalidation error when reduce fine checkbox is selected; see 3 options above
      res <- try(ui_charges(),silent = TRUE) 
      validate(
        need(class(res)!="try-error", message = FALSE)
      )
      ui_charges <- ui_charges()
      #email with Angela 2022/11/21, round down to nearest hour -> since CS is always >= 0, can use either floor or trunc
      # div(HTML(paste0(round(sum(sapply(ui_charges,'[[',4),na.rm = TRUE)/input$cswRate,digits = 1)," hours"))
      div(HTML(paste0(trunc(sum(sapply(ui_charges,'[[',4),na.rm = TRUE)/input$cswRate)," hours"))
      )#div
    })
}
# Run the application----
shinyApp(ui = ui, server = server)
