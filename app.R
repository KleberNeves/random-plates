library(shiny)
library(ggplot2)
library(reshape2)
library(lubridate)
library(shinyMatrix)

prows = 8
pcols = 12
plate = matrix(nrow = prows, ncol = pcols, data = rep("", 96))
dplate = data.frame()

matopt = list(names = F, editableNames = F, extend = F)

groups = 0
ns = 0

# Define UI for application that draws a histogram
ui = fluidPage(
    tags$head(
        tags$style(
            HTML(
            ".shiny-notification {
             font-size: 1.5em;
             }
             "
            )
        )
    ),
    
    titlePanel("Distributing groups in 96- or 24-well plates"),

    tabsetPanel(id = "tabset",
        tabPanel("Setup", fluidRow(
            column(3,
                selectInput("wells", "Type of Plate:", choices = c(96, 24)),
                checkboxInput("use.border", "Use border wells?", value = F),
                br(),
                h5("Choose a scheme to distribute the groups in wells automatically, then click Setup Plate!"),
                selectInput("dist.method", "Distribution of groups in wells:",
                            choices = c("sequential", "alternating", "random")),
                actionButton("setbtn", "Setup plate!"),
                br(),
                br(),
                h5("Click to randomize the order of the groups you defined."),
                actionButton("orderbtn", "Randomize group order")
            ),
            
            column(2,
                h5("Choose a short name for each group (up to 5 characters)."),
                textInput("g1", "Group 1:", value = "TREAT"),
                textInput("g2", "Group 2:", value = "CTRL"),
                textInput("g3", "Group 3:", value = "BLANK"),
                textInput("g4", "Group 4:", value = "+CTRL"),
                textInput("g5", "Group 5:", value = "VEHIC"),
                textInput("g6", "Group 6:", value = "EXTRA")
            ),
            
            column(2,
               h5("Choose the number of replicates for each group (or 0 to exclude it)."),
               numericInput("n1", "# Reps:", min = 1, max = 5, value = 3, step = 1),
               numericInput("n2", "# Reps:", min = 1, max = 5, value = 3, step = 1),
               numericInput("n3", "# Reps:", min = 1, max = 5, value = 2, step = 1),
               numericInput("n4", "# Reps:", min = 1, max = 5, value = 1, step = 1),
               numericInput("n5", "# Reps:", min = 1, max = 5, value = 3, step = 1),
               numericInput("n6", "# Reps:", min = 1, max = 5, value = 0, step = 1)
            )
        )),
    
        tabPanel("Values", fluidRow(
            h4("Edit here to customize the distribution of wells."),
            br(),
            matrixInput("mat", value = plate, rows = matopt, cols = matopt)
        )),

        tabPanel("Image", fluidRow(
            br(),
            downloadButton("savePlate", label = "Save Plate Image"),
            h5("Go to the Values tab if you want to change the individual wells."),
            plotOutput("platePlot")
        ))
    )
)

server = function(input, output, session) {

    observeEvent(input$setbtn, {
        
        groups = c(input$g1, input$g2, input$g3, input$g4, input$g5, input$g6)
        ns = c(input$n1, input$n2, input$n3, input$n4, input$n5, input$n6)
        
        if (input$wells == 96) {
            prows = 8
            pcols = 12
        } else if (input$wells == 24) {
            prows = 4
            pcols = 6
        }
        
        # Sets up plate
        plate = matrix(nrow = prows, ncol = pcols, data = rep("", input$wells))
        
        # Sets up border
        if (!input$use.border) {
            plate[1, 1:pcols] = "PBS"
            plate[prows, 1:pcols] = "PBS"
            plate[1:prows, 1] = "PBS"
            plate[1:prows, pcols] = "PBS"
        }
        
        # Check if it fits
        if (sum(plate == "") < sum(ns)) {
            showNotification((paste0("Número necessário de poços (", sum(ns), ") é menor do que o disponível na placa (",sum(plate == ""),").")))
        }
        
        # Sequential method
        if (input$dist.method == "sequential") {
            p = 1
            pseq = rep(groups, ns)
            for (i in 1:prows) {
                for (j in 1:pcols) {
                    if (plate[i,j] == "") {
                        plate[i,j] = pseq[p]
                        p = p + 1
                    }
                } 
            }
        }
        
        # Alternating method
        if (input$dist.method == "alternating") {
            qs = ns
            pseq = c()
            while (any(qs > 0)) {
                pseq = c(pseq, groups[qs > 0])
                qs = qs - 1
            }
            p = 1
            for (i in 1:prows) {
                for (j in 1:pcols) {
                    if (plate[i,j] == "") {
                        plate[i,j] = pseq[p]
                        p = p + 1
                    }
                } 
            }
        }
        
        # Random method
        if (input$dist.method == "random") {
            pseq = rep(groups, ns)
            pseq = sample(pseq, length(pseq), replace = F)
            p = 1
            for (i in 1:prows) {
                for (j in 1:pcols) {
                    if (plate[i,j] == "") {
                        plate[i,j] = pseq[p]
                        p = p + 1
                    }
                } 
            }
        }
        
        plate[is.na(plate)] = "PBS"
        
        updateMatrixInput(session, "mat", value = plate)
        updateTabsetPanel(session, "tabset", selected = "Image")
        
    })
    
    observeEvent(input$orderbtn, {
        
        groups = c(input$g1, input$g2, input$g3, input$g4, input$g5, input$g6)
        ns = c(input$n1, input$n2, input$n3, input$n4, input$n5, input$n6)
        
        neworder = sample(1:6, 6)
        groups = groups[neworder]
        ns = ns[neworder]
        
        lapply(1:6, function (i) {
            g = paste0("g",i)
            n = paste0("n",i)
            updateTextInput(session, g, value = groups[i])
            updateTextInput(session, n, value = ns[i])
        })
        
    })
    
    make.plot = eventReactive(c(input$setbtn, input$mat), {
        
        dplate = as.data.frame(input$mat, stringsAsFactors = F)
        colnames(dplate) = 1:ncol(dplate)
        dplate$Y = 1:nrow(dplate)
        dplate = melt(dplate, id.vars = "Y")
        dplate$X = as.numeric(dplate$variable)
        dplate$WELL = (dplate$Y - 1) * pcols + dplate$X
        dplate$LABEL = paste0(dplate$WELL, "\n", dplate$value)
        
        p = ggplot(dplate, aes(x = X, y = Y, label = LABEL, fill = value)) +
            geom_label(color = "white") +
            scale_x_continuous(breaks = 1:pcols, position = "top") +
            scale_y_reverse(breaks = 1:prows) + labs(x = "", y = "") +
            theme_linedraw() + theme(legend.position = "none")
        
        p
    })
    
    make.plot2 = function() {
        
        dplate = as.data.frame(input$mat, stringsAsFactors = F)
        colnames(dplate) = 1:ncol(dplate)
        dplate$Y = 1:nrow(dplate)
        dplate = melt(dplate, id.vars = "Y")
        dplate$X = as.numeric(dplate$variable)
        dplate$WELL = (dplate$Y - 1) * pcols + dplate$X
        dplate$LABEL = paste0(dplate$WELL, "\n", dplate$value)
        
        p = ggplot(dplate, aes(x = X, y = Y, label = LABEL, fill = value)) +
            geom_label(color = "white") +
            scale_x_continuous(breaks = 1:pcols, position = "top") +
            scale_y_reverse(breaks = 1:prows) + labs(x = "", y = "") +
            theme_linedraw() + theme(legend.position = "none")
        
        p
    }
    
    output$platePlot = renderPlot({
        make.plot()
    })
    
    output$savePlate = downloadHandler(
        filename = function() {
            paste0("plate ",
                  day(now()),"-",month(now()),"-",year(now()),
                  " ", hour(now()),"h",minute(now()),".png")
        },
        content = function(file) {
            if (input$wells == 96) {
                p = ggsave(file, plot = make.plot2(), device = "png",
                      width = pcols * 0.85, height = prows * 0.7, dpi = 150)
            } else {
                p = ggsave(file, plot = make.plot2(), device = "png",
                      width = pcols * 0.75, height = prows * 0.65, dpi = 150)
            }
            p
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
