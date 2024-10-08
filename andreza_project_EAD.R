install.packages('rsconnect')
install.packages("shiny")
install.packages("visNetwork")

library(shiny)
library(visNetwork)
library(rsconnect)

required_packages <- c("shiny", "visNetwork", "rsconnect")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

rsconnect::setAccountInfo(name='studiesshiny',
                          token='BAF4F3068C3E6B84B99CD1F0F4A7FC35',
                          secret='w8J1UIPj1PLnFgYGK4Qf++wzmDsmwQrmq7zqo8fv')



rsconnect::deployApp('C:/Users/andre/EAD/andreza_project_EAD.R')


ui <- fluidPage(
  titlePanel("Mapa Interativo de R e Shiny"),
  
  # Novo passo a passo
  h4("Passo a Passo para Manipular os N�s:"),
  tags$ol(
    tags$li("Clique em 'Adicionar N�' para criar um novo n�, depois selecione o n� que voc� criou e ajuste o que desejar em editar."),
    tags$li("Lembre-se de sempre, antes de tudo, selecionar o n� que voc� vai manusear."),
    tags$li("Selecione o nome do n� que deseja editar e ajuste a descri��o ou o nome do n�."),
    tags$li("Se desejar conectar um n� com outro, selecione os dois n�s e depois clique em 'Conectar N�s'."),
    tags$li("A descri��o aparece abaixo do mapa.'.")
  ),
  
  # Organiza��o lado a lado com espa�amento
  fluidRow(
    column(6, 
           h3("Gerenciamento de N�s"),
           actionButton("add_node", label = "Adicionar N�", icon = icon("plus")),
           br(), br(),
           selectInput("select_node_to_delete", "Selecione o N� para Excluir", choices = NULL),
           actionButton("delete_node", label = "Excluir N�", icon = icon("minus")),
           br(), br(),
           selectInput("select_node_1", "Selecione o 1� N� para Conectar", choices = NULL),
           selectInput("select_node_2", "Selecione o 2� N� para Conectar", choices = NULL),
           actionButton("add_edge", label = "Conectar N�s", icon = icon("arrows-alt-h"))
    ),
    column(6,
           h3("Editar N� Selecionado"),
           selectInput("select_node", "Selecione o N� para Editar", choices = NULL),
           textInput("node_name", "Editar Nome do N�", ""),
           textAreaInput("node_description", "Editar Descri��o do N�", ""),
           actionButton("save_changes", label = "Salvar Altera��es", icon = icon("save"))
    )
  ),
  
  # Rede interativa
  visNetworkOutput("net", height = "600px"),
  verbatimTextOutput("description")
)

server <- function(input, output, session) {
  # N�s iniciais com ramifica��es j� prontas
  nodes <- data.frame(
    id = 1:8,
    label = c("R", "Instala��o de Bibliotecas", "Utiliza��o do RStudio", "Como a Linguagem Funciona", 
              "Shiny", "Arquitetura do Shiny", "Utilidade do Shiny", "Como o Shiny Funciona"),
    group = c("R", "R", "R", "R", "Shiny", "Shiny", "Shiny", "Shiny"),
    stringsAsFactors = FALSE
  )
  
  # Arestas conectando as ramifica��es ao "R"
  edges <- data.frame(
    from = c(1, 1, 1, 1, 5, 5, 5),
    to = c(2, 3, 4, 5, 6, 7, 8)
  )
  
  # Descri��es
  descriptions <- reactiveValues(
    data = list(
      "R" = "R � uma linguagem poderosa para estat�stica e ci�ncia de dados. Voc� pode fazer desde c�lculos b�sicos at� modelagem complexa de dados.",
      "Instala��o de Bibliotecas" = "Instalar bibliotecas no R � f�cil! Voc� s� precisa do comando install.packages('nome_da_biblioteca').",
      "Utiliza��o do RStudio" = "RStudio � o ambiente onde voc� pode escrever seus scripts, rodar c�digos e visualizar os resultados de maneira organizada.",
      "Como a Linguagem Funciona" = "R � uma linguagem interpretada, permitindo rodar c�digo linha por linha, ideal para an�lises interativas.",
      "Shiny" = "Shiny � um pacote para criar aplicativos web interativos com R.",
      "Arquitetura do Shiny" = "Shiny segue o modelo reativo. O front-end e o back-end conversam dinamicamente, atualizando resultados automaticamente.",
      "Utilidade do Shiny" = "Shiny � extremamente �til para criar dashboards interativos, especialmente para an�lise de dados.",
      "Como o Shiny Funciona" = "Shiny conecta as entradas dos usu�rios com sa�das reativas, alterando conforme a intera��o do usu�rio."
    )
  )
  
  current_nodes <- reactiveVal(nodes)  # N�s exibidos no grafo
  current_edges <- reactiveVal(edges)  # Arestas exibidas no grafo
  selected_node <- reactiveVal(NULL)  # N� atualmente selecionado
  next_node_id <- reactiveVal(9)  # ID para novos n�s
  
  # Atualiza a lista de n�s no selectInput
  update_node_choices <- function() {
    node_labels <- current_nodes()$label
    updateSelectInput(session, "select_node", choices = node_labels)
    updateSelectInput(session, "select_node_to_delete", choices = node_labels)
    updateSelectInput(session, "select_node_1", choices = node_labels)
    updateSelectInput(session, "select_node_2", choices = node_labels)
  }
  
  output$net <- renderVisNetwork({
    visNetwork(current_nodes(), current_edges(), width = "100%") %>%
      visEdges(arrows = "to") %>%
      visOptions(manipulation = list(enabled = FALSE), nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE, dragNodes = TRUE, zoomView = TRUE, hover = TRUE) %>%
      visEvents(click = "function(nodes) {
        Shiny.onInputChange('selected_node', nodes.nodes);
      }")
  })
  
  output$description <- renderText({
    node_id <- selected_node()
    if (!is.null(node_id) && length(node_id) > 0 && node_id <= length(current_nodes()$label)) {
      node_label <- current_nodes()$label[node_id]
      if (!is.null(descriptions$data[[node_label]])) {
        return(descriptions$data[[node_label]])
      } else {
        return("Este n� ainda n�o tem descri��o. Adicione uma!")
      }
    }
    return("Clique em um n� para ver ou adicionar uma descri��o.")
  })
  
  # Fun��o para atualizar campos de edi��o com base no n� selecionado
  update_edit_fields <- function(node_label) {
    updateTextInput(session, "node_name", value = node_label)
    if (is.null(descriptions$data[[node_label]])) {
      updateTextAreaInput(session, "node_description", value = "Adicione uma descri��o para este n�.")
    } else {
      updateTextAreaInput(session, "node_description", value = descriptions$data[[node_label]])
    }
  }
  
  # Sele��o de n�s via grafo
  observeEvent(input$selected_node, {
    node_id <- as.numeric(input$selected_node)
    if (!is.null(node_id) && node_id > 0) {  # Verifica se o n� selecionado � v�lido
      selected_node(node_id)
      node_label <- current_nodes()$label[node_id]
      
      # Atualiza a sele��o do n� na caixa de sele��o para editar
      updateSelectInput(session, "select_node", selected = node_label)
      
      # Atualiza os campos de edi��o
      update_edit_fields(node_label)
    }
  })
  
  # Sele��o de n�s via caixa de sele��o "Selecionar N� para Editar"
  observeEvent(input$select_node, {
    node_label <- input$select_node
    if (!is.null(node_label) && node_label != "") {
      selected_node(which(current_nodes()$label == node_label))
      
      # Atualiza os campos de edi��o
      update_edit_fields(node_label)
    }
  })
  
  # Salva as altera��es feitas no nome e descri��o do n�
  observeEvent(input$save_changes, {
    node_label <- input$select_node
    if (!is.null(node_label)) {
      updated_nodes <- current_nodes()
      updated_nodes$label[updated_nodes$label == node_label] <- input$node_name
      current_nodes(updated_nodes)
      
      descriptions$data[[input$node_name]] <- input$node_description
      
      update_node_choices()
    }
  })
  
  # Adiciona um novo n� ao clicar no bot�o
  observeEvent(input$add_node, {
    new_id <- next_node_id()
    new_label <- paste("N�", new_id)
    updated_nodes <- rbind(current_nodes(), data.frame(id = new_id, label = new_label, group = "Novo", stringsAsFactors = FALSE))
    current_nodes(updated_nodes)
    next_node_id(new_id + 1)
    
    descriptions$data[[new_label]] <- paste("Descri��o do", new_label)
    
    update_node_choices()
  })
  
  # Conecta dois n�s ao clicar no bot�o de conectar n�s
  observeEvent(input$add_edge, {
    node_1 <- which(current_nodes()$label == input$select_node_1)
    node_2 <- which(current_nodes()$label == input$select_node_2)
    
    if (!is.null(node_1) && !is.null(node_2) && length(node_1) > 0 && length(node_2) > 0 && node_1 != node_2) {
      updated_edges <- current_edges()
      updated_edges <- rbind(updated_edges, data.frame(from = node_1, to = node_2))
      current_edges(updated_edges)
      showNotification("N�s conectados com sucesso.")
    } else {
      showNotification("Selecione dois n�s diferentes para conectar.", type = "error")
    }
  })
  
  # Excluir um n� selecionado
  observeEvent(input$delete_node, {
    node_label <- input$select_node_to_delete
    if (!is.null(node_label)) {
      updated_nodes <- current_nodes()
      updated_nodes <- updated_nodes[updated_nodes$label != node_label, ]
      current_nodes(updated_nodes)
      
      descriptions$data[[node_label]] <- NULL
      
      update_node_choices()
    }
  })
  
  # Atualizar a lista de n�s ao iniciar o app
  observe({
    update_node_choices()
  })
}

shinyApp(ui, server)
