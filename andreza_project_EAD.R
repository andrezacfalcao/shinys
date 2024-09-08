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
  h4("Passo a Passo para Manipular os Nós:"),
  tags$ol(
    tags$li("Clique em 'Adicionar Nó' para criar um novo nó, depois selecione o nó que você criou e ajuste o que desejar em editar."),
    tags$li("Lembre-se de sempre, antes de tudo, selecionar o nó que você vai manusear."),
    tags$li("Selecione o nome do nó que deseja editar e ajuste a descrição ou o nome do nó."),
    tags$li("Se desejar conectar um nó com outro, selecione os dois nós e depois clique em 'Conectar Nós'."),
    tags$li("A descrição aparece abaixo do mapa.'.")
  ),
  
  # Organização lado a lado com espaçamento
  fluidRow(
    column(6, 
           h3("Gerenciamento de Nós"),
           actionButton("add_node", label = "Adicionar Nó", icon = icon("plus")),
           br(), br(),
           selectInput("select_node_to_delete", "Selecione o Nó para Excluir", choices = NULL),
           actionButton("delete_node", label = "Excluir Nó", icon = icon("minus")),
           br(), br(),
           selectInput("select_node_1", "Selecione o 1º Nó para Conectar", choices = NULL),
           selectInput("select_node_2", "Selecione o 2º Nó para Conectar", choices = NULL),
           actionButton("add_edge", label = "Conectar Nós", icon = icon("arrows-alt-h"))
    ),
    column(6,
           h3("Editar Nó Selecionado"),
           selectInput("select_node", "Selecione o Nó para Editar", choices = NULL),
           textInput("node_name", "Editar Nome do Nó", ""),
           textAreaInput("node_description", "Editar Descrição do Nó", ""),
           actionButton("save_changes", label = "Salvar Alterações", icon = icon("save"))
    )
  ),
  
  # Rede interativa
  visNetworkOutput("net", height = "600px"),
  verbatimTextOutput("description")
)

server <- function(input, output, session) {
  # Nós iniciais com ramificações já prontas
  nodes <- data.frame(
    id = 1:8,
    label = c("R", "Instalação de Bibliotecas", "Utilização do RStudio", "Como a Linguagem Funciona", 
              "Shiny", "Arquitetura do Shiny", "Utilidade do Shiny", "Como o Shiny Funciona"),
    group = c("R", "R", "R", "R", "Shiny", "Shiny", "Shiny", "Shiny"),
    stringsAsFactors = FALSE
  )
  
  # Arestas conectando as ramificações ao "R"
  edges <- data.frame(
    from = c(1, 1, 1, 1, 5, 5, 5),
    to = c(2, 3, 4, 5, 6, 7, 8)
  )
  
  # Descrições
  descriptions <- reactiveValues(
    data = list(
      "R" = "R é uma linguagem poderosa para estatística e ciência de dados. Você pode fazer desde cálculos básicos até modelagem complexa de dados.",
      "Instalação de Bibliotecas" = "Instalar bibliotecas no R é fácil! Você só precisa do comando install.packages('nome_da_biblioteca').",
      "Utilização do RStudio" = "RStudio é o ambiente onde você pode escrever seus scripts, rodar códigos e visualizar os resultados de maneira organizada.",
      "Como a Linguagem Funciona" = "R é uma linguagem interpretada, permitindo rodar código linha por linha, ideal para análises interativas.",
      "Shiny" = "Shiny é um pacote para criar aplicativos web interativos com R.",
      "Arquitetura do Shiny" = "Shiny segue o modelo reativo. O front-end e o back-end conversam dinamicamente, atualizando resultados automaticamente.",
      "Utilidade do Shiny" = "Shiny é extremamente útil para criar dashboards interativos, especialmente para análise de dados.",
      "Como o Shiny Funciona" = "Shiny conecta as entradas dos usuários com saídas reativas, alterando conforme a interação do usuário."
    )
  )
  
  current_nodes <- reactiveVal(nodes)  # Nós exibidos no grafo
  current_edges <- reactiveVal(edges)  # Arestas exibidas no grafo
  selected_node <- reactiveVal(NULL)  # Nó atualmente selecionado
  next_node_id <- reactiveVal(9)  # ID para novos nós
  
  # Atualiza a lista de nós no selectInput
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
        return("Este nó ainda não tem descrição. Adicione uma!")
      }
    }
    return("Clique em um nó para ver ou adicionar uma descrição.")
  })
  
  # Função para atualizar campos de edição com base no nó selecionado
  update_edit_fields <- function(node_label) {
    updateTextInput(session, "node_name", value = node_label)
    if (is.null(descriptions$data[[node_label]])) {
      updateTextAreaInput(session, "node_description", value = "Adicione uma descrição para este nó.")
    } else {
      updateTextAreaInput(session, "node_description", value = descriptions$data[[node_label]])
    }
  }
  
  # Seleção de nós via grafo
  observeEvent(input$selected_node, {
    node_id <- as.numeric(input$selected_node)
    if (!is.null(node_id) && node_id > 0) {  # Verifica se o nó selecionado é válido
      selected_node(node_id)
      node_label <- current_nodes()$label[node_id]
      
      # Atualiza a seleção do nó na caixa de seleção para editar
      updateSelectInput(session, "select_node", selected = node_label)
      
      # Atualiza os campos de edição
      update_edit_fields(node_label)
    }
  })
  
  # Seleção de nós via caixa de seleção "Selecionar Nó para Editar"
  observeEvent(input$select_node, {
    node_label <- input$select_node
    if (!is.null(node_label) && node_label != "") {
      selected_node(which(current_nodes()$label == node_label))
      
      # Atualiza os campos de edição
      update_edit_fields(node_label)
    }
  })
  
  # Salva as alterações feitas no nome e descrição do nó
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
  
  # Adiciona um novo nó ao clicar no botão
  observeEvent(input$add_node, {
    new_id <- next_node_id()
    new_label <- paste("Nó", new_id)
    updated_nodes <- rbind(current_nodes(), data.frame(id = new_id, label = new_label, group = "Novo", stringsAsFactors = FALSE))
    current_nodes(updated_nodes)
    next_node_id(new_id + 1)
    
    descriptions$data[[new_label]] <- paste("Descrição do", new_label)
    
    update_node_choices()
  })
  
  # Conecta dois nós ao clicar no botão de conectar nós
  observeEvent(input$add_edge, {
    node_1 <- which(current_nodes()$label == input$select_node_1)
    node_2 <- which(current_nodes()$label == input$select_node_2)
    
    if (!is.null(node_1) && !is.null(node_2) && length(node_1) > 0 && length(node_2) > 0 && node_1 != node_2) {
      updated_edges <- current_edges()
      updated_edges <- rbind(updated_edges, data.frame(from = node_1, to = node_2))
      current_edges(updated_edges)
      showNotification("Nós conectados com sucesso.")
    } else {
      showNotification("Selecione dois nós diferentes para conectar.", type = "error")
    }
  })
  
  # Excluir um nó selecionado
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
  
  # Atualizar a lista de nós ao iniciar o app
  observe({
    update_node_choices()
  })
}

shinyApp(ui, server)
