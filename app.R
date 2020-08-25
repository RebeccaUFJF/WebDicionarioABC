library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(stringr)

databanco = read_excel("dicionarioABC.xlsx")

# colocando funcoes de pseudo-palavras alternativas para aparecer no banco de dados completos

troca_vogalA <- function(s) {
  s %>% 
    str_replace("a", "e") %>% 
    str_replace("á", "e") %>% 
    str_replace("à" ,"e") %>% 
    str_replace("ss", "ç") %>% 
    str_replace("ç", "ss") %>% 
    str_replace("x", "cs") %>% 
    str_replace("b" , "p") %>% 
    str_replace("c", "s")
}

trocaB_P_I_E= function(s) {
  s %>% 
    str_replace("b" , "p") %>%
    str_replace("p", "b") %>% 
    str_replace("i" , "e") %>% 
    str_replace ("s" , "z") %>% 
    str_replace("z", "s")%>% 
    str_replace("g" , "j")
}

troca_vogalE = function(s) {
  s %>% 
    str_replace("e", "i") %>% 
    str_replace("é", "i") %>% 
    str_replace("ê", "i") %>% 
    str_replace("g" , "j") %>% 
    str_replace("j", "g") 
}

troca_vogalO = function(s) {
  s %>% 
    str_replace("o", "u") %>% 
    str_replace("ó", "u") %>% 
    str_replace("õ", "u") %>% 
    str_replace("g" , "j") %>% 
    str_replace("j", "g") %>% 
    str_replace("b" , "p") %>%
    str_replace("p", "b") 
} 

trocaP_B_U_O =function(s) {
  s %>% 
    str_replace("p" , "b") %>% 
    str_replace("b" , "p") %>%
    str_replace ("u" , "o") %>% 
    str_replace ("o", "u") %>% 
    str_replace("x", "ch") %>% 
    str_replace("c" , "ch")
}


databanco["Pseudo1"] = troca_vogalA(databanco$Palavra)
databanco["Pseudo2"] = trocaB_P_I_E(databanco$Palavra)
databanco["Pseudo3"] = troca_vogalE(databanco$Palavra)
databanco["Pseudo4"] = trocaP_B_U_O(databanco$Palavra)
databanco["Pseudo5"] = troca_vogalO(databanco$Palavra)


ui = dashboardPage(
dashboardHeader(title = span (img (src = "logo.png", largura = 150,"Aplicativo Web: Dicionário ABC" )), titleWidth = 700),

dashboardSidebar(
  sidebarMenu(
    menuItem("INÍCIO", tabName = "inicio", icon = icon("home")),
    menuItem("DADOS", tabName = "dados", icon = icon("table")),
    menuItem("CLASSIFICAÇÃO", tabName = "class", icon = icon("list-alt")),
    menuItem("DOWNLOAD", tabName = "down", icon = icon("download")))

),

dashboardBody( align="center",
  
  
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #000000;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #000000;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #000000;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #000000;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ffffff;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #ff8400;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #fff41c;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #fff41c;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                '))), 
  
  tabItems(
    tabItem(tabName = "inicio", 
            fluidRow(
              tabBox( 
        id = "tabset1", width = 12, 
        tabPanel("Ideia",  box(align="center", width = 12, background = "yellow", 
                              tags$p("Uma forma de avaliar a capacidade de leitura de uma criança é verificar a capacidade de decodificação, ou seja, a capacidade de uma criança ler uma palavra qualquer. Para isso, costuma-se utilizar testes com palavras e com pseudo-palavras, ou seja, palavras que não existem na língua portuguesa, mas que são palavras “corretas”."),
                              tags$br(),
                              tags$p("O aplicativo Web dicionário ABC foi desenvolvido para o avaliador do CAEd selecionar palavras com base em algumas características e gerar um arquivo com pseudo-palavras, para que um futuro teste possa ser aplicado afim de avaliar a aprendizagem de uma criança."),
                              tags$br(),
                              tags$p("Navegue pelas abas seguintes para entender o funcionamento deste aplicativo."))),
        tabPanel("Como funciona o app?", 
                 box(align="center", width = 12, background = "yellow", 
                     tags$p("Para um melhor aproveitamento do App Web Dicionário ABC, siga o passo a passo a seguir:"),
                     tags$br(),
                     tags$p("Na aba “DADOS” é possível visualizar todo o banco de dados do dicionário ABC, e possibilita ao usuário pesquisar uma palavra específica e identificar sua respectiva classificação."),
                     tags$br(),
                     tags$p("Na aba “CLASSIFICAÇÃO” é necessário que o usuário marque as classificações que deseja, sendo para a Canonicidade : “Canônica” e “Não Canônica” e sua Tonicidade: “Oxítona”, “Paroxítona” e “Proparoxítona”. "),
                     tags$br(),
                     tags$p("Feita as Classificações é possível navegar na aba “DOWNLOAD”, onde possibilita ao usuário apertar um botão de aleatoriedade, que gera uma lista de 30 palavras até que se sinta satisfeito. Logo abaixo, tem-se uma opção de escolha de quantas pseudo-palavras que o sistema gerará, em seguida o botão download para baixar o arquivo em seu computador. "),
                     tags$br(),
                     
                     tags$p("Espero que você usuário, possa aproveitar bastante o app!"),
                     tags$br(),
                     tags$p("Em caso de dúvidas ou erro de servidor, entre em contato através do e-mail: rebecca.ufjf@gmail.com")
                     
                 )), 
        tabPanel("O que é Canonicidade?", 
                 box(align="center",width = 12, background = "yellow", 
                     tags$p("Sílaba canônica é a sílaba constituída por uma consoante (C) e por uma vogal (V) - nesta ordem. Ela é também conhecida como sílaba CV e ocorre, por exemplo, nas duas sílabas da palavra mato (ma-to)."),
                                   tags$br(),
                                   tags$p("Estudos apontam a sílaba CV como sendo a mais frequente de todas as estruturas silábicas da língua portuguesa, razão pela qual ela recebe o nome de canônica. Além da sílaba CV, vale notar que a língua portuguesa apresenta outras estruturas ou padrões silábicos não canônicos, tais como: V (a-bacate), VC (es-ca-da), CVC (por-ta), CCV (pro-va)."),
                                   tags$br(),
                                   tags$p("Na alfabetização, o desenvolvimento da consciência fonológica e, especificamente, da consciência silábica, deve envolver o trabalho com sílaba canônica CV e também com sílabas não canônicas em diferentes posições na palavra (no início, no meio ou no final). O confronto entre palavras que apresentam sílabas canônicas e não canônicas, compostas por fonemas semelhantes (por exemplo, pato, prato, parto), pode ser interessante para o aprendiz perceber que tais sílabas são diferentes, de modo que devem ser escritas de modo diferente, especialmente porque veiculam significados distintos.")
                     )),
        tabPanel("O que é Tonicidade?", 
                 box(align="center",width = 12, background = "yellow", 
                     tags$p("Tonicidade diz respeito à sílaba que apresenta maior proeminência em uma palavra. Essa sílaba é chamada de tônica ou acentuada. Ela recebe o acento principal ou primário na palavra."),
                     tags$br(),
                     tags$p("Na língua portuguesa, as palavras que são monossílabos (palavras de uma só sílaba) átonos, como as preposições de e em, não apresentam sílaba tônica.
           No entanto, as demais palavras – monossílabos tônicos (como pé e chão), dissílabas, trissílabas e polissílabas – apresentam sílaba tônica.
           Na nossa língua, a sílaba tônica pode ocorrer em três posições da palavra, que será, assim, classificada como: oxítona, quando a sílaba tônica é a última sílaba da palavra (café); paroxítona, quando a sílaba tônica é a penúltima (escola); proparoxítona, quando a sílaba tônica é a antepenúltima (sábado). No português, palavras paroxítonas são as mais frequentes"),
                     tags$br(),
                     tags$p("Saber sobre a tonicidade é importante, ainda, para o alfabetizador e para o professor de Língua Portuguesa, porque as regras de acentuação gráfica, a serem conhecidas pelos aprendizes, relacionam-se à classificação das palavras quanto à posição em que o acento recai na sílaba. Por exemplo, há uma regra segundo a qual todas as palavras proparoxítonas (óculos, único, pêssego etc.) devem ser acentuadas graficamente.")
                 )),
        tabPanel("Referências", box(align="center",width = 12, background = "yellow", 
                                    tags$p("Foi usada algumas refêrencias para o desenvolvimento desse aplicativo!"),
                                    tags$p("O que é canonicidade?"),
                                    tags$a(href="http://www.ceale.fae.ufmg.br/app/webroot/glossarioceale/verbetes/silaba-canonica", "Click", target="_blank"),
                                    tags$p("O que é tonicidade?"),
                                    tags$a(href="http://www.ceale.fae.ufmg.br/app/webroot/glossarioceale/verbetes/tonicidade", "Click", target="_blank"),
                                    tags$p("Origem do banco de dados"),
                                    tags$a(href="http://www.portaldalinguaportuguesa.org/index.php?action=fonetica&act=list&region=lbx", "Click", target="_blank"),
                                    tags$p("Fundação CAEd"),
                                    tags$a(href="http://fundacaocaed.org.br/#!/pagina-inicial", "Click", target="_blank")
                                    
        )
        )
        
      ))),
    
    tabItem(tabName = "dados", DT::dataTableOutput("mytable3")),
    tabItem(tabName = "class", 
            fluidRow(box( title = "Classificação", width = 4 , solidHeader = TRUE, status = "warning",
                          tags$strong("Escolha a melhor classificação para a lista de palavras que serão selecionadas : "),
                          tags$br(),
                          tags$br(),
                          
                          awesomeCheckboxGroup(
                            inputId = "IdCanon",
                            label = "Canonicidade",
                            choices = c("Canônica", "Não Canônica"),
                            selected = "Canônica",
                            inline = FALSE,
                            status = "warning"
                          ),
                          
                          tags$br(),
                          awesomeCheckboxGroup(
                            inputId = "IdTonin",
                            label = "Tonicidade",
                            choices = c("Oxítona","Paroxítona","Proparoxítona"),
                            selected = "Oxítona",
                            inline = FALSE,
                            status = "warning"
                          )),
                     
                     box (title = "Banco de dados filtrado", width = 8, solidHeader = TRUE, status = "warning",
                          dataTableOutput("filtrocheck"),
                          
                          tags$br(),
                          tags$br(),
                          
                     ))),
    
    tabItem(tabName = "down",
fluidRow( column(12,align="center", box(title ="Lista aleatória de 30 palavras",width = 10, solidHeader = TRUE, status = "warning",
      textOutput("Lista"),
      tags$br(),
      column(12,align="center", actionButton("IdGerar", "Botão de aleatorização", icon = icon("spinner")))
      ))),

fluidRow(column(12,align="center", box(title="Download: Pseudo-palavras",width = 10, solidHeader = TRUE, status = "warning",
                                       box(width = 5 , solidHeader = TRUE, status = "warning",
                                           tags$strong("Escolha a quantidade de pseudo-palavras que será gerada:"),
                                           tags$br(),
                                           tags$br(),
                                           
                                           awesomeRadio(
                                             inputId = "PseudoEsc",
                                             label = "Quantidade de pseudos por palavra:", 
                                             choices = c("2","3", "4", "5"),
                                             selected = "2",
                                             inline = FALSE, 
                                             status = "warning"
                                           )),downloadButton("downloadData", "Download")))
)))))





server = function(input,output) {
  
  
  datatonincanon = reactiveValues(data= NULL)

  dataa = reactiveValues(data=NULL)
  
  datapseudo = reactiveValues(data=NULL)
  
  datapseudodownload =reactiveValues(data=NULL)
  

  output$mytable3 <- DT::renderDataTable({
    DT::datatable(databanco[,c(-4,-5,-8,-9,-10,-11,-12,-13)],options = list(searching=TRUE),rownames= FALSE)
  })
 
  
  
  
  output$filtrocheck = DT::renderDataTable({
    datatonincanon$data = filter(databanco, (databanco$Tonicidade %in% (input$IdTonin) & databanco$Canonicidade %in% (input$IdCanon)))
    DT::datatable(datatonincanon$data[,c(-4,-5,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18)],options = list(searching=TRUE),rownames= FALSE)
    })
  
  observeEvent (input$IdGerar, {
    output$Lista = renderPrint({
      printf= function(...){
                   invisible(cat(sprintf(...)))
      }
      
      req(datatonincanon$data)
      dataa = datatonincanon$data
      datapseudo = list(sample(dataa$Palavra, 30))
      printf(as.character(datapseudo))
      })}
    )
  
  #datapseudodownload  = as.data.frame(datapseudo)
  # o problema ocorreu aqui, pois uma variável reativa nao pode ser atribuida em um data frame.
  

   output$downloadData <- downloadHandler(
     filename = "pseudopalavras.csv",
    content = function(file) {
      write.csv(datapseudodownload, file)
    }
  )

   # colocando funcoes que criam pseudo-palavras no servidor
   # colocando funcao observeEvent que define quantidade de palavras que o sistema irá gerar
   
   #troca1 <- function(s) {
   # s %>% 
   #    str_replace("a", "e") %>% 
   #     str_replace("á", "e") %>% 
   #     str_replace("à" ,"e") %>% 
   #     str_replace("ss", "ç") %>% 
   #     str_replace("ç", "ss") %>% 
   #     str_replace("x", "cs")
   # }
   
   # troca2= function(s) {
   #   s %>% 
   #     str_replace("b" , "p") %>%
   #     str_replace("p", "b") %>% 
   #     str_replace("i" , "e") %>% 
   #     str_replace ("s" , "z") %>% 
   #     str_replace("z", "s")
   # }
   
   #troca3 = function(s) {
   #  s %>% 
   #    str_replace("e", "i") %>% 
   #    str_replace("é", "i") %>% 
   #    str_replace("ê", "i") %>% 
   #    str_replace("g" , "j") %>% 
   #    str_replace("j", "g") 
   #}
   
   #troca4 = function(s) {
   #  s %>% 
   #    str_replace("o", "u") %>% 
   #    str_replace("ó", "u") %>% 
   #    str_replace("õ", "u") %>% 
   #    str_replace("g" , "j") %>% 
   #    str_replace("j", "g") %>% 
   #    str_replace("b" , "p") %>%
   #    str_replace("p", "b")
   #}
   
   #troca5 =function(s) {
   #  s %>% 
   #    str_replace("p" , "b") %>% 
   #    str_replace("b" , "p") %>%
   #    str_replace ("u" , "o") %>% 
   #   str_replace ("o", "u") %>% 
   #    str_replace("x", "ch") %>% 
   #    str_replace("c" , "ch") %>% 
   #    str_replace("g" , "j") %>% 
   #    str_replace("j", "g")
   #}
   
   # observeEvent(input$PseudoEsc , {
   #  datapseudodownload$data = as.data.frame(datapseudo)
   #  names(datapseudodownload$data)= "palavra"
   
   
   #  if(input$PseudoEsc == '2'){
   #      datapseudodownload["Pseudo1"] = troca1(datapseudodownload$palavra)
   #     datapseudodownload["Pseudo2"] = troca2(datapseudodownload$palavra)
   
   # }
   #   else{
   # if(input$PseudoEsc == '3'){
   #   datapseudodownload["Pseudo1"] = troca1(datapseudodownload$palavra)
   #   datapseudodownload["Pseudo2"] = troca2(datapseudodownload$palavra)
   #   datapseudodownload["Pseudo3"] = troca3(datapseudodownload$palavra)
   # }
   # else{
   #   if(input$PseudoEsc == '4'){
   #     datapseudodownload["Pseudo1"] = troca1(datapseudodownload$palavra)
   #     datapseudodownload["Pseudo2"] = troca2(datapseudodownload$palavra)
   #     datapseudodownload["Pseudo3"] = troca3(datapseudodownload$palavra)
   #     datapseudodownload["Pseudo4"] = troca4(datapseudodownload$palavra)
   #   }
   #   else{
   #     if(input$PseudoEsc == '5'){
   #       datapseudodownload["Pseudo1"] = troca1(datapseudodownload$palavra)
   #       datapseudodownload["Pseudo2"] = troca2(datapseudodownload$palavra)
   #        datapseudodownload["Pseudo3"] = troca3(datapseudodownload$palavra)
   #     datapseudodownload["Pseudo4"] = troca4(datapseudodownload$palavra)
   #       datapseudodownload["Pseudo5"] = troca5(datapseudodownload$palavra)
   #     }
   #   }
   # }
   # }})
  
  }

shinyApp(ui, server)
