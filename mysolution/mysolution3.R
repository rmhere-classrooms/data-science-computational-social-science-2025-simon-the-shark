# Data Science - Laboratorium
# Obliczeniowe nauki o sieciach
# Zadanie III: Rozprzestrzenianie się informacji w sieciach - dane rzeczywiste + ShinyApps

library(shiny)
library(bslib)
library(igraph)

# --- Wczytywanie i przetwarzanie danych ---

data_url <- "https://bergplace.org/share/out.radoslaw_email_email"

# Wczytanie danych, pomijając pierwsze 2 wiersze (metadane)
df <- read.table(data_url, skip = 2)

# Zachowanie tylko dwóch pierwszych kolumn (Nadawca, Odbiorca)
df <- df[, 1:2]
colnames(df) <- c("from", "to")

# Tworzenie grafu skierowanego
g_raw <- graph_from_data_frame(df, directed = TRUE)

# Uproszczenie grafu (usunięcie wielokrotnych krawędzi i pętli)
g <- simplify(g_raw, remove.multiple = TRUE, remove.loops = TRUE)

# Weryfikacja statystyk grafu
v_count <- vcount(g)
e_count <- ecount(g)
cat("Węzły:", v_count, "(Oczekiwano: 167)\n")
cat("Krawędzie:", e_count, "(Oczekiwano: 5783)\n")

# Obliczanie wag krawędzi: wij = cntij / cnti

# Agregacja liczności z oryginalnego data frame
edge_counts <- aggregate(list(count = rep(1, nrow(df))), list(from = df$from, to = df$to), length)

# Obliczenie całkowitej liczby maili wysłanych przez każdego nadawcę (cnti)
sender_counts <- aggregate(list(total = edge_counts$count), list(from = edge_counts$from), sum)

# Połączenie w celu obliczenia wag
weights_df <- merge(edge_counts, sender_counts, by = "from")
weights_df$weight <- weights_df$count / weights_df$total

E(g)$weight <- 0

for (i in 1:nrow(weights_df)) {
  eid <- get_edge_ids(g, c(weights_df$from[i], weights_df$to[i]))
  if (eid > 0) {
    E(g)[eid]$weight <- weights_df$weight[i]
  }
}

# --- Logika Symulacji ---

run_simulation <- function(g, seed_nodes, prob_multiplier, max_iter) {
  active_nodes <- rep(FALSE, vcount(g))
  active_nodes[seed_nodes] <- TRUE

  history <- numeric(max_iter)
  history[1] <- sum(active_nodes)

  newly_active <- seed_nodes

  for (i in 2:max_iter) {
    if (length(newly_active) == 0) {
      history[i:max_iter] <- history[i-1]
      break
    }
    
    next_active <- c()
    
    # Pobranie krawędzi wychodzących od nowo aktywowanych węzłów
    adj_edges <- lapply(newly_active, function(x) incident(g, x, mode = "out"))
    
    potential_targets <- c()
    
    for (edges in adj_edges) {
      if (length(edges) > 0) {
        targets <- head_of(g, edges)
        weights <- edges$weight * prob_multiplier
        
        # Próba aktywacji
        success <- runif(length(weights)) < weights
        
        if (any(success)) {
          potential_targets <- c(potential_targets, as.integer(targets[success]))
        }
      }
    }
    
    # Filtrowanie już aktywnych
    potential_targets <- unique(potential_targets)
    actually_new <- potential_targets[!active_nodes[potential_targets]]
    
    if (length(actually_new) > 0) {
      active_nodes[actually_new] <- TRUE
      next_active <- actually_new
    }
    
    newly_active <- next_active
    history[i] <- sum(active_nodes)
  }
  
  return(history)
}

get_seeds <- function(g, strategy, n_seeds) {
  if (strategy == "Outdegree") {
    deg <- degree(g, mode = "out")
    return(order(deg, decreasing = TRUE)[1:n_seeds])
  } else if (strategy == "Betweenness") {
    bet <- betweenness(g, weights = NA)
    return(order(bet, decreasing = TRUE)[1:n_seeds])
  } else if (strategy == "Closeness") {
    clo <- closeness(g, mode = "all", weights = NA) 
    return(order(clo, decreasing = TRUE)[1:n_seeds])
  } else if (strategy == "Random") {
    return(sample(vcount(g), n_seeds))
  } else if (strategy == "Eigenvector") {
    # Wybrana miara własna: Eigenvector Centrality
    # Mierzy wpływ węzła w sieci. Węzeł ma wysoki eigenvector centrality, jeśli jest połączony z innymi węzłami, które również mają wysoki wynik.
    # Jest to dobre uzupełnienie dla degree i betweenness, ponieważ uwzględnia jakość połączeń, a nie tylko ich ilość czy pośrednictwo.
    eig <- eigen_centrality(g)$vector
    return(order(eig, decreasing = TRUE)[1:n_seeds])
  }
  return(sample(vcount(g), n_seeds))
}


# --- Shiny UI ---

ui <- page_sidebar(
  title = "Symulacja Dyfuzji Informacji",
  sidebar = sidebar(
    sliderInput("prob_mult", "Mnożnik prawdopodobieństwa (%)", min = 10, max = 200, value = 100),
    sliderInput("iterations", "Liczba iteracji", min = 1, max = 50, value = 10),
    actionButton("run", "Uruchom Symulację")
  ),
  card(
    card_header("Przebieg dyfuzji"),
    plotOutput("diffPlot")
  ),
  card(
    card_header("Opis"),
    p("Symulacja modelu Independent Cascade na sieci e-mail."),
    p("Porównanie 5 strategii wyboru węzłów początkowych (5% sieci)."),
    p("Wyniki uśrednione ze 100 powtórzeń.")
  )
)

# --- Shiny Server ---

server <- function(input, output) {
  
  simulation_data <- eventReactive(input$run, {
    
    n_seeds <- round(0.05 * vcount(g))
    strategies <- c("Outdegree", "Betweenness", "Closeness", "Random", "Eigenvector")
    results <- list()
    
    prob_mult <- input$prob_mult / 100
    max_iter <- input$iterations
    n_sims <- 100 # Zadanie wymaga 100 powtórzeń
    
    withProgress(message = 'Trwa symulacja...', value = 0, {
      
      for (strat in strategies) {
        incProgress(1/length(strategies), detail = paste("Strategia:", strat))
        
        mat <- matrix(0, nrow = n_sims, ncol = max_iter)
        
        for (s in 1:n_sims) {
          seeds <- get_seeds(g, strat, n_seeds)
          hist <- run_simulation(g, seeds, prob_mult, max_iter)
          mat[s, ] <- hist
        }
        
        results[[strat]] <- colMeans(mat)
      }
      
    })
    
    return(results)
  })
  
  output$diffPlot <- renderPlot({
    data <- simulation_data()
    
    max_y <- max(sapply(data, max))
    max_x <- input$iterations
    
    plot(NULL, xlim = c(1, max_x), ylim = c(0, max_y), 
         xlab = "Iteracja", ylab = "Liczba aktywnych węzłów",
         main = "Porównanie strategii dyfuzji")
    
    colors <- c("red", "blue", "green", "orange", "purple")
    strategies <- names(data)
    
    for (i in 1:length(strategies)) {
      lines(1:max_x, data[[strategies[i]]], col = colors[i], lwd = 2)
    }
    
    legend("bottomright", legend = strategies, col = colors, lwd = 2)
  })
}

shinyApp(ui = ui, server = server)
