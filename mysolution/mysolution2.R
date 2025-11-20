# Data Science - Laboratorium
# Obliczeniowe nauki o sieciach
# Zadanie II: Grafy preferential attachment (Barabási-Albert)

library(igraph)

# 1. Wygeneruj graf wedle modelu Barabási-Albert z tysiącem węzłów
cat("\n--- 1. Generowanie grafu Barabási-Albert ---\n")
g <- barabasi.game(n = 1000, directed = FALSE)
print(summary(g))

# 2. Zwizualizuj graf layoutem Fruchterman & Reingold
cat("\n--- 2. Wizualizacja (Fruchterman & Reingold) ---\n")
pdf("mysolution/mysolution2_network.pdf")
plot(g, 
     layout = layout.fruchterman.reingold, 
     vertex.size = 3, 
     vertex.label = NA, 
     main = "Graf Barabási-Albert (n=1000)")
dev.off()
cat("Wizualizację zapisano do pliku mysolution/mysolution2_network.pdf\n")

# 3. Znajdź najbardziej centralny węzeł według miary betweenness, jaki ma numer?
cat("\n--- 3. Najbardziej centralny węzeł (betweenness) ---\n")
bet <- betweenness(g)
max_bet_node <- which.max(bet)
cat("Węzeł o największym betweenness:", max_bet_node, "\n")
cat("Wartość betweenness:", bet[max_bet_node], "\n")

# 4. Jaka jest średnica grafu?
cat("\n--- 4. Średnica grafu ---\n")
diam <- diameter(g)
cat("Średnica grafu:", diam, "\n")

# 5. W komentarzu napisz czym różnią się grafy Barabási-Albert i Erdős-Rényi.
# Grafy Erdős-Rényi (ER) są modelami grafów losowych, gdzie krawędzie tworzone są z jednakowym prawdopodobieństwem niezależnie od siebie.
# Rozkład stopni wierzchołków w grafach ER dąży do rozkładu Poissona (dla dużych n). Większość węzłów ma stopień bliski średniemu.
#
# Grafy Barabási-Albert (BA) modelują zjawisko 'preferential attachment' (bogaci stają się bogatsi).
# Nowe węzły chętniej łączą się z węzłami, które już mają wysoki stopień.
# Prowadzi to do powstania sieci bezskalowych (scale-free), gdzie rozkład stopni wierzchołków jest potęgowy (power-law).
# W grafach BA występuje niewielka liczba 'hubów' o bardzo wysokim stopniu, podczas gdy większość węzłów ma niski stopień.