# Data Science - Laboratorium
# Obliczeniowe nauki o sieciach
# Zadanie I: Grafy losowe (Erdős-Rényi)

library(igraph)

# 1. Wygeneruj sieć Erdős-Rényi o stu wierzchołkach i prawdopodobieństwie krawędzi = 0.05.
cat("\n--- 1. Generowanie grafu Erdős-Rényi ---\n")
g <- erdos.renyi.game(n = 100, p.or.m = 0.05, type = "gnp")

# 2. Wydrukuj podsumowanie grafu - czy graf jest ważony?
cat("\n--- 2. Podsumowanie grafu (przed wagami) ---\n")
print(summary(g))
is_weighted_before <- is_weighted(g)
cat("Czy graf jest ważony?", is_weighted_before, "\n")

# 3. Wylistuj wszystkie wierzchołki i krawędzie.
cat("\n--- 3. Lista wierzchołków i krawędzi ---\n")
cat("Wierzchołki:\n")
print(V(g))
cat("Krawędzie:\n")
print(E(g))

# 4. Ustaw wagi wszystkich krawędzi na losowe z zakresu 0.01 do 1
cat("\n--- 4. Ustawianie losowych wag ---\n")
E(g)$weight <- runif(ecount(g), min = 0.01, max = 1)

# 5. Wydrukuj ponownie podsumowanie grafu - czy teraz graf jest ważony?
cat("\n--- 5. Podsumowanie grafu (po wagach) ---\n")
print(summary(g))
is_weighted_after <- is_weighted(g)
cat("Czy graf jest ważony?", is_weighted_after, "\n")

# 6. Jaki jest stopień każdego węzła? Następnie stwórz histogram stopni węzłów.
cat("\n--- 6. Stopnie węzłów i histogram ---\n")
deg <- degree(g)
print(deg)
pdf("mysolution/mysolution1_histogram.pdf") # Zapisz do pliku, żeby nie blokować
hist(deg, main = "Histogram stopni węzłów", xlab = "Stopień", col = "lightblue", border = "black")
dev.off()
cat("Histogram zapisano do pliku mysolution/mysolution1_histogram.pdf\n")

# 7. Ile jest klastrów (connected components) w grafie?
cat("\n--- 7. Liczba klastrów (connected components) ---\n")
comps <- components(g)
cat("Liczba klastrów:", comps$no, "\n")

# 8. Zwizualizuj graf w taki sposób, aby rozmiar węzłów odpowiadał mierze PageRank.
cat("\n--- 8. Wizualizacja z PageRank ---\n")
pr <- page_rank(g)$vector
# Skalowanie rozmiaru węzłów dla lepszej widoczności
V(g)$size <- pr * 1000 

pdf("mysolution/mysolution1_network.pdf")
plot(g, 
     layout = layout.fruchterman.reingold,
     vertex.label = NA, 
     main = "Graf Erdős-Rényi (rozmiar ~ PageRank)")
dev.off()
cat("Wizualizację zapisano do pliku mysolution/mysolution1_network.pdf\n")
