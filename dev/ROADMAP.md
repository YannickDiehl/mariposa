# 🗺️ Statistical Framework - Verfahrens-basierte Roadmap

**Projekt**: SPSS-kompatibles statistisches Testing-Framework für R  
**Version**: 2.0 - Verfahrens-Integration Phase  
**Ziel**: Vollständige Abdeckung statistischer Analyseverfahren  
**Stand**: Dezember 2024  
**Basis**: Statistischer Entscheidungsbaum für empirische Sozialforschung

---

## 📋 **Projekt-Übersicht**

### 🎯 **Mission Statement**
Entwicklung eines umfassenden statistischen Frameworks, das **alle gängigen Analyseverfahren** der empirischen Sozialforschung abdeckt - strukturiert nach dem statistischen Entscheidungsbaum mit optimaler S3-Generic-Integration.

### ✅ **Bereits implementiert (Meilensteine erreicht)**

| Kategorie | Verfahren | Status | S3 Generics | Beschreibung |
|-----------|-----------|--------|-------------|-------------|
| **Unterschiede - zentrale Tendenz** | t-Test (unabhängig) | ✅ **Vollständig** | `levene_test()`, `emmeans()` | 2 Variablen, 2 Stufen |
| **Unterschiede - zentrale Tendenz** | t-Test (verbunden) | ✅ **Vollständig** | `emmeans()` | Repeated Measures |
| **Unterschiede - zentrale Tendenz** | Mann-Whitney | ✅ **Vollständig** | `effect_sizes()` | Non-parametrisch |
| **Unterschiede - zentrale Tendenz** | einfaktorielle ANOVA | ✅ **Vollständig** | `levene_test()`, `tukey_test()`, `emmeans()` | > 2 Stufen |
| **Unterschiede - Varianzen** | Levene-Test | ✅ **Vollständig** | - | Homogenitätsprüfung |
| **Repeated Measures** | RM-ANOVA (2 Zeitpunkte) | ✅ **Vollständig** | `mauchly_test()`, `emmeans()` | Messwiederholung |

---

## 🎯 **Meilenstein-Roadmap nach Verfahrenskategorien**

### **🔵 UNTERSCHIEDE - Zentrale Tendenz (Erweitert)**

#### **Meilenstein 1.1: Wilcoxon-Test** 
**Fertigstellung**: Januar 2025 (Woche 1-2)  
**Aufwand**: 1-2 Wochen  
**Anwendung**: Verbundene Stichproben, non-parametrisch

| Feature | Implementation | S3 Generics |
|---------|---------------|-------------|
| **Wilcoxon Signed-Rank** | `wilcoxon_test.R` | `effect_sizes()` (r, η²) |
| **Exact p-values** | Für kleine Stichproben | `assumptions()` (Symmetrie) |
| **Tied values handling** | SPSS-kompatibel | `bootstrap()` (CI) |

```r
# Verwendung
data %>% wilcoxon_test(pre, post, paired = TRUE)
data %>% wilcoxon_test(pre, post, paired = TRUE) %>% effect_sizes()
```

#### **Meilenstein 1.2: Kruskal-Wallis Test**
**Fertigstellung**: Januar 2025 (Woche 3-4)  
**Aufwand**: 1-2 Wochen  
**Anwendung**: > 2 Stufen, non-parametrisch

| Feature | Implementation | S3 Generics |
|---------|---------------|-------------|
| **Kruskal-Wallis** | `kruskal_wallis_test.R` | `effect_sizes()` (η², ε²) |
| **Post-hoc Comparisons** | Dunn's test | `pairwise_comparisons()` |
| **Tied ranks** | SPSS-kompatible Behandlung | `assumptions()` |

```r
# Verwendung  
data %>% kruskal_wallis_test(outcome, group = treatment)
data %>% kruskal_wallis_test(outcome, group = treatment) %>% pairwise_comparisons()
```

#### **Meilenstein 1.3: Mehrfaktorielle ANOVA**
**Fertigstellung**: Februar 2025 (Woche 1-4)  
**Aufwand**: 3-4 Wochen  
**Anwendung**: 1 AV, > 2 UVs, factorial design

| Feature | Implementation | S3 Generics |
|---------|---------------|-------------|
| **Two-way ANOVA** | `factorial_anova.R` | `emmeans()` (main effects, interactions) |
| **Three-way ANOVA** | Erweiterte Implementierung | `contrasts()` (simple effects) |
| **Interaction Analysis** | Simple effects | `effect_sizes()` (partial η²) |
| **Post-hoc Tests** | Für alle Faktoren | `assumptions()` (sphericity, homogeneity) |

```r
# Verwendung
data %>% factorial_anova(outcome, group1 * group2)
data %>% factorial_anova(outcome, group1 * group2 * group3) %>% emmeans("interaction")
```

---

### **🟡 UNTERSCHIEDE - Varianzen**

#### **Meilenstein 2.1: Erweiterte Homogenitätstests**
**Fertigstellung**: März 2025 (Woche 1-2)  
**Aufwand**: 1-2 Wochen

| Test | Implementation | Anwendung |
|------|---------------|-----------|
| **Brown-Forsythe** | Erweitere `levene_test.R` | Robuster als Levene |
| **Bartlett Test** | `bartlett_test.R` | Normalverteilte Daten |
| **Fligner-Killeen** | `fligner_test.R` | Non-parametrisch |

```r
# Verwendung
result %>% levene_test(center = "median")  # Brown-Forsythe
result %>% bartlett_test()
result %>% fligner_test()
```

---

### **🟢 UNTERSCHIEDE - Proportionen/Häufigkeiten**

#### **Meilenstein 3.1: Chi²-Tests**
**Fertigstellung**: März 2025 (Woche 3-4)  
**Aufwand**: 2 Wochen

| Test | Implementation | S3 Generics | Anwendung |
|------|---------------|-------------|-----------|
| **Chi²-Anpassungstest** | `chi_square_test.R` | `effect_sizes()` (Cramér's V, φ) | Goodness of fit |
| **Chi²-Unabhängigkeitstest** | Erweiterte Implementierung | `assumptions()` (erwartete Häufigkeiten) | Kontingenz-Tabellen |
| **Exakter Test (Fisher)** | Für kleine Erwartungswerte | `bootstrap()` (CI für Effektstärken) | 2×2 Tabellen |

```r
# Verwendung
data %>% chi_square_test(variable, expected = c(0.25, 0.25, 0.5))
data %>% chi_square_test(row_var, col_var) %>% effect_sizes()
```

#### **Meilenstein 3.2: Binomial-Test**
**Fertigstellung**: April 2025 (Woche 1)  
**Aufwand**: 1 Woche

```r
# binomial_test.R
data %>% binomial_test(success_var, p = 0.5)
```

---

### **🟠 ZUSAMMENHÄNGE**

#### **Meilenstein 4.1: Korrelationen**
**Fertigstellung**: April 2025 (Woche 2-3)  
**Aufwand**: 2 Wochen

| Korrelation | Implementation | S3 Generics | Anwendung |
|-------------|---------------|-------------|-----------|
| **Pearson** | `correlation_test.R` | `effect_sizes()` (r, r²) | Intervallskaliert, normalverteilt |
| **Spearman** | Erweiterte Implementierung | `bootstrap()` (robuste CI) | Ordinalskaliert/verteilungsfrei |
| **Kendall's τ** | Non-parametrisch | `assumptions()` (Linearität) | Kleine Stichproben |

```r
# Verwendung
data %>% correlation_test(var1, var2, method = "pearson")
data %>% correlation_test(var1, var2, method = "spearman") %>% bootstrap()
```

#### **Meilenstein 4.2: Einfache Regression**
**Fertigstellung**: Mai 2025 (Woche 1-4)  
**Aufwand**: 3-4 Wochen

| Feature | Implementation | S3 Generics | Beschreibung |
|---------|---------------|-------------|-------------|
| **Linear Regression** | `regression_test.R` | `effect_sizes()` (R², adj. R²) | Vorhersage-Modelle |
| **Model Diagnostics** | Residualanalyse | `diagnostics()` (residuals, outliers) | Modell-Validierung |
| **Assumption Testing** | Normalität, Homoskedastizität | `assumptions()` (linearity, normality) | Voraussetzungsprüfung |

```r
# Verwendung
data %>% regression_test(outcome ~ predictor)
data %>% regression_test(outcome ~ predictor) %>% assumptions()
data %>% regression_test(outcome ~ predictor) %>% diagnostics()
```

#### **Meilenstein 4.3: Multiple Regression**
**Fertigstellung**: Juni 2025 (Woche 1-4)  
**Aufwand**: 3-4 Wochen

```r
# Erweiterte regression_test.R
data %>% regression_test(outcome ~ predictor1 + predictor2 + predictor3)
data %>% regression_test(outcome ~ predictor1 * predictor2) %>% emmeans("interaction")
```

#### **Meilenstein 4.4: Logistische Regression**
**Fertigstellung**: Juli 2025 (Woche 1-4)  
**Aufwand**: 3-4 Wochen

```r
# logistic_regression.R
data %>% logistic_regression(binary_outcome ~ predictor1 + predictor2)
data %>% logistic_regression(binary_outcome ~ predictor1 + predictor2) %>% effect_sizes()
```

---

### **🔴 INTERDEPENDENZ-ANALYSE**

#### **Meilenstein 5.1: Clusteranalyse**
**Fertigstellung**: August 2025 (Woche 1-4)  
**Aufwand**: 3-4 Wochen

| Method | Implementation | S3 Generics | Anwendung |
|--------|---------------|-------------|-----------|
| **Hierarchical Clustering** | `cluster_analysis.R` | `diagnostics()` (silhouette, elbow) | Explorative Gruppierung |
| **K-Means** | Erweiterte Implementierung | `bootstrap()` (cluster stability) | Partitionierung |
| **Optimal Clusters** | Gap statistic, silhouette | `effect_sizes()` (R², η²) | Cluster-Validierung |

```r
# Verwendung
data %>% cluster_analysis(var1, var2, var3, method = "hierarchical")
data %>% cluster_analysis(var1, var2, var3, method = "kmeans", k = 3) %>% diagnostics()
```

#### **Meilenstein 5.2: Faktoranalyse**
**Fertigstellung**: September 2025 (Woche 1-4)  
**Aufwand**: 4 Wochen

| Method | Implementation | S3 Generics | Anwendung |
|--------|---------------|-------------|-----------|
| **Principal Component Analysis** | `factor_analysis.R` | `effect_sizes()` (communalities) | Datenreduktion |
| **Exploratory Factor Analysis** | EFA mit Rotation | `assumptions()` (KMO, Bartlett) | Faktor-Extraktion |
| **Confirmatory Factor Analysis** | CFA (basis implementation) | `diagnostics()` (fit indices) | Faktor-Validierung |

```r
# Verwendung
data %>% factor_analysis(var1, var2, var3, var4, method = "pca")
data %>% factor_analysis(var1, var2, var3, var4, method = "efa", factors = 2) %>% assumptions()
```

---

## 📊 **Verfahrens-Kompatibilitäts-Matrix**

### **S3 Generic Anwendbarkeit nach Verfahren**

| Verfahren | effect_sizes | emmeans | contrasts | assumptions | bootstrap | diagnostics |
|-----------|-------------|---------|-----------|-------------|-----------|-------------|
| **t-Test** | ✅ Cohen's d | ✅ Group means | ❌ | ✅ Normality, homogeneity | ✅ | ✅ |
| **Mann-Whitney** | ✅ r, η² | ❌ | ❌ | ✅ Symmetry | ✅ | ✅ |
| **ANOVA** | ✅ η², ω² | ✅ Marginal means | ✅ Polynomial | ✅ All assumptions | ✅ | ✅ |
| **RM-ANOVA** | ✅ Partial η² | ✅ Time × Group | ✅ Trend analysis | ✅ + Sphericity | ✅ | ✅ |
| **Wilcoxon** | 🎯 r, η² | ❌ | ❌ | 🎯 Symmetry | 🎯 | 🎯 |
| **Kruskal-Wallis** | 🎯 η², ε² | ❌ | ❌ | 🎯 Basic | 🎯 | 🎯 |
| **Chi²** | 🎯 Cramér's V, φ | ❌ | ❌ | 🎯 Expected freq. | 🎯 | ❌ |
| **Correlation** | 🎯 r, r² | ❌ | ❌ | 🎯 Linearity | 🎯 | 🎯 |
| **Regression** | 🎯 R², adj. R² | 🎯 Predicted values | 🎯 Custom | 🎯 All regression | 🎯 | 🎯 |
| **Factor Analysis** | 🔮 Communalities | ❌ | ❌ | 🔮 KMO, Bartlett | 🔮 | 🔮 |
| **Cluster Analysis** | 🔮 R², η² | ❌ | ❌ | ❌ | 🔮 Stability | 🔮 Validation |

**Legende:**
- ✅ = Implementiert und funktional
- 🎯 = Geplant (2025)  
- 🔮 = Zukünftig geplant (2026)
- ❌ = Nicht anwendbar

---

## 🧪 **S3 Generic Entwicklungsplan**

### **Neue S3 Generics nach Priorität**

#### **HIGH PRIORITY (Q1-Q2 2025)**

| S3 Generic | Meilensteine | Implementation Date |
|------------|--------------|-------------------|
| **`effect_sizes()`** | 1.1, 1.2, 3.1, 4.1 | Januar 2025 |
| **`pairwise_comparisons()`** | 1.2, 1.3 | Februar 2025 |
| **`assumptions()`** | Alle Verfahren | März 2025 |
| **`bootstrap()`** | 3.1, 4.1 | April 2025 |

#### **MEDIUM PRIORITY (Q3 2025)**

| S3 Generic | Meilensteine | Implementation Date |
|------------|--------------|-------------------|
| **`diagnostics()`** | 4.2, 4.3, 5.1 | August 2025 |
| **`model_comparison()`** | 4.2, 4.3, 4.4 | September 2025 |

#### **Erweiterte S3 Generics (2026)**

| S3 Generic | Beschreibung | Anwendung |
|------------|-------------|-----------|
| **`power_analysis()`** | Teststärke-Analysen | Alle Hypothesentests |
| **`multivariate_tests()`** | MANOVA, MANCOVA | Mehrere AVs |
| **`time_series()`** | Zeitreihen-Analysen | Längsschnitt-Daten |

---

## 🎯 **Quartalsziele 2025**

### **Q1 2025: Non-parametrische Tests**
- ✅ Wilcoxon-Test mit Effektstärken
- ✅ Kruskal-Wallis mit Post-hoc Tests  
- ✅ Erweiterte Homogenitätstests
- ✅ Chi²-Tests mit exakten Verfahren

### **Q2 2025: Korrelationen & Einfache Regression**
- ✅ Vollständige Korrelations-Suite
- ✅ Einfache lineare Regression
- ✅ Logistische Regression (Basis)
- ✅ Bootstrap-Verfahren für CI

### **Q3 2025: Multiple Regression & Faktoranalyse**
- ✅ Multiple Regression mit Interaktionen
- ✅ Erweiterte Regressionsdiagnostik
- ✅ Explorative Faktoranalyse
- ✅ Clusteranalyse-Grundlagen

### **Q4 2025: Erweiterte Verfahren**
- ✅ Konfirmatorische Faktoranalyse
- ✅ Erweiterte Clusterverfahren
- ✅ MANOVA (Basis)
- ✅ Vollständige S3-Integration

---

## 📈 **Meilenstein-Tracking**

### **Implementierungsfortschritt**

| Kategorie | Verfahren geplant | Verfahren implementiert | Fortschritt |
|-----------|------------------|------------------------|-------------|
| **Unterschiede (zentral)** | 6 | 4 | 67% |
| **Unterschiede (Varianzen)** | 4 | 1 | 25% |
| **Unterschiede (Proportionen)** | 3 | 0 | 0% |
| **Zusammenhänge** | 5 | 0 | 0% |
| **Interdependenz** | 3 | 0 | 0% |
| **GESAMT** | **21** | **5** | **24%** |

### **S3 Generic Abdeckung**

| S3 Generic | Verfahren unterstützt | Ziel 2025 | Fortschritt |
|------------|---------------------|----------|-------------|
| **effect_sizes()** | 5 | 15 | 33% |
| **emmeans()** | 3 | 8 | 38% |
| **assumptions()** | 5 | 18 | 28% |
| **bootstrap()** | 0 | 12 | 0% |
| **diagnostics()** | 2 | 10 | 20% |

---

## 🚀 **Konkrete nächste Schritte**

### **Januar 2025 - Woche 1-2: Wilcoxon Implementation**

```r
# wilcoxon_test.R - Neue Hauptfunktion
wilcoxon_test <- function(data, ..., paired = FALSE, exact = NULL) {
  UseMethod("wilcoxon_test")
}

# Integration in effect_sizes.R
effect_sizes.wilcoxon_test_results <- function(x, ...) {
  # r = Z / sqrt(N)
  # η² für Wilcoxon
}
```

### **Januar 2025 - Woche 3-4: Kruskal-Wallis Implementation**

```r
# kruskal_wallis_test.R
kruskal_wallis_test <- function(data, ..., group) {
  UseMethod("kruskal_wallis_test")  
}

# Neue S3 Generic: pairwise_comparisons.R
pairwise_comparisons <- function(x, method = "dunn", ...) {
  UseMethod("pairwise_comparisons")
}
```

### **Februar 2025: Mehrfaktorielle ANOVA**

```r
# factorial_anova.R - Erweitere oneway_anova_test.R
factorial_anova <- function(data, formula, ...) {
  # Two-way, Three-way ANOVA
  # Integration mit emmeans() für Interaktionen
}
```

---

## 🔮 **Vision: Vollständiger Entscheidungsbaum 2025**

Bis Ende 2025 soll das Framework **alle Verfahren** des statistischen Entscheidungsbaums abdecken:

- ✅ **100% Abdeckung** aller Unterschiedstests
- ✅ **100% Abdeckung** aller Zusammenhangsanalysen  
- ✅ **80% Abdeckung** der Interdependenz-Verfahren
- ✅ **Vollständige S3-Integration** für alle Verfahren
- ✅ **SPSS-Kompatibilität** bei allen implementierten Tests

**Ziel**: Das Framework als **Standard-Tool** für statistische Analysen in der empirischen Sozialforschung etablieren! 🎯

---

**Last Updated**: Dezember 2024  
**Next Milestone Review**: Januar 2025  
**Maintainer**: Statistical Framework Development Team 