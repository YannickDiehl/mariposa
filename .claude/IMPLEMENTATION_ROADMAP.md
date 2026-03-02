# mariposa - Strategischer Implementierungsplan
## Erweiterung des Funktionsumfangs auf Basis der SPSS-Kurs-Syntax

**Erstellt:** 2026-02-26
**Zuletzt aktualisiert:** 2026-03-01
**Autor:** Yannick Diehl / Claude
**Basis:** SPSS-Syntax "Statistik mit SPSS" (WS 25/26, Uni Marburg, A. Heyder)
**Referenzdiagramm:** Methodentaxonomie "Art der Analyse"

---

## 1. Ausgangslage

### 1.1 Aktueller Funktionsumfang (v0.1.0)

mariposa umfasst aktuell **37 exportierte Funktionen** in 9 Kategorien:

| Kategorie | Funktionen | Anzahl |
|---|---|---|
| Deskriptive Statistik | `describe()`, `frequency()`, `crosstab()` | 3 |
| Hypothesentests | `t_test()`, `oneway_anova()`, `mann_whitney()`, `chi_square()` | 4 |
| Nicht-parametrische Tests | `kruskal_wallis()`, `wilcoxon_test()`, `friedman_test()`, `binomial_test()` | 4 |
| Korrelationsanalyse | `pearson_cor()`, `spearman_rho()`, `kendall_tau()` | 3 |
| Post-Hoc Analyse | `tukey_test()`, `scheffe_test()`, `levene_test()` | 3 |
| Skalenanalyse | `reliability()`, `efa()`, `scale_index()`, `pomps()` | 4 |
| Regressionsanalyse | `linear_regression()`, `logistic_regression()` | 2 |
| Gewichtete Statistiken | `w_mean()`, `w_median()`, `w_sd()`, `w_var()`, `w_se()`, `w_iqr()`, `w_range()`, `w_quantile()`, `w_modus()`, `w_skew()`, `w_kurtosis()` | 11 |
| Effektstaerke-Hilfsfunktionen | `phi()`, `cramers_v()`, `goodman_gamma()` | 3 |

### 1.2 Aktuelle Dependencies

**Imports (8 Pakete):** cli, stats, utils, dplyr, rlang, tidyselect, tibble, tidyr
**Suggests (5 Pakete):** testthat (>= 3.0.0), knitr, rmarkdown, covr, haven (>= 2.4.0)

### 1.3 Validierungsstatus

**v0.1.0:** 83/83 SPSS-Validierungstests bestanden.
**v0.2.0:** 6 neue Funktionen (reliability, efa, scale_index, pomps, linear_regression, logistic_regression) - SPSS-validiert.
**v0.3.0:** 294 neue SPSS-Validierungen (Kruskal-Wallis, Wilcoxon, Friedman, Binomial).
**Gesamt (v0.3.0): 2.227+ Tests bestanden (0 Fehler, 0 Skips).**

Toleranzen:
- Counts: Exakt (0)
- Prozente: +/- 0.1
- Teststatistiken: +/- 0.00001
- Gewichtete Summen: +/- 1
- P-Werte: +/- 0.0001

---

## 2. Gap-Analyse

### 2.1 SPSS-Kurs-Syntax vs. mariposa

Die SPSS-Syntax deckt folgende Verfahren ab:

| SPSS-Prozedur | SPSS-Befehl | mariposa | Status |
|---|---|---|---|
| Haeufigkeiten | `FREQUENCIES` | `frequency()` | Vorhanden |
| Deskriptive Statistiken | `FREQUENCIES /STATISTICS` | `describe()` | Vorhanden |
| Kreuztabellen (2-Wege) | `CROSSTABS /TABLES=AV BY UV` | `crosstab()` | Vorhanden |
| Kreuztabellen (3-Wege) | `CROSSTABS /TABLES=AV BY UV BY Schicht` | `group_by(schicht) %>% crosstab()` | Vorhanden (via group_by) |
| Phi / Cramers V | `CROSSTABS /STATISTICS=PHI` | `chi_square()` | Vorhanden |
| Goodman-Kruskal Gamma | `CROSSTABS /STATISTICS=GAMMA` | `chi_square()` | Vorhanden |
| T-Test (unabhaengig) | `T-TEST GROUPS=` | `t_test()` | Vorhanden |
| Einfaktorielle ANOVA | `ONEWAY` | `oneway_anova()` | Vorhanden |
| Scheffe Post-Hoc | `ONEWAY /POSTHOC=SCHEFFE` | `scheffe_test()` | Vorhanden |
| Pearson-Korrelation | `CORRELATIONS` | `pearson_cor()` | Vorhanden |
| Pairwise/Listwise Missing | `CORRELATIONS /MISSING=PAIRWISE/LISTWISE` | `pearson_cor(use=)` | Vorhanden |
| Gewichtung | `WEIGHT BY` | alle `w_*()` | Vorhanden |
| **Reliabilitaetsanalyse** | `RELIABILITY /MODEL=ALPHA` | — | **FEHLT** |
| **Explorative Faktorenanalyse** | `FACTOR /EXTRACTION PC /ROTATION VARIMAX/OBLIMIN` | — | **FEHLT** |
| **Skalenbildung (Mittelwertindex)** | `COMPUTE m_X = mean(var1, var2, ...)` | — | **FEHLT** |
| **POMPS-Transformation** | `COMPUTE vp = ((v - min) / (max - min)) * 100` | — | **FEHLT** |
| **Lineare Regression (bivariat)** | `REGRESSION /METHOD=ENTER /DEPENDENT AV /METHOD=ENTER UV` | — | **FEHLT** |
| **Lineare Regression (multipel)** | `REGRESSION /METHOD=ENTER /DEPENDENT AV /METHOD=ENTER UV1 UV2 UV3` | — | **FEHLT** |
| **Logistische Regression** | `LOGISTIC REGRESSION` (nur erwaehnt, kein Code) | — | **FEHLT** |
| Rekodierung | `RECODE ... INTO` | — | Ueber `dplyr::mutate()` + `case_when()` |
| Variablenberechnung | `COMPUTE` | — | Ueber `dplyr::mutate()` |
| COUNT | `COUNT var = vars (value)` | — | Ueber `dplyr::rowSums()` |
| SELECT IF | `SELECT IF / TEMPORARY` | — | Ueber `dplyr::filter()` |

### 2.2 Methodentaxonomie-Diagramm vs. mariposa

Das Referenzdiagramm "Art der Analyse" zeigt den vollstaendigen Methodenkanon:

| Kategorie | Verfahren | Skalenniveau | Status |
|---|---|---|---|
| **Unterschiede - Unabhaengig** | | | |
| | Kruskal-Wallis-Test | Ordinal | **v0.3.0 ERLEDIGT** |
| | Dunn-Test (Post-Hoc fuer KW) | Ordinal | **v0.4.0 GEPLANT** |
| | Mehrfaktorielle ANOVA | Intervall | Offen |
| **Unterschiede - Verbunden** | | | |
| | Wilcoxon-Vorzeichen-Rang-Test | Ordinal | **v0.3.0 ERLEDIGT** |
| | Pairwise Wilcoxon (Post-Hoc fuer Friedman) | Ordinal | **v0.4.0 GEPLANT** |
| | Friedman-Test | Ordinal | **v0.3.0 ERLEDIGT** |
| **Dependenzanalyse** | | | |
| | Binomial-Test | Nominal | **v0.3.0 ERLEDIGT** |
| | Fisher's Exact Test | Nominal | **v0.4.0 GEPLANT** |
| | McNemar-Test | Nominal | **v0.4.0 GEPLANT** |
| | Chi-Quadrat Goodness-of-Fit | Nominal | **v0.4.0 GEPLANT** |
| **Interdependenzanalyse** | | | |
| | Clusteranalyse | Intervall | Offen |

### 2.3 Zusammenfassung der Luecken

**v0.2.0 - Skalenanalyse & Regression (ERLEDIGT):**
1. ~~`reliability()` - Cronbachs Alpha~~ **ERLEDIGT**
2. ~~`efa()` - Explorative Faktorenanalyse~~ **ERLEDIGT**
3. ~~`scale_index()` / `pomps()` - Skalenbildung~~ **ERLEDIGT**
4. ~~`linear_regression()` - Lineare Regression~~ **ERLEDIGT**
5. ~~`logistic_regression()` - Logistische Regression~~ **ERLEDIGT**

**v0.3.0 - Nicht-parametrische Tests (ERLEDIGT):**
6. ~~`kruskal_wallis()` - K+ unabhaengige Gruppen~~ **ERLEDIGT**
7. ~~`wilcoxon_test()` - Verbundene Stichproben~~ **ERLEDIGT**
8. ~~`friedman_test()` - K+ verbundene Messungen~~ **ERLEDIGT**
9. ~~`binomial_test()` - Proportionen~~ **ERLEDIGT**

**v0.4.0 - Post-Hoc-Oekosystem & Ergaenzungstests (GEPLANT):**
10. ~~`dunn_test()` - Post-Hoc fuer Kruskal-Wallis~~ **ERLEDIGT**
11. ~~`pairwise_wilcoxon()` - Post-Hoc fuer Friedman~~ **ERLEDIGT**
12. ~~`fisher_test()` - Exakter Test bei kleinen Zellen~~ **ERLEDIGT**
13. ~~`mcnemar_test()` - Abhaengige Proportionen~~ **ERLEDIGT**
14. ~~`chisq_gof()` - Goodness-of-Fit Ein-Stichproben-Test~~ **ERLEDIGT**

**Langfristig (v0.5.0+):**
15. Mehrfaktorielle ANOVA / ANCOVA
16. Clusteranalyse / Diskriminanzanalyse

---

## 3. Architekturentscheidungen

### 3.1 Wrapper vs. Genuine Implementierung

Fuer jede neue Funktion wurde bewertet, ob ein Wrapper um bestehende R-Funktionen oder eine genuine Neuimplementierung sinnvoller ist.

#### 3.1.1 `reliability()` - **GENUINE IMPLEMENTIERUNG**

**Begruendung:**
- Die Mathematik ist ueberschaubar: `alpha = (k / (k-1)) * (1 - sum(sigma_i^2) / sigma_total^2)`
- Item-Total-Korrelationen und "Alpha if Item Deleted" sind einfache Schleifen
- Die alternative Dependency `psych::alpha()` waere ein Schwergewicht (200+ Funktionen)
- Gewichtungsunterstuetzung muss ohnehin selbst implementiert werden
- **Keine neue Dependency noetig**

**Kernanforderungen:**
- Cronbachs Alpha (standardisiert und unstandardisiert)
- Item-Total-Korrelationen (korrigiert)
- Alpha-if-Item-Deleted fuer jedes Item
- Inter-Item-Korrelationsmatrix
- Deskriptive Statistiken pro Item
- Gewichtungsunterstuetzung (Survey Weights)
- SPSS-kompatibles Output-Format

**Interne Synergien:**
- Nutzt `pearson_cor()` intern fuer Korrelationsmatrix
- Nutzt `.process_variables()` und `.process_weights()` aus `helpers.R`
- Print-Methode nutzt `print_helpers.R`

#### 3.1.2 `efa()` - **HYBRIDANSATZ** (Base-R Kern + 1 optionale Dependency)

**Begruendung:**
- PCA-Extraktion: `stats::prcomp()` / `stats::princomp()` - Base R
- Varimax-Rotation: `stats::varimax()` - Base R
- Oblimin-Rotation: Benoetigt `GPArotation`-Paket
- KMO-Test und Bartlett-Test: Einfache Formeln, selbst programmieren
- `psych::fa()` waere maechtig, aber riesige Dependency + keine Gewichte

**Dependency-Entscheidung:**
- `GPArotation` als `Suggests` (nicht `Imports`)
- Varimax funktioniert immer (Base R)
- Oblimin nur verfuegbar, wenn User `GPArotation` installiert hat
- Bei fehlender Installation: Informative Fehlermeldung

**Kernanforderungen:**
- PCA-Extraktion (Eigenwerte, erklaerte Varianz)
- Kaiser-Kriterium (Eigenwert > 1) oder manuelle Faktorenanzahl
- Varimax-Rotation (orthogonal) - Standard
- Oblimin-Rotation (oblique) - Optional
- Sortierte Faktorladungsmatrix mit Blank-Schwelle (z.B. < 0.40)
- KMO-Mass (Kaiser-Meyer-Olkin) und Bartlett-Test
- Kommunalitaeten
- Pattern- und Strukturmatrix (bei Oblimin)
- Scree-Plot (optional, Base-R `plot()`)
- Gewichtungsunterstuetzung

**Interne Synergien:**
- Nutzt gewichtete Korrelationsmatrix aus `pearson_cor()` intern
- Eigenwertberechnung ueber `eigen()` (Base R)

#### 3.1.3 `scale_index()` / `pomps()` - **100% GENUINE IMPLEMENTIERUNG**

**Begruendung:**
- Reine Arithmetik: `rowMeans()` und `((x - min) / (max - min)) * 100`
- Kein existierendes R-Paket bietet hier Mehrwert
- Fokus liegt auf SPSS-freundlichem Interface

**Kernanforderungen:**
- `scale_index()`: Mittelwertindex ueber beliebige Items (analog SPSS `COMPUTE mean()`)
  - `min_valid`-Parameter: Mindestanzahl gueltiger Werte (analog SPSS `MEAN.x()`)
  - Integration mit `reliability()`-Ergebnissen moeglich
- `pomps()`: Percent of Maximum Possible Scores
  - Formel: `((x - scale_min) / (scale_max - scale_min)) * 100`
  - Automatische Erkennung von Skalenminimum/-maximum oder manuelle Angabe
  - Korrelationserhalt (Validierung: r_original == r_pomps)

#### 3.1.4 `linear_regression()` - **WRAPPER um `stats::lm()`**

**Begruendung:**
- `stats::lm()` ist Base R - keine neue Dependency
- OLS-Algebra selbst zu implementieren waere fehleranfaellig und sinnlos
- `lm()` ist seit Jahrzehnten validiert und optimiert
- Gewichtete Regression: `lm(..., weights = w)` nativ unterstuetzt
- Mehrwert von mariposa: SPSS-Style Output-Formatting

**Kernanforderungen:**
- ENTER-Methode (alle Praediktoren gleichzeitig, Standard in SPSS)
- Koeffiziententabelle (B, SE, Beta standardisiert, t, p) - SPSS-Stil
- ANOVA-Tabelle (Regression SS, Residual SS, F, p)
- Modellguete: R^2, korrigiertes R^2, Standardfehler der Schaetzung
- Standardisierte Koeffizienten (Beta) - SPSS berechnet diese automatisch
- Kollinearitaetsdiagnostik (VIF, Toleranz) - optional
- Residualdiagnostik (optional)
- Gewichtungsunterstuetzung (ueber `lm(weights=)`)
- Multi-Modell-Vergleich (bivariate vs. trivariate vs. multiple Regression)

**Interner Wrapper-Code:**
```r
linear_regression <- function(data, formula, ..., weights = NULL) {
  # 1. Formel aufbauen (SPSS-Stil: AV ~ UV1 + UV2 + UV3)
  # 2. lm() aufrufen mit optionalen Gewichten
  # 3. Ergebnis in mariposa-Struktur umwandeln
  # 4. Standardisierte Koeffizienten berechnen
  # 5. S3-Klasse zuweisen fuer Print-Methode
}
```

#### 3.1.5 `logistic_regression()` - **WRAPPER um `stats::glm()`**

**Begruendung:**
- `stats::glm(family = binomial)` ist Base R
- Gleiche Logik wie lineare Regression
- Odds Ratios = `exp(coef)` - trivial
- Pseudo-R^2-Masse (Nagelkerke, Cox-Snell, McFadden) sind einfache Formeln
- Gewichtete logistische Regression: `glm(..., weights = w)` nativ

**Kernanforderungen:**
- Binaere logistische Regression (Logit-Link)
- Koeffiziententabelle (B, SE, Wald, df, p, Exp(B), CI fuer Exp(B))
- Odds Ratios mit Konfidenzintervallen
- Modellguete: Nagelkerke R^2, Cox-Snell R^2, McFadden R^2
- Klassifikationstabelle (Trefferquote)
- Hosmer-Lemeshow-Test (Modellanpassung) - optional
- -2 Log-Likelihood
- Gewichtungsunterstuetzung

### 3.2 Dependency-Bilanz

| Funktion | Neue Imports | Neue Suggests | Begruendung |
|---|---|---|---|
| `reliability()` | keine | keine | Genuine Implementierung |
| `efa()` | keine | `GPArotation` | Nur fuer Oblimin-Rotation |
| `scale_index()` / `pomps()` | keine | keine | Reine Arithmetik |
| `linear_regression()` | keine | keine | `stats::lm()` ist bereits importiert |
| `logistic_regression()` | keine | keine | `stats::glm()` ist bereits importiert |

**Ergebnis: Maximal 1 neue optionale Dependency (`GPArotation` als `Suggests`)**

---

## 4. Ueberschneidungen und Code-Wiederverwendung

### 4.1 Interne Synergien

| Bestehender Code | Wiederverwendung in | Art |
|---|---|---|
| `helpers.R :: .process_variables()` | Alle neuen Funktionen | Tidyselect-Verarbeitung |
| `helpers.R :: .process_weights()` | Alle neuen Funktionen | Gewichtsvalidierung |
| `helpers.R :: .effective_n()` | `reliability()`, `efa()`, Regression | Effektive Stichprobengroesse |
| `print_helpers.R` | Alle Print-Methoden | Einheitliches Formatting |
| `pearson_cor()` (intern) | `reliability()`, `efa()` | Korrelationsmatrix-Berechnung |
| `w_mean()`, `w_sd()` | `reliability()`, `efa()`, `scale_index()` | Gewichtete Deskriptive |
| `chi_square()` (Phi, V, Gamma) | Bestehend, keine Aenderung | Effektstaerken |

### 4.2 Neue gemeinsame Infrastruktur

Folgende Helper-Funktionen koennten fuer mehrere neue Funktionen relevant sein:

```r
# Gewichtete Kovarianzmatrix (fuer EFA und Regression)
.weighted_cov_matrix(data, vars, weights)

# Standardisierte Koeffizienten (fuer Regression)
.standardize_coefficients(model, data)

# Modellguete-Masse (fuer lineare + logistische Regression)
.model_fit_measures(model, type = c("linear", "logistic"))
```

### 4.3 Abgrenzung zu dplyr/tidyverse

Folgende SPSS-Befehle werden **nicht** als eigene Funktionen implementiert, da sie durch bestehende tidyverse-Funktionen optimal abgedeckt sind:

| SPSS-Befehl | R/tidyverse-Aequivalent | Begruendung |
|---|---|---|
| `RECODE ... INTO` | `dplyr::mutate()` + `dplyr::case_when()` / `dplyr::case_match()` | Flexibler, maechtiger als SPSS |
| `COMPUTE` | `dplyr::mutate()` | Standard-R-Paradigma |
| `COUNT var = vars (value)` | `dplyr::mutate()` + `rowSums(across(...) == value)` | Einfach, kein Wrapper noetig |
| `SELECT IF` | `dplyr::filter()` | Kernfunktion von dplyr |
| `TEMPORARY` | Piping `%>%` / `|>` | Nativ in tidyverse |
| `WEIGHT BY` | `weights =` Parameter in allen mariposa-Funktionen | Bereits integriert |

**Empfehlung:** Statt eigener Funktionen ein Vignetten-Kapitel "SPSS-to-R Translation Guide" erstellen, das die Uebersetzung fuer SPSS-Umsteiger dokumentiert.

---

## 5. Implementierungsplan

### 5.1 Phasenuebersicht

```
Phase 1: Skalenanalyse          [ERLEDIGT]  -> reliability(), efa(), scale_index(), pomps()
Phase 2: Regressionsanalyse     [ERLEDIGT]  -> linear_regression(), logistic_regression()
Phase 3: Erweiterungen          [ERLEDIGT]  -> nicht-parametrische Tests (4 Funktionen)
Phase 4: Validierung & Doku     [ERLEDIGT]  -> SPSS-Validierung, Vignetten, pkgdown
Phase 5: Post-Hoc & Ergaenz.    [OFFEN]     -> dunn_test(), pairwise Wilcoxon, Fisher, McNemar, GoF-Chi2
```

### 5.2 Phase 1: Skalenanalyse - **ERLEDIGT**

#### Tag 1: `reliability()` - Cronbachs Alpha - **ERLEDIGT**

**Datei:** `R/reliability.R`
**S3-Klasse:** `reliability`
**Print-Methode:** `print.reliability`

**Funktionssignatur:**
```r
reliability <- function(data, ..., weights = NULL, na.rm = TRUE)
```

**Algorithmus:**
1. Items auswaehlen via tidyselect (`...`)
2. Inter-Item-Korrelationsmatrix berechnen
3. Cronbachs Alpha: `alpha = (k / (k-1)) * (1 - sum(item_varianzen) / total_varianz)`
4. Fuer jedes Item: Alpha-if-Deleted, korrigierte Item-Total-Korrelation
5. Deskriptive Statistiken pro Item (M, SD, N)
6. Bei Gewichtung: Gewichtete Varianz/Kovarianz verwenden

**Rueckgabeobjekt:**
```r
list(
  alpha = numeric,               # Cronbachs Alpha
  alpha_standardized = numeric,   # Standardisiertes Alpha
  n_items = integer,              # Anzahl Items
  item_statistics = tibble,       # M, SD, N pro Item
  item_total = tibble,            # Korrigierte Item-Total-r, Alpha-if-Deleted
  inter_item_cor = matrix,        # Korrelationsmatrix
  scale_statistics = tibble,      # M, SD, Varianz der Gesamtskala
  variables = character,          # Item-Namen
  weights = character/NULL,       # Gewichtsvariable
  n = integer,                    # Stichprobengroesse
  effective_n = numeric           # Effektives N (bei Gewichtung)
)
```

**SPSS-Referenzoutput:**
```
RELIABILITY
  /VARIABLES=mj01 mj04
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL.
```

**Testdatei:** `tests/testthat/test-reliability-spss-validation.R`

---

#### Tag 2-3: `efa()` - Explorative Faktorenanalyse - **ERLEDIGT**

**Datei:** `R/efa.R`
**S3-Klasse:** `efa`
**Print-Methode:** `print.efa`

**Funktionssignatur:**
```r
efa <- function(data, ...,
                n_factors = NULL,        # NULL = Kaiser-Kriterium
                rotation = "varimax",    # "varimax", "oblimin", "none"
                extraction = "pca",      # "pca" (Hauptkomponenten)
                weights = NULL,
                use = "pairwise",        # Missing-Behandlung
                sort = TRUE,             # Ladungen sortieren
                blank = 0.40,            # Ladungen unter Schwelle ausblenden
                na.rm = TRUE)
```

**Algorithmus:**
1. Items auswaehlen via tidyselect
2. Korrelationsmatrix berechnen (mit Gewichtung, pairwise/listwise)
3. KMO-Mass und Bartlett-Test berechnen
4. Eigenwertzerlegung: `eigen(cor_matrix)`
5. Faktorenzahl bestimmen (Kaiser-Kriterium oder manuell)
6. Eigenvektoren extrahieren und skalieren
7. Rotation anwenden:
   - Varimax: `stats::varimax(loadings)`
   - Oblimin: `GPArotation::oblimin(loadings)` (optional)
8. Kommunalitaeten berechnen
9. Bei Oblimin: Pattern- und Strukturmatrix trennen

**Rueckgabeobjekt:**
```r
list(
  loadings = matrix,              # Faktorladungsmatrix (rotiert)
  eigenvalues = numeric,          # Alle Eigenwerte
  variance_explained = tibble,    # Erklaerte Varianz pro Faktor
  communalities = numeric,        # Kommunalitaeten
  kmo = list(overall, per_item),  # KMO-Mass
  bartlett = list(chi_sq, df, p), # Bartlett-Test
  rotation = character,           # Rotationstyp
  extraction = character,         # Extraktionstyp
  n_factors = integer,            # Anzahl extrahierter Faktoren
  correlation_matrix = matrix,    # Zugrundeliegende Korrelationsmatrix
  pattern_matrix = matrix/NULL,   # Pattern Matrix (nur Oblimin)
  structure_matrix = matrix/NULL, # Structure Matrix (nur Oblimin)
  factor_correlations = matrix/NULL, # Faktorkorrelationen (nur Oblimin)
  variables = character,
  weights = character/NULL,
  n = integer,
  effective_n = numeric,
  sort = logical,
  blank = numeric
)
```

**SPSS-Referenzoutput:**
```
FACTOR
  /VARIABLES ma01b to ma04 mj01 mj04 mm01 mm02r mm03 mm04 mm05r mm06
  /MISSING PAIRWISE
  /PRINT UNIVARIATE INITIAL CORRELATION SIG EXTRACTION ROTATION
  /FORMAT SORT BLANK(.40)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION VARIMAX
  /METHOD=CORRELATION.
```

---

#### Tag 3-4: `scale_index()` und `pomps()` - **ERLEDIGT**

**Datei:** `R/scale_helpers.R`

**`scale_index()` Signatur:**
```r
scale_index <- function(data, ..., min_valid = NULL, na.rm = TRUE)
```
- Berechnet Mittelwertindex ueber ausgewaehlte Items
- `min_valid`: Mindestanzahl gueltiger Werte (analog SPSS `MEAN.x()`)
- Gibt Vektor zurueck (fuer Verwendung in `mutate()`)

**`pomps()` Signatur:**
```r
pomps <- function(data, ..., scale_min = NULL, scale_max = NULL, na.rm = TRUE)
```
- POMPS-Transformation: `((x - min) / (max - min)) * 100`
- Automatische Erkennung oder manuelle Angabe von min/max
- Gibt transformierten Data Frame zurueck

**Verwendungsbeispiel (SPSS-Stil):**
```r
# SPSS: compute m_Ausl = mean(ma02, ma03, ma04).
data <- data %>% mutate(m_Ausl = scale_index(., ma02, ma03, ma04))

# SPSS: compute v81p = ((v81 - 1) / 6) * 100.
data <- data %>% mutate(v81p = pomps(., v81, scale_min = 1, scale_max = 7))
```

---

### 5.3 Phase 2: Regressionsanalyse - **ERLEDIGT**

#### Tag 5-7: `linear_regression()` - Lineare Regression - **ERLEDIGT**

**Datei:** `R/linear_regression.R`
**S3-Klasse:** `linear_regression`
**Print-Methode:** `print.linear_regression`

**Funktionssignatur:**
```r
linear_regression <- function(data, formula = NULL,
                              dependent = NULL, predictors = NULL,
                              weights = NULL,
                              standardized = TRUE,
                              conf.level = 0.95)
```

Zwei Interface-Optionen:
1. **Formel-Interface:** `linear_regression(data, income ~ age + education)`
2. **SPSS-Stil:** `linear_regression(data, dependent = income, predictors = c(age, education))`

**Rueckgabeobjekt:**
```r
list(
  coefficients = tibble,     # B, SE, Beta, t, p, CI
  anova_table = tibble,      # SS, df, MS, F, p
  model_summary = tibble,    # R, R^2, adj. R^2, SE
  residuals = numeric,       # Residuen (fuer Diagnostik)
  fitted = numeric,          # Vorhergesagte Werte
  formula = formula,
  dependent = character,
  predictors = character,
  weights = character/NULL,
  n = integer,
  lm_object = lm             # Originales lm-Objekt fuer Erweiterbarkeit
)
```

**SPSS-Referenzoutput:**
```
REGRESSION
  /MISSING PAIRWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT m_Anti
  /METHOD=ENTER pa01 age educr.
```

---

#### Tag 7-8: `logistic_regression()` - Logistische Regression - **ERLEDIGT**

**Datei:** `R/logistic_regression.R`
**S3-Klasse:** `logistic_regression`
**Print-Methode:** `print.logistic_regression`

**Funktionssignatur:**
```r
logistic_regression <- function(data, formula = NULL,
                                dependent = NULL, predictors = NULL,
                                weights = NULL,
                                conf.level = 0.95)
```

**Rueckgabeobjekt:**
```r
list(
  coefficients = tibble,        # B, SE, Wald, df, p, Exp(B), CI
  model_summary = tibble,       # -2LL, Cox-Snell R^2, Nagelkerke R^2, McFadden R^2
  classification_table = tibble, # Trefferquote
  hosmer_lemeshow = list/NULL,  # Goodness-of-fit (optional)
  formula = formula,
  dependent = character,
  predictors = character,
  weights = character/NULL,
  n = integer,
  glm_object = glm             # Originales glm-Objekt
)
```

---

### 5.4 Phase 3: Erweiterungen - **ERLEDIGT**

#### Tag 9: Entfaellt - 3-Wege-Kreuztabellen

> **Entscheidung (2026-02-26):** 3-Wege-Kreuztabellen sind bereits ueber
> `group_by(schicht) %>% crosstab(row, col)` realisierbar. Eine Erweiterung
> von `crosstab()` mit einem eigenen `layer`-Parameter ist nicht noetig und
> wuerde gegen das tidyverse-Designprinzip verstossen (Gruppierung gehoert
> in `group_by()`, nicht in Funktionsparameter).
>
> SPSS: `CROSSTABS /TABLES=pv01r BY eastwest BY sexr`
> mariposa: `data %>% group_by(sexr) %>% crosstab(row = pv01r, col = eastwest)`

#### Tag 9-10: Nicht-parametrische Tests - **ERLEDIGT (v0.3.0)**

Alle vier nicht-parametrischen Tests wurden implementiert, SPSS-validiert und bestehen R CMD check:

| Funktion | Datei | Tests | SPSS-Validierungen | Strategie |
|---|---|---|---|---|
| `kruskal_wallis()` | `R/kruskal_wallis.R` | 187 | 105 | Hybrid (Base R + genuine gewichtete Impl.) |
| `wilcoxon_test()` | `R/wilcoxon_test.R` | 109 | 82 | Hybrid (Base R + genuine gewichtete Impl.) |
| `friedman_test()` | `R/friedman_test.R` | 129 | 77 | Hybrid (Base R + genuine gewichtete Impl.) |
| `binomial_test()` | `R/binomial_test.R` | 89 | 30 | Wrapper um `stats::binom.test()` |

**Gesamt v0.3.0: 514 Tests, 294 SPSS-Validierungen**

Alle Funktionen unterstuetzen:
- Ungewichtete und gewichtete Analyse (SPSS-kompatible Frequenzgewichte)
- `group_by()`-Integration
- Multi-Variable-Support (ausser `friedman_test()` wo `...` die Messzeitpunkte sind)
- Professionelle Print-Methoden im SPSS-Stil
- Effektstaerken (Eta-Quadrat, r, Kendalls W)

---

### 5.5 Phase 4: Validierung und Dokumentation - **ERLEDIGT**

Alle Dokumentations-Updates fuer v0.3.0 abgeschlossen:

**Aktualisierte Dateien:**
- `DESCRIPTION` - Version 0.3.0, 37 Funktionen, pkgdown-URL
- `NEWS.md` - Eintraege fuer v0.2.0 und v0.3.0
- `README.md` - Erweiterte Funktionstabelle, neue Beispiele
- `_pkgdown.yml` - Neue Referenz-Sektionen (Scale Analysis, Regression, Non-Parametric)
- `.claude/CLAUDE.md` - Korrigierte Funktionslisten, aktualisierte Version History

**Neue Dateien:**
- `vignettes/scale-analysis.Rmd` - Reliability, EFA, scale_index, pomps
- `vignettes/regression-analysis.Rmd` - Lineare und logistische Regression

**pkgdown-Site:** Erfolgreich gebaut und deployed.

---

### 5.6 Phase 5: Post-Hoc-Oekosystem und Ergaenzungstests (v0.4.0)

#### Gap-Analyse: Nicht-parametrische Post-Hoc-Luecke

Der parametrische Analysepfad ist komplett:
```
oneway_anova() → tukey_test() / scheffe_test()   ✓ Post-Hoc
oneway_anova() → levene_test()                    ✓ Annahme-Check
t_test()       → levene_test()                    ✓ Annahme-Check
```

Der nicht-parametrische Pfad endet nach dem Omnibus-Test:
```
kruskal_wallis() → ???   (kein Dunn-Test, keine pairwise Mann-Whitney)
friedman_test()  → ???   (keine pairwise Wilcoxon-Vergleiche)
```

#### 5.6.1 `dunn_test()` - Post-Hoc fuer Kruskal-Wallis - **ERLEDIGT**

**Datei:** `R/dunn_test.R`
**S3-Klasse:** `dunn_test`
**S3-Dispatch:** `dunn_test.kruskal_wallis`
**Print-Methode:** `print.dunn_test`

**Begruendung:**
- Nicht-parametrisches Aequivalent zu Tukey/Scheffe
- SPSS bietet "Pairwise Comparisons" nach signifikantem Kruskal-Wallis automatisch an
- Ohne `dunn_test()` muessen User manuell paarweise Mann-Whitney-Tests mit Bonferroni-Korrektur durchfuehren

**Strategie:** Genuine Implementierung
- Dunn's Test basiert auf Rangdifferenzen und Z-Statistiken
- P-Wert-Korrektur: Bonferroni (Standard), Holm, Benjamini-Hochberg
- Keine neue Dependency noetig

**Funktionssignatur:**
```r
dunn_test <- function(x, ...) UseMethod("dunn_test")

dunn_test.kruskal_wallis <- function(x, p_adjust = "bonferroni", ...)
```

**Rueckgabeobjekt:**
```r
list(
  comparisons = tibble,      # Gruppe1, Gruppe2, Z, p, p_adj, Signifikanz
  p_adjust_method = character,
  n_comparisons = integer,
  variable = character,
  group = character,
  n = integer
)
```

**SPSS-Referenz:**
```
NPAR TESTS /K-W = AV BY UV(1,3)
  → "Pairwise Comparisons" → Dunn-Bonferroni
```

**Workflow:**
```r
result <- kruskal_wallis(data, score, group = treatment)
result %>% dunn_test()                          # Standard: Bonferroni
result %>% dunn_test(p_adjust = "holm")         # Alternative Korrektur
```

#### 5.6.2 Pairwise Wilcoxon nach Friedman - **ERLEDIGT**

**Option A:** Eigene `pairwise_wilcoxon()` S3-Methode auf `friedman_test`
**Option B:** `dunn_test.friedman_test` (Nemenyi-Test als Friedman-Post-Hoc)

**Empfehlung:** Option A - passt besser zur SPSS-Logik ("Pairwise Comparisons" nach Friedman verwendet Wilcoxon mit Bonferroni)

**Funktionssignatur (Option A):**
```r
pairwise_wilcoxon <- function(x, ...) UseMethod("pairwise_wilcoxon")

pairwise_wilcoxon.friedman_test <- function(x, p_adjust = "bonferroni", ...)
```

**Workflow:**
```r
result <- friedman_test(data, score_T1, score_T2, score_T3)
result %>% pairwise_wilcoxon()
```

#### 5.6.3 `fisher_test()` - Exakter Test **ERLEDIGT**

**Datei:** `R/fisher_test.R`
**Strategie:** Wrapper um `stats::fisher.test()`
**Tests:** 16 PASS (test-fisher-test-spss-validation.R)

#### 5.6.4 `mcnemar_test()` - Abhaengige Proportionen **ERLEDIGT**

**Datei:** `R/mcnemar_test.R`
**Strategie:** Manuelle Implementierung mit exaktem Binomialtest
**Tests:** 19 PASS (test-mcnemar-test-spss-validation.R)

#### 5.6.5 `chisq_gof()` - Chi-Quadrat Goodness-of-Fit **ERLEDIGT**

**Datei:** `R/chisq_gof.R`
**Strategie:** Eigene Implementierung mit Einzel-Stichproben-Logik, Multi-Variable-Support, Custom Expected Proportions
**Tests:** 53 PASS (test-chisq-gof-spss-validation.R)

#### 5.6.6 UX-Fix: Konsistente S3-Fehlermeldungen (Prioritaet NIEDRIG)

`levene_test()` auf `mann_whitney` gibt eine hilfreiche Fehlermeldung.
Aber auf `kruskal_wallis`, `wilcoxon_test` oder `friedman_test` kommt ein generischer S3-Dispatch-Fehler.

**Fix:** `levene_test.kruskal_wallis`, `levene_test.wilcoxon_test`, `levene_test.friedman_test` als informative Fehlermethoden analog zu `levene_test.mann_whitney`.

---

#### 5.6.7 Priorisierte Reihenfolge Phase 5

| # | Funktion | Strategie | Prioritaet | Abhaengigkeit |
|---|---|---|---|---|
| 1 | `dunn_test()` | Genuin + S3 auf `kruskal_wallis` | **HOCH** | **ERLEDIGT** |
| 2 | `pairwise_wilcoxon()` | Genuin + S3 auf `friedman_test` | **HOCH** | **ERLEDIGT** |
| 3 | `fisher_test()` | Wrapper (`fisher.test()`) | MITTEL | **ERLEDIGT** |
| 4 | `mcnemar_test()` | Wrapper (`mcnemar.test()`) | MITTEL | **ERLEDIGT** |
| 5 | `chisq_gof()` | Wrapper (`chisq.test()`) | MITTEL | **ERLEDIGT** |
| 6 | S3-Fehlermethoden | Fix | NIEDRIG | **ERLEDIGT** |

---

## 6. Priorisierte Reihenfolge

| # | Funktion | Strategie | Abhaengigkeit | Status |
|---|---|---|---|---|
| 1 | `reliability()` | Genuin | keine | **v0.2.0 - ERLEDIGT** |
| 2 | `efa()` | Hybrid | `GPArotation`, `MASS` (Suggests) | **v0.2.0 - ERLEDIGT** |
| 3 | `scale_index()` / `pomps()` | Genuin | keine | **v0.2.0 - ERLEDIGT** |
| 4 | `linear_regression()` | Wrapper (`lm()`) | keine | **v0.2.0 - ERLEDIGT** |
| 5 | `logistic_regression()` | Wrapper (`glm()`) | keine | **v0.2.0 - ERLEDIGT** |
| ~~6~~ | ~~`crosstab()` 3-Wege~~ | ~~Erweiterung~~ | — | Entfaellt |
| 7 | `kruskal_wallis()` | Hybrid | keine | **v0.3.0 - ERLEDIGT** |
| 8 | `wilcoxon_test()` | Hybrid | keine | **v0.3.0 - ERLEDIGT** |
| 9 | `friedman_test()` | Hybrid | keine | **v0.3.0 - ERLEDIGT** |
| 10 | `binomial_test()` | Wrapper | keine | **v0.3.0 - ERLEDIGT** |
| 11 | Validierung + Doku | — | — | **v0.3.0 - ERLEDIGT** |
| 12 | `dunn_test()` | Genuin + S3 | keine | **v0.4.0 - ERLEDIGT** |
| 13 | `pairwise_wilcoxon()` | Genuin + S3 | keine | **v0.4.0 - ERLEDIGT** |
| 14 | `fisher_test()` | Wrapper (`fisher.test()`) | keine | **v0.4.0 - ERLEDIGT** |
| 15 | `mcnemar_test()` | Wrapper (`mcnemar.test()`) | keine | **v0.4.0 - ERLEDIGT** |
| 16 | `chisq_gof()` | Wrapper (`chisq.test()`) | keine | **v0.4.0 - ERLEDIGT** |
| 17 | S3-Fehlermethoden (UX) | Fix | keine | **v0.4.0 - ERLEDIGT** |

---

## 7. Technische Hinweise

### 7.1 Namenskonventionen

Alle neuen Funktionen folgen den bestehenden Konventionen:
- **snake_case** fuer Funktionsnamen
- **Keine Praefixe** fuer Hauptfunktionen (wie `describe()`, nicht `mp_describe()`)
- **S3-Klassen** tragen den Funktionsnamen (z.B. Klasse `reliability`)
- **Print-Methoden** via `print.{klasse}()`

### 7.2 Parameter-Konsistenz

Alle neuen Funktionen folgen dem bestehenden Parameter-Schema:
```r
funktion <- function(data, ..., weights = NULL, na.rm = TRUE, ...)
```
- `data`: Immer erster Parameter
- `...`: Variablenauswahl via tidyselect
- `weights`: Optionale Gewichtung
- Weitere testspezifische Parameter

### 7.3 Rueckgabe-Konsistenz

Alle neuen Funktionen geben S3-Objekte (Listen) zurueck mit:
- Konsistentem `class`-Attribut
- `variables`, `weights`, `n`-Feldern
- Print-Methode im SPSS-Stil

### 7.4 Aenderungen an DESCRIPTION

```
Version: 0.2.0
Suggests:
    ...(bestehend)...,
    GPArotation
```

### 7.5 Aenderungen an NAMESPACE

Neue Exports:
```r
export(reliability)
export(efa)
export(scale_index)
export(pomps)
export(linear_regression)
export(logistic_regression)

S3method(print, reliability)
S3method(print, efa)
S3method(print, linear_regression)
S3method(print, logistic_regression)
```

---

## 8. Risiken und Mitigationsstrategien

| Risiko | Wahrscheinlichkeit | Auswirkung | Mitigation |
|---|---|---|---|
| SPSS-Validierung nicht moeglich | Mittel | Hoch | Validierung gegen `psych`, `car`, `stats` als Zwischenschritt |
| Gewichtete EFA weicht von SPSS ab | Mittel | Mittel | SPSS-Dokumentation zu gewichteter PCA konsultieren |
| `GPArotation` nicht auf CRAN | Sehr niedrig | Mittel | Ist ein stabiles, etabliertes Paket |
| Oblimin-Ergebnisse numerisch instabil | Niedrig | Mittel | Delta-Parameter konfigurierbar machen |
| Standardisierte Beta in Regression | Niedrig | Niedrig | Formel: `beta = b * (sd_x / sd_y)` - simpel |

---

## 9. Erfolgskriterien

### v0.2.0 + v0.3.0 (ERFUELLT)

1. **Funktionalitaet**: 10 neue Funktionen implementiert und exportiert (37 gesamt) ✓
2. **SPSS-Kompatibilitaet**: 2.227+ Tests bestehen innerhalb der Toleranzen ✓
3. **Tests**: 294 neue SPSS-Validierungen (v0.3.0) ✓
4. **Dokumentation**: Alle Funktionen mit roxygen2-Doku und Beispielen ✓
5. **Konsistenz**: Alle Funktionen folgen konsistenten API-Patterns ✓
6. **Dependencies**: 2 optionale Suggests (`GPArotation`, `MASS`) ✓
7. **Vignetten**: 2 neue Vignetten (scale-analysis, regression-analysis) ✓
8. **pkgdown**: Vollstaendige Referenz-Site mit allen 37 Funktionen ✓

### v0.4.0 (IN BEARBEITUNG)

1. **Funktionalitaet**: 5 neue Funktionen + S3-Fehlermethoden
2. **Post-Hoc-Oekosystem**: Nicht-parametrischer Pfad komplett (KW → Dunn, Friedman → pairwise Wilcoxon)
3. **SPSS-Kompatibilitaet**: SPSS-Validierung fuer alle neuen Funktionen
4. **Konsistenz**: S3-Dispatch-Kette analog zum parametrischen Pfad

**Zusaetzliche Verbesserungen (v0.4.0):**
- `linear_regression()`: Neuer Parameter `use = c("listwise", "pairwise")` fuer SPSS-kompatible Pairwise Deletion (analog SPSS `/MISSING PAIRWISE`). Regression wird ueber paarweise Kovarianzmatrix berechnet.

---

## 10. Langfristige Roadmap (nach v0.2.0)

Fuer zukuenftige Versionen:

| Version | Funktionen | Status |
|---|---|---|
| v0.2.0 | Reliability, EFA, Scale, Regression (linear + logistisch) | **ERLEDIGT** |
| v0.3.0 | Kruskal-Wallis, Wilcoxon, Friedman, Binomial-Test + Doku-Update | **ERLEDIGT** (37 Funktionen, 2.227+ Tests) |
| v0.4.0 | Post-Hoc-Oekosystem: `dunn_test()`, `pairwise_wilcoxon()`, `fisher_test()`, `mcnemar_test()`, `chisq_gof()` | **ERLEDIGT** (42 Funktionen, 3.491+ Tests) |
| v0.5.0 | Mehrfaktorielle ANOVA, Kovarianzanalyse (ANCOVA) | Offen |
| v0.6.0 | Clusteranalyse, Diskriminanzanalyse | Offen |
| v1.0.0 | CRAN-Submission, vollstaendige Dokumentation | Offen |

---

*Dieses Dokument wird fortlaufend aktualisiert.*
*Zuletzt aktualisiert: 2026-03-01*
