# mariposa - Strategischer Implementierungsplan
## Erweiterung des Funktionsumfangs auf Basis der SPSS-Kurs-Syntax

**Erstellt:** 2026-02-26
**Autor:** Yannick Diehl / Claude
**Basis:** SPSS-Syntax "Statistik mit SPSS" (WS 25/26, Uni Marburg, A. Heyder)
**Referenzdiagramm:** Methodentaxonomie "Art der Analyse"

---

## 1. Ausgangslage

### 1.1 Aktueller Funktionsumfang (v0.1.0)

mariposa umfasst aktuell **27 exportierte Funktionen** in 6 Kategorien:

| Kategorie | Funktionen | Anzahl |
|---|---|---|
| Deskriptive Statistik | `describe()`, `frequency()`, `crosstab()` | 3 |
| Hypothesentests | `t_test()`, `oneway_anova()`, `mann_whitney()`, `chi_square()` | 4 |
| Korrelationsanalyse | `pearson_cor()`, `spearman_rho()`, `kendall_tau()` | 3 |
| Post-Hoc Analyse | `tukey_test()`, `scheffe_test()`, `levene_test()` | 3 |
| Gewichtete Statistiken | `w_mean()`, `w_median()`, `w_sd()`, `w_var()`, `w_se()`, `w_iqr()`, `w_range()`, `w_quantile()`, `w_modus()`, `w_skew()`, `w_kurtosis()` | 11 |
| Effektstaerke-Hilfsfunktionen | `phi()`, `cramers_v()`, `goodman_gamma()` | 3 |

**Zusaetzlich vorhanden** (in CLAUDE.md erwaehnt, nicht in der aktuellen Analyse bestaetigt):
- `rm_t_test()` - Repeated-Measures t-Test
- `rm_anova_test()` - Repeated-Measures ANOVA
- `emmeans()` - Estimated Marginal Means
- `mauchly_test()` - Spharizitaetstest

### 1.2 Aktuelle Dependencies

**Imports (8 Pakete):** cli, stats, utils, dplyr, rlang, tidyselect, tibble, tidyr
**Suggests (5 Pakete):** testthat (>= 3.0.0), knitr, rmarkdown, covr, haven (>= 2.4.0)

### 1.3 Validierungsstatus

83/83 SPSS-Validierungstests bestanden. Toleranzen:
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

Das Referenzdiagramm "Art der Analyse" zeigt den vollstaendigen Methodenkanon. Folgende Verfahren fehlen **zusaetzlich** zur SPSS-Syntax:

| Kategorie | Verfahren | Skalenniveau | Prioritaet |
|---|---|---|---|
| **Unterschiede - Unabhaengig** | | | |
| | Kruskal-Wallis-Test | Ordinal | Mittel |
| | Mehrfaktorielle ANOVA | Intervall | Niedrig |
| **Unterschiede - Verbunden** | | | |
| | Wilcoxon-Vorzeichen-Rang-Test | Ordinal | Mittel |
| | Vorzeichentest | Ordinal | Niedrig |
| | Friedman-Test | Ordinal | Mittel |
| **Dependenzanalyse** | | | |
| | Binomial-Test | Nominal | Niedrig |
| **Interdependenzanalyse** | | | |
| | Clusteranalyse | Intervall | Niedrig |

### 2.3 Zusammenfassung der Luecken

**Aus der SPSS-Kurs-Syntax (Prioritaet HOCH):**
1. `reliability()` - Cronbachs Alpha mit Item-Statistiken
2. `efa()` - Explorative Faktorenanalyse (PCA, Varimax, Oblimin)
3. `scale_index()` / `pomps()` - Skalenbildung und Normierung
4. `linear_regression()` - Bivariate und multiple lineare Regression
5. `logistic_regression()` - Binaere logistische Regression
6. `crosstab()` Erweiterung - 3-Wege-Tabellen (AV BY UV BY Schicht)

**Aus dem Diagramm (Prioritaet MITTEL bis NIEDRIG):**
7. `kruskal_wallis()` - Nicht-parametrische ANOVA-Alternative
8. `wilcoxon_test()` - Nicht-parametrischer verbundener Test
9. `friedman_test()` - Nicht-parametrische RM-ANOVA-Alternative
10. `binomial_test()` - Test auf Proportionen
11. Mehrfaktorielle ANOVA
12. Clusteranalyse

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
Phase 1: Skalenanalyse          [~4 Tage]  -> reliability(), efa(), scale_index(), pomps()
Phase 2: Regressionsanalyse     [~4 Tage]  -> linear_regression(), logistic_regression()
Phase 3: Erweiterungen          [~3 Tage]  -> crosstab() 3-Wege, nicht-parametrische Tests
Phase 4: Validierung & Doku     [~2 Tage]  -> SPSS-Validierung, Vignetten, pkgdown
```

### 5.2 Phase 1: Skalenanalyse (4 Tage)

#### Tag 1: `reliability()` - Cronbachs Alpha

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

#### Tag 2-3: `efa()` - Explorative Faktorenanalyse

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

#### Tag 3-4: `scale_index()` und `pomps()`

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

### 5.3 Phase 2: Regressionsanalyse (4 Tage)

#### Tag 5-7: `linear_regression()` - Lineare Regression

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

#### Tag 7-8: `logistic_regression()` - Logistische Regression

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

### 5.4 Phase 3: Erweiterungen (3 Tage)

#### Tag 9: Entfaellt - 3-Wege-Kreuztabellen

> **Entscheidung (2026-02-26):** 3-Wege-Kreuztabellen sind bereits ueber
> `group_by(schicht) %>% crosstab(row, col)` realisierbar. Eine Erweiterung
> von `crosstab()` mit einem eigenen `layer`-Parameter ist nicht noetig und
> wuerde gegen das tidyverse-Designprinzip verstossen (Gruppierung gehoert
> in `group_by()`, nicht in Funktionsparameter).
>
> SPSS: `CROSSTABS /TABLES=pv01r BY eastwest BY sexr`
> mariposa: `data %>% group_by(sexr) %>% crosstab(row = pv01r, col = eastwest)`

#### Tag 9-10: Nicht-parametrische Tests (optional)

Falls zeitlich moeglich:
- `kruskal_wallis()` - Wrapper um `stats::kruskal.test()` mit mariposa-Formatting
- `wilcoxon_test()` - Wrapper um `stats::wilcox.test(paired = TRUE)` mit mariposa-Formatting
- `friedman_test()` - Wrapper um `stats::friedman.test()` mit mariposa-Formatting

Alle drei sind einfache Wrapper mit minimalem Aufwand.

---

### 5.5 Phase 4: Validierung und Dokumentation (2 Tage)

#### Tag 12: SPSS-Validierung

**Neue Testdateien:**
```
tests/testthat/test-reliability-spss-validation.R
tests/testthat/test-efa-spss-validation.R
tests/testthat/test-linear-regression-spss-validation.R
tests/testthat/test-logistic-regression-spss-validation.R
```

**Validierungsszenarien (pro Funktion):**
1. Ungewichtet, ungruppiert
2. Gewichtet, ungruppiert
3. Ungewichtet, gruppiert (wo sinnvoll)
4. Gewichtet, gruppiert (wo sinnvoll)

**Toleranzen (wie bestehend):**
- Koeffizienten: +/- 0.001
- Teststatistiken: +/- 0.00001
- P-Werte: +/- 0.0001
- R^2: +/- 0.001
- Faktorladungen: +/- 0.001

#### Tag 13: Dokumentation

**Neue/aktualisierte Dateien:**
1. `vignettes/scale-analysis.Rmd` - Faktorenanalyse und Reliabilitaet
2. `vignettes/regression-analysis.Rmd` - Lineare und logistische Regression
3. `vignettes/spss-translation-guide.Rmd` - SPSS-zu-R Uebersetzungshilfe
4. `_pkgdown.yml` - Neue Referenz-Sektionen
5. `NEWS.md` - Changelog
6. `DESCRIPTION` - Version 0.2.0, ggf. neue Suggests

**Neue pkgdown-Sektionen:**
```yaml
- title: "Scale Analysis"
  desc: "Factor analysis, reliability, and scale construction"
  contents:
  - efa
  - reliability
  - scale_index
  - pomps

- title: "Regression Analysis"
  desc: "Linear and logistic regression with SPSS-compatible output"
  contents:
  - linear_regression
  - logistic_regression
```

---

## 6. Priorisierte Reihenfolge

| # | Funktion | Strategie | Abhaengigkeit | Aufwand | Begruendung |
|---|---|---|---|---|---|
| 1 | `reliability()` | Genuin | keine | 1 Tag | Einfachste Funktion, hoher Nutzen, Grundlage fuer EFA |
| 2 | `efa()` | Hybrid | `GPArotation` (Suggests) | 2 Tage | Zentrales Element des Kurses |
| 3 | `scale_index()` / `pomps()` | Genuin | keine | 1 Tag | Bindeglied zwischen EFA und Regression |
| 4 | `linear_regression()` | Wrapper (`lm()`) | keine | 2-3 Tage | Bivariate + multiple Regression |
| 5 | `logistic_regression()` | Wrapper (`glm()`) | keine | 1-2 Tage | Abschluss der Kernfunktionalitaet |
| ~~6~~ | ~~`crosstab()` 3-Wege~~ | ~~Erweiterung~~ | — | — | Entfaellt: `group_by()` + `crosstab()` genuegt |
| 6 | Nicht-parametrische Tests | Wrapper | keine | 1-2 Tage | Nice-to-have |
| 7 | Validierung + Doku | — | SPSS-Zugang | 2 Tage | Qualitaetssicherung |

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

Die Erweiterung gilt als erfolgreich, wenn:

1. **Funktionalitaet**: Alle 5 neuen Kernfunktionen sind implementiert und exportiert
2. **SPSS-Kompatibilitaet**: Ergebnisse stimmen innerhalb der Toleranzen mit SPSS ueberein
3. **Tests**: Mindestens 20 neue SPSS-Validierungstests bestehen
4. **Dokumentation**: Alle Funktionen haben roxygen2-Dokumentation mit Beispielen
5. **Konsistenz**: Neue Funktionen fuegen sich nahtlos in das bestehende API-Design ein
6. **Dependencies**: Maximal 1 neue optionale Dependency (`GPArotation`)
7. **Vignetten**: Mindestens 2 neue Vignetten (Skalenanalyse, Regression)

---

## 10. Langfristige Roadmap (nach v0.2.0)

Fuer zukuenftige Versionen:

| Version | Funktionen | Prioritaet |
|---|---|---|
| v0.2.0 | Reliability, EFA, Scale, Regression (linear + logistisch) | **AKTUELL** |
| v0.3.0 | Kruskal-Wallis, Wilcoxon, Friedman, Binomial-Test | Mittel |
| v0.4.0 | Mehrfaktorielle ANOVA, Kovarianzanalyse (ANCOVA) | Mittel |
| v0.5.0 | Clusteranalyse, Diskriminanzanalyse | Niedrig |
| v1.0.0 | CRAN-Submission, vollstaendige Dokumentation | Hoch |

---

*Dieses Dokument wird fortlaufend aktualisiert.*
*Zuletzt aktualisiert: 2026-02-26*
