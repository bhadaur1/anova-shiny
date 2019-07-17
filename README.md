# anova-shiny
A shiny web application to perform 2-way ANOVA, pairwise t-tests and display bar charts

### TODO:
 * Check Fisher LSD instead of pairwise t test
 * ~~Implement support for dataset without row factors (one way ANOVA etc)
 * ~~Plot tab should offer to download
 * geom_text in barplot in the center
 * Action button "Calculate" should automatically freeze

### Screenshots:

#### Landing page -- input data here

![Landing/Data Input tab](/Screenshots/Landing-page-input-data.png?raw=true "Landing/Data Input tab")

#### Freeze inputs after specifying row/column factors to avoid reset

![Freeze inputs](/Screenshots/Freeze-inputs.png?raw=true "Freeze inputs")

#### Hit calculate to get outputs, sample ANOVA output

![ANOVA output tab](/Screenshots/ANOVA-output.png?raw=true "ANOVA output tab")

#### Sample pairwise t-test output

![Pairwise t-test output tab](/Screenshots/Pairwise-t-test-output.png?raw=true "Pairwise t-test output tab")

#### Barplot using ggplot

![Barchart output tab](/Screenshots/Barchart-output.png?raw=true "Barchart output tab")
