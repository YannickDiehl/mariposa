# Creating the Hex Logo for mariposa

To create a hex sticker version of the mariposa logo for R package conventions:

## Prerequisites
Install the required package:
```r
install.packages("hexSticker")
```

## Create the Hex Logo
Run the script:
```r
source("inst/scripts/create_hex_logo.R")
```

This will create a hex sticker at `man/figures/logo-hex.png` that follows R package conventions.

## Alternative: Manual Creation
If you prefer to create the hex logo manually or with other tools:
- Target size: 2 inches wide
- Format: PNG with transparent background
- Place at: `man/figures/logo-hex.png`
- The logo is already set up in the README to display correctly