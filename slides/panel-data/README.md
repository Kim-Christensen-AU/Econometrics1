# Panel Data Deck Conversion

Converted source:

- `8 - Panel Data Models.tex`

Original copied PDF:

- `original_8 - Panel Data Models.pdf`

## What Changed

- Replaced the Aarhus `Aarhus` Beamer theme with Kim Christensen's usual `Berlin`/`dove` Beamer setup.
- Added the maroon headline and custom three-part footline used in the OLS/IV/ML decks.
- Replaced the automatic table-of-contents section slides with Kim-style plain maroon section divider frames.
- Replaced the title page metadata with Kim Christensen / Econometrics I / Panel Data Models.
- Added a hand-built outline slide in the same style as the OLS and IV decks.
- Copied in `ee.sty` and the BSS logo assets so the folder is self-contained.
- Capped over-wide figure widths at `\textwidth` to better fit the default Beamer aspect ratio.

## Not Yet Done

- The PDF has not been rebuilt in this Codex environment because no TeX executable (`pdflatex`, `latexmk`, `xelatex`, or `lualatex`) is available on PATH.
- After compiling locally, inspect dense equation slides for vertical overflow caused by moving from Morten's 16:9 AU-theme deck to Kim's default Beamer format.

