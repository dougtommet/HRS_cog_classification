# letter.css – Quarto HTML Stylesheet for US Letter Documents

A CSS stylesheet for rendering Quarto Markdown documents as US Letter-sized (8.5" × 11") print-ready HTML with footnotes anchored to their originating pages, self-contained embedding, and zero external CDN dependencies.

## Features

- **US Letter Format** — 8.5" × 11" with 1.0" margins on all sides
- **Footnotes on Originating Pages** — Endnotes automatically relocate to the page where they're referenced, not collected at document end
- **References on Their Own Page** — Place an explicit `#refs` div inside a `.page` block to control exactly where the bibliography renders
- **Print-to-PDF Optimized** — Tested with Chrome headless and standard browser print dialogs
- **Self-Contained** — All fonts and resources embedded; no external CDN loads. Safe for AWS hosting.
- **LaTeX-Style Text Sizing** — Inline span and block div classes for `HUGE`, `large`, `small`, `tiny`, etc.
- **Academic Formatting** — 11pt Arial body text, 0.5" abstract margins (1.5" total inset), 8pt footnotes and code
- **Table of Contents** — Fixed-position sidebar TOC on screen; in print/PDF, a copy appears on page 1 with clickable links to each section
- **Clean Screen Simulation** — Stacked Letter sheets on gray background for realistic preview

## Basic YAML Setup

```yaml
---
title: "Your Document Title"
author: "Author Name"
date: 2026-09-25
format:
  html:
    css: "path/to/letter.css"
    theme: none
    embed-resources: true
    toc: true
    toc-location: right
    include-after-body: "path/to/letter_css_scripts.html"
    html-math-method: mathjax
editor: source
---
```

### Key YAML Details

- **`css: "path/to/letter.css"`** — Points to this stylesheet
- **`theme: none`** — Disables Quarto's default theme so letter.css takes full control
- **`embed-resources: true`** — Self-contained HTML; all CSS, images, fonts inline
- **`toc-location: right`** — Positions table of contents as a fixed sidebar on screen
- **`include-after-body: "path/to/letter_css_scripts.html"`** — Required for footnote relocation script
- **`html-math-method: mathjax`** — Enables MathJax rendering for LaTeX math (`$...$` inline, `$$...$$` display). Essential when `theme: none` is used; otherwise MathJax may not load

## Important Quirks & Workarounds

### 1. Title Block Relocated onto Page 1

Quarto auto-generates a `#title-block-header` element from your YAML `title`/`author`/`date` fields. It renders as a sibling of the `.page` divs (not nested inside them), so **a script (`letter_css_scripts.html`) moves its children (`h1.title`, `p.author`, `p.date`) into the start of the first `.page` at render time, then removes the now-empty `#title-block-header` wrapper** — no need to retype metadata in the markdown body, and YAML stays the single source of truth.

```markdown
---
title: "Your Document Title"
author: "Author Name"
date: 2026-09-25
format:
  html:
    css: "path/to/letter.css"
    theme: none
    embed-resources: true
    toc: true
    toc-location: right
    include-after-body: "path/to/letter_css_scripts.html"
    html-math-method: mathjax
---

::: {.page}
## First Content Heading
Body text starts here — no need to retype the title/author/date.
:::
```

**Requires JavaScript** (same as footnote relocation, below). If JS doesn't run, the title block will render unstyled, outside the page stack.

> **CSS gotcha:** Because the `#title-block-header` wrapper is *removed*, not just relocated, any styling for the title/author/date must target `.page > h1.title`, `.page > p.author`, and `.page > p.date` directly — selectors scoped under `#title-block-header` will silently never match once the script runs. Title renders bold, left-aligned, at normal body size (11pt, 1.2 line height); author/date render at 11pt with 1.2 line height and zero margin between them for tight spacing.

### 2. Page Structure

All content must be wrapped in `::: {.page}` blocks (Quarto divs). Each block renders as one Letter-sized sheet:

```markdown
::: {.page}
# Page 1 Content
Content here...
:::

::: {.page}
# Page 2 Content
Content here...
:::
```

### 3. Footnotes Require Script

Footnotes work through a companion script file (`letter_css_scripts.html`). Without it, footnotes collect at document end (off the page). The script must be included via `include-after-body` in YAML.

### 4. Bibliography / References Placement

Unlike footnotes, Pandoc's citeproc does **not** need a relocation script. Add `bibliography: your-file.bib` to the YAML, cite sources with `[@key]`, and place an explicit `#refs` div inside its own `.page` block wherever you want the reference list to render — citeproc fills that div in place instead of always appending it at the very end of the document:

```markdown
::: {.page}
## References

::: {#refs}
:::
:::
```

A single-heading `.page` like this gets promoted to `<section class="page">` (see Quirk #6); it still renders as its own Letter sheet, and `## References` is picked up by the manual TOC script since it's an `h2` inside `.page`.

### 5. Math Rendering Requires MathJax

When using `theme: none`, Quarto does not automatically load MathJax. To render LaTeX math (`$n = \frac{16}{ES^2}$` inline or `$$ES = \frac{4}{\sqrt{n}}$$` display), add `html-math-method: mathjax` to the YAML frontmatter:

```yaml
format:
  html:
    theme: none
    html-math-method: mathjax
```

Without this setting, LaTeX delimiters render as literal text. MathJax is embedded into the self-contained HTML file when `embed-resources: true`.

### 6. Table of Contents Is Built Manually (h2/h3 Only)

Pandoc's native `--toc` only scans headings that are direct children of the document body; headings nested inside a div — including `.page` — are invisible to it, so `toc: true` alone renders an empty sidebar. To work around this, `letter_css_scripts.html` includes a script that scans `.page h2, .page h3` after render and builds its own `#TOC` nav into `#quarto-margin-sidebar`, reusing the same selectors `letter.css` already styles.

A related Pandoc behavior: when a `.page` div's first block is its *only* top-level heading, Pandoc promotes the div to `<section class="page">` and moves the heading's id onto the section. Pages with several `##` headings stay plain `<div class="page">`s. Pandoc's native TOC sees the promoted sections but not the plain divs, so it can silently list only some pages. The script therefore always discards Pandoc's TOC and rebuilds it from every page, falling back to the section's id for promoted headings.

This means:
- Only `##` (h2) and `###` (h3) headings appear in the TOC.
- The document `h1` (the title, relocated per Quirk #1) is intentionally excluded — it's shown above the TOC, not inside it.
- `####` (h4) headings and deeper are **not** picked up; extend the selector in `letter_css_scripts.html` if you need them.

The sidebar TOC is screen-only. For the TOC in printed output, see [Table of Contents in the PDF](#table-of-contents-in-the-pdf).

## Custom Classes

### Text Sizing

Available sizes: `.HUGE` (36pt), `.huge` (24pt), `.LARGE` (16pt), `.Large` (14pt), `.large` (12pt), `.small` (9pt), `.footnotesize` (8pt), `.tiny` (6pt).

Each size class works two ways:

**Inline span** — wrap a run of text (or an entire paragraph's text) in a bracketed span. The class lands on a `<span>`, which has no competing font-size rule, so it applies directly:

```markdown
[HUGE text]{.HUGE}
[Regular text]{.large}
[small text]{.small}
[tiny text]{.tiny}
```

**Block div** — wrap one or more paragraphs in a fenced div to size everything inside it:

```markdown
::: {.large}
This whole paragraph, and any others in this div, render at 12pt.
:::
```

> **CSS note:** the base rule `body, p { font-size: 11pt; }` targets `<p>` directly, which would otherwise override a size class inherited from a wrapping div. `letter.css` avoids this by pairing each class with an explicit descendant rule (e.g. `.large, .large p { font-size: 12pt; }`), so both the inline-span and block-div forms work.

### Other Utility Classes

- **`.abstract`** — Indented summary block with 1.5" left/right inset (0.5" additional margin beyond page padding)
- **`.center`** — Centered text and blocks with 1.5em top/bottom margin
- **`.compact`** — Dense 9pt text for data, metadata, notes
- **`.page-footnotes`** — Auto-generated container for page-specific footnotes (handled by script)

## Print & PDF Export

### Browser Print to PDF

```bash
# Example: Chrome headless
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --headless=new \
  --disable-gpu \
  --no-pdf-header-footer \
  --print-to-pdf="output.pdf" \
  "file://$(pwd)/document.html"
```

**Tips:**
- Use `--no-pdf-header-footer` to suppress browser chrome
- Disable custom headers/footers in browser print settings
- Set margins to 0 (the stylesheet handles page margins)

### Browser Print Dialog

1. Open HTML in Chrome/Firefox/Safari
2. Print → Save as PDF
3. Set margins to **None** or **0**
4. Disable headers/footers
5. Print

### Table of Contents in the PDF

The floating sidebar TOC is for screen viewing only and is hidden when you print. In its place, printed and PDF output get a TOC on page 1, just below the title, author, and date:

- **What's in it:** the same entries as the sidebar, under the heading "Contents": every `##` heading, with `###` headings indented beneath it.
- **Clickable links:** each entry links to its heading. Chrome's Print to PDF (headless or the print dialog) keeps these as internal links, so clicking an entry in the PDF jumps to that section. Other browsers and PDF engines may drop the links, leaving a plain list.
- **Nothing to set up:** it's built by `letter_css_scripts.html` (via `include-after-body`) together with `letter.css`. It doesn't appear on screen, only in print.

**Placing it elsewhere.** To put the TOC somewhere other than the top of page 1, for example on a page of its own, add an empty `#print-toc` div where you want it. The script fills that div instead of adding one to page 1:

```markdown
::: {.page}
::: {#print-toc}
:::
:::
```

**Turning it off.** Hide it with a rule in your own stylesheet, listed after `letter.css` (e.g. `css: ["letter.css", "custom.css"]`):

```css
@media print {
  #print-toc { display: none; }
}
```

**Limitations:**

- **No page numbers:** Chrome doesn't support CSS `target-counter()`, and a `.page` block can run onto more than one printed sheet, so page numbers can't be filled in ahead of time.
- **Page 1 gets fuller:** the TOC takes space on page 1, so figures and tables (which the stylesheet won't split across pages) may move to page 2. Placing the TOC on its own page avoids this.

## Screen vs. Print Rendering

- **Screen (@media screen)** — Gray background, drop shadows, visual spacing between sheets, fixed TOC sidebar, page numbers
- **Print (@media print)** — Clean white pages, no shadows/borders, native print engine pagination, sidebar TOC hidden and the page-1 TOC shown instead

## Dependencies

- **letter.css** — Main stylesheet (this file)
- **letter_css_scripts.html** — JavaScript for title relocation, per-page footnotes, and the TOC (required for all three)
- **Quarto** — Version 1.2+

## Browser & PDF Engine Support

- ✅ Chrome (headless and interactive)
- ✅ Firefox
- ✅ Safari
- ✅ Print to PDF (all modern browsers)

## Limitations

- Footnotes appear at *end of page content*, not pixel-perfect anchored to physical page bottom (browser print engines don't reliably support CSS footnote floats)
- Page numbering uses CSS `counter(page)` in @page rule (native print engine), visible only in printed output, not on screen preview
- External SVG/image URLs will break self-containment; embed all graphics inline or use base64 data URIs

## Examples

### Minimal Document

```markdown
---
title: "Report Title"
author: "Author Name"
format:
  html:
    css: "letter.css"
    theme: none
    embed-resources: true
    include-after-body: "letter_css_scripts.html"
---

::: {.page}

::: {.abstract}
This is the abstract/summary.
:::

## Section 1

Body text here with a footnote[^1].

[^1]: Footnote text appears on this same page.

:::
```

### Multi-Page with Sizing

```markdown
::: {.page}

## Introduction

[Large introduction]{.LARGE}

Body text...

:::

::: {.page}

## Page 2

[Smaller section heading]{.Large}

::: {.center}
[Centered, tiny label]{.tiny}
:::

:::
```

## License

MIT License — see [LICENSE](LICENSE) for the full text.
