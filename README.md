# bbsvg

Author: Sebastian Kranz, Ulm University

`bbsvg` creates SVG figures in the style of graphs drawn on a blackboard in economics classes. A figure is built as a pipeline: create a plotting pane, add axes and graphical objects, and render the result as SVG or export it to another format.

The package is particularly useful for:

- supply-and-demand diagrams and other economic models;
- curves defined by equations or slopes;
- areas, markers, tangents, arrows, and annotations;
- lightweight mathematical notation in ordinary SVG text;
- full LaTeX labels rendered through `latexsvg`;
- data-driven diagrams and annotated time series.

## Installation from r-universe


```r
install.packages('bbsvg', repos = c('https://skranz.r-universe.dev', 'https://cloud.r-project.org'))

```


Full LaTeX labels use `latexsvg`. Conversion from SVG to PDF, PNG, or PostScript uses `rsvg`.

## Basic workflow

A `bbsvg` figure starts with `bb_pane()`. The default coordinate variables in curve equations are `x_` and `y_`.

```r
library(bbsvg)

bb = bb_pane(
  xrange = c(0, 10),
  yrange = c(0, 10),
  org.width = 520,
  org.height = 360
) %>%
  bb_xaxis(label = "Quantity", labelpos = "right") %>%
  bb_yaxis(label = "Price", labelpos = "top") %>%
  bb_curve(
    id = "demand",
    eq = "y_ = 10 - x_",
    color = "#8b1a1a"
  ) %>%
  bb_curve(
    id = "supply",
    eq = "y_ = 2 + 0.5 * x_",
    color = "#1f4e79"
  ) %>%
  bb_text(
    x = 8.6,
    y = 1.4,
    label = "D",
    color = "#8b1a1a"
  ) %>%
  bb_text(
    x = 8,
    y = 6,
    label = "S",
    color = "#1f4e79"
  )

view.bb(bb)
```

`view.bb()` opens an interactive preview. Calling `bb_to_svg(bb)` instead returns the SVG as a character string.

## Equilibrium markers and annotations

Coordinates can be computed in ordinary R code and then reused in several objects. The next example adds an equilibrium point and dashed marker lines.

```r
q_eq = 16 / 3
p_eq = 14 / 3

bb = bb_pane(
  xrange = c(0, 10),
  yrange = c(0, 10),
  org.width = 520,
  org.height = 360
) %>%
  bb_xaxis(label = "Quantity", labelpos = "right") %>%
  bb_yaxis(label = "Price", labelpos = "top") %>%
  bb_curve(
    id = "demand",
    eq = "y_ = 10 - x_",
    color = "#8b1a1a"
  ) %>%
  bb_curve(
    id = "supply",
    eq = "y_ = 2 + 0.5 * x_",
    color = "#1f4e79"
  ) %>%
  bb_point(x = q_eq, y = p_eq, r = 4) %>%
  bb_xmarker(
    x = q_eq,
    y = p_eq,
    label = "q^{*}"
  ) %>%
  bb_ymarker(
    x = q_eq,
    y = p_eq,
    label = "p^{*}"
  ) %>%
  bb_text(
    x = q_eq,
    y = p_eq,
    label = "Equilibrium",
    x.offset = 12,
    y.offset = 12,
    align = "left",
    fill.background = TRUE
  )

view.bb(bb)
```

Useful annotation functions include `bb_text()`, `bb_point()`, `bb_xtick()`, `bb_ytick()`, `bb_xmarker()`, `bb_ymarker()`, `bb_segment()`, `bb_hline()`, `bb_vline()`, and `bb_arrow()`.

## Mathematical notation without LaTeX rendering

The `label` argument creates ordinary SVG text. It can still display common mathematical notation:

- `_` creates subscripts;
- `^` creates superscripts;
- common LaTeX-style names such as `\\alpha`, `\\beta`, `\\pi`, `\\leq`, and `\\infty` are converted to Unicode symbols.

This mode is fast, keeps the SVG simple, and does not invoke the LaTeX renderer.

```r
bb = bb_pane(
  xrange = c(0, 10),
  yrange = c(0, 10)
) %>%
  bb_xaxis(label = "q_t") %>%
  bb_yaxis(label = "p_t") %>%
  bb_text(
    x = 5,
    y = 7,
    label = "p_t = \\alpha + \\beta q_t"
  ) %>%
  bb_text(
    x = 5,
    y = 5,
    label = "q^{*} \\leq q_max"
  ) %>%
  bb_text(
    x = 5,
    y = 3,
    label = "U_1 > U_0"
  )

view.bb(bb)
```

Use this lightweight mode for short expressions that only need symbols, subscripts, or superscripts.

## Full LaTeX rendering

Use the `latex` argument when an expression needs fractions, roots, matrices, complex operators, or other LaTeX layout. Supplying at least one `latex` label tells `bb_to_svg()` and `view.bb()` to process the SVG with `latexsvg`.

```r
bb = bb_pane(
  xrange = c(0.5, 5),
  yrange = c(0.5, 5),
  org.width = 500,
  org.height = 340
) %>%
  bb_xaxis(latex = "x_1") %>%
  bb_yaxis(latex = "x_2") %>%
  bb_curve(
    eq = "y_ = 4 / x_",
    color = "#1f4e79"
  ) %>%
  bb_text(
    x = 3.2,
    y = 1.8,
    latex = "x_1 x_2 = \\bar{U}"
  ) %>%
  bb_text(
    x = 2.5,
    y = 4.2,
    latex = "\\frac{\\partial U}{\\partial x_1} = \\lambda p_1"
  )

view.bb(bb)
```

The distinction is:

```r
bb_text(label = "x_1^2 + x_2^2")
bb_text(latex = "\\frac{x_1^2}{a^2} + \\frac{x_2^2}{b^2} = 1")
```

The first expression uses lightweight SVG text. The second is rendered as full LaTeX.

## Shaded areas

`bb_area()` draws an arbitrary polygon, while `bb_area_rect()` draws a rectangle. The helpers `bb_area_above_curve()`, `bb_area_below_curve()`, `bb_area_left_of_curve()`, and `bb_area_right_of_curve()` fill one side of a curve.

The following example shades consumer and producer surplus with polygons.

```r
q_eq = 16 / 3
p_eq = 14 / 3

bb = bb_pane(
  xrange = c(0, 10),
  yrange = c(0, 10),
  org.width = 520,
  org.height = 360
) %>%
  bb_xaxis(label = "Quantity", labelpos = "right") %>%
  bb_yaxis(label = "Price", labelpos = "top") %>%
  bb_area(
    x = c(0, 0, q_eq),
    y = c(p_eq, 10, p_eq),
    fill = "#9ecae1",
    alpha = 0.45,
    tooltip = "Consumer surplus"
  ) %>%
  bb_area(
    x = c(0, 0, q_eq),
    y = c(2, p_eq, p_eq),
    fill = "#a1d99b",
    alpha = 0.45,
    tooltip = "Producer surplus"
  ) %>%
  bb_curve(
    eq = "y_ = 10 - x_",
    color = "#8b1a1a"
  ) %>%
  bb_curve(
    eq = "y_ = 2 + 0.5 * x_",
    color = "#1f4e79"
  ) %>%
  bb_hline(
    y = p_eq,
    linetype = "dashed",
    color = "#555555"
  ) %>%
  bb_vline(
    x = q_eq,
    linetype = "dashed",
    color = "#555555"
  )

view.bb(bb)
```

A region directly below a curve can be created more compactly:

```r
bb = bb_pane(
  xrange = c(0, 6),
  yrange = c(0, 6)
) %>%
  bb_xaxis(label = "x") %>%
  bb_yaxis(label = "y") %>%
  bb_area_below_curve(
    eq = "y_ = 5 - 0.5 * x_",
    xrange = c(1, 5),
    fill = "#9ecae1",
    alpha = 0.4
  ) %>%
  bb_curve(
    eq = "y_ = 5 - 0.5 * x_",
    color = "#1f4e79"
  )

view.bb(bb)
```

## Isoquants, slope curves, and tangents

`bb_slopecurve()` numerically traces a curve through a point from a slope expression. `bb_isoquant()` is a convenience wrapper that obtains the slope of an isoquant from a production function.

```r
bb = bb_pane(
  xrange = c(0.5, 5),
  yrange = c(0.5, 5),
  org.width = 460,
  org.height = 360
) %>%
  bb_xaxis(latex = "x_1") %>%
  bb_yaxis(latex = "x_2") %>%
  bb_isoquant(
    id = "q4",
    Q = "x_ * y_",
    x = 2,
    y = 2,
    color = "#1f4e79"
  ) %>%
  bb_tangent(
    x = 2,
    y = 2,
    slope = -1,
    width = 1.5,
    color = "#8b1a1a"
  ) %>%
  bb_point(x = 2, y = 2) %>%
  bb_text(
    x = 3.4,
    y = 1.4,
    latex = "x_1 x_2 = 4"
  )

view.bb(bb)
```

For explicit or implicit equations, use `bb_curve()`. For example, `eq = "y_ = 4 / x_"` defines an explicit curve, while an equation involving both variables can be handled as an implicit curve when it cannot be solved directly.

## Data-driven coordinates

A pane can hold a data frame and a selected row. Formula arguments such as `~q` and `~p` are evaluated from that row when the graphic is computed. This is useful for generating a sequence of related diagrams from simulation results.

```r
states = data.frame(
  period = 1:3,
  q = c(3, 5, 7),
  p = c(7, 5, 4)
)

bb = bb_pane(
  data = states,
  data.row = 2,
  xrange = c(0, 8),
  yrange = c(0, 8)
) %>%
  bb_xaxis(label = "q") %>%
  bb_yaxis(label = "p") %>%
  bb_point(x = ~q, y = ~p, r = 5) %>%
  bb_xmarker(
    x = ~q,
    y = ~p,
    label = ~q
  ) %>%
  bb_ymarker(
    x = ~q,
    y = ~p,
    label = ~p
  ) %>%
  bb_text(
    x = ~q,
    y = ~p,
    label = "Selected state",
    x.offset = 12,
    align = "left"
  )

view.bb(bb)
```

Curve equations can use columns or values from the pane as parameters. The same mechanism is used in model simulations, for example when an IS or LM curve depends on the current values of money, prices, or other state variables.

## Simulation-state diagrams

The selected data row can also parameterize complete model curves. This is useful when a data frame contains the states of a dynamic simulation. The data may come from `ddsim` or any other simulation package.

```r
state_data = data.frame(
  M = 87.5,
  P = 1,
  Y = 95,
  r = 0.0125
)

bb = bb_pane(
  data = state_data,
  data.row = 1,
  xrange = c(80, 100),
  yrange = c(0, 0.04),
  show.ticks = FALSE,
  org.width = 360,
  org.height = 300
) %>%
  bb_xaxis(label = "Y") %>%
  bb_yaxis(label = "r") %>%
  bb_curve(
    id = "IS",
    eq = "x_ == 100 - 400 * y_",
    color = "#880000"
  ) %>%
  bb_curve(
    id = "LM",
    eq = "M / P == x_ - 600 * y_",
    color = "#000088"
  ) %>%
  bb_point(x = ~Y, y = ~r) %>%
  bb_xmarker(
    x = ~Y,
    y = ~r,
    label = ~Y
  ) %>%
  bb_ymarker(
    x = ~Y,
    y = ~r,
    label = ~r
  ) %>%
  bb_text(
    x = 99,
    y = 0.003,
    label = "IS",
    color = "#880000",
    align = "left"
  ) %>%
  bb_text(
    x = 99,
    y = 0.035,
    label = "LM",
    color = "#000088",
    align = "left"
  )

view.bb(bb)
```

Changing `data.row` selects another simulation period. The same figure definition can therefore be reused to render a sequence of states.

## Time series and event periods

`bb_series()` adds a line or point series and automatically determines missing coordinate ranges. `bb_period()` marks an event with a vertical line or shades an interval. `bb_series_tooltip_bars()` adds hover tooltips that combine values from all series at each horizontal coordinate.

```r
series_data = data.frame(
  year = 2015:2024,
  growth = c(1.7, 2.2, 2.7, 1.1, 1.0, -4.1, 3.7, 1.4, -0.3, 0.2)
)

bb = bb_pane(
  show.ticks = TRUE,
  org.width = 720,
  org.height = 320
) %>%
  bb_series(
    data = series_data,
    xvar = "year",
    yvar = "growth",
    name = "Growth",
    color = "#1f4e79",
    lwd = 2,
    draw.points = TRUE
  ) %>%
  bb_hline(
    y = 0,
    color = "#555555"
  ) %>%
  bb_xaxis(
    label = "Year",
    num.ticks = 10
  ) %>%
  bb_yaxis(
    label = "Growth rate",
    num.ticks = 6
  ) %>%
  bb_period(
    from = 2020,
    to = 2021,
    label = "Pandemic",
    shade = "#999999",
    alpha = 0.2
  ) %>%
  bb_series_tooltip_bars(xname = "Year")

view.bb(bb)
```

## A merit-order diagram

Rectangles, text, vertical lines, and custom ticks can be combined to illustrate electricity markets.

```r
bb = bb_pane(
  xrange = c(0, 110),
  yrange = c(0, 600),
  show.ticks = FALSE,
  org.width = 480,
  org.height = 320
) %>%
  bb_xaxis(label = "GW") %>%
  bb_yaxis(
    label = "Euro / MWh",
    labelpos = "top"
  ) %>%
  bb_area_rect(
    x1 = 0,
    x2 = 80,
    y1 = 0,
    y2 = 200,
    fill = "#aa6666"
  ) %>%
  bb_text(
    x = 40,
    y = 100,
    label = "Coal"
  ) %>%
  bb_area_rect(
    x1 = 80,
    x2 = 100,
    y1 = 0,
    y2 = 500,
    fill = "#8888aa"
  ) %>%
  bb_text(
    x = 90,
    y = 100,
    label = "Gas"
  ) %>%
  bb_vline(x = 100) %>%
  bb_text(
    x = 100,
    y = 560,
    label = "Demand"
  ) %>%
  bb_segment(
    y = 500,
    x1 = 0,
    x2 = 100,
    linetype = "dashed"
  ) %>%
  bb_ytick(
    y = 500,
    label = "p"
  )

view.bb(bb)
```

## Styling and tooltips

Most drawing functions accept a common set of style arguments:

- `color` controls the stroke or text color;
- `fill` controls an area's fill color;
- `alpha` controls opacity;
- `lwd` controls line width;
- `linetype` accepts styles such as `"solid"` and `"dashed"`;
- `style` accepts a named list of SVG style properties;
- `tooltip` adds text shown when the pointer hovers over an SVG object;
- `level` controls drawing order, with lower values drawn first.

The default stylesheet is returned by `bb_svg_css()`. A modified stylesheet can be passed to `bb_pane(css = ...)`.

## Export

Render directly to an SVG file:

```r
bb_to_svg(bb, file = "figure.svg")
```

Or obtain the SVG string in memory:

```r
svg = bb_to_svg(bb)
writeLines(svg, "figure.svg", useBytes = TRUE)
```

The `rsvg` package is used for conversion to PDF, PNG, and PostScript:

```r
bb_to_pdf(bb, "figure.pdf")
bb_to_png(bb, "figure.png")
bb_to_ps(bb, "figure.ps")
```

Existing SVG files can also be converted:

```r
svg_to_pdf("figure.svg")
svg_to_png("figure.svg")
svg_to_ps("figure.svg")
```

## Main functions

| Task | Functions |
| --- | --- |
| Create a pane | `bb_pane()`, `bbsvg()` |
| Axes and margins | `bb_xaxis()`, `bb_yaxis()`, `bb_margins()` |
| Curves | `bb_curve()`, `bb_slopecurve()`, `bb_isoquant()` |
| Lines | `bb_segment()`, `bb_hline()`, `bb_vline()`, `bb_arrow()`, `bb_tangent()` |
| Points and labels | `bb_point()`, `bb_text()`, `bb_xtick()`, `bb_ytick()` |
| Markers | `bb_xmarker()`, `bb_ymarker()` |
| Areas | `bb_area()`, `bb_area_rect()`, `bb_area_above_curve()`, `bb_area_below_curve()`, `bb_area_left_of_curve()`, `bb_area_right_of_curve()` |
| Data series | `bb_series()`, `bb_period()`, `bb_series_tooltip_bars()` |
| Rendering | `view.bb()`, `bb_to_svg()` |
| Export | `bb_to_pdf()`, `bb_to_png()`, `bb_to_ps()` |

## Notes

- Curves use `x_` and `y_` by default. Alternative variable names can be supplied to `bb_pane()` through `xvar`, `yvar`, or `xy`.
- Coordinate ranges are required for most model diagrams. `bb_series()` can infer them from its data.
- Plain `label` text is preferable for simple notation. Use `latex` only when full mathematical layout is needed.
- SVG is the native output format. It preserves sharp lines and text when figures are scaled.
  
  

