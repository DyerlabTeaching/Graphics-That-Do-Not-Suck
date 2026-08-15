# Copy-Edit Log — Graphics-That-Do-Not-Suck

Tracks copy-editing passes over this module's `.qmd` documents. Clear-cut
typos/grammar/spelling errors are corrected directly in the source. Items
that need an author decision are left in place but wrapped in
`<mark>...</mark>` so they show up highlighted in the rendered HTML.

## narrative.qmd — 2026-08-15

### Fixed directly (typos, spelling, grammar)

| Line | Issue | Fix |
|---|---|---|
| 6 | "gramattical approach" | "grammatical approach" |
| 19 | "Human Develoment" (in the figure's `fig-cap`, so it's visible caption text) | "Human Development" |
| 41 | `"Spane"` in the `country2label` vector — see note below | `"Spain"` |
| 68 | "A *aesthetic* statement" | "An *aesthetic* statement" |
| 84 | "which is descbribed as" | "which is described as" |
| 92 | "the basis of this appraoch" | "approach" |
| 116 | missing terminal period | added |
| 124 | "## Aestheics and Scope" (heading) | "## Aesthetics and Scope" |
| 128 | "in the the `ggplot()`" (duplicate word); "data and `aes()` is" (agreement) | "in the `ggplot()`"; "data and `aes()` are" |
| 129 | "then the they are localized" | "then they are localized" |
| 140 | "has it's own specification" (wrong its/it's) | "has its own specification" |
| 143 | "country names are plot" | "are plotted" |
| 153 | "I'm goint to add" | "I'm going to add" |
| 203 | "there is a an actual column" (duplicate article) | "there is an actual column" |
| 232 | "plot varible is" | "plot variable is:" |
| 274 | "minimzie" | "minimize" |
| 312 | "puting" / "layes" | "putting" / "lays" |
| 321 | "added direclty" | "added directly" |
| 337 | missing colon before code example | added |
| 354 | "to the the *aesthetics*" (duplicate word) | "to the *aesthetics*" |
| footnote 1 | "add these toghter" | "add these together" |

### Flagged for your review (highlighted with `<mark>` in the source)

| Line | Text | Why it's flagged |
|---|---|---|
| ~213 | "Notice in the last graph I put the name of the data column in the aesthetic but have the color (`col`) within the `aes()` function call in the graph before that, I put color outside of the `aes()` in the `geom_point()` function." | Run-on sentence comparing two different graphs — it's not clear where the clause about "the last graph" ends and the one about "the graph before that" begins. Needs to be split, but I didn't want to guess wrong about which graph you meant. |
| ~295 | "Just like in the previous" | Cuts off right after the "## Overlays" heading, before the code block. |
| ~305 | "The order by which you add the components to the `ggplot()` will determine the order of the layers from bottom to top—the." | The dangling "—the." after the em dash doesn't complete a thought — looks like the rest of a clause got cut off during editing. |
| footnote 2 | `[here](.../narriative.nb.html)` | The URL's filename reads "narriative" (extra `i`). I didn't change it since I can't confirm whether the actual hosted file is spelled that way — worth checking that the link still resolves. |

### Other notes

- Line 41: `"Spane"` in the `country2label` vector wasn't just a display typo — because it didn't match any real `Country` value in the dataset, Spain was silently **not** being labeled in the corruption/HDI plot. Fixing the spelling means Spain will now show up labeled when you re-render, which changes the rendered figure (one more labeled point than before).
