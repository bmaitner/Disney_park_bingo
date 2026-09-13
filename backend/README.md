# Player inbox setup

The phone app sends two things to a Google Sheet that only you can see, through a
small Google Apps Script web app ([`inbox.gs`](inbox.gs)). Players don't need an
account for either:

- **Square suggestions**, one row each in the **suggestions** tab. Nothing is
  added to the game until you review it and add it to the squares CSV.
- **Card results**, sent when a player taps "Done playing" with sharing on. Each
  card adds one row per square to the **results** tab: the card's settings, when
  it was started and finished, the square id, and whether it was crossed off.
  Nothing identifies the player or the phone.

## One-time setup (about 10 minutes)

1. **Create the sheet.** Go to <https://sheets.new> and name it something like
   "Park bingo suggestions".
2. **Add the script.** In the sheet, choose **Extensions → Apps Script**. Delete
   the placeholder code, paste in all of `inbox.gs`, and click **Save**.
3. **Create your read token.** In the function dropdown at the top of the editor,
   pick `createReadToken`, then click **Run**. Approve the permission prompt. It
   asks for access to this spreadsheet. Google may warn that the app isn't
   verified; choose **Advanced → Go to (project name)**. Open the **Execution log**
   and copy the `PARKBINGO_TOKEN=...` value. Keep it private.
4. **Deploy.** Click **Deploy → New deployment**. Click the gear next to
   "Select type" and choose **Web app**. Set:
   - **Execute as:** Me
   - **Who has access:** Anyone

   Click **Deploy**, then copy the **Web app URL**. It ends in `/exec`.
5. **Check it.** Open the web app URL in a browser. You should see
   `{"ok":true,"service":"parkbingo"}`.
6. **Connect the app.** Share the web app URL with Claude, or paste it into
   `app/config.js` yourself. The URL isn't secret: anyone can see it in the app's
   code, and it only accepts suggestions. The token is what protects reading.
7. **Connect R.** Add both values to your `~/.Renviron` (run
   `usethis::edit_r_environ()` to open it), then restart R:

   ```
   PARKBINGO_ENDPOINT=https://script.google.com/macros/s/.../exec
   PARKBINGO_TOKEN=the-token-from-step-3
   ```

## Reviewing suggestions

The first suggestion creates a **suggestions** tab with one row per submission.
Use the empty `status` column to track what you've reviewed, e.g. `added` or
`skip`. From R:

```r
library(parkbingo)
new <- fetch_suggestions()
new[new$status == "", c("text", "mode", "age", "park", "note")]

# Add the ones you like, choosing a category and a starting difficulty guess
keep <- new[c(1, 3), ]
keep$category <- c("people", "food")
squares <- add_squares(bingo_squares(), keep)
write_squares(squares, "inst/extdata/bingo_squares.csv")
```

Then run `Rscript tools/update_app.R` so the app gets the new squares.

## Using results

```r
results <- fetch_results(min_minutes = 30)   # skip cards that weren't really played
squares <- update_difficulty(bingo_squares(), results)
write_squares(squares, "inst/extdata/bingo_squares.csv")
```

Then run `Rscript tools/update_app.R`. Each result row feeds `update_difficulty()`
exactly once, so after you've applied a batch, keep track of what's been used, e.g.
by only using results received after your last update:

```r
results <- results[results$received_at > "2026-10-01", ]
```

## Updating the script

If `inbox.gs` changes (it was called `suggestions.gs` before results were added),
paste the new version over the old code in the editor and click **Save**. Then use
**Deploy → Manage deployments → Edit (pencil) → Version: New version → Deploy**.
This keeps the same URL. Creating a *new* deployment instead changes the URL.

Until the new version is deployed, the app keeps finished-card results waiting on
players' phones and sends them once the inbox accepts them.

## Abuse protection

- A hidden trap field catches simple bots.
- Text is limited to 60 characters and notes to 200.
- Mode, age, park, difficulty, card ids, square ids, and times must be valid.
- Retried submissions are de-duplicated.
- The script accepts at most 300 suggestions and 300 card results per hour in total.
- Text that looks like a spreadsheet formula is stored as plain text.

Results can't be verified, so someone could send made-up cards. If a batch looks
wrong, e.g. many cards with every square crossed off in a couple of minutes, filter
it out in R before calling `update_difficulty()`. If junk gets through, delete the
rows. If it keeps coming, create a new
deployment (which gets a new URL) and update `app/config.js`.
