# Suggestion inbox setup

Players suggest squares from the phone app without any account. Suggestions go to
a Google Sheet that only you can see, through a small Google Apps Script web app
([`suggestions.gs`](suggestions.gs)). Nothing is added to the game until you
review it and add it to the squares CSV.

## One-time setup (about 10 minutes)

1. **Create the sheet.** Go to <https://sheets.new> and name it something like
   "Park bingo suggestions".
2. **Add the script.** In the sheet, choose **Extensions → Apps Script**. Delete
   the placeholder code, paste in all of `suggestions.gs`, and click **Save**.
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

## Updating the script

If `suggestions.gs` changes, paste the new version into the editor. Then use
**Deploy → Manage deployments → Edit (pencil) → Version: New version → Deploy**.
This keeps the same URL. Creating a *new* deployment instead changes the URL.

## Abuse protection

- A hidden trap field catches simple bots.
- Text is limited to 60 characters and notes to 200.
- Mode, age, and park must be valid values.
- Retried submissions are de-duplicated.
- The script accepts at most 300 suggestions per hour in total.
- Text that looks like a spreadsheet formula is stored as plain text.

If junk gets through, delete the rows. If it keeps coming, create a new
deployment (which gets a new URL) and update `app/config.js`.
