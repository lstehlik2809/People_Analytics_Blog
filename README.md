# People_Analytics_Blog

## People Analytics News

The News tab shows up to five new items per Prague calendar day, with original source links, summaries, and implications for practice. Each collection looks at publications from the past 14 calendar days and skips previously featured work. The [RSS feed](https://blog-about-people-analytics.netlify.app/news-assets/news.xml) and [Markdown digest](https://blog-about-people-analytics.netlify.app/news-assets/news-digest.md) use the same rolling window of 30 Prague calendar days, including the current day.

After merging the workflow, add `OPENAI_API_KEY` under **Settings → Secrets and variables → Actions → Repository secrets**. The **People Analytics News** workflow is scheduled daily at 07:17 Prague time. GitHub can delay or drop scheduled runs under heavy load. [GitHub Actions schedule documentation](https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#schedule)

Once deployed, the independent **People Analytics News Scheduler** Cloudflare Worker checks every 15 minutes from 05:02 through 11:47 UTC. From 07:17 Prague time it checks for today's archive and dispatches the GitHub collector if the archive is missing and no collection run is active. It can dispatch the collector even when GitHub's own cron is delayed. The **People Analytics News Recovery** GitHub workflow remains available on its own schedule and through manual dispatch. All collection and scheduling run in the cloud.

Once the day's archive exists, later collection runs reuse it without calling OpenAI. The collector uses GPT‑6.1 Terra with high reasoning effort and web search. The workflow commits the daily archive and generated assets for the existing Netlify deployment. If another commit lands during collection and the push is rejected, rerun the workflow from the latest default branch.

For a local preview without API access, run `python scripts/collect_news.py --render-only`, then serve `_site` with `python -m http.server 8000 --directory _site` and visit `http://localhost:8000/news.html`. The collector requires Python 3.10+ and timezone data for Prague. `--date YYYY-MM-DD` sets the collection day explicitly. Use `python -B -m unittest discover -s scripts -p 'test_*.py'` to run the offline tests.

For live local collection, set `OPENAI_API_KEY` in the process environment and run `python scripts/collect_news.py`. Successful runs, including days with zero items, are saved in `news_archive/`. Repeating a saved day regenerates the public files without another API call. The copies under `news-assets/` survive a full Distill rebuild, and the `_site/news-assets/` copies are served by the existing deployment. Edit `scripts/news_prompt.txt` to adjust the editorial priorities.

### Deploy the cloud scheduler

Create a private GitHub App with **Actions: Read and write** repository permission and install it on `lstehlik2809/People_Analytics_Blog` only. The Worker reads the public archive and requests a short-lived installation token scoped to Actions on that repository. The Worker has no public routes.

Keep a local JSON file outside the repository containing `GITHUB_APP_ID`, `GITHUB_INSTALLATION_ID`, and `GITHUB_APP_PRIVATE_KEY` as strings. Store the private key as an unencrypted PKCS#8 PEM. If GitHub supplied a PKCS#1 PEM, convert it with `openssl pkcs8 -topk8 -nocrypt -in github-app.pem -out github-app-pkcs8.pem`.

```sh
cd .github/news-scheduler
node --test worker.test.mjs
npx wrangler login
npx wrangler deploy --secrets-file /path/to/news-scheduler-secrets.json
npx wrangler tail
```

The Worker logs the Prague date and whether it found an archive, found an active run, or dispatched collection. When GitHub returns a workflow run ID, the Worker logs that ID too. Check Cloudflare Workers Logs for the scheduled invocation and GitHub Actions for the corresponding collector run. [Cloudflare Worker secrets](https://developers.cloudflare.com/workers/configuration/secrets/)
