# People_Analytics_Blog

## People Analytics News

The News tab shows up to five new items per Prague calendar day, with original source links, summaries, and implications for practice. Each collection looks at publications from the past 14 calendar days and skips previously featured work. The [RSS feed](https://blog-about-people-analytics.netlify.app/news-assets/news.xml) and [Markdown digest](https://blog-about-people-analytics.netlify.app/news-assets/news-digest.md) use the same rolling window of 30 Prague calendar days, including the current day.

After merging the workflow, add `OPENAI_API_KEY` under **Settings → Secrets and variables → Actions → Repository secrets**. The **People Analytics News** workflow is scheduled daily at 07:17 Prague time. GitHub can delay or drop scheduled runs under heavy load. [GitHub Actions schedule documentation](https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#schedule)

The separate **People Analytics News Recovery** workflow is scheduled at 05:47, 06:47, 07:23, 08:47, 09:23, 10:47, and 11:23 UTC. Scheduled recovery checks start from 07:17 Prague time and dispatch the collector if today's archive is missing and no News run is active. Both workflows run on GitHub-hosted runners and can also be dispatched from the default branch in Actions.

Once the day's archive exists, later collection runs reuse it without calling OpenAI. The collector uses GPT‑6.1 Terra with high reasoning effort and web search. The workflow commits the daily archive and generated assets for the existing Netlify deployment. If another commit lands during collection and the push is rejected, rerun the workflow from the latest default branch.

For a local preview without API access, run `python scripts/collect_news.py --render-only`, then serve `_site` with `python -m http.server 8000 --directory _site` and visit `http://localhost:8000/news.html`. The collector requires Python 3.10+ and timezone data for Prague. `--date YYYY-MM-DD` sets the collection day explicitly. Use `python -B -m unittest discover -s scripts -p 'test_*.py'` to run the offline tests.

For live local collection, set `OPENAI_API_KEY` in the process environment and run `python scripts/collect_news.py`. Successful runs, including days with zero items, are saved in `news_archive/`. Repeating a saved day regenerates the public files without another API call. The copies under `news-assets/` survive a full Distill rebuild, and the `_site/news-assets/` copies are served by the existing deployment. Edit `scripts/news_prompt.txt` to adjust the editorial priorities.
