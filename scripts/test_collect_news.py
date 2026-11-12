import io
import json
import os
import unittest
from contextlib import redirect_stderr, redirect_stdout
from datetime import date, timedelta
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch
from urllib.error import HTTPError
from xml.etree import ElementTree as ET

import collect_news as news


TODAY = date(2026, 11, 12)


def entry(title, *, url=None, published=TODAY, doi=None):
    return {
        "title": title,
        "url": url or f"https://journal.example/{title.lower().replace(' ', '-')}",
        "source": "Example Research Journal",
        "source_type": "Preprint",
        "published_date": published.isoformat(),
        "summary": "The paper compares two ways to measure employee mobility.",
        "why_it_matters": "Use both measures when auditing internal promotion rates.",
        "doi": doi,
    }


def response(entries, *, sources=(), citations=()):
    return {
        "status": "completed",
        "output": [
            {"type": "web_search_call", "status": "completed", "action": {
                "type": "search", "sources": [{"url": url} for url in sources],
            }},
            {"type": "message", "content": [{
                "type": "output_text", "text": json.dumps({"entries": entries}),
                "annotations": [{"type": "url_citation", "url": url} for url in citations],
            }]},
        ],
    }


class ResponseTests(unittest.TestCase):
    def test_request_uses_required_web_search_and_strict_output(self):
        payload = news.request_payload(TODAY, {})
        self.assertEqual(payload["model"], "gpt-5.4-mini")
        self.assertEqual(payload["reasoning"], {"effort": "medium"})
        self.assertEqual(payload["tools"], [{"type": "web_search"}])
        self.assertEqual(payload["tool_choice"], "required")
        self.assertEqual(payload["include"], ["web_search_call.action.sources"])
        self.assertIs(payload["store"], False)
        self.assertIs(payload["text"]["format"]["strict"], True)
        self.assertEqual(payload["text"]["format"]["schema"]["properties"]["entries"]["maxItems"], 5)
        self.assertEqual(json.loads(payload["input"])["earliest_publication_date"], "2026-10-30")

    def test_accepts_consulted_and_cited_sources_omitting_ungrounded_candidates(self):
        first = entry("Study A", url="https://Journal.example/one?utm_source=news&paper=7#abstract")
        second = entry("Study B")
        third = entry("Study C")
        raw = response([first, second, third], sources=["https://journal.example/one?paper=7"], citations=[third["url"]])
        errors = io.StringIO()
        with redirect_stderr(errors):
            selected = news.extract_entries(raw, TODAY)
        self.assertEqual([item["title"] for item in selected], ["Study A", "Study C"])
        self.assertEqual(selected[0]["url"], "https://journal.example/one?paper=7")
        self.assertIn("Skipped 1 items", errors.getvalue())
        self.assertNotIn("Study B", errors.getvalue())

    def test_unrelated_source_cannot_ground_entry(self):
        raw = response([entry("Wrong article")], sources=["https://journal.example/other-article"])
        with self.assertRaisesRegex(news.NewsError, r"No original source URLs.*\(1 candidates, 1 usable evidence URLs\)"):
            news.extract_entries(raw, TODAY)

    def test_completed_page_actions_ground_entries_without_search_sources(self):
        opened = entry("Opened article")
        found = entry("Searched article")
        failed = entry("Failed article")
        raw = response([opened, found, failed])
        del raw["output"][0]["action"]["sources"]
        raw["output"][1:1] = [
            {"type": "web_search_call", "status": "completed", "action": {
                "type": "open_page", "url": opened["url"],
            }},
            {"type": "web_search_call", "status": "completed", "action": {
                "type": "find_in_page", "url": found["url"], "pattern": "Published online",
            }},
            {"type": "web_search_call", "status": "failed", "action": {
                "type": "open_page", "url": failed["url"],
            }},
        ]
        with redirect_stderr(io.StringIO()):
            result = news.extract_entries(raw, TODAY)
        self.assertEqual([item["title"] for item in result], ["Opened article", "Searched article"])

        raw["output"].pop(0)
        with self.assertRaisesRegex(news.NewsError, "complete a web search"):
            news.extract_entries(raw, TODAY)

    def test_requires_completed_search_for_empty_response(self):
        raw = response([])
        self.assertEqual(news.extract_entries(raw, TODAY), [])
        raw["output"][0]["status"] = "failed"
        with self.assertRaisesRegex(news.NewsError, "complete a web search"):
            news.extract_entries(raw, TODAY)

    def test_rejects_refusal_and_incomplete_response(self):
        raw = response([])
        raw["output"][1]["content"] = [{"type": "refusal", "refusal": "Private API details"}]
        with self.assertRaisesRegex(news.NewsError, "structured digest"):
            news.extract_entries(raw, TODAY)
        raw["status"] = "incomplete"
        with self.assertRaisesRegex(news.NewsError, "did not complete"):
            news.extract_entries(raw, TODAY)

    def test_rejects_unsafe_urls_and_invalid_dates(self):
        for url in ("javascript:alert(1)", "https://name@journal.example/article", "https://example.com:999/article"):
            with self.subTest(url=url), self.assertRaisesRegex(news.NewsError, "Invalid original source URL"):
                news.validate_entries([entry("Unsafe", url=url)], TODAY)
        news.validate_entries([entry("14th day", published=date(2026, 10, 30))], TODAY)
        for day in (date(2026, 10, 29), date(2026, 11, 13)):
            with self.subTest(day=day), self.assertRaisesRegex(news.NewsError, "outside the collection window"):
                news.validate_entries([entry("Invalid day", published=day)], TODAY)

    def test_removes_only_matching_trailing_citations(self):
        url = "https://onlinelibrary.wiley.com/doi/10.1111/1748-8583.70073"
        item = entry("Mediation study", url=url)
        summary = item["summary"]
        practice = item["why_it_matters"]
        citation = f"([onlinelibrary.wiley.com]({url}))"
        item["summary"] = f"{summary} {citation}"
        item["why_it_matters"] = f"{practice} ([onlinelibrary.wiley.com]({url}?utm_source=search#abstract))"

        cleaned, = news.validate_entries([item], TODAY)
        self.assertEqual(cleaned["summary"], summary)
        self.assertEqual(cleaned["why_it_matters"], practice)
        self.assertEqual(cleaned["url"], url)

        item["summary"] = f"{summary} ([example.org](https://example.org/related-research))"
        item["why_it_matters"] = f"{practice} {citation} Consider the evaluation design too."
        preserved, = news.validate_entries([item], TODAY)
        self.assertEqual(preserved["summary"], item["summary"])
        self.assertEqual(preserved["why_it_matters"], item["why_it_matters"])

    def test_rejects_more_than_five_candidates(self):
        entries = [entry(f"Study {number}") for number in range(6)]
        with self.assertRaisesRegex(news.NewsError, "Invalid news entry count"):
            news.extract_entries(response(entries, sources=[item["url"] for item in entries]), TODAY)

    def test_api_failure_logs_only_safe_status(self):
        error = HTTPError("https://api.openai.com/v1/responses", 400, "private error", {}, io.BytesIO(b"private body"))
        with patch.dict(os.environ, {"OPENAI_API_KEY": "fake-key"}), patch.object(news, "urlopen", side_effect=error):
            with self.assertRaises(news.NewsError) as raised:
                news.request_news(TODAY, {})
        self.assertEqual(str(raised.exception), "OpenAI request failed with HTTP 400.")


class PublicationTests(unittest.TestCase):
    def setUp(self):
        workspace = TemporaryDirectory()
        self.addCleanup(workspace.cleanup)
        self.root = Path(workspace.name)

    def save_day(self, day, entries):
        directory = self.root / "news_archive"
        directory.mkdir(exist_ok=True)
        (directory / f"{day}.json").write_text(json.dumps({"date": str(day), "entries": entries}), encoding="utf-8")

    def collect(self, *, render_only=False):
        with redirect_stdout(io.StringIO()):
            news.run(self.root, TODAY, render_only)

    def snapshot(self):
        return {path.relative_to(self.root): path.read_bytes() for path in self.root.rglob("*") if path.is_file()}

    def test_daily_and_historical_deduplication_and_editorial_order(self):
        old = TODAY - timedelta(days=40)
        self.save_day(old, [
            entry("Earlier item", url="http://www.journal.example/shared/?a=1&b=2", published=old),
            entry("Earlier research", doi="10.1000/STUDY", published=old),
            entry("Long-term, trends in hiring", published=old),
        ])
        candidates = [
            entry("Most relevant", published=TODAY - timedelta(days=2)),
            entry("Same URL", url="https://journal.example/shared?b=2&a=1&utm_source=ref"),
            entry("DOI variant", url="https://doi.org/10.1000/study"),
            entry("Long-Term Trends in Hiring", url="https://another.example/article"),
            entry("Newer research"),
        ]
        # Run a second day with an exact same-day URL duplication independently of the historical keys.
        raw = response(candidates, sources=[item["url"] for item in candidates])
        with patch.object(news, "request_news", return_value=raw):
            self.collect()
        path = self.root / "news_archive" / f"{TODAY}.json"
        saved = json.loads(path.read_text(encoding="utf-8"))["entries"]
        self.assertEqual([item["title"] for item in saved], ["Most relevant", "Newer research"])
        duplicate = entry("Different headline", url="https://journal.example/newer-research#abstract")
        validated = news.validate_entries([candidates[4], duplicate], TODAY)
        self.assertEqual([item["title"] for item in news.deduplicate(validated, {})], ["Newer research"])

    def test_rolling_window_and_parity_across_all_six_public_files(self):
        last_included = date(2026, 10, 14)
        expired = date(2026, 10, 13)
        future = date(2026, 11, 13)
        first = entry("Today & work")
        first["url"] = "https://journal.example/today?x=1&y=2"
        first["summary"] = "Compared <script>alert(1)</script> and another format."
        second = entry("Earliest included", published=last_included)
        for day, item in [(expired, entry("Expired", published=expired)), (last_included, second),
                          (TODAY, first), (future, entry("Future", published=future))]:
            self.save_day(day, [item])
        with patch.object(news, "request_news") as request:
            self.collect(render_only=True)
            request.assert_not_called()

        assets = self.root / "news-assets"
        expected = [first["url"], second["url"]]
        data = json.loads((assets / "news-data.json").read_bytes())
        self.assertEqual(data["as_of"], "2026-11-12")
        self.assertEqual([group["date"] for group in data["days"]], ["2026-11-12", "2026-10-14"])
        self.assertEqual([item["url"] for group in data["days"] for item in group["entries"]], expected)
        items = ET.parse(assets / "news.xml").findall("./channel/item")
        self.assertEqual([item.findtext("link") for item in items], expected)
        self.assertEqual([item.findtext("guid") for item in items], expected)
        self.assertEqual(items[0].findtext("title"), first["title"])
        description = items[0].findtext("description")
        self.assertIn("&lt;script&gt;", description)
        self.assertIn(first["why_it_matters"], description)
        markdown = (assets / "news-digest.md").read_text(encoding="utf-8")
        self.assertEqual([line.split("](<")[1][:-2] for line in markdown.splitlines() if line.startswith("### [")], expected)
        self.assertIn("\\<script\\>", markdown)
        self.assertIn(first["why_it_matters"], markdown)
        self.assertNotIn("Expired", markdown)
        self.assertNotIn("Future", markdown)
        for path in assets.iterdir():
            self.assertEqual(path.read_bytes(), (self.root / "_site" / "news-assets" / path.name).read_bytes())

    def test_failure_preserves_existing_files_and_allows_retry(self):
        self.collect(render_only=True)
        before = self.snapshot()
        with patch.object(news, "request_news", side_effect=news.NewsError("API failed")):
            with self.assertRaisesRegex(news.NewsError, "API failed"):
                self.collect()
        self.assertEqual(before, self.snapshot())
        raw = response([entry("Ungrounded article")])
        with patch.object(news, "request_news", return_value=raw):
            with self.assertRaisesRegex(news.NewsError, "No original source URLs"):
                self.collect()
        self.assertEqual(before, self.snapshot())

        result = entry("Grounded after retry")
        with patch.object(news, "request_news", return_value=response([result], sources=[result["url"]])):
            self.collect()
        data = json.loads((self.root / "news-assets/news-data.json").read_bytes())
        self.assertEqual(data["days"][0]["entries"][0]["title"], "Grounded after retry")

    def test_empty_collection_is_recorded_and_rebuilds_without_api(self):
        with patch.object(news, "request_news", return_value=response([])) as request:
            self.collect()
            request.assert_called_once()
        snapshot = self.snapshot()
        self.assertEqual(json.loads(snapshot[Path("news_archive") / f"{TODAY}.json"])["entries"], [])
        (self.root / "_site/news-assets/news.xml").unlink()
        with patch.object(news, "request_news") as request:
            self.collect()
            request.assert_not_called()
        self.assertEqual(snapshot, self.snapshot())


if __name__ == "__main__":
    unittest.main()
