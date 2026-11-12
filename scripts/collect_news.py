"""Collect and publish the People Analytics News digest."""

import argparse
import html
import json
import os
import re
import sys
import tempfile
import unicodedata
from datetime import date, datetime, time, timedelta, timezone
from email.utils import format_datetime
from http.client import HTTPException
from pathlib import Path
from urllib.error import HTTPError
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit
from urllib.request import Request, urlopen
from xml.etree import ElementTree as ET
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError


ROOT = Path(__file__).resolve().parents[1]
PROMPT = Path(__file__).with_name("news_prompt.txt")
MODEL = "gpt-5.4-mini"
TITLE = "People Analytics News"
BASE_URL = "https://blog-about-people-analytics.netlify.app/"
MAX_ENTRIES = 5
RECENT_DAYS = 14
PUBLIC_DAYS = 30
SOURCE_TYPES = (
    "Peer-reviewed research", "Preprint", "Company case study", "Expert newsletter", "LinkedIn post"
)
TEXT_LIMITS = {"title": 400, "source": 200, "summary": 600, "why_it_matters": 600}
TRACKERS = {"fbclid", "gclid", "mc_cid", "mc_eid", "li_fat_id", "trk", "trackingid"}
INVALID_TEXT = re.compile(r"[\x00-\x1f\x7f-\x9f\ud800-\udfff\ufffe\uffff]")
DOMAIN_LABEL = re.compile(r"[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?")
DOI = re.compile(r"10\.\d{4,9}/\S+", re.ASCII)
TRAILING_SOURCE_CITATION = re.compile(r"(?:^| )\(\[[^\[\]]+\]\((\S+)\)\)\Z")
ENTRY_PROPERTIES = {
    **{field: {"type": "string"} for field in TEXT_LIMITS},
    "url": {"type": "string"},
    "published_date": {"type": "string"},
    "source_type": {"type": "string", "enum": list(SOURCE_TYPES)},
    "doi": {"type": ["string", "null"]},
}
RESPONSE_SCHEMA = {
    "type": "object",
    "properties": {
        "entries": {
            "type": "array",
            "maxItems": MAX_ENTRIES,
            "items": {
                "type": "object",
                "properties": ENTRY_PROPERTIES,
                "required": list(ENTRY_PROPERTIES),
                "additionalProperties": False,
            },
        }
    },
    "required": ["entries"],
    "additionalProperties": False,
}


class NewsError(Exception):
    """An error whose message is safe to log."""


def calendar_date(value):
    parsed = date.fromisoformat(value)
    if parsed.isoformat() != value:
        raise ValueError("Expected YYYY-MM-DD")
    return parsed


def window_start(day, days):
    return day - timedelta(days=days - 1)


def normalize_url(value):
    if (
        not isinstance(value, str)
        or len(value) > 4096
        or INVALID_TEXT.search(value)
        or any(character.isspace() or character in "\\<>" for character in value)
    ):
        return None
    try:
        parts = urlsplit(value)
        if parts.scheme not in {"http", "https"} or not parts.hostname or parts.username is not None:
            return None
        if parts.port not in {None, 443 if parts.scheme == "https" else 80}:
            return None
        host = parts.hostname.rstrip(".").encode("idna").decode("ascii").lower()
        labels = host.split(".")
        if len(labels) < 2 or labels[-1].isdigit() or not all(DOMAIN_LABEL.fullmatch(label) for label in labels):
            return None
        query = parse_qsl(parts.query, keep_blank_values=True, errors="strict")
    except ValueError:
        return None
    query = [
        (name, value) for name, value in query
        if not name.lower().startswith("utm_") and name.lower() not in TRACKERS
    ]
    return urlunsplit((parts.scheme, host, parts.path or "/", urlencode(query), ""))


def validate_entries(entries, day):
    if not isinstance(entries, list) or len(entries) > MAX_ENTRIES:
        raise NewsError("Invalid news entry count.")
    result = []
    for entry in entries:
        if not isinstance(entry, dict) or entry.keys() != ENTRY_PROPERTIES.keys():
            raise NewsError("Invalid news entry fields.")

        url = normalize_url(entry["url"])
        if url is None:
            raise NewsError("Invalid original source URL.")

        item = {}
        for field, limit in TEXT_LIMITS.items():
            value = entry[field]
            cleaned = " ".join(value.split()) if isinstance(value, str) else ""
            if field in ("summary", "why_it_matters"):
                citation = TRAILING_SOURCE_CITATION.search(cleaned)
                if citation and normalize_url(citation[1]) == url:
                    cleaned = cleaned[:citation.start()]
            if not cleaned or len(cleaned) > limit or INVALID_TEXT.search(cleaned):
                raise NewsError(f"Invalid news entry {field}.")
            item[field] = cleaned

        item["url"] = url
        try:
            published = calendar_date(entry["published_date"])
        except (ValueError, TypeError):
            raise NewsError("Invalid original publication date.") from None
        if not window_start(day, RECENT_DAYS) <= published <= day:
            raise NewsError("Original publication date outside the collection window.")
        item["published_date"] = published.isoformat()

        if entry["source_type"] not in SOURCE_TYPES:
            raise NewsError("Invalid news source type.")
        item["source_type"] = entry["source_type"]
        doi = entry["doi"]
        if doi is not None and (not isinstance(doi, str) or len(doi) > 300 or not DOI.fullmatch(doi)):
            raise NewsError("Invalid DOI.")
        item["doi"] = doi.casefold() if doi else None
        result.append(item)
    return result


def read_archive(directory):
    archive = {}
    for path in sorted(directory.glob("*.json")):
        try:
            day = calendar_date(path.stem)
            record = json.loads(path.read_text(encoding="utf-8"))
            if not isinstance(record, dict) or record.get("date") != day.isoformat():
                raise ValueError
            archive[day] = validate_entries(record.get("entries"), day)
        except (ValueError, NewsError):
            raise NewsError("A news archive record is invalid.") from None
    return archive


def entry_keys(entry):
    url = urlsplit(entry["url"])
    host = url.netloc.removeprefix("www.")
    yield "url", (host, url.path.rstrip("/"), tuple(sorted(parse_qsl(url.query, keep_blank_values=True))))
    title = unicodedata.normalize("NFKC", entry["title"]).casefold()
    yield "title", "".join(character for character in title if character.isalnum()) or title
    if entry["doi"]:
        yield "doi", entry["doi"]
    if host in {"doi.org", "dx.doi.org"} and DOI.fullmatch(url.path[1:]):
        yield "doi", url.path[1:].casefold()


def deduplicate(entries, archive):
    seen = {key for day in archive.values() for entry in day for key in entry_keys(entry)}
    selected = []
    for entry in entries:
        keys = set(entry_keys(entry))
        if seen.isdisjoint(keys):
            selected.append(entry)
        seen.update(keys)
    return selected


def request_payload(day, archive):
    recent = window_start(day, PUBLIC_DAYS)
    prior_titles = [
        entry["title"] for archive_day, entries in sorted(archive.items())
        if recent <= archive_day <= day for entry in entries
    ]
    return {
        "model": MODEL,
        "reasoning": {"effort": "medium"},
        "max_output_tokens": 9000,
        "store": False,
        "instructions": PROMPT.read_text(encoding="utf-8"),
        "input": json.dumps({
            "collection_date": day.isoformat(),
            "earliest_publication_date": window_start(day, RECENT_DAYS).isoformat(),
            "previously_featured_titles": prior_titles,
        }, ensure_ascii=False),
        "tools": [{"type": "web_search"}],
        "tool_choice": "required",
        "include": ["web_search_call.action.sources"],
        "text": {"format": {
            "type": "json_schema", "name": "people_analytics_news", "strict": True, "schema": RESPONSE_SCHEMA,
        }},
    }


def request_news(day, archive):
    api_key = os.environ.get("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise NewsError("Set OPENAI_API_KEY to collect news.")
    request = Request(
        "https://api.openai.com/v1/responses",
        data=json.dumps(request_payload(day, archive)).encode("utf-8"),
        headers={"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"},
    )
    try:
        with urlopen(request, timeout=300) as response:
            return json.load(response)
    except HTTPError as error:
        error.close()
        raise NewsError(f"OpenAI request failed with HTTP {error.code}.") from None
    except (OSError, HTTPException):
        raise NewsError("Could not reach OpenAI or the request timed out.") from None
    except ValueError:
        raise NewsError("OpenAI returned an invalid JSON response.") from None


def response_list(item, field):
    value = item.get(field)
    if value is None:
        return []
    if not isinstance(value, list):
        raise NewsError("OpenAI response contained invalid output.")
    return value


def extract_entries(response, day):
    if not isinstance(response, dict) or response.get("status") != "completed":
        raise NewsError("OpenAI response did not complete.")
    output = response.get("output")
    if not isinstance(output, list):
        raise NewsError("OpenAI response was missing output.")

    searched = False
    sources = set()
    texts = []
    for item in output:
        if not isinstance(item, dict):
            raise NewsError("OpenAI response contained invalid output.")
        if item.get("type") == "web_search_call" and item.get("status") == "completed":
            action = item.get("action") or {}
            if isinstance(action, dict):
                if action.get("type") == "search":
                    searched = True
                    for source in response_list(action, "sources"):
                        if isinstance(source, dict):
                            sources.add(normalize_url(source.get("url")))
                elif action.get("type") in ("open_page", "find_in_page"):
                    sources.add(normalize_url(action.get("url")))
        if item.get("type") == "message":
            for part in response_list(item, "content"):
                if not isinstance(part, dict) or part.get("type") != "output_text":
                    raise NewsError("OpenAI did not return a structured digest.")
                texts.append(part.get("text"))
                for annotation in response_list(part, "annotations"):
                    if isinstance(annotation, dict) and annotation.get("type") == "url_citation":
                        sources.add(normalize_url(annotation.get("url")))

    if not searched:
        raise NewsError("OpenAI did not complete a web search.")
    if len(texts) != 1 or not isinstance(texts[0], str):
        raise NewsError("OpenAI did not return a single structured digest.")
    try:
        digest = json.loads(texts[0])
    except ValueError:
        raise NewsError("OpenAI returned malformed digest JSON.") from None
    if not isinstance(digest, dict) or digest.keys() != {"entries"}:
        raise NewsError("OpenAI returned invalid digest fields.")
    entries = validate_entries(digest["entries"], day)
    sources.discard(None)
    grounded = [entry for entry in entries if entry["url"] in sources]
    if entries and not grounded:
        raise NewsError(
            f"No original source URLs matched a consulted page, search source, or citation "
            f"({len(entries)} candidates, {len(sources)} usable evidence URLs)."
        )
    if len(grounded) < len(entries):
        print(f"Skipped {len(entries) - len(grounded)} items without matching web search evidence.", file=sys.stderr)
    return grounded


def json_bytes(value):
    return (json.dumps(value, ensure_ascii=False, indent=2) + "\n").encode("utf-8")


def rss_date(value):
    return format_datetime(datetime.combine(calendar_date(value), time.min, timezone.utc), usegmt=True)


def render_rss(digest):
    atom = "http://www.w3.org/2005/Atom"
    ET.register_namespace("atom", atom)
    rss = ET.Element("rss", version="2.0")
    channel = ET.SubElement(rss, "channel")
    ET.SubElement(channel, "title").text = TITLE
    ET.SubElement(channel, "link").text = BASE_URL + "news.html"
    ET.SubElement(channel, "description").text = "Recent research and writing with practical implications for people analytics."
    ET.SubElement(channel, "language").text = "en"
    ET.SubElement(channel, "lastBuildDate").text = rss_date(digest["as_of"])
    ET.SubElement(channel, f"{{{atom}}}link", {
        "href": BASE_URL + "news-assets/news.xml", "rel": "self", "type": "application/rss+xml",
    })
    for day in digest["days"]:
        for entry in day["entries"]:
            item = ET.SubElement(channel, "item")
            ET.SubElement(item, "title").text = entry["title"]
            ET.SubElement(item, "link").text = entry["url"]
            ET.SubElement(item, "guid", isPermaLink="true").text = entry["url"]
            ET.SubElement(item, "pubDate").text = rss_date(day["date"])
            ET.SubElement(item, "category").text = entry["source_type"]
            source = " · ".join((entry["source"], entry["source_type"], entry["published_date"]))
            summary = html.escape(entry["summary"])
            practice = html.escape(entry["why_it_matters"])
            ET.SubElement(item, "description").text = (
                f"<p>{html.escape(source)}</p><p>{summary}</p>"
                f"<p><strong>Why it matters for practice</strong> — {practice}</p>"
            )
    ET.indent(rss, space="  ")
    return ET.tostring(rss, encoding="utf-8", xml_declaration=True) + b"\n"


def markdown_text(value):
    escaped = re.sub(r"([\\`*_{}\[\]<>#|~!])", r"\\\1", value)
    return re.sub(r"^([+-]|\d+[.)])(?= )", lambda match: match[0][:-1] + "\\" + match[0][-1], escaped)


def render_markdown(digest):
    lines = [f"# {TITLE}", "", f"30-day digest through {digest['as_of']} (Europe/Prague).", ""]
    if not digest["days"]:
        lines.extend(["There are no news items in the last 30 days.", ""])
    for day in digest["days"]:
        lines.extend([f"## {day['date']}", ""])
        for entry in day["entries"]:
            title = markdown_text(entry["title"])
            source = " · ".join((markdown_text(entry["source"]), entry["source_type"], entry["published_date"]))
            lines.extend([
                f"### [{title}](<{entry['url']}>)", "", source, "", markdown_text(entry["summary"]), "",
                f"**Why it matters for practice** — {markdown_text(entry['why_it_matters'])}", "",
            ])
    return "\n".join(lines).encode("utf-8")


def atomic_write(path, content):
    if path.exists() and path.read_bytes() == content:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    temp_path = None
    try:
        with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as temporary:
            temp_path = Path(temporary.name)
            temporary.write(content)
        temp_path.replace(path)
    finally:
        if temp_path is not None:
            temp_path.unlink(missing_ok=True)


def run(root, day, render_only):
    directory = root / "news_archive"
    archive = read_archive(directory)
    collected = not render_only and day not in archive
    if collected:
        candidates = extract_entries(request_news(day, archive), day)
        archive[day] = deduplicate(candidates, archive)

    days = [
        {"date": archive_day.isoformat(), "entries": entries}
        for archive_day, entries in sorted(archive.items(), reverse=True)
        if window_start(day, PUBLIC_DAYS) <= archive_day <= day and entries
    ]
    digest = {"as_of": day.isoformat(), "days": days}
    assets = {
        "news-data.json": json_bytes(digest), "news.xml": render_rss(digest), "news-digest.md": render_markdown(digest),
    }
    if collected:
        # A retry after a filesystem failure can rebuild the assets without another API request.
        record = {"date": day.isoformat(), "entries": archive[day]}
        atomic_write(directory / f"{day.isoformat()}.json", json_bytes(record))
    for public_root in (root, root / "_site"):
        for name, content in assets.items():
            atomic_write(public_root / "news-assets" / name, content)

    if render_only:
        result = "Rendered existing archive"
    elif collected:
        result = f"Collected {len(archive[day])} new items"
    else:
        result = "Reused the existing daily collection"
    print(f"{day}: {result}. Published {sum(len(group['entries']) for group in days)} items in the 30-day digest.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--date", type=calendar_date, help="Use a specific Prague calendar day (YYYY-MM-DD).")
    parser.add_argument("--render-only", action="store_true", help="Regenerate public files without calling OpenAI.")
    args = parser.parse_args()
    try:
        day = args.date or datetime.now(ZoneInfo("Europe/Prague")).date()
        run(ROOT, day, args.render_only)
    except ZoneInfoNotFoundError:
        parser.exit(1, "Prague timezone data is unavailable. Install tzdata or supply --date.\n")
    except NewsError as error:
        parser.exit(1, f"{error}\n")
    except OSError:
        parser.exit(1, "Could not read or write a news file.\n")


if __name__ == "__main__":
    main()
