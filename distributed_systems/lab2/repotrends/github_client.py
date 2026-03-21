import os
from datetime import date, timedelta
from typing import Optional

import httpx

GITHUB_API = "https://api.github.com"
GITHUB_TOKEN = os.getenv("GITHUB_TOKEN", "")


def _headers() -> dict:
    h = {
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }
    if GITHUB_TOKEN:
        h["Authorization"] = f"Bearer {GITHUB_TOKEN}"
    return h


def _since_date(days: int) -> str:
    return (date.today() - timedelta(days=days)).isoformat()


async def search_trending(
    language: Optional[str],
    days: int,
    limit: int,
    client: httpx.AsyncClient,
) -> list[dict]:
    q = f"created:>{_since_date(days)}"
    if language:
        q += f" language:{language}"

    params = {
        "q": q,
        "sort": "stars",
        "order": "desc",
        "per_page": min(limit, 100),
    }

    resp = await client.get(
        f"{GITHUB_API}/search/repositories",
        headers=_headers(),
        params=params,
        timeout=15.0,
    )
    resp.raise_for_status()
    items = resp.json().get("items", [])
    return [_normalise(r) for r in items]


async def get_languages(client: httpx.AsyncClient) -> list[str]:
    return [
        "Python",
        "JavaScript",
        "TypeScript",
        "Go",
        "Rust",
        "Java",
        "C",
        "C++",
        "C#",
        "Ruby",
        "PHP",
        "Swift",
        "Kotlin",
        "Scala",
        "Shell",
        "Haskell",
        "Elixir",
        "Zig",
        "Lua",
        "R",
    ]


def _normalise(r: dict) -> dict:
    return {
        "platform": "github",
        "name": r.get("name", ""),
        "full_name": r.get("full_name", ""),
        "description": r.get("description") or "",
        "url": r.get("html_url", ""),
        "language": r.get("language") or "Unknown",
        "stars": r.get("stargazers_count", 0),
        "forks": r.get("forks_count", 0),
        "open_issues": r.get("open_issues_count", 0),
        "topics": r.get("topics", []),
        "created_at": r.get("created_at", ""),
        "updated_at": r.get("updated_at", ""),
    }
