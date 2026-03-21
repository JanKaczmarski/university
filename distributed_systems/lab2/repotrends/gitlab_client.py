import os
from datetime import date, timedelta
from typing import Optional

import httpx

GITLAB_API = "https://gitlab.com/api/v4"
GITLAB_TOKEN = os.getenv("GITLAB_TOKEN", "")


def _headers() -> dict:
    h = {"Content-Type": "application/json"}
    if GITLAB_TOKEN:
        h["PRIVATE-TOKEN"] = GITLAB_TOKEN
    return h


def _since_date(days: int) -> str:
    return (date.today() - timedelta(days=days)).isoformat() + "T00:00:00Z"


async def search_trending(
    language: Optional[str],
    days: int,
    limit: int,
    client: httpx.AsyncClient,
) -> list[dict]:
    params: dict = {
        "order_by": "star_count",
        "sort": "desc",
        "visibility": "public",
        "last_activity_after": _since_date(days),
        "per_page": min(limit, 100),
        "page": 1,
        "with_statistics": True,
    }
    if language:
        params["search"] = language

    resp = await client.get(
        f"{GITLAB_API}/projects",
        headers=_headers(),
        params=params,
        timeout=15.0,
    )
    resp.raise_for_status()
    items = resp.json()
    return [_normalise(r) for r in items]


def _normalise(r: dict) -> dict:
    return {
        "platform": "gitlab",
        "name": r.get("name", ""),
        "full_name": r.get("path_with_namespace", ""),
        "description": r.get("description") or "",
        "url": r.get("web_url", ""),
        "language": _detect_language(r),
        "stars": r.get("star_count", 0),
        "forks": r.get("forks_count", 0),
        "open_issues": r.get("open_issues_count", 0),
        "topics": r.get("topics", []),
        "created_at": r.get("created_at", ""),
        "updated_at": r.get("last_activity_at", ""),
    }


def _detect_language(r: dict) -> str:
    topics: list[str] = r.get("topics") or []
    common = {
        "python", "javascript", "typescript", "go", "golang", "rust",
        "java", "c", "cpp", "c++", "ruby", "php", "swift", "kotlin",
        "scala", "shell", "haskell", "elixir", "zig", "lua", "r",
    }
    for t in topics:
        if t.lower() in common:
            return t.capitalize()
    return "Unknown"
