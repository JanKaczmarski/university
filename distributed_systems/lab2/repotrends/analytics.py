import re
from collections import Counter
from typing import Optional

CATEGORY_KEYWORDS = {
    "AI / ML": ["ai", "ml", "llm", "neural", "pytorch", "gpt"],
    "Systems": ["kernel", "os", "embedded", "compiler", "systems"],
    "Web": ["web", "http", "api", "rest", "frontend", "framework"],
    "DevOps / Cloud": ["docker", "kubernetes", "cloud", "ci", "aws"],
    "Data / Analytics": ["data", "analytics", "etl", "pipeline", "spark"],
}


def _classify(repo: dict) -> str:
    tokens: set[str] = set()
    for t in repo.get("topics", []):
        tokens.add(t.lower())
    for word in (repo.get("description") or "").lower().split():
        tokens.add(word.strip(".,;:!?\"'()[]"))
    name_raw = repo.get("name", "").lower()
    tokens.add(name_raw)
    for part in re.split(r"[-_]", name_raw):
        tokens.add(part)

    scores: dict[str, int] = {}
    for cat, keywords in CATEGORY_KEYWORDS.items():
        score = sum(1 for kw in keywords if kw in tokens)
        if score:
            scores[cat] = score

    return max(scores, key=scores.__getitem__) if scores else "Other"


def _star_fork_stats(repos: list[dict]) -> dict:
    if not repos:
        return {
            "count": 0,
            "stars_avg": 0,
            "stars_max": 0,
            "stars_min": 0,
            "forks_avg": 0,
            "forks_max": 0,
            "forks_min": 0,
        }
    stars = [r["stars"] for r in repos]
    forks = [r["forks"] for r in repos]
    n = len(repos)
    return {
        "count": n,
        "stars_avg": round(sum(stars) / n, 1),
        "stars_max": max(stars),
        "stars_min": min(stars),
        "forks_avg": round(sum(forks) / n, 1),
        "forks_max": max(forks),
        "forks_min": min(forks),
    }


def analyse(github_repos: list[dict], gitlab_repos: list[dict], language_filter: Optional[str]) -> dict:
    for r in github_repos:
        r["category"] = _classify(r)
    for r in gitlab_repos:
        r["category"] = _classify(r)

    all_repos = github_repos + gitlab_repos

    lang_counter = Counter(r["language"] for r in all_repos)
    language_distribution = [{"language": lang, "count": cnt} for lang, cnt in lang_counter.most_common(15)]

    cat_counter = Counter(r["category"] for r in all_repos)
    category_distribution = [{"category": cat, "count": cnt} for cat, cnt in cat_counter.most_common()]

    gh_names = {r["name"].lower() for r in github_repos}
    gl_names = {r["name"].lower() for r in gitlab_repos}
    overlap_names = gh_names & gl_names
    only_github_names = gh_names - gl_names
    only_gitlab_names = gl_names - gh_names

    overlap_repos = [r for r in all_repos if r["name"].lower() in overlap_names]
    only_github = [r for r in github_repos if r["name"].lower() in only_github_names]
    only_gitlab = [r for r in gitlab_repos if r["name"].lower() in only_gitlab_names]

    top_repos = sorted(all_repos, key=lambda r: r["stars"], reverse=True)[:20]

    return {
        "query": {"language_filter": language_filter or "all"},
        "github": {"repos": github_repos, "stats": _star_fork_stats(github_repos)},
        "gitlab": {"repos": gitlab_repos, "stats": _star_fork_stats(gitlab_repos)},
        "combined": {
            "stats": _star_fork_stats(all_repos),
            "top_repos": top_repos,
            "language_distribution": language_distribution,
            "category_distribution": category_distribution,
        },
        "cross_platform": {
            "overlap_count": len(overlap_names),
            "overlap_repos": overlap_repos[:10],
            "only_on_github": only_github[:10],
            "only_on_gitlab": only_gitlab[:10],
        },
    }
