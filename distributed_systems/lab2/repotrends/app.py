import os
from pathlib import Path
from typing import Annotated, Optional

import httpx
from dotenv import load_dotenv
from fastapi import (Depends, FastAPI, Header, HTTPException, Query, Request,
                     status)
from fastapi.responses import FileResponse, JSONResponse
from fastapi.templating import Jinja2Templates
from slowapi import Limiter
from slowapi.errors import RateLimitExceeded
from slowapi.util import get_remote_address

import analytics
import github_client
import gitlab_client

load_dotenv()
API_KEY = os.getenv("API_KEY", "secret-demo-key")
STATIC_DIR = Path(__file__).parent / "static"
TEMPLATES_DIR = Path(__file__).parent / "templates"
templates = Jinja2Templates(directory=TEMPLATES_DIR)

limiter = Limiter(key_func=get_remote_address, default_limits=["10/minute"])

app = FastAPI(
    title="RepoTrends",
    description="Trending repositories from GitHub and GitLab with statistics and cross-platform comparison.",
    version="1.0.0",
)
app.state.limiter = limiter


@app.exception_handler(RateLimitExceeded)
async def rate_limit_handler(request: Request, exc: RateLimitExceeded):
    return JSONResponse(
        status_code=status.HTTP_429_TOO_MANY_REQUESTS,
        content={"detail": "Rate limit exceeded. Try again later."},
    )


async def require_api_key(x_api_key: Annotated[str | None, Header()] = None):
    if x_api_key != API_KEY:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid or missing API key. Provide it via the X-API-Key header.",
        )


def trending_params(
    language: Annotated[
        Optional[str],
        Query(
            description="Filter by programming language.",
            min_length=1,
            max_length=30,
            pattern=r"^[A-Za-z0-9+#\-]+$",
        ),
    ] = None,
    days: Annotated[
        int,
        Query(description="Look-back window in days.", ge=1, le=365),
    ] = 30,
    limit: Annotated[
        int,
        Query(description="Number of results per platform.", ge=1, le=50),
    ] = 10,
):
    return {"language": language, "days": days, "limit": limit}


@app.get("/", include_in_schema=False)
async def serve_index():
    return FileResponse(STATIC_DIR / "index.html")


@app.get("/search")
@limiter.limit("10/minute")
async def search(
    request: Request,
    params: dict = Depends(trending_params),
    api_key: Annotated[str, Query(description="API key.", min_length=1)] = "",
):
    if api_key != API_KEY:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Invalid or missing api_key.")

    language, days, limit = params["language"], params["days"], params["limit"]
    gh_repos = await _fetch(lambda c: github_client.search_trending(language, days, limit, c))
    gl_repos = await _fetch(lambda c: gitlab_client.search_trending(language, days, limit, c))
    result = analytics.analyse(gh_repos, gl_repos, language)
    result["query"].update({"days": days, "limit": limit})
    return templates.TemplateResponse(request=request, name="results.html", context=result)


@app.get("/api/languages")
@limiter.limit("30/minute")
async def get_languages(request: Request):
    async with httpx.AsyncClient() as client:
        langs = await github_client.get_languages(client)
    return {"languages": langs}


@app.get(
    "/api/trending",
    dependencies=[Depends(require_api_key)],
)
@limiter.limit("10/minute")
async def get_trending_combined(request: Request, params: dict = Depends(trending_params)):
    language = params["language"]
    days = params["days"]
    limit = params["limit"]

    gh_repos = await _fetch(lambda c: github_client.search_trending(language, days, limit, c))
    gl_repos = await _fetch(lambda c: gitlab_client.search_trending(language, days, limit, c))

    result = analytics.analyse(gh_repos, gl_repos, language)
    result["query"].update({"days": days, "limit": limit})
    return result


@app.get("/api/trending/github", dependencies=[Depends(require_api_key)])
@limiter.limit("10/minute")
async def get_trending_github(request: Request, params: dict = Depends(trending_params)):
    language, days, limit = params["language"], params["days"], params["limit"]
    repos = await _fetch(lambda c: github_client.search_trending(language, days, limit, c))
    result = analytics.analyse(repos, [], language)
    result["query"].update({"days": days, "limit": limit})
    return result


@app.get("/api/trending/gitlab", dependencies=[Depends(require_api_key)])
@limiter.limit("10/minute")
async def get_trending_gitlab(request: Request, params: dict = Depends(trending_params)):
    language, days, limit = params["language"], params["days"], params["limit"]
    repos = await _fetch(lambda c: gitlab_client.search_trending(language, days, limit, c))
    result = analytics.analyse([], repos, language)
    result["query"].update({"days": days, "limit": limit})
    return result


async def _fetch(fn):
    try:
        async with httpx.AsyncClient() as client:
            return await fn(client)
    except httpx.HTTPStatusError as exc:
        _handle_upstream_error(exc)
    except httpx.RequestError as exc:
        raise HTTPException(status_code=status.HTTP_502_BAD_GATEWAY, detail=f"Could not reach upstream: {exc}")


def _handle_upstream_error(exc: httpx.HTTPStatusError):
    code = exc.response.status_code
    if code == 401:
        raise HTTPException(
            status_code=status.HTTP_502_BAD_GATEWAY, detail="Upstream service returned 401 – check your API token."
        )
    if code == 403:
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail="Upstream rate limit hit. Try again later or add a GitHub token.",
        )
    if code == 422:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST, detail="Upstream service rejected the query – check parameters."
        )
    raise HTTPException(status_code=status.HTTP_502_BAD_GATEWAY, detail=f"Upstream service error {code}.")
