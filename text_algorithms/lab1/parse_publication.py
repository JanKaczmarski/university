import re
from typing import Optional


def parse_publication(reference: str) -> Optional[dict]:
    """
    Parse academic publication reference and extract structured information.

    Expected reference format:
    Lastname, I., Lastname2, I2. (Year). Title. Journal, Volume(Issue), StartPage-EndPage.

    Example:
    Kowalski, J., Nowak, A. (2023). Analiza algorytmów tekstowych. Journal of Computer Science, 45(2), 123-145.

    Args:
        reference (str): Publication reference string

    Returns:
        Optional[dict]: A dictionary containing parsed publication data or None if the reference doesn't match expected format
    """
    authors_year_pattern = r"(?P<authors>(?:[A-ZĄĆĘŁŃÓŚŹŻ][a-ząćęłńóśźż]+, [A-ZĄĆĘŁŃÓŚŹŻ]\.(?:, )?)+) \((?P<year>\d{4})\)"
    title_journal_pattern = (
        r"\. (?P<title>.+?)\. (?P<journal>[A-ZĄĆĘŁŃÓŚŹŻa-ząćęłńóśźż\s]+), "
    )
    volume_issue_pages_pattern = (
        r"(?P<volume>\d+)\(?(?P<issue>\d+)?\)?, (?P<start_page>\d+)-(?P<end_page>\d+)\."
    )

    full_pattern = (
        authors_year_pattern + title_journal_pattern + volume_issue_pages_pattern
    )

    match = re.match(full_pattern, reference)
    if not match:
        return None

    authors_raw = match.group("authors")
    authors_list = []
    author_pattern = r"([A-ZĄĆĘŁŃÓŚŹŻ][a-ząćęłńóśźż]+), ([A-ZĄĆĘŁŃÓŚŹŻ])\."
    for author_match in re.finditer(author_pattern, authors_raw):
        authors_list.append(
            {"last_name": author_match.group(1), "initial": author_match.group(2)}
        )

    return {
        "authors": authors_list,
        "year": int(match.group("year")),
        "title": match.group("title"),
        "journal": match.group("journal"),
        "volume": int(match.group("volume")),
        "issue": int(match.group("issue")) if match.group("issue") else None,
        "pages": {
            "start": int(match.group("start_page")),
            "end": int(match.group("end_page")),
        },
    }
