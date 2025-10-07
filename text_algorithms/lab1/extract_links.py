import re


def extract_links(html: str) -> list[dict[str, str]]:
    """
    Extract all links from the given HTML string.

    Args:
        html (str): HTML content to analyze

    Returns:
        list[dict]: A list of dictionaries where each dictionary contains:
            - 'url': the href attribute value
            - 'title': the title attribute value (or None if not present)
            - 'text': the text between <a> and </a> tags
    """

    # TODO: Implement a regular expression pattern to extract links from HTML.
    # The pattern should capture three groups:
    # 1. The URL (href attribute value)
    # 2. The title attribute (which might not exist)
    # 3. The link text (content between <a> and </a> tags)
    pattern = r'<a\s+[^>]*?href=["\'](?P<url>[^"\']+)["\'][^>]*?(?:\s+title=["\'](?P<title>[^"\']+)["\'])?[^>]*?>(?P<text>.*?)</a>'

    links = []

    # TODO: Use re.finditer to find all matches of the pattern in the HTML
    # For each match, extract the necessary information and create a dictionary
    # Then append that dictionary to the 'links' list

    for match in re.finditer(pattern, html):
        links.append(
            {
                "url": match.group("url"),
                "title": match.group("title") if match.group("title") else None,
                "text": match.group("text").strip(),
            }
        )

    return links
