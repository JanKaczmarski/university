import re
from collections import Counter


def analyze_text_file(filename: str) -> dict:
    try:
        with open(filename, "r", encoding="utf-8") as file:
            content = file.read()
    except Exception as e:
        return {"error": f"Could not read file: {str(e)}"}

    # Common English stop words to filter out from frequency analysis
    stop_words = {
        "the",
        "a",
        "an",
        "and",
        "or",
        "but",
        "in",
        "on",
        "at",
        "to",
        "for",
        "with",
        "by",
        "about",
        "as",
        "into",
        "like",
        "through",
        "after",
        "over",
        "between",
        "out",
        "of",
        "is",
        "are",
        "was",
        "were",
        "be",
        "been",
        "being",
        "have",
        "has",
        "had",
        "do",
        "does",
        "did",
        "this",
        "that",
        "these",
        "those",
        "it",
        "its",
        "from",
        "there",
        "their",
    }

    # TODO: Implement word extraction using regex
    # Find all words in the content (lowercase for consistency)
    words = re.findall(r"\b\w+\b", content)
    word_count = len(words)

    # TODO: Implement sentence splitting using regex
    # A sentence typically ends with ., !, or ? followed by a space
    # Be careful about abbreviations (e.g., "Dr.", "U.S.A.")

    sentence_pattern = r"\s*([^.!?]*?(?:Dr|Mr|Mrs|Ms|Prof|Jr|Sr|U\.S\.A|etc|e\.g|i\.e)\.|\s*[^.!?]+)[.!?]"
    sentences = [m.group().strip() for m in re.finditer(sentence_pattern, content)]
    sentence_count = len([s for s in sentences if s.strip()])

    # TODO: Implement email extraction using regex
    # Extract all valid email addresses from the content
    email_pattern = r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}"
    emails = re.findall(email_pattern, content)

    # TODO: Calculate word frequencies
    # Count occurrences of each word, excluding stop words and short words
    # Use the Counter class from collections
    frequent_words = Counter([word for word in words if len(word) > 1])
    for word in stop_words:
        if word in frequent_words:
            del frequent_words[word]
    frequent_words = dict(frequent_words.most_common(10))

    # TODO: Implement date extraction with multiple formats
    # Detect dates in various formats: YYYY-MM-DD, DD.MM.YYYY, MM/DD/YYYY, etc.
    # Create multiple regex patterns for different date formats
    date_patterns = [
        r"\b\d{4}-\d{2}-\d{2}\b",  # YYYY-MM-DD
        r"\b\d{2}\.\d{2}\.\d{4}\b",  # DD.MM.YYYY
        r"\b\d{2}/\d{2}/\d{4}\b",  # MM/DD/YYYY or DD/MM/YYYY
        r"\b\d{2}-\d{2}-\d{4}\b",  # MM-DD-YYYY or DD-MM-YYYY
        r"(?<!\d)\b\d{1,2} [A-Za-z]+ \d{4}\b",  # 12 March 2025 (Avoids matching "23")
        r"\b[A-Za-z]+ \d{1,2}, \d{4}\b",  # March 12, 2025
    ]
    date_regex = re.compile("|".join(date_patterns))
    dates = date_regex.findall(content)

    # TODO: Analyze paragraphs
    # Split the content into paragraphs and count words in each
    # Paragraphs are typically separated by one or more blank lines
    paragraphs = re.split(r"\n\s*\n", content)
    paragraph_sizes = {
        i + 1: len(re.findall(r"\b\w+\b", para)) for i, para in enumerate(paragraphs)
    }

    return {
        "word_count": word_count,
        "sentence_count": sentence_count,
        "emails": emails,
        "frequent_words": frequent_words,
        "dates": dates,
        "paragraph_sizes": paragraph_sizes,
    }


print(analyze_text_file("tests/test_file.md"))
