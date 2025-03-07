def text_counter():
    # Pobieranie danych od użytkownika
    text = input("Wprowadź tekst do analizy: ")
 
    # Obliczenia
    char_count = len(text)

    char_count_no_spaces = char_count - cnt_occurences(text, " ")

    words = text.split(" ")
    word_count = len(words)
 
    # Liczenie samogłosek i spółgłosek
    vowels = "aeiouAEIOUąęióóyĄĘIÓÓY"
    consonants = "bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQRSTVWXZćłńśźżĆŁŃŚŹŻ"
 
    vowel_count = cnt_occurences(text, vowels)

    consonant_count = cnt_occurences(text, consonants)
 
    # Wyświetlanie wyników
    print(f"\nAnaliza tekstu: \"{text}\"")
    print(f"Liczba słów: {word_count}")
    print(f"Liczba znaków (ze spacjami): {char_count}")
    print(f"Liczba znaków (bez spacji): {char_count_no_spaces}")
    print(f"Liczba samogłosek: {vowel_count}")
    print(f"Liczba spółgłosek: {consonant_count}")


def cnt_occurences(text: str, source: str):
    set_source = {s for s in source}
    cnt = 0
    for t in text:
        if t in set_source:
            cnt += 1

    return cnt

# TODO: jk: Think about changing keys from list[str] to set[str]
def cnt_len_without(text: str, keys: list[str]) -> int:
    set_keys = {k for k in keys}
    """
    Return length of string but don't count char `key`

    Positional Arguments:
    text -- string in which len you want to get
    keys -- characters you don't want included in this count
    """
    i = 0
    for char in text:
        if char not in set_keys:
            i += 1

    return i
 
# Wywołanie funkcji
if __name__ == "__main__":
    text_counter()
