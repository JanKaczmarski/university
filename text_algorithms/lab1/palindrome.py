def is_palindrome(text):
    text = text.lower().replace(" ", "")

    # WSKAZÓWKA: Użyj notacji text[::-1] aby odwrócić tekst
    return text == text[::-1]

def make_palindrome(text):
    # Usuwamy spacje i konwertujemy do małych liter
    text = text.lower().replace(" ", "")

    # Sprawdzamy, czy już jest palindromem
    if is_palindrome(text):
        return text

    option1 = text + text[-2::-1]
    print(option1)

    option2 = text[-1:0:-1] + text
    print(option2)

    return option1 if len(option1) < len(option2) else option2

def palindrome_checker():
    # Pobieranie danych od użytkownika
    text = input("Wprowadź słowo lub frazę: ")

    # WSKAZÓWKA: Wykorzystaj funkcję isalnum() i list comprehension lub wyrażenie generujące
    clean_text = ''.join([s for s in text if s.isalnum()])

    # Sprawdzanie, czy to palindrom
    if is_palindrome(clean_text):
        print(f"\"{text}\" jest palindromem!")
    else:
        print(f"\"{text}\" nie jest palindromem.")
        suggested = make_palindrome(clean_text)
        print(f"Sugerowany palindrom: {suggested}")

# Wywołanie funkcji
if __name__ == "__main__":
    palindrome_checker()
