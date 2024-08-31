"""
W pewnum panstwie znajduje sie N miast, wszytkie miasta laczymy siecia 
autostrad tak zeby kazde miasto mialo polaczenie z kazdym (Graf pelny)
Autostrady sa proste, dla kazdej pary 2 miast mamy ich czas budowy
Wszystkie autstrady zaczeto budowac rownoczesnie i jako cel
postanowiono zminimalizowac czas pomiedzy otwarciem pierwszej i ostatniej
autostrady
"""

"""
Pomysl:
1. Sortujemy
2. Bierzemy pod uwage n-1 elementow i sprawdzamy czy tworza drzewo
rozpinajace
3. jesli nie to usuwamy najmniejsza krawedz i sprawdzamy dalej
"""