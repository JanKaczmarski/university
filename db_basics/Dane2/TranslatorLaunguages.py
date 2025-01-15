import csv
import random
with open('TranslatorLaunguages.csv', mode='w', newline='') as csvfile:
    fieldnames = ['TranslatorID','LanguageID']
    writer = csv.DictWriter(csvfile,fieldnames=fieldnames)
    
    writer.writeheader()

    for i in range(1,101):
        lang = random.randint(1,7)
        writer.writerow({f"TranslatorID":i,"LanguageID":lang})
    
    
    