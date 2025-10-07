import csv
import random
from datetime import datetime, timedelta


def read_webinar_date(file_name, webinar_id):
    """Funkcja odczytująca datę webinaru na podstawie WebinarID."""
    with open(file_name, mode='r') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if int(row['WebinarID']) == webinar_id:
                # Zwraca datę webinaru jako obiekt datetime
                return datetime.strptime(row['WebinarDate'], "%d-%m-%Y")
    raise ValueError(f"Nie znaleziono daty dla WebinarID: {webinar_id}")


with open('WebinarPresenceAndRecordingAvability.csv', mode='w', newline='') as csvfile:
    fieldnames = ['UserID', 'WebinarID', 'AvailableDue']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    
    writer.writeheader()
    
    for webID in range(1, 201):
        # Pobierz datę webinaru z pliku Webinars.csv
        webinar_date = read_webinar_date('Webinars.csv', webID)
        
        
        
        # Wybierz losowych 50 użytkowników spośród 1000
        random_users = random.sample(range(1, 1001), 50)
        
        for userID in random_users:
            # Wygeneruj dostępność do nagrania poprzez dodanie losowych dni
            random_days = random.randint(5, 60)
            available_due_date = webinar_date + timedelta(days=random_days)
            available_due_str = available_due_date.strftime("%d-%m-%Y")
            
            writer.writerow({
                "UserID": userID,
                "WebinarID": webID,
                "AvailableDue": available_due_str
            })
