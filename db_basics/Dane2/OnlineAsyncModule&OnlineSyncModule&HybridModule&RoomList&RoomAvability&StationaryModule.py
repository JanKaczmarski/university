import csv
import random
import string
from datetime import datetime, timedelta


# Funkcje do generowania linków
def generate_random_youtube_link():
    video_id = ''.join(random.choices(string.ascii_letters + string.digits, k=11))
    return f"https://www.youtube.com/watch?v={video_id}"


def generate_random_meeting_link():
    meeting_platforms = ["https://zoom.us/j/", "https://meet.google.com/", "https://teams.microsoft.com/l/"]
    meeting_id = ''.join(random.choices(string.ascii_letters + string.digits, k=10))
    return f"{random.choice(meeting_platforms)}{meeting_id}"


# Funkcja do generowania kalendarza dat
def generate_date_range(start_year, end_year):
    start_date = datetime(start_year, 1, 1)
    end_date = datetime(end_year, 12, 31)
    delta = (end_date - start_date).days + 1
    return [(start_date + timedelta(days=i)).strftime("%d-%m-%Y") for i in range(delta)]


# Generowanie RoomList
room_ids = list(range(1, 31))
room_sizes = random.choices([20, 30, 50], weights=[0.2, 0.3, 0.5], k=len(room_ids))

with open('RoomList.csv', mode='w', newline='') as room_file:
    room_writer = csv.DictWriter(room_file, fieldnames=['RoomID', 'Size'])
    room_writer.writeheader()
    for room_id, size in zip(room_ids, room_sizes):
        room_writer.writerow({"RoomID": room_id, "Size": size})

# Generowanie RoomAvability
dates = generate_date_range(2025, 2027)

with open('RoomAvability.csv', mode='w', newline='') as avability_file:
    avability_writer = csv.DictWriter(avability_file, fieldnames=['RoomID', 'Date'])
    avability_writer.writeheader()
    for room_id in room_ids:
        for date in dates:
            avability_writer.writerow({"RoomID": room_id, "Date": date})

# Generowanie modułów
module_types = ["StationaryModule", "HybridModule", "OnlineSyncModule", "OnlineAsyncModule"]
stationary_rooms = [f"{floor}.{room}{suffix}" for floor in range(1, 5) for room in range(26, 30) for suffix in "AB"]

with open('StationaryModule.csv', mode='w', newline='') as stationary_file, \
        open('HybridModule.csv', mode='w', newline='') as hybrid_file, \
        open('OnlineSyncModule.csv', mode='w', newline='') as sync_file, \
        open('OnlineAsyncModule.csv', mode='w', newline='') as async_file:
    stationary_writer = csv.DictWriter(stationary_file, fieldnames=['ModuleID', 'Room', 'RoomID'])
    hybrid_writer = csv.DictWriter(hybrid_file, fieldnames=['ModuleID', 'VideoLink', 'RoomID'])
    sync_writer = csv.DictWriter(sync_file, fieldnames=['ModuleID', 'Link'])
    async_writer = csv.DictWriter(async_file, fieldnames=['ModuleID', 'VideoLink'])
    
    stationary_writer.writeheader()
    hybrid_writer.writeheader()
    sync_writer.writeheader()
    async_writer.writeheader()
    
    for module_id in range(1, 101):
        module_type = random.choice(module_types)
        
        if module_type == "StationaryModule":
            room = random.choice(stationary_rooms)
            room_id = random.randint(1, 30)
            stationary_writer.writerow({"ModuleID": module_id, "Room": room, "RoomID": room_id})
        
        elif module_type == "HybridModule":
            video_link = generate_random_youtube_link()
            room_id = random.randint(1, 30)
            hybrid_writer.writerow({"ModuleID": module_id, "VideoLink": video_link, "RoomID": room_id})
        
        elif module_type == "OnlineSyncModule":
            meeting_link = generate_random_meeting_link()
            sync_writer.writerow({"ModuleID": module_id, "Link": meeting_link})
        
        elif module_type == "OnlineAsyncModule":
            video_link = generate_random_youtube_link()
            async_writer.writerow({"ModuleID": module_id, "VideoLink": video_link})
