import csv
from datetime import datetime

INPUT_NAME = "plan_zajec.csv"
OUT_NAME = "plan_zajec_gcal.csv"
DONT_INCLUDE = ["wykład"]

def convert_time_12h(time_str):
    """Konwertuje 24h HH:MM na 12h h:MMam/pm"""
    if not time_str or time_str.strip() == "":
        return ""
    t = datetime.strptime(time_str.strip(), "%H:%M")
    return t.strftime("%-I:%M%p").lower()  # np. 14:05 → 2:05pm

def convert_date(date_str):
    """Konwertuje YYYY-MM-DD → MM/DD/YYYY (Google Calendar format)"""
    return datetime.strptime(date_str.strip(), "%Y-%m-%d").strftime("%m/%d/%Y")

def to_gcal(input_name, output_name, dont_include=[]):
    out_rows = []

    # Nagłówek wymagany przez Google Calendar
    header = [
        "Subject", "Start Date", "Start Time", "End Date", "End Time",
        "All Day", "Description", "Location"
    ]
    out_rows.append(header)

    with open(input_name) as input_file:
        reader = csv.reader(input_file, delimiter=";")
        next(reader)  # pomijamy nagłówek wejściowy

        for row in reader:
            if row[4] in dont_include:
                continue

            subject = row[3].strip('"')
            start_date = convert_date(row[7])
            end_date = convert_date(row[7])  # to samo co start_date (jednodniowe)
            start_time = convert_time_12h(row[8])
            end_time = convert_time_12h(row[9])
            all_day = "false"
            description = f"Grupa: {row[5]} Prowadzący: {row[6]}"
            location = f"{row[10]} sala {row[11]}"

            new_row = [
                subject, start_date, start_time,
                end_date, end_time, all_day,
                description, location
            ]
            out_rows.append(new_row)

    with open(output_name, "w", newline="") as output_file:
        writer = csv.writer(output_file)
        writer.writerows(out_rows)


# uruchomienie
to_gcal(INPUT_NAME, OUT_NAME, DONT_INCLUDE)
