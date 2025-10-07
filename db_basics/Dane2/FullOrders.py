import csv
import random
from datetime import datetime, timedelta


# Funkcja do generowania losowych dat
def generate_random_date(start_year, end_year):
    start_date = datetime(start_year, 1, 1)
    end_date = datetime(end_year, 12, 31)
    random_days = random.randint(0, (end_date - start_date).days)
    return start_date + timedelta(days=random_days)


# Liczba zamówień
total_orders = 4000

# Generowanie `Orders`
orders = []
order_details = []
order_details_id = 1
for order_id in range(1, total_orders + 1):
    user_id = random.randint(1, 1000)
    paid = 0
    order_date = generate_random_date(2023, 2024)
    orders.append({"OrderID": order_id, "UserID": user_id, "Paid": paid, "OrderDate": order_date.strftime("%Y-%m-%d")})
    
    # Liczba przedmiotów w zamówieniu (1 do 4)
    num_items = random.randint(1, 4)
    for _ in range(num_items):
        paid_date = order_date + timedelta(days=random.randint(1, 7))
        order_details.append(
            {"OrderDetailsID": order_details_id, "OrderID": order_id, "PaidDate": paid_date.strftime("%Y-%m-%d")})
        order_details_id += 1

# Łączna liczba OrderDetailsID
total_order_details = len(order_details)

# Podział OrderDetailsID
order_studies_count = int(0.1 * total_order_details)
order_webinars_count = int(0.4 * total_order_details)
order_courses_count = int(0.3 * total_order_details)
order_study_meeting_count = total_order_details - (order_studies_count + order_webinars_count + order_courses_count)

# Generowanie `OrderStudies`
order_studies = [{"OrderDetailsID": i, "StudiesID": random.randint(1, 10)}
                 for i in range(1, order_studies_count + 1)]

# Generowanie `OrderWebinars`
order_webinars = [{"OrderDetailsID": i, "WebinarID": random.randint(1, 200)}
                  for i in range(order_studies_count + 1, order_studies_count + order_webinars_count + 1)]

# Generowanie `OrderCourse`
order_courses = [{"OrderDetailsID": i, "CourseID": random.randint(1, 20)}
                 for i in range(order_studies_count + order_webinars_count + 1,
                                order_studies_count + order_webinars_count + order_courses_count + 1)]

# Generowanie `OrderStudyMeeting`
order_study_meetings = [{"OrderDetailsID": i, "StudyMeetingID": random.randint(1, 15000)}
                        for i in range(order_studies_count + order_webinars_count + order_courses_count + 1,
                                       total_order_details + 1)]


# Zapisywanie do plików CSV
def save_to_csv(filename, fieldnames, data):
    with open(filename, mode='w', newline='', encoding='utf-8') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(data)


save_to_csv("Orders.csv", ["OrderID", "UserID", "Paid", "OrderDate"], orders)
save_to_csv("OrderDetails.csv", ["OrderDetailsID", "OrderID", "PaidDate"], order_details)
save_to_csv("OrderStudies.csv", ["OrderDetailsID", "StudiesID"], order_studies)
save_to_csv("OrderWebinars.csv", ["OrderDetailsID", "WebinarID"], order_webinars)
save_to_csv("OrderCourse.csv", ["OrderDetailsID", "CourseID"], order_courses)
save_to_csv("OrderStudyMeeting.csv", ["OrderDetailsID", "StudyMeetingID"], order_study_meetings)

print("Pliki CSV zostały wygenerowane.")
