import csv
from os import DirEntry
import random
import string
from datetime import datetime, timedelta
from types import new_class

STUDIES_COUNT=50
USERS_COUNT=1000
SEM_COUNT=7
EMPLOYEE_COUNT=100
SUBJECTS_PER_SEMESTER=(4,6)

desc_filler="""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. """

# 7 semesters per studies
# 4 to 8 subjects per semester
# 40 students per 1 studies
# Internship 1 per studies
# Intership presence - 14 days each - for each student in studies
# Studies Grade - pers studies - per student


# Funkcja do generowania kalendarza dat
def generate_past_date_time():
    year = random.randint(0, 10)
    month = random.randint(0, 11)
    day = random.randint(0, 20)
    total_days = year * 365 + month * 31 + day

    return (datetime.now() - timedelta(days=total_days)).strftime("%Y-%m-%d %H:%M:%S")


# Generowanie RoomList
room_ids = list(range(1, 31))
room_sizes = random.choices([20, 30, 50], weights=[0.2, 0.3, 0.5], k=len(room_ids))

studies_id = list(range(1, STUDIES_COUNT))
subjects = [
    "Physics", 
    "Computer Science", 
    "Biology", 
    "Mathematics", 
    "Chemistry", 
    "Economics", 
    "Engineering", 
    "Philosophy", 
    "Linguistics", 
    "Psychology"
]
adjectives = [
    "Applied", 
    "Theoretical", 
    "Experimental", 
    "Computational", 
    "Advanced"
]

core_subjects = [
    "Mathematics", "Physics", "Chemistry", "Biology",
    "Computer Science", "Statistics", "Economics", "Philosophy",
    "Ethics", "Introduction to Programming", "Data Structures",
    "Algorithms", "Technical Writing"
]
specialized_subjects = [
    "Network Security", "Artificial Intelligence", "Marine Biology",
    "Quantum Physics", "Digital Marketing", "Thermodynamics",
    "Genetic Engineering", "Cognitive Psychology", "Astrophysics",
    "Environmental Policy", "Biomechanics", "Human-Computer Interaction"
]
sub_adjectives = [
    "Applied", "Theoretical", "Advanced", "Fundamentals of",
    "Modern", "Experimental", "Computational"
]

studies_names = []
for adj in adjectives:
    for subj in subjects:
        studies_names.append(adj + " " + subj)

subj_names = [f"{adj} {subj}" for adj in sub_adjectives for subj in core_subjects + specialized_subjects]

with open('Studies.csv', mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=['StudiesID', 'StudiesName', 'EnrollFee', 'EmployeeID', 'Studies Description'])
    writer.writeheader()
    for i in range(STUDIES_COUNT):
        writer.writerow({"StudiesID": i + 1, "StudiesName": studies_names[i], "EnrollFee": float(random.randint(1000, 1500)),
                              "EmployeeID": random.randint(1, EMPLOYEE_COUNT), "Studies Description": desc_filler * random.randint(1,3)})

with open('Semesters.csv', mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=['StudiesID', 'SemesterNumber'])
    writer.writeheader()
    for i in range(STUDIES_COUNT):
        for j in range(SEM_COUNT):
            writer.writerow({"StudiesID": i + 1, "SemesterNumber": j + 1})


# Generate Subject Distribution
subject_id = 1
with open("Subject.csv", mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=['SubjectID', 'SubjectName', 'SubjectDescription', 'EmployeeID', 'SemesterNumber', 'StudiesID'])
    writer.writeheader()
    
    for study_id in range(1, STUDIES_COUNT + 1):
        for semester in range(1, SEM_COUNT + 1):
            # Unpacking tuple values using *tuple[]
            num_subjects = random.randint(*SUBJECTS_PER_SEMESTER)
            selected_subjects = random.sample(subj_names, num_subjects)

            for subject in selected_subjects:
                writer.writerow({
                    "SubjectID": subject_id,
                    "SubjectName": subject,
                    "SubjectDescription": f"{subject} is a course designed to deepen understanding.",
                    "EmployeeID": random.randint(1, EMPLOYEE_COUNT),
                    "SemesterNumber": semester,
                    "StudiesID": study_id
                })
                subject_id += 1


interID = 1
with open("Internship.csv", mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=["InternshipID", "StudiesID", "StartDate"])
    writer.writeheader()
    for studie_id in range(1, STUDIES_COUNT + 1):
        writer.writerow({"InternshipID": interID, "StudiesID": studie_id, "StartDate": generate_past_date_time()})
        interID += 1


USER_START = 1
RISE = USERS_COUNT//STUDIES_COUNT

user_start = USER_START
rise = RISE
with open("InternshipPresence.csv", mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=["InternshipID", "UserID", "Presence"])
    writer.writeheader()
    for studie_id in range(1, STUDIES_COUNT + 1):
        for user_id in range(user_start, user_start + rise):
            writer.writerow({"InternshipID": studie_id, "UserID": user_id, "Presence": 1 if random.random() >= 0.05 else 0})
        user_start += rise


user_start = USER_START
rise = RISE
with open("StudiesGrades.csv", mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=["StudiesID", "UserID", "Grade"])
    writer.writeheader()
    for studie_id in range(1, STUDIES_COUNT + 1):
        for user_id in range(user_start, user_start + rise):
            writer.writerow({"StudiesID": studie_id, "UserID": user_id, "Grade": random.randint(3, 5) if random.random() >= 0.1 else 2})
        user_start += rise



