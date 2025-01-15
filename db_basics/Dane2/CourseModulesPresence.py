import csv
import random

with open('CourseModulesPresence.csv', mode='w', newline='') as csvfile:
    fieldnames = ['ModuleID', 'UserID','Presence']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    
    writer.writeheader()
    
    for i in range(1, 101):
        for _ in range(50):
            userID = random.randint(1, 1000)
            bit = 1 if random.randint(0,10) < 7 else 0
            writer.writerow({f"ModuleID": i,
                             "UserID": userID,
                            "Presence":bit})


