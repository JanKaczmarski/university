import csv
import random
from datetime import datetime, timedelta

# Słownik z nazwami i opisami kursów
courses_data = [
    {"name": "Python Programming Basics",
     "description": "Learn the basics of Python programming in this introductory course. Master fundamental syntax and programming techniques."},
    {"name": "Advanced Excel",
     "description": "Unlock the full potential of Excel with advanced formulas and tools. Learn to analyze data effectively."},
    {"name": "Project Management Essentials",
     "description": "Discover the fundamentals of successful project management. Learn planning, execution, and team management techniques."},
    {"name": "Digital Marketing Strategies",
     "description": "Understand how to create impactful digital marketing campaigns. Explore SEO, social media, and email marketing."},
    {"name": "Public Speaking Skills",
     "description": "Enhance your confidence and clarity in public speaking. Master the art of engaging presentations."},
    {"name": "Basics of Web Development",
     "description": "Learn the core technologies behind websites, including HTML, CSS, and JavaScript. Build your first web pages."},
    {"name": "Data Science Introduction",
     "description": "Dive into the world of data science. Learn data analysis, visualization, and introductory machine learning."},
    {"name": "Creative Writing Workshop",
     "description": "Develop your creative writing skills in this interactive workshop. Explore storytelling, poetry, and more."},
    {"name": "Graphic Design Fundamentals",
     "description": "Learn the principles of graphic design using modern tools. Create visually appealing designs for various media."},
    {"name": "AI and Machine Learning Basics",
     "description": "Understand the fundamentals of AI and machine learning. Explore real-world applications and techniques."},
    {"name": "Cybersecurity Essentials",
     "description": "Learn how to protect systems and data from cyber threats. Gain foundational knowledge in cybersecurity principles."},
    {"name": "Introduction to Blockchain",
     "description": "Explore the basics of blockchain technology. Understand its applications in cryptocurrencies and beyond."},
    {"name": "Photography for Beginners",
     "description": "Master the essentials of photography, from camera settings to composition. Capture stunning images."},
    {"name": "Social Media Marketing",
     "description": "Learn to leverage social media platforms for marketing. Create strategies for audience engagement."},
    {"name": "Time Management Tips",
     "description": "Improve your productivity with effective time management techniques. Learn to prioritize and plan efficiently."},
    {"name": "Introduction to SQL",
     "description": "Learn to manage and query databases using SQL. Gain hands-on experience with real-world scenarios."},
    {"name": "Financial Planning Basics",
     "description": "Understand personal finance and budgeting. Learn to create a financial plan for long-term success."},
    {"name": "Basics of Video Editing",
     "description": "Edit videos like a pro with industry-standard tools. Learn cutting, transitions, and effects."},
    {"name": "Mobile App Development",
     "description": "Discover how to create mobile apps for Android and iOS. Learn the fundamentals of app development."},
    {"name": "Interior Design Essentials",
     "description": "Explore the principles of interior design. Learn to create aesthetically pleasing and functional spaces."},
]
courses_modules_data = [
    {
        "course_name": "Python Programming Basics",
        "modules": [
            "Introduction to Python",
            "Variables and Data Types",
            "Control Flow (Loops and Conditionals)",
            "Functions and Modules",
            "Working with Libraries"
        ]
    },
    {
        "course_name": "Advanced Excel",
        "modules": [
            "Advanced Formulas and Functions",
            "Pivot Tables and Data Analysis",
            "Data Visualization in Excel",
            "Excel Macros and Automation",
            "Excel for Data Cleaning"
        ]
    },
    {
        "course_name": "Project Management Essentials",
        "modules": [
            "Project Planning and Scheduling",
            "Budgeting and Cost Control",
            "Risk Management in Projects",
            "Team Collaboration and Communication",
            "Project Evaluation and Closure"
        ]
    },
    {
        "course_name": "Digital Marketing Strategies",
        "modules": [
            "SEO Basics and Strategies",
            "Social Media Marketing Techniques",
            "Content Marketing Fundamentals",
            "Pay-Per-Click Advertising",
            "Email Marketing Campaigns"
        ]
    },
    {
        "course_name": "Public Speaking Skills",
        "modules": [
            "Overcoming Stage Fright",
            "Effective Speech Writing",
            "Body Language and Delivery",
            "Audience Engagement Techniques",
            "Handling Q&A Sessions"
        ]
    },
    {
        "course_name": "Basics of Web Development",
        "modules": [
            "Introduction to HTML",
            "CSS for Styling Websites",
            "JavaScript for Web Interactivity",
            "Responsive Web Design",
            "Basic Web Hosting and Deployment"
        ]
    },
    {
        "course_name": "Data Science Introduction",
        "modules": [
            "Introduction to Data Science",
            "Data Collection and Cleaning",
            "Exploratory Data Analysis",
            "Data Visualization Techniques",
            "Intro to Machine Learning"
        ]
    },
    {
        "course_name": "Creative Writing Workshop",
        "modules": [
            "Developing a Writing Routine",
            "Character Development",
            "Building Compelling Plots",
            "Writing Dialogue",
            "Editing and Revising Your Work"
        ]
    },
    {
        "course_name": "Graphic Design Fundamentals",
        "modules": [
            "Principles of Design",
            "Typography and Fonts",
            "Color Theory in Design",
            "Creating Digital Graphics",
            "Using Graphic Design Software"
        ]
    },
    {
        "course_name": "AI and Machine Learning Basics",
        "modules": [
            "Introduction to Artificial Intelligence",
            "Supervised vs Unsupervised Learning",
            "Neural Networks and Deep Learning",
            "Machine Learning Algorithms",
            "Evaluating and Tuning Models"
        ]
    },
    {
        "course_name": "Cybersecurity Essentials",
        "modules": [
            "Introduction to Cybersecurity",
            "Types of Cyber Threats",
            "Basic Cryptography",
            "Network Security Fundamentals",
            "Incident Response and Mitigation"
        ]
    },
    {
        "course_name": "Introduction to Blockchain",
        "modules": [
            "What is Blockchain?",
            "How Blockchain Works",
            "Blockchain Use Cases",
            "Smart Contracts and Decentralized Apps",
            "Blockchain Security and Privacy"
        ]
    },
    {
        "course_name": "Photography for Beginners",
        "modules": [
            "Understanding Your Camera",
            "Composition and Framing",
            "Lighting Techniques",
            "Portrait Photography Basics",
            "Editing Photos with Software"
        ]
    },
    {
        "course_name": "Social Media Marketing",
        "modules": [
            "Creating a Social Media Strategy",
            "Content Creation for Social Media",
            "Social Media Advertising",
            "Social Media Analytics",
            "Community Management"
        ]
    },
    {
        "course_name": "Time Management Tips",
        "modules": [
            "Setting SMART Goals",
            "Prioritizing Your Tasks",
            "Managing Distractions",
            "Using Time Management Tools",
            "Building Productive Habits"
        ]
    },
    {
        "course_name": "Introduction to SQL",
        "modules": [
            "SQL Basics and Syntax",
            "Selecting Data with SQL Queries",
            "Data Filtering and Sorting",
            "Joins and Relationships",
            "Advanced SQL Functions"
        ]
    },
    {
        "course_name": "Financial Planning Basics",
        "modules": [
            "Setting Financial Goals",
            "Creating a Budget",
            "Managing Debt",
            "Investing for Beginners",
            "Planning for Retirement"
        ]
    },
    {
        "course_name": "Basics of Video Editing",
        "modules": [
            "Introduction to Video Editing Software",
            "Editing Techniques and Transitions",
            "Adding Text and Effects",
            "Audio Editing for Videos",
            "Exporting and Sharing Videos"
        ]
    },
    {
        "course_name": "Mobile App Development",
        "modules": [
            "Introduction to Mobile App Development",
            "Building User Interfaces",
            "Developing App Logic",
            "Mobile App Testing",
            "Publishing Your App"
        ]
    },
    {
        "course_name": "Interior Design Essentials",
        "modules": [
            "Introduction to Interior Design",
            "Color Schemes and Layouts",
            "Furniture Selection and Arrangement",
            "Lighting Design",
            "Sustainable Design Practices"
        ]
    }
]


# Funkcja generująca losową datę na przełomie lat 2025/26
def generate_random_start_date(start_date,end_date):
    random_days = random.randint(0, (end_date - start_date).days)
    return start_date + timedelta(days=random_days)


# Tworzenie pliku CSV dla kursów
with open('Courses.csv', mode='w', newline='') as csvfile:
    fieldnames = ['CourseID', 'CourseName', 'CourseDescription', 'CoursePrice', 'EmployeeID', 'CourseDurationID']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()

    # Tworzenie pliku CSV dla dat kursów
    with open('CourseDates.csv', mode='w', newline='') as datefile:
        date_fieldnames = ['CourseDurationID', 'StartDate', 'EndDate']
        date_writer = csv.DictWriter(datefile, fieldnames=date_fieldnames)
        date_writer.writeheader()

        # Tworzenie pliku CSV dla modułów kursów
        with open('CourseModules.csv', mode='w', newline='') as modulefile:
            module_fieldnames = ['ModuleID', 'CourseID', 'ModuleName', 'Date', 'LanguageID', 'EmployeeID', 'TranslatorID']
            module_writer = csv.DictWriter(modulefile, fieldnames=module_fieldnames)
            module_writer.writeheader()

            # Generowanie danych
            prev_module_amnt = 0
            for course_id, course in enumerate(courses_data, start=1):
                course_price = random.randint(50, 500) * 100
                employee_id = random.randint(1, 100)
                course_duration = random.randint(7, 60)

                # Generowanie daty rozpoczęcia i obliczanie daty zakończenia
                start_date = generate_random_start_date(datetime(2025, 1, 1),datetime(2026, 12, 31))
                end_date = start_date + timedelta(days=course_duration)

                # Zapis do Courses.csv
                writer.writerow({
                    "CourseID": course_id,
                    "CourseName": course['name'],
                    "CourseDescription": course['description'],
                    "CoursePrice": course_price,
                    "EmployeeID": employee_id,
                    "CourseDurationID": course_duration
                })

                # Zapis do CourseDates.csv
                date_writer.writerow({
                    "CourseDurationID": course_duration,
                    "StartDate": start_date.strftime("%d-%m-%Y"),
                    "EndDate": end_date.strftime("%d-%m-%Y")
                })

                # Zapis do CourseModules.csv
                for module_id, module_name in enumerate(courses_modules_data[course_id - 1]["modules"], start=1):
                    language_id = random.randint(1, 7)
                    translator_id = random.randint(1, 100)
                    module_date = generate_random_start_date(start_date,start_date+timedelta(days=course_duration//5)) # Żeby się mieściło w zakresie kursu

                    module_writer.writerow({
                        "ModuleID": module_id+prev_module_amnt,
                        "CourseID": course_id,
                        "ModuleName": module_name,
                        "Date": module_date.strftime("%d-%m-%Y"),
                        "LanguageID": language_id,
                        "EmployeeID": employee_id,
                        "TranslatorID": translator_id
                    })
                prev_module_amnt += module_id