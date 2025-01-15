## 1. CourseModules

```sql
-- Table: CourseModules
CREATE TABLE CourseModules (
    ModuleID int NOT NULL,
    CourseID int NOT NULL,
    ModuleName varchar(50) NOT NULL,
    Date datetime NOT NULL,
    LanguageID int NOT NULL,
    EmployeeID int NOT NULL,
    TranslatorID int NOT NULL,
    CONSTRAINT CourseModules_pk PRIMARY KEY (ModuleID),
    CONSTRAINT CourseModules_Courses FOREIGN KEY (CourseID) REFERENCES Courses (CourseID),
    CONSTRAINT CourseModules_Employees FOREIGN KEY (EmployeeID) REFERENCES Employees (EmployeeID),
    CONSTRAINT CourseModules_Languages FOREIGN KEY (LanguageID) REFERENCES Languages (LanguageID),
    CONSTRAINT CourseModules_Translators FOREIGN KEY (TranslatorID) REFERENCES Translators (TranslatorID)
);
```

Opis: Tabela przechowuje moduły kursów, zawiera informacje takie jak nazwa modułu, data, język, odpowiedzialny pracownik i tłumacz.
Warunki integralności: Klucz główny ModuleID, klucze obce do tabel Courses, Employees, Languages, Translators.

## 2. CourseModulesDetails

```sql
-- Table: CourseModulesDetails
CREATE TABLE CourseModulesDetails (
    ModuleID int NOT NULL,
    UserID int NOT NULL,
    Presence bit NULL,
    CONSTRAINT CourseModulesDetails_pk PRIMARY KEY (ModuleID, UserID),
    CONSTRAINT CourseModulesDetails_CourseModules FOREIGN KEY (ModuleID) REFERENCES CourseModules (ModuleID),
    CONSTRAINT CourseModulesDetails_Students FOREIGN KEY (UserID) REFERENCES Users (UserID)
);
```

Opis: Detale obecności użytkowników na modułach kursu.
Warunki integralności: Klucz główny (ModuleID, UserID), klucze obce do tabel CourseModules, Users.

## 3. Courses

```sql
-- Table: Courses
CREATE TABLE Courses (
    CourseID int NOT NULL,
    CourseName varchar(50) NOT NULL,
    CourseDescription text NULL,
    CoursePrice money NULL DEFAULT 1000,
    EmployeeID int NOT NULL,
    CONSTRAINT Courses_pk PRIMARY KEY (CourseID),
    CONSTRAINT CHECK_0 CHECK ((CoursePrice > 0)),
    CONSTRAINT Courses_Employees FOREIGN KEY (EmployeeID) REFERENCES Employees (EmployeeID)
);
```

Opis: Informacje o kursach, takie jak nazwa, opis, cena i odpowiedzialny pracownik.
Warunki integralności: Klucz główny CourseID, klucz obcy do Employees, cena musi być większa niż 0.

4. EmployeeRoles



-- Table: EmployeeRoles
CREATE TABLE EmployeeRoles (
    RoleID int NOT NULL,
    RoleName varchar(255) NOT NULL,
    CONSTRAINT EmployeeRoles_pk PRIMARY KEY (RoleID)
);

Opis: Role pracowników.
Warunki integralności: Klucz główny RoleID.

5. Employees



-- Table: Employees
CREATE TABLE Employees (
    EmployeeID int NOT NULL,
    RoleID int NOT NULL,
    FirstName varchar(255) NOT NULL,
    LastName varchar(255) NOT NULL,
    PhoneNumber varchar(16) NOT NULL,
    CONSTRAINT Employees_pk PRIMARY KEY (EmployeeID),
    CONSTRAINT Employees_EmployeeRoles FOREIGN KEY (RoleID) REFERENCES EmployeeRoles (RoleID)
);

Opis: Dane pracowników, w tym imię, nazwisko, numer telefonu i rola.
Warunki integralności: Klucz główny EmployeeID, klucz obcy do EmployeeRoles.

6. Internship



-- Table: Internship
CREATE TABLE Internship (
    InternshipID int NOT NULL,
    StudiesID int NOT NULL,
    StartDate datetime NOT NULL,
    CONSTRAINT Internship_pk PRIMARY KEY (InternshipID),
    CONSTRAINT Internship_Studies FOREIGN KEY (StudiesID) REFERENCES Studies (StudiesID)
);

Opis: Informacje o stażach, zawierające ID studiów i datę rozpoczęcia.
Warunki integralności: Klucz główny InternshipID, klucz obcy do Studies.

7. InternshipDetails



-- Table: InternshipDetails
CREATE TABLE InternshipDetails (
    InternshipID int NOT NULL,
    UserID int NOT NULL,
    Presence bit NULL DEFAULT 1,
    CONSTRAINT InternshipDetails_pk PRIMARY KEY (InternshipID, UserID),
    CONSTRAINT InternshipDetails_Internship FOREIGN KEY (InternshipID) REFERENCES Internship (InternshipID),
    CONSTRAINT InternshipDetails_Students FOREIGN KEY (UserID) REFERENCES Users (UserID)
);

Opis: Detale dotyczące obecności użytkowników na stażach.
Warunki integralności: Klucz główny (InternshipID, UserID), klucze obce do Internship, Users.

8. Languages



-- Table: Languages
CREATE TABLE Languages (
    LanguageID int NOT NULL,
    LanguageName int NOT NULL,
    CONSTRAINT Languages_pk PRIMARY KEY (LanguageID)
);

Opis: Informacje o językach.
Warunki integralności: Klucz główny LanguageID.

9. OnlineAsyncMeeting



-- Table: OnlineAsyncMeeting
CREATE TABLE OnlineAsyncMeeting (
    MeetingID int NOT NULL,
    VideoLink varchar(50) NOT NULL,
    CONSTRAINT OnlineAsyncMeeting_pk PRIMARY KEY (MeetingID)
);

Opis: Spotkania online asynchroniczne z linkiem do wideo.
Warunki integralności: Klucz główny MeetingID.

10. OnlineAsyncModule



-- Table: OnlineAsyncModule
CREATE TABLE OnlineAsyncModule (
    ModuleID int NOT NULL,
    VideoLink varchar(50) NOT NULL,
    CONSTRAINT OnlineAsyncModule_pk PRIMARY KEY (ModuleID),
    CONSTRAINT CourseModules_OnlineAsynchronicModule FOREIGN KEY (ModuleID) REFERENCES CourseModules (ModuleID)
);

Opis: Moduły asynchroniczne online z linkiem do wideo.
Warunki integralności: Klucz główny ModuleID, klucz obcy do CourseModules.

11. OnlineSyncMeeting



-- Table: OnlineSyncMeeting
CREATE TABLE OnlineSyncMeeting (
    MeetingID int NOT NULL,
    Link varchar(50) NOT NULL,
    CONSTRAINT OnlineSyncMeeting_pk PRIMARY KEY (MeetingID)
);

Opis: Spotkania online synchroniczne z linkiem do spotkania.
Warunki integralności: Klucz główny MeetingID.

12. OnlineSyncModule



-- Table: OnlineSyncModule
CREATE TABLE OnlineSyncModule (
    ModuleID int NOT NULL,
    Link varchar(50) NOT NULL,
    CONSTRAINT OnlineSyncModule_pk PRIMARY KEY (ModuleID),
    CONSTRAINT CourseModules_OnlineSynchronicModule FOREIGN KEY (ModuleID) REFERENCES CourseModules (ModuleID)
);

Opis: Moduły synchroniczne online z linkiem do modułu.
Warunki integralności: Klucz główny ModuleID, klucz obcy do CourseModules.

13. OrderCourse



-- Table: OrderCourse
CREATE TABLE OrderCourse (
    OrderDetailsID int NOT NULL,
    CourseID int NOT NULL,
    CONSTRAINT OrderCourse_pk PRIMARY KEY (OrderDetailsID),
    CONSTRAINT OrderCourse_Courses FOREIGN KEY (CourseID) REFERENCES Courses (CourseID),
    CONSTRAINT OrderCourse_OrderDetails FOREIGN KEY (OrderDetailsID) REFERENCES OrderDetails (OrderDetailsID)
);

Opis: Zamówienia kursów.
Warunki integralności: Klucz główny OrderDetailsID, klucze obce do Courses, OrderDetails.

14. OrderDetails



-- Table: OrderDetails
CREATE TABLE OrderDetails (
    OrderDetailsID int NOT NULL,
    OrderID int NOT NULL,
    PaidDate datetime NOT NULL,
    CONSTRAINT OrderDetails_pk PRIMARY KEY (OrderDetailsID),
    CONSTRAINT OrderDetails_Orders FOREIGN KEY (OrderID) REFERENCES Orders (OrderID)
);

Opis: Szczegóły zamówienia, takie jak data płatności.
Warunki integralności: Klucz główny OrderDetailsID, klucz obcy do Orders.

15. OrderStudies



-- Table: OrderStudies
CREATE TABLE OrderStudies (
    OrderDetailsID int NOT NULL,
    StudiesID int NOT NULL,
    CONSTRAINT OrderStudies_pk PRIMARY KEY (OrderDetailsID),
    CONSTRAINT OrderStudies_OrderDetails FOREIGN KEY (OrderDetailsID) REFERENCES OrderDetails (OrderDetailsID),
    CONSTRAINT OrderStudies_Studies FOREIGN KEY (StudiesID) REFERENCES Studies (StudiesID)
);

Opis: Zamówienia na studia.
Warunki integralności: Klucz główny OrderDetailsID, klucze obce do OrderDetails, Studies.

16. OrderStudyMeeting



-- Table: OrderStudyMeeting
CREATE TABLE OrderStudyMeeting (
    OrderDetailsID int NOT NULL,
    StudyMeetingID int NOT NULL,
    CONSTRAINT OrderStudyMeeting_pk PRIMARY KEY (OrderDetailsID),
    CONSTRAINT OrderStudyMeeting_OrderDetails FOREIGN KEY (OrderDetailsID) REFERENCES OrderDetails (OrderDetailsID),
    CONSTRAINT OrderStudyMeeting_StudyMeeting FOREIGN KEY (StudyMeetingID) REFERENCES StudyMeeting (StudyMeetingID)
);

Opis: Zamówienia na spotkania studiów.
Warunki integralności: Klucz główny OrderDetailsID, klucze obce do OrderDetails, StudyMeeting.

17. OrderWebinars



-- Table: OrderWebinars
CREATE TABLE OrderWebinars (
    OrderDetailsID int NOT NULL,
    WebinarID int NOT NULL,
    CONSTRAINT OrderWebinars_pk PRIMARY KEY (OrderDetailsID),
    CONSTRAINT OrderWebinars_OrderDetails FOREIGN KEY (OrderDetailsID) REFERENCES OrderDetails (OrderDetailsID),
    CONSTRAINT Webinars_OrderWebinars FOREIGN KEY (WebinarID) REFERENCES Webinars (WebinarID)
);

Opis: Zamówienia na webinary.
Warunki integralności: Klucz główny OrderDetailsID, klucze obce do OrderDetails, Webinars.

18. Orders



-- Table: Orders
CREATE TABLE Orders (
    OrderID int NOT NULL,
    UserID int NOT NULL,
    Paid money NULL DEFAULT 0,
    OrderDate datetime NOT NULL,
    CONSTRAINT Orders_pk PRIMARY KEY (OrderID),
    CONSTRAINT CHECK_17 CHECK ((Paid >= 0)),
    CONSTRAINT Orders_Students FOREIGN KEY (UserID) REFERENCES Users (UserID)
);

Opis: Informacje o zamówieniach, w tym identyfikator użytkownika i data zamówienia.
Warunki integralności: Klucz główny OrderID, klucz obcy do Users, płatność musi być >= 0.

19. StationaryMeeting



-- Table: StationaryMeeting
CREATE TABLE StationaryMeeting (
    MeetingID int NOT NULL,
    Room varchar(10) NOT NULL,
    Limit int NULL DEFAULT 30,
    CONSTRAINT StationaryMeeting_pk PRIMARY KEY (MeetingID),
    CONSTRAINT CHECK_13 CHECK ((Limit > 0)),
    CONSTRAINT StudyMeeting_StationaryMeeting FOREIGN KEY (MeetingID) REFERENCES StudyMeeting (StudyMeetingID)
);

Opis: Spotkania stacjonarne, wraz z pokojem i limitem uczestników.
Warunki integralności: Klucz główny MeetingID, limit musi być > 0.

20. StationaryModule



-- Table: StationaryModule
CREATE TABLE StationaryModule (
    ModuleID int NOT NULL,
    Room varchar(10) NOT NULL,
    Limit int NULL DEFAULT 30,
    CONSTRAINT StationaryModule_pk PRIMARY KEY (ModuleID),
    CONSTRAINT CHECK_2 CHECK ((Limit > 0)),
    CONSTRAINT StationaryCourse_CourseModules FOREIGN KEY (ModuleID) REFERENCES CourseModules (ModuleID)
);

Opis: Moduły stacjonarne z pokojem i limitem uczestników.
Warunki integralności: Klucz główny ModuleID, limit musi być > 0.

21. Studies



-- Table: Studies
CREATE TABLE Studies (
    StudiesID int NOT NULL,
    StudiesName varchar(50) NOT NULL,
    EnrollFee money NOT NULL DEFAULT 1000,
    Employees_EmployeeID int NOT NULL,
    StudiesDescription text NULL,
    PriceIncrease real NULL DEFAULT 0.2,
    CONSTRAINT Studies_pk PRIMARY KEY (StudiesID),
    CONSTRAINT CHECK_9 CHECK ((EnrollFee > 0)),
    CONSTRAINT CHECK_10 CHECK ((PriceIncrease > 0)),
    CONSTRAINT Studies_Employees FOREIGN KEY (Employees_EmployeeID) REFERENCES Employees (EmployeeID)
);

Opis: Informacje o studiach, w tym opłata za zapis i pracownik odpowiedzialny.
Warunki integralności: Klucz główny StudiesID, klucz obcy do Employees, opłata i wzrost ceny muszą być > 0.

22. StudiesDetails



-- Table: StudiesDetails
CREATE TABLE StudiesDetails (
    StudiesID int NOT NULL,
    UserID int NOT NULL,
    CONSTRAINT StudiesDetails_pk PRIMARY KEY (StudiesID, UserID),
    CONSTRAINT StudiesDetails_Students FOREIGN KEY (UserID) REFERENCES Users (UserID),
    CONSTRAINT StudiesDetails_Studies FOREIGN KEY (StudiesID) REFERENCES Studies (StudiesID)
);

Opis: Detale dotyczące uczestnictwa użytkowników w studiach.
Warunki integralności: Klucz główny (StudiesID, UserID), klucze obce do Studies, Users.

23. StudyMeeting



-- Table: StudyMeeting
CREATE TABLE StudyMeeting (
    StudyMeetingID int NOT NULL,
    SubjectID int NOT NULL,
    MeetingName varchar(50) NOT NULL,
    Price money NOT NULL DEFAULT 100,
    Date datetime NOT NULL,
    LanguageID int NOT NULL,
    EmployeeID int NOT NULL,
    TranslatorID int NOT NULL,
    CONSTRAINT StudyMeeting_pk PRIMARY KEY (StudyMeetingID),
    CONSTRAINT CHECK_11 CHECK ((Price > 0)),
    CONSTRAINT StudyMeeting_Subject FOREIGN KEY (SubjectID) REFERENCES Subject (SubjectID),
    CONSTRAINT StudyMeeting_Employees FOREIGN KEY (EmployeeID) REFERENCES Employees (EmployeeID),
    CONSTRAINT StudyMeeting_Languages FOREIGN KEY (LanguageID) REFERENCES Languages (LanguageID),
    CONSTRAINT StudyMeeting_Translators FOREIGN KEY (TranslatorID) REFERENCES Translators (TranslatorID)
);

Opis: Spotkania studiów, wraz z nazwą, ceną, datą, językiem, pracownikiem i tłumaczem.
Warunki integralności: Klucz główny StudyMeetingID, klucze obce do Subject, Employees, Languages, Translators, cena musi być > 0.

24. StudyMeetingDetails



-- Table: StudyMeetingDetails
CREATE TABLE StudyMeetingDetails (
    StudyMeetingID int NOT NULL,
    UserID int NOT NULL,
    Presence bit NULL DEFAULT 1,
    CONSTRAINT StudyMeetingDetails_pk PRIMARY KEY (StudyMeetingID, UserID),
    CONSTRAINT StudyMeetingDetails_Students FOREIGN KEY (UserID) REFERENCES Users (UserID),
    CONSTRAINT StudyMeetingDetails_StudyMeeting FOREIGN KEY (StudyMeetingID) REFERENCES StudyMeeting (StudyMeetingID)
);

Opis: Detale dotyczące obecności użytkowników na spotkaniach studiów.
Warunki integralności: Klucz główny (StudyMeetingID, UserID), klucze obce do StudyMeeting, Users.

25. Subject



-- Table: Subject
CREATE TABLE Subject (
    SubjectID int NOT NULL,
    StudiesID int NOT NULL,
    SubjectName varchar(50) NOT NULL,
    SubjectDescription text NULL,
    Employees_EmployeeID int NOT NULL,
    CONSTRAINT Subject_pk PRIMARY KEY (SubjectID),
    CONSTRAINT Subject_Studies FOREIGN KEY (StudiesID) REFERENCES Studies (StudiesID),
    CONSTRAINT Subject_Employees FOREIGN KEY (Employees_EmployeeID) REFERENCES Employees (EmployeeID)
);

Opis: Informacje o przedmiotach, w tym nazwa i opis przedmiotu.
Warunki integralności: Klucz główny SubjectID, klucze obce do Studies, Employees.

26. SubjectDetails



-- Table: SubjectDetails
CREATE TABLE SubjectDetails (
    SubjectID int NOT NULL,
    UserID int NOT NULL,
    CONSTRAINT SubjectDetails_pk PRIMARY KEY (SubjectID, UserID),
    CONSTRAINT SubjectDetails_Students FOREIGN KEY (UserID) REFERENCES Users (UserID),
    CONSTRAINT SubjectDetails_Subject FOREIGN KEY (SubjectID) REFERENCES Subject (SubjectID)
);

Opis: Detale dotyczące uczestnictwa użytkowników w przedmiotach.
Warunki integralności: Klucz główny (SubjectID, UserID), klucze obce do Subject, Users.

27. TranslatorLanguages



-- Table: TranslatorLanguages
CREATE TABLE TranslatorLanguages (
    TranslatorID int NOT NULL,
    LanguageID int NOT NULL,
    CONSTRAINT TranslatorLanguages_pk PRIMARY KEY (TranslatorID, LanguageID),
    CONSTRAINT TranslatorLanguages_Languages FOREIGN KEY (LanguageID) REFERENCES Languages (LanguageID),
    CONSTRAINT TranslatorLanguages_Translators FOREIGN KEY (TranslatorID) REFERENCES Translators (TranslatorID)
);

Opis: Języki tłumaczy.
Warunki integralności: Klucz główny (TranslatorID, LanguageID), klucze obce do Languages, Translators.

28. Translators



-- Table: Translators
CREATE TABLE Translators (
    TranslatorID int NOT NULL,
    FirstName varchar(255) NOT NULL,
    LastName varchar(255) NOT NULL,
    CONSTRAINT Translators_pk PRIMARY KEY (TranslatorID)
);

Opis: Dane tłumaczy, w tym imię i nazwisko.
Warunki integralności: Klucz główny TranslatorID.

29. Users



-- Table: Users
CREATE TABLE Users (
    UserID int NOT NULL,
    FirstName varchar(30) NOT NULL,
    LastName varchar(30) NOT NULL,
    AccountName varchar(16) NOT NULL,
    AccountPassword varchar(255) NOT NULL,
    Adress varchar(255) NOT NULL,
    PhoneNumber varchar(16) NOT NULL,
    Email varchar(255) NOT NULL,
    CONSTRAINT Students_pk PRIMARY KEY (UserID),
    CONSTRAINT CHECK_3 CHECK ((LEN(PhoneNumber) = 9 AND ISNUMERIC(PhoneNumber) = 1)),
    CONSTRAINT CHECK_4 CHECK ((Email LIKE '%_@__%.__%'))
);

Opis: Informacje o użytkownikach, w tym dane logowania i kontaktowe.
Warunki integralności: Klucz główny UserID, numer telefonu musi mieć 9 cyfr, email musi mieć poprawny format.

30. WebinarDetails



-- Table: WebinarDetails
CREATE TABLE WebinarDetails (
    UserID int NOT NULL,
    WebinarID int NOT NULL,
    AvailableDue date NOT NULL,
    CONSTRAINT WebinarDetails_pk PRIMARY KEY (UserID, WebinarID),
    CONSTRAINT WebinarDetails_Students FOREIGN KEY (UserID) REFERENCES Users (UserID),
    CONSTRAINT WebinarDetails_Webinars FOREIGN KEY (WebinarID) REFERENCES Webinars (WebinarID)
);

Opis: Detale dotyczące dostępności webinarów dla użytkowników.
Warunki integralności: Klucz główny (UserID, WebinarID), klucze obce do Users, Webinars.

31. Webinars



-- Table: Webinars
CREATE TABLE Webinars (
    WebinarID int NOT NULL,
    WebinarName varchar(50) NOT NULL,
    WebinarPrice money NULL DEFAULT 500,
    VideoLink varchar(50) NOT NULL,
    WebinarDate datetime NOT NULL,
    WebinarDescription text NULL,
    LanguageID int NOT NULL,
    EmployeeID int NOT NULL,
    TranslatorID int NOT NULL,
    CONSTRAINT Webinars_pk PRIMARY KEY (WebinarID),
    CONSTRAINT CHECK_15 CHECK ((WebinarPrice >= 0)),
    CONSTRAINT Webinars_Employees FOREIGN KEY (EmployeeID) REFERENCES Employees (EmployeeID),
    CONSTRAINT Webinars_Languages FOREIGN KEY (LanguageID) REFERENCES Languages (LanguageID),
    CONSTRAINT Webinars_Translators FOREIGN KEY (TranslatorID) REFERENCES Translators (TranslatorID)
);

Opis: Informacje o webinarach, w tym nazwa, cena, data i język.
Warunki integralności: Klucz główny WebinarID, klucze obce do Employees, Languages, Translators, cena musi być >= 0.
