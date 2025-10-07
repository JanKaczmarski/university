CREATE TABLE Webinar (
    WebinarID INT NOT NULL, -- PK
    Title VARCHAR(100) NOT NULL,
    HostedDate DATE NOT NULL,
    RecordingID INT NOT NULL, -- FK
    Price MONEY NOT NULL, -- if price is 0.0 then webinar is free
    Language VARCHAR(255) NOT NULL,
    TranslatorID INT,-- FK
    CONSTRAINT Webinar_pk PRIMARY KEY (webinarID)
);

CREATE TABLE Recordings (
    RecordingID INT NOT NULL, --
    Path VARCHAR(255) NOT NULL,
    Length DECIMAL(10, 2) NOT NULL,
    CONSTRAINT Recordings_pk PRIMARY KEY (RecordingID)
);

-- Created by Vertabelo (http://vertabelo.com)
-- Last modification date: 2024-12-11 21:19:41.209

-- tables
-- Table: EmployeeRoles
CREATE TABLE EmployeeRoles (
    RoleID int  NOT NULL,
    RoleName varchar  NOT NULL,
    CONSTRAINT EmployeeRoles_pk PRIMARY KEY  (RoleID)
);

-- Table: Employees
CREATE TABLE Employees (
    EmployeeID int  NOT NULL,
    RoleID int  NOT NULL,
    FirstName varchar  NOT NULL,
    LastName varchar  NOT NULL,
    PhoneNumber varchar  NOT NULL,
    CONSTRAINT Employees_pk PRIMARY KEY  (EmployeeID)
);

-- Table: Languages
CREATE TABLE Languages (
    LanguageID int  NOT NULL,
    LanguageName int  NOT NULL,
    CONSTRAINT Languages_pk PRIMARY KEY  (LanguageID)
);

-- Table: OrderDetails
CREATE TABLE OrderDetails (
    OrderID int  NOT NULL,
    PaymentID int  NOT NULL,
    OrderDate int  NOT NULL,
    CONSTRAINT OrderDetails_pk PRIMARY KEY  (OrderID)
);

-- Table: Orders
CREATE TABLE Orders (
    OrderID int  NOT NULL,
    UserID int  NOT NULL,
    CONSTRAINT Orders_pk PRIMARY KEY  (OrderID,UserID)
);

-- Table: Payments
CREATE TABLE Payments (
    PaymentID int  NOT NULL,
    PaymentLink varchar  NOT NULL,
    Status int  NOT NULL,
    DueDate int  NOT NULL,
    Amount money  NOT NULL,
    CONSTRAINT Payments_pk PRIMARY KEY  (PaymentID)
);

-- Table: TranslatorLaunguages
CREATE TABLE TranslatorLaunguages (
    TranslatorID int  NOT NULL,
    LanguageID int  NOT NULL,
    CONSTRAINT TranslatorLaunguages_pk PRIMARY KEY  (TranslatorID,LanguageID)
);

-- Table: Translators
CREATE TABLE Translators (
    TranslatorID int  NOT NULL,
    FirstName int  NOT NULL,
    LastName int  NOT NULL,
    CONSTRAINT Translators_pk PRIMARY KEY  (TranslatorID)
);

-- Table: Users
CREATE TABLE Users (
    UserID int  NOT NULL,
    FirstName varchar  NOT NULL,
    LastName varchar  NOT NULL,
    AccountName varchar  NOT NULL,
    AccountPassword varchar  NOT NULL,
    Adress varchar  NOT NULL,
    PhoneNumber varchar  NOT NULL,
    Email varchar  NOT NULL,
    CONSTRAINT Users_pk PRIMARY KEY  (UserID)
);

-- End of file.
