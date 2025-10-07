# Widoki
## Zestawienie przychodów dla każdego szkolenia


CREATE VIEW FINANCIAL_REPORT AS
SELECT w.WebinarID AS ID, w.WebinarName AS Name, 'Webinar' AS Type, w.WebinarPrice *
    (SELECT count(*)
    FROM OrderWebinars ow JOIN
    OrderDetails od ON ow.OrderDetailsID = od.OrderDetailsID JOIN
    Orders o ON od.OrderID = o.OrderID
    WHERE ow.WebinarID = w.WebinarID) AS TotalIncome
FROM Webinars w
UNION
SELECT c.CourseID AS ID, c.CourseName AS Name, 'Course' AS Type, c.CoursePrice *
    (SELECT count(*)
    FROM OrderCourse oc JOIN
    OrderDetails od ON oc.OrderDetailsID = od.OrderDetailsID JOIN
    Orders o ON od.OrderID = o.OrderID
    WHERE oc.CourseID = c.CourseID) AS TotalIncome
FROM Courses c
UNION
SELECT s.StudiesID AS ID, s.StudiesName AS Name, 'Study' AS Type, s.StudiesEntryFeePrice *
    (SELECT count(*)
    FROM OrderStudies os JOIN
    OrderDetails od ON os.OrderDetailsID = od.OrderDetailsID JOIN
    Orders o ON od.OrderID = o.OrderID
    WHERE os.StudiesID = s.StudiesID) +
    (SELECT sum(sm.MeetingPrice)
    FROM StudyMeeting sm JOIN
    Subject sb ON sm.SubjectID = sb.SubjectID
    WHERE sb.StudiesID = s.StudiesID) AS TotalIncome
FROM Studies s


## Zestawienie przychodów dla każdego webinaru


CREATE VIEW WEBINARS_FINANCIAL_REPORT AS
SELECT ID AS 'Webinar ID', Name, TotalIncome
FROM FINANCIAL_REPORT
WHERE Type = 'Webinar'


## Zestawienie przychodów dla każdego kursu


CREATE VIEW COURSES_FINANCIAL_REPORT AS
SELECT ID AS 'Course ID', Name, TotalIncome
FROM FINANCIAL_REPORT
WHERE Type = 'Course'


## Zestawienie przychodów dla każdego studium


CREATE VIEW STUDIES_FINANCIAL_REPORT AS
SELECT ID AS 'Study ID', Name, TotalIncome
FROM FINANCIAL_REPORT
WHERE Type = 'Study'


## Lista dłużników


CREATE VIEW LIST_OF_DEBTORS AS
SELECT DISTINCT
u.UserID,
u.FirstName,
u.LastName,
u.Email,
u.PhoneNumber
FROM Users u
WHERE UserID NOT IN
(SELECT DISTINCT u.UserID
FROM Users s JOIN Orders o ON u.UserID = o.UserID
JOIN OrderDetails od ON o.OrderID = od.OrderID
JOIN OrderWebinars ow ON od.OrderDetailsID = ow.OrderDetailsID
JOIN Webinars w ON ow.WebinarID = w.WebinarID
WHERE o.OrderDate < w.WebinarDate
UNION
SELECT DISTINCT u.UserID
FROM Users u
JOIN Orders o ON u.UserID = o.UserID
JOIN OrderDetails od ON o.OrderID = od.OrderID
JOIN OrderCourse oc ON od.OrderDetailsID = oc.OrderDetailsID
JOIN Courses c ON oc.CourseID = c.CourseID
WHERE o.OrderDate < dateadd(day, - 3,
    (SELECT min(cm.Date)
    FROM CourseModules cm
    WHERE cm.CourseID = c.CourseID))
UNION
SELECT DISTINCT u.UserID
FROM Users u
JOIN Orders o ON u.UserID = o.UserID
JOIN OrderDetails od ON o.OrderID = od.OrderID
JOIN OrderStudies os ON od.OrderDetailsID = os.OrderDetailsID
JOIN Studies st ON os.StudiesID = st.StudiesID
WHERE o.OrderDate < dateadd(day, - 3,
    (SELECT min(sm.Date)
    FROM StudyMeeting sm
          JOIN Subject sb ON sm.SubjectID = sb.SubjectID
    WHERE sb.StudiesID = st.StudiesID))


## Liczba osob zapisanych na przyszle wydazenia


CREATE VIEW NUMBER_OF_PEOPLE_REGISTERED_FOR_FUTURE_EVENTS AS

## Na przyszłe webinary Vol.2


SELECT 
    W.WebinarID AS ID,
    W.WebinarName AS Name,
    COUNT(WPARA.UserID) AS NumberOfPeople,
    'true' AS Online
FROM Webinars AS W
JOIN WebinarPresenceAndRecordingAvability AS WPARA 
    ON W.WebinarID = WPARA.WebinarID
WHERE W.WebinarDate > GETDATE()
GROUP BY W.WebinarID, W.WebinarName

UNION ALL

## Na przyszłe moduły kursów


SELECT 
    CM.ModuleID AS ID,
    CM.ModuleName AS Name,
    COUNT(CMP.UserID) AS NumberOfPeople,
    CASE 
        WHEN CM.ModuleID IN (
            SELECT ModuleID FROM OnlineAsyncModule
            UNION ALL
            SELECT ModuleID FROM OnlineSyncModule
            UNION ALL
            SELECT ModuleID FROM HybridModule
            WHERE VideoLink IS NOT NULL
        ) THEN 'true'
        ELSE 'false'
    END AS Online
FROM CourseModules AS CM
LEFT JOIN CourseModulesPresence AS CMP 
    ON CM.ModuleID = CMP.ModuleID
GROUP BY CM.ModuleID, CM.ModuleName

UNION ALL

## Na przyszłe spotkania studyjne


SELECT 
    SM.StudyMeetingID AS ID,
    SM.MeetingName AS Name,
    COUNT(SMP.UserID) AS NumberOfPeople,
    'false' AS Online
FROM StudyMeeting AS SM
JOIN StudyMeetingPresence AS SMP 
    ON SM.StudyMeetingID = SMP.StudyMeetingID
WHERE SM.Date > GETDATE()
GROUP BY SM.StudyMeetingID, SM.MeetingName;

## Frekwencja na zakończonych wydarzeniach


CREATE VIEW ATTENDANCE_SUMMARY AS
SELECT smp.StudyMeetingID AS 'Event ID',
100 * SUM(CAST(smp.Presence AS Int)) / COUNT(smp.Presence) AS [% Frequence],
'Study meeting' AS 'Event type'
FROM dbo.StudyMeetingPresence AS smp INNER JOIN
dbo.StudyMeeting AS sm ON smp.StudyMeetingID = sm.StudyMeetingID
WHERE (sm.Date < GETDATE())
GROUP BY smp.StudyMeetingID
UNION
SELECT cmp.ModuleID AS 'Event ID',
100 * SUM(CAST(cmp.Presence AS Int)) / COUNT(cmp.Presence) AS [% Frequence],
'Course module' AS 'Event type'
FROM dbo.CourseModulesPresence AS cmp INNER JOIN
dbo.CourseModules AS cm ON cmp.ModuleID = cm.ModuleID
WHERE (cm.Date < GETDATE())
GROUP BY cmp.ModuleID
UNION
SELECT wp.WebinarID AS 'Event ID',
100 * COUNT(*) / (SELECT COUNT(*) AS Expr1
FROM dbo.Students) AS [% Frequence],
'Webinar' AS 'Event type'
FROM dbo.WebinarPresenceAndRecordingAvability AS wp INNER JOIN
dbo.Webinars AS w ON wp.WebinarID = w.WebinarID
WHERE (w.WebinarDate < GETDATE())
GROUP BY wp.WebinarID


## Frekwencja na zakończonych spotkaniach studyjnych


CREATE VIEW STUDY_MEETINGS_ATTENDANCE_SUMMARY AS
SELECT [Event ID] AS 'Study Meeting ID', [% Frequence]
FROM ATTENDANCE_SUMMARY
WHERE [Event type] = 'Study Meeting'


## Frekwencja na zakończonych modułach kursów


CREATE VIEW COURSE_MODULES_ATTENDANCE_SUMMARY AS
SELECT [Event ID] AS 'Course Module ID', [% Frequence]
FROM ATTENDANCE_SUMMARY
WHERE [Event type] = 'Course Module'


## Frekwencja na zakończonych webinarach


CREATE VIEW WEBINARS_ATTENDANCE_SUMMARY AS
SELECT [Event ID] AS 'Webinar ID', [% Frequence]
FROM ATTENDANCE_SUMMARY
WHERE [Event type] = 'Webinar'


## Lista obecności na każde szkolenie


CREATE VIEW PRESENCE_LIST AS
SELECT smd.StudyMeetingID AS 'Event ID', sm.Date, u.FirstName AS [First Name],
u.LastName as [Last Name], CASE WHEN Presence = 1 THEN 'Present' ELSE 'Absent' END AS [Presence information],
'Study Meeting' AS 'Event type'
FROM dbo.StudyMeetingPresence AS smd INNER JOIN
dbo.Users AS s ON smd.UserID = s.UserID INNER JOIN
dbo.StudyMeeting AS sm ON smd.StudyMeetingID = sm.StudyMeetingID
WHERE (sm.Date < GETDATE())

UNION ALL

SELECT cmd.ModuleID AS 'Event ID', cm.Date, s.FirstName AS [First Name],
s.LastName as [Last Name], CASE WHEN Presence = 1 THEN 'Present' ELSE 'Absent' END AS [Presence information],
'Course module' AS 'Event type'
FROM dbo.CourseModulesPresence AS cmd INNER JOIN
dbo.Users AS s ON cmd.UserID = s.UserID INNER JOIN
dbo.CourseModules AS cm ON cmd.ModuleID = cm.ModuleID
WHERE (cm.Date < GETDATE())

UNION ALL

SELECT wd.WebinarID AS 'Event ID', w.WebinarDate, s.FirstName AS [First Name],
s.LastName as [Last Name], CASE WHEN wd.UserID IN (SELECT UserID
FROM Users) THEN 'Present' ELSE 'Absent' END AS [Presence information],
'Webinar' AS 'Event type'
FROM dbo.WebinarPresenceAndRecordingAvability AS wd INNER JOIN
dbo.Users AS s ON wd.UserID = s.UserID INNER JOIN
dbo.Webinars AS w ON wd.WebinarID = w.WebinarID
WHERE (w.WebinarDate < GETDATE())


## Lista osób zapisanych jednocześnie na dwa i więcej kolidujące ze sobą wydarzenia


create view BILOCATION_RAPORT as
with user_and_date as (
    select 
        UserID, 
        WebinarDate as date
    from WebinarPresenceAndRecordingAvability as wpara
    left join Webinars as w on wpara.WebinarID = w.WebinarID

    union all

    select 
        UserID, 
        cm.Date as date
    from CourseModulesPresence as cmp
    left join CourseModules as cm on cmp.ModuleID = cm.ModuleID

    union all

    select 
        UserID, 
        sm.Date as date
    from StudyMeetingPresence as smp
    left join StudyMeeting as sm on smp.StudyMeetingID = sm.StudyMeetingID
)

select 
    UserID, 
    Date, 
    count(*) as collisions
from user_and_date
group by UserID, Date
having count(*) > 1;

