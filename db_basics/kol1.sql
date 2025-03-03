/*
Zadanie 1
Wybierz klientów, którzy byli obsługiwani wyłącznie przez jednego pracownika, podaj imię i nazwisko tego pracownika

Zadanie 2
Wybierz liczbę i wartość zamówień ( z przesyłką) dla każdego pracownika, które zostały obsłużone w lutym 1997, wybierz też pracowników, którzy nic nie obsługiwali

Zadanie 3
Wybierz imię, nazwisko, liczbę dzieci i liczbę książek pożyczonych kiedyś oraz obecnie ( łącznie z tymi wypożyczonymi przez dzieci)
*/

-- zadanie 1
SELECT  e.FirstName, e.LastName
FROM Employees e
WHERE e.EmployeeID IN (
    SELECT MAX(o.EmployeeID)
    FROM Orders o
    GROUP BY o.CustomerID
    HAVING COUNT(o.EmployeeID) = 1
);

-- zadanie 2
SELECT
    o.EmployeeID, COUNT(o.OrderID), COALESCE(SUM((od.UnitPrice * (1 - od.Discount)) * od.Quantity + MAX(o.Freight)), 0) as TotalOrderValue
FROM Orders o
LEFT JOIN [Order Details] od ON o.OrderID = od.OrderID 
WHERE YEAR(o.OrderDate) = 1997 AND MONTH(o.OrderDate) = 2
GROUP BY o.EmployeeID, o.OrderID

-- zadanie 3


