import numpy as np
import seaborn as sns
from scipy.integrate import quad as integration
import matplotlib.pyplot as plt

# Definicja funkcji k(x)
def k(x):
    return 1 if x <= 1 else 2

# Funkcja e dla funkcji bazowej
def e(n, i, x):
    h = 2 / n
    return max(0, 1 - abs((x / h - i)))

# Pochodna funkcji bazowej e_prim
def e_prim(n, i, x):
    h = 2 / n
    if x <= (i - 1) * h or x >= (i + 1) * h:
        return 0
    else:
        return 1 / h if x <= i * h else -1 / h

import numpy as np


def calculate_integral(n, i, j):
    start = 2 * max(max(i, j) - 1, 0) / n
    end = 2 * min(min(i, j) + 1, n) / n
    return integration(lambda x: k(x) * e_prim(n, i, x) * e_prim(n, j, x), start, end)[0] if abs(j - i) <= 1 else 0

# Wypełnianie macierzy B i wektora L
def fill(n):
    B, L = np.zeros((n, n)), np.zeros(n)
    L[0] = -17 * e(n, 0, 0)  # Uwzględniamy przesunięcie 3 w warunku brzegowym
    for i in range(n):
        for j in range(n):
            integral = calculate_integral(n, i, j)
            B[i, j] = -e(n, i, 0) * e(n, j, 0) + integral
    return B, L

# Wizualizacja rozwiązania
def show_plot(solution, n):
    w_solution = np.concatenate((np.linalg.solve(B, L), [0]))  # Rozwiązanie dla w(x)
    u_solution = w_solution + 3  # Przesunięcie funkcji o 3 w górę

    # Wykres przesuniętej funkcji u(x)
    sns.lineplot(x=np.linspace(0, 2, n + 1), y=u_solution)
    plt.title('Rozkład temperatury (u(x))')
    plt.xlabel('x')
    plt.ylabel('u(x)')
    plt.grid(True)
    plt.savefig('heat_transfer_plot.png')
    plt.show()


# Wizualizacja funkcji elementarnych
def plot_elementary_functions(n):
    x_values = np.linspace(0, 2, 1000)
    plt.figure(figsize=(10, 6))
    for i in range(n):
        y_values = [e(n, i, x) for x in x_values]
        plt.plot(x_values, y_values, label=f'e_{i}(x)')
    plt.title('Funkcje elementarne')
    plt.xlabel('x')
    plt.ylabel('e_i(x)')
    plt.legend()
    plt.grid(True)
    plt.savefig('elementary_functions_plot.png')
    plt.show()

if __name__ == '__main__':
    user_input = int(input("Podaj liczbę elementów (n): "))
    n=3
    B, L = fill(user_input)
    print(np.linalg.solve(B, L))
    show_plot(np.concatenate((np.linalg.solve(B, L), [0])), user_input)
    plot_elementary_functions(user_input)
