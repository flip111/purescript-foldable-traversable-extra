var input = [3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3, 3, 4, 4, 7, 7, 8, 9, 9, 9, 1, 5, 5, 7, 4, 7, 5, 2, 9, 6, 9, 4, 10, 3, 5, 5, 9, 9, 4, 10, 2, 1, 1, 9, 2, 9, 7, 9, 7, 7, 10, 8, 9, 7, 7, 3, 2, 2, 10, 5, 2, 3, 1, 6, 6, 3, 6, 9, 4, 7, 9, 1, 4, 8, 2, 5, 8, 2, 3, 4, 1, 8, 10, 6, 8, 7, 5, 10, 8, 9, 2, 6, 6, 8, 10, 3, 5, 5, 10, 8, 2, 5, 7, 3, 4, 9, 1, 5, 5, 8, 4, 10, 10, 5, 1, 3, 5, 9, 9, 7, 1, 7, 3, 1, 4, 10, 5, 5, 10, 5, 1, 9, 7, 9, 4, 3, 1, 5, 5, 9, 8, 9, 5, 2, 9, 10, 8, 10, 5, 9, 9, 10, 10, 3, 10, 3, 10, 1, 2, 3, 3, 1, 8, 5, 3, 4, 1, 3, 7, 1, 9, 7, 5, 7, 6, 8, 2, 3, 4, 3, 8, 5, 6, 5, 8, 5, 6, 1, 6, 1, 9, 1, 5, 3, 10, 4, 4, 4, 5, 8, 10, 7, 6, 10, 8, 5, 8, 7, 4, 8, 3];

function go1(arr) {
  const counts = {};

  for (const e of arr) {
    counts[e] = counts[e] ? counts[e] + 1 : 1;
  }

  return counts;
}

function go2(arr) {
  const counts = {};

  for (const e of arr) {
    counts[e] = (counts[e] || 0) + 1;
  }

  return counts;
}

function go3(arr) {
  return arr.reduce(function (acc, curr) {
    return acc[curr] ? ++acc[curr] : acc[curr] = 1, acc
  }, {});
}

function go4(arr) {
  return arr.reduce((acc, e) => acc.set(e, (acc.get(e) || 0) + 1), new Map());
}

function go5(arr) {
  result = {};

  for (var i = 0; i < arr.length; ++i) {
    if (! result[arr[i]]) {
      result[arr[i]] = 0;
    }

    ++result[arr[i]];
  }

  return result;
}

function go6(arr) {
  return new Map([...new Set(arr)].map(
    x => [x, arr.filter(y => y === x).length]
  ));
}

function go7(arr) {
  return arr.reduce((prev, curr) => (prev[curr] = ++prev[curr] || 1, prev), {})
}

function go8(arr) {
  return arr.reduce((acc, curr) => {
    acc[curr] ??= {[curr]: 0};
    acc[curr][curr]++;
    
    return acc;
  }, {});
}

function go9(arr) {
  return arr.reduce(function(countMap, word) {
    countMap[word] = ++countMap[word] || 1;return countMap
  }, {});
}

function go10(arr) {
  return arr.reduce((acc, val) => acc.set(val, 1 + (acc.get(val) || 0)), new Map());
}

function go11(arr) {
  return arr.reduce((acc, value) => ({ ...acc, [value]: acc[value] + 1 || 1}), {} );
}

function go12(nums) {
  return nums.reduce((acc, curr) => {
    acc[curr] = -~acc[curr];
    return acc;
  }, {});
}

function go12(arr) {
  return arr.reduce((r,k)=>{r[k]=1+r[k]||1;return r},{});
}

function go13(a) {
  var result = {};
  for(var i in a){
    if(result[a[i]] == undefined) {
      result[a[i]] = 0;
    }
    result[a[i]]++;
  }
  return result;
}

function go13(arr) {
  const map = {};
  for ( var i = 0; i < arr.length; i++ ) {
      map[arr[i]] = ~~map[arr[i]] + 1;
  }
  return map;
}

function go13(arr) {
  return Array.from(new Set(arr)).map(val => arr.filter(v => v === val).length);
}




"use strict";

export const occurrencesImpl = function (mkTuple, xs) {
  var counts = xs.reduce(function (acc, curr) {
    return acc[curr] ? ++acc[curr] : acc[curr] = 1, acc
  }, {});

  var result = [];
  Object.keys(counts).forEach(function(key) {
    result.push(mkTuple(key, counts(key)));
  });

  return result
};


-- | Count the amount of times a value occurs in an array.
-- | Uses javascript native comparison [loose quality checking](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using)
-- |
-- | ```purescript
-- | occurrencesMap ["A", "B", "B"] == [Tuple "A" 1, Tuple "B" 2]
-- | ```
occurrencesJS :: forall a. Eq a => Array a -> Array (Tuple a Int)
occurrencesJS = runFn2 occurrencesImpl Tuple

foreign import occurrencesImpl :: forall a. Fn2 (forall b. a -> b -> Tuple a b) (Array a) (Array (Tuple a Int))

