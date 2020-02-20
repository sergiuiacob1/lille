import os
import math

filename = 'b.txt'


class Library:
    def __init__(self, no_books, signup, limit, books):
        self.no_books = no_books
        self.signup = signup
        self.limit = limit
        self.books = books

    def __str__(self):
        return "Library: {0}, {1}, {2}\n{3}".format(self.no_books, self.signup, self.limit, self.books)


libraries = []
books_to_libs = {}

with open(filename, 'r') as f:
    B, L, D = [int(x) for x in next(f).split()]
    book_scores = [int(x) for x in next(f).split()]

    for i in range(0, B):
        books_to_libs[i] = set()

    for i in range(0, L):
        N, T, M = [int(x) for x in next(f).split()]
        lib_books = [int(x) for x in next(f).split()]
        for x in lib_books:
            books_to_libs[x].add(i)
        lib = Library(N, T, M, lib_books)
        libraries.append(lib)

books_to_scan = []
books_already_scanned = set()
libraries_already_signed = []


def get_best_library_for_signup(current_day):
    choices = [i for i in range(0, L)]
    choices = set(choices)
    choices = choices - set(libraries_already_signed)

    books_not_scanned = {}
    how_many_books_can_lib_scan = [0] * L
    lib_scores = [-math.inf] * L
    for lib in choices:
        # sort books by their score
        books_not_scanned[lib] = sorted(
            libraries[lib].books, key=lambda x: book_scores[x], reverse=True)

        how_many_books_can_lib_scan[lib] = (
            (D - current_day) - libraries[lib].signup) * libraries[lib].limit

        if how_many_books_can_lib_scan[lib] <= 0:
            lib_scores[lib] = -math.inf

        lib_scores[lib] = sum(
            [book_scores[x] for x in books_not_scanned[lib][:how_many_books_can_lib_scan[lib]]])

    best_lib = lib_scores.index(max(lib_scores))
    return best_lib, libraries[best_lib].books[:how_many_books_can_lib_scan[best_lib]]


signup_in_process = False
days_left_for_signup = 0

libraries_scan_order = {}
for i in range (0, L):
    libraries_scan_order[i] = []

signup_order = []

for i in range(0, D):
    # check signup process progress
    if signup_in_process is True:
        days_left_for_signup -= 1
        if days_left_for_signup == 0:
            signup_in_process = False

    if signup_in_process is False and len(libraries_already_signed) < L:
        # choose a library for signup process
        lib, scannable_books = get_best_library_for_signup(i)
        # start signup process
        signup_in_process = True
        days_left_for_signup = libraries[lib].signup

        # update our data structures
        libraries_already_signed.append(lib)
        books_to_scan.extend(scannable_books)
        books_to_scan = list(set(books_to_scan))
        # sort the books
        books_to_scan = sorted(books_to_scan, key = lambda x: book_scores[x], reverse=True)

        # remove books that will be scanned from libraries
        for x in scannable_books:
            for library in books_to_libs[x]:
                libraries[library].books.remove(x)

        signup_order.append(lib)

    # choose books to scan

    how_many_books_lib_is_scanning = [0] * L

    scanned = set()
    for x in books_to_scan:
        # I want to scan the book x
        choices = list(books_to_libs[x])
        choices = set(choices).intersection(set(libraries_already_signed))
        # choose the library that can scan the book x
        for lib in choices:
            if how_many_books_lib_is_scanning[lib] < libraries[lib].limit:
                # choose lib
                how_many_books_lib_is_scanning[lib] += 1
                libraries_scan_order[lib].append(x)
                scanned.add(x)

    for x in scanned:
        books_to_scan.remove(x)


filename_output = filename.split('.txt')[0] + '_out.txt'
with open (filename_output, 'w') as f:
    f.write (f'{len(libraries_already_signed)}\n')
    for x in signup_order:
        f.write (f'{x} {len(libraries_scan_order[x])}\n')
        for book in libraries_scan_order[x]:
            f.write (f'{book} ')
        f.write('\n')




# ideas
# binary search when looking if a book is in a library
