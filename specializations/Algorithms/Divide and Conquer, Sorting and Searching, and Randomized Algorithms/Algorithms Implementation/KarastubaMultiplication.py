def karatsuba(x, y):
    # Get the number of digits in each, and cut both by half of the longest
    m, n = len(str(x)), len(str(y))
    length = max(m, n)
    cut = length // 2

    # Base case
    if m == 1 or n == 1:
        return x * y

    # Part the numbers into half
    a, c = x // 10**cut, y // 10**cut
    b, d = x % 10**cut, y % 10**cut

    # Recursively calculate the multiplication
    ac, bd = karatsuba(a, c), karatsuba(b, d)
    cross = karatsuba(a+b, c+d) - ac - bd

    return 10**(2*cut)*ac + 10**cut*cross + bd
