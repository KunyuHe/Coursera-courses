largest = None
smallest = None
while True:
    num = input("Enter a number: ")
    if num == "done":
        break
    else:
        try:
            num = int(num)
        except:
            num = None
            print("Invalid input")
            continue
    if largest is None:
        largest = num
    elif largest > num:
        largest = largest
    else:
        largest = num
    if smallest is None:
        smallest = num
    elif smallest < num:
        smallest = smallest
    else:
        smallest = num
print("Maximum is", largest)
print("Minimum is", smallest
