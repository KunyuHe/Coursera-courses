# Assignment 1
fname = input("Enter file name: ")
fh = open(fname)
lst = list()
for line in fh:
    line = line.rstrip()
    splst = line.split()
    for ele in splst:
        exi = ele in lst
        if exi is True:
            ele = None
        else:
            lst.append(ele)
lst.sort()

# Assignment 2
fname = input("Enter file name: ")
fh = open(fname)
count = 0
for line in fh:
    if not line.startswith("From "): continue
    line = line.rstrip()
    splst = line.split()
    print(splst[1])
    count = count+1
print("There were", count, "lines in the file with From as the first word")
