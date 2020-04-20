# Sample Assignment
lines = open("regex_sum_42.txt")
counts = list()
total = 0
num = 0
import re
for line in lines:
# list of numbers in line (character)
    count = re.findall("([0-9]+)", line)
# exclude those with blanks
    if len(count) is 0: continue
    for a in count:
        num = num + int(a)
    counts.append(num)
    num = 0
    continue
for add in counts:
    total = add + total
print(total)

# Assignment
lines = open("regex_sum_37243.txt")
counts = list()
total = 0
num = 0
import re
for line in lines:
    count = re.findall("([0-9]+)", line)
    if len(count) is 0: continue
    for a in count:
        num = num + int(a)
    counts.append(num)
    num = 0
    continue
for add in counts:
    total = add + total
print(total)
