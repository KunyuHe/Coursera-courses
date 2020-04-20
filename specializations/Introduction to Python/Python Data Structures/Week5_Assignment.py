name = input("Enter file:")
handle = open(name)

lst = list()
counts = dict()
for line in handle:
    if not line.startswith("From "):
        continue
    words = line.split()
    lst.append(words[1])
for add in lst:
    counts[add] = counts.get(add, 0) + 1

Mostadd = None
Mostcon = None
for k,v in counts.items():
    if Mostcon is None or v > Mostcon:
        Mostcon = v
        Mostadd = k
print(Mostadd, Mostcon)
