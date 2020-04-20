import sqlite3
import re

# build up the connection
conn = sqlite3.connect('org_db.sqlite')
cur = conn.cursor()

cur.execute('DROP TABLE IF EXISTS Counts')

cur.execute('CREATE TABLE Counts (org TEXT, count INTEGER)')

fname = 'mbox.txt'
fh = open(fname)
for line in fh:
    if not line.startswith('From '): continue
    org = re.findall("@(\S+)", line)
    org = org[0]

    # open a file handle for the table 'Counts'
    cur.execute('SELECT count FROM Counts WHERE org = ? ', (org,))
    row = cur.fetchone()
    # check if there is a record for the email, if not, insert one
    if row is None:
        cur.execute('INSERT INTO Counts (org, count) VALUES (?, 1)', (org,))
    else:
        cur.execute('UPDATE Counts SET count = count + 1 WHERE org = ?',
                    (org,))

# remove commit() from the loop to accelerate the process
conn.commit()

for row in cur.execute('SELECT org, count FROM Counts ORDER BY count DESC LIMIT 10'):
    print(str(row[0]), row[1])

cur.close()
