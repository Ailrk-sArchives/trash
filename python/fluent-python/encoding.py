import os

"""
Use chardet to determine string encoding
"""

cafe = 'Café'
fp = open('cafe.txt', 'w', encoding='utf_8')
print('-> fp:', fp)
print('size of utf8 str Café:', len(cafe))
print('size of utf8 byte Café:', len(cafe.encode('utf_8')))
fp.write(cafe)
fp.close()

fp2 = open('cafe.txt', encoding='cp1252')
print(fp2.encoding)
print(fp2.read())
fp2.close()

fp3 = open('cafe.txt', 'rb')
print('-> fp3:', fp3)
print(fp3.read())
fp3.close()

print(os.stat('cafe.txt').st_size)
