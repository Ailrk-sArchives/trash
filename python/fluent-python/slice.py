invoice = """
1909  Picoco                                   $121
1939  Cocois                                   $121
1999  papaka                                   $11
1919  shishi                                   $12
1909  kokoai                                   $121
"""

# slice used to abstract complicated sequence operations.

year = slice(0, 6)
name = slice(6, 40)
price = slice(40, None)

lines = invoice.split('\n')
for l in lines:
    print(l[name].strip(), l[price])


