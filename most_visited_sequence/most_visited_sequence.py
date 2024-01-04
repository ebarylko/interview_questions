import parsy as p

number_parser = p.regex(r'\d+').map(int)
time_parser = p.string("T") >> number_parser
id_parser = p.string("C") >> number_parser
site_parser = p.regex(r'.*')

