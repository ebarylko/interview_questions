import parsy as p

time_parser = p.string("T") >> p.regex(r'\d+').map(int)
id_parser = p.string("C") >> p.digit.map(int)
