import parsy as p

time_parser = p.string("T") >> p.digit.map(int)
