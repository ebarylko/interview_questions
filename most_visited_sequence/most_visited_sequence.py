import toolz as tz
import parsy as p
import operator as op

number_parser = p.regex(r'\d+').map(int)
time_parser = p.string("T") >> number_parser
id_parser = p.string("C") >> number_parser
site_parser = p.regex(r'.*')
entry_parser = p.seq(time_parser << p.string(","), id_parser << p.string(","), site_parser)


def parse_input(input):
    """
    :param input: a collection of entries containing the time, customer id, and the site visited
    :return: a collection of triplets containing the time, customer id, and site id
    """
    return tz.thread_last(
        input,
        (map, tz.partial(entry_parser.parse)),
        list)


def most_visited_sequence(input):
    """
    :param input: a collection of entries containing the time, customer id, and the site visited
    :return: the most visited sequence of the sites
    """
    return tz.thread_last(
        input,
        parse_input,
        (tz.groupby, tz.second),
        op.methodcaller("values"),
        list

        # (map, sites_visited)

    )