import toolz as tz
import parsy as p
import operator as op
import re

number_parser = p.regex(r'\d+').map(int)
time_parser = p.string("T") >> number_parser
id_parser = p.string("C") >> number_parser
site_parser = p.regex(r'.*')
entry_parser = p.seq(time_parser << p.string(","), id_parser << p.string(","), site_parser)


def entry_parser_2(entry):
    """
    :param entry: a line containing the time, the client id, and the site visited
    :return: a collection with the time, client id, and site visited
    """
    time, id, site = re.split(",", entry)
    return [int(time[1:]), int(id[1:]), site]


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
    # N = the number of logs
    # U = the number of users
    # N >= U
    # S = the number of sites visited
    return tz.thread_last(
        input,
        parse_input, # O(N)
        (tz.groupby, tz.second), #O(N)
        (tz.valmap, tz.compose(list, tz.partial(map, tz.partial(tz.nth, 2)))), # O(U * S)
        op.methodcaller("values"), # O(U)
        (tz.mapcat, tz.compose(list, tz.partial(tz.sliding_window, 3))), #O(U * S)
        tz.frequencies, # O(U * S)
        op.methodcaller("items"), # O(1)
        lambda coll: sorted(coll, key=tz.second, reverse=True), # O(U * S * log(U * S))
        lambda coll: coll if not coll else tz.first(tz.first(coll)) # O(1)
    )


def parse_input_2(input):
    """
    :param input: a collection of entries containing the time, customer id, and the site visited
    :return: a collection of triplets containing the time, customer id, and site id
    """
    return list(
        map(entry_parser_2, input)
    )


def most_visited_sequence_2(input):
    """
    :param input: a collection of entries containing the time, customer id, and the site visited
    :return: the most visited sequence of the sites
    """
    # N = the number of logs
    # U = the number of users
    # N >= U
    # S = the number of sites visited
    return tz.thread_last(
        input,
        parse_input, # O(N)
        (tz.groupby, tz.second), #O(N)
        (tz.valmap, tz.compose(list, tz.partial(map, tz.partial(tz.nth, 2)))), # O(U * S)
        op.methodcaller("values"), # O(U)
        (tz.mapcat, tz.compose(list, tz.partial(tz.sliding_window, 3))), #O(U * S)
        tz.frequencies, # O(U * S)
        op.methodcaller("items"), # O(1)
        lambda coll: sorted(coll, key=tz.second, reverse=True), # O(U * S * log(U * S))
        lambda coll: coll if not coll else tz.first(tz.first(coll)) # O(1)
    )
