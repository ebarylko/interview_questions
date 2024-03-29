import most_visited_sequence as mvs

sample = ["T0,C1,A",
          "T0,C2,E",
          "T1,C1,B",
          "T1,C2,B",
          "T2,C1,C",
          "T2,C2,C",
          "T3,C1,D",
          "T3,C2,D",
          "T4,C1,E",
          "T5,C2,A"]

sample_2 = ["T0,C1,A",
            "T0,C2,E",
            "T1,C1,B",
            "T1,C2,B",
            "T2,C1,C",
            "T2,C2,C",
            "T3,C1,D",
            "T3,C2,D",
            "T4,C1,E",
            "T5,C2,A",
            "T6,C2,B",
            "T7,C2,C"]

sample_3 = ["T0,C3,A"]

sample_4 = ["T0,C1,A",
            "T0,C2,E",
            "T0,C3,Q",
            "T0,C4,Q",
            "T1,C1,B",
            "T1,C2,B",
            "T1,C3,R",
            "T1,C4,R",
            "T2,C1,C",
            "T2,C2,C",
            "T2,C3,S",
            "T2,C4,S",
            "T3,C1,D",
            "T3,C2,D",
            "T4,C1,E",
            "T5,C2,A"]


def test_time_parser():
    assert mvs.time_parser.parse("T0") == 0
    assert mvs.time_parser.parse("T1") == 1
    assert mvs.time_parser.parse("T12") == 12
    assert mvs.time_parser.parse("T1292") == 1292


def test_id_parser():
    assert mvs.id_parser.parse("C1") == 1
    assert mvs.id_parser.parse("C9") == 9
    assert mvs.id_parser.parse("C980") == 980


def test_site_parser():
    assert mvs.site_parser.parse("A") == "A"
    assert mvs.site_parser.parse("Ab^728%Q($)") == "Ab^728%Q($)"
    assert mvs.site_parser.parse("bih") == "bih"


def test_entry_parser():
    assert mvs.entry_parser.parse("T0,C1,A") == [0, 1, "A"]


def test_entry_parser_2():
    assert mvs.entry_parser_2("T0,C1,A") == [0, 1, "A"]


def test_parse_input_2():
    assert mvs.parse_input_2(["T0,C1,A", "T9,C12,BDI"]) == [[0, 1, "A"], [9, 12, "BDI"]]


def test_parse_input():
    assert mvs.parse_input(["T0,C1,A", "T9,C12,BDI"]) == [[0, 1, "A"], [9, 12, "BDI"]]


def test_most_visited_sequence():
    assert mvs.most_visited_sequence(sample) == ("B", "C", "D")
    assert mvs.most_visited_sequence(sample_2) == ("A", "B", "C")
    assert not mvs.most_visited_sequence(sample_3)
    assert mvs.most_visited_sequence(sample_4) == ("B", "C", "D")
