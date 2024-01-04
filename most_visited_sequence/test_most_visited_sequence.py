import most_visited_sequence as mvs


sample = ["T0,C1,A"
          "T0,C2,E"
          "T1,C1,B"
          "T1,C2,B"
          "T2,C1,C"
          "T2,C2,C"
          "T3,C1,D"
          "T3,C2,D"
          "T4,C1,E"
          "T5,C2,A"]


def test_time_parser():
    assert mvs.time_parser.parse("T0") == 0
    assert mvs.time_parser.parse("T1") == 1
    assert mvs.time_parser.parse("T12") == 12
    assert mvs.time_parser.parse("T1292") == 1292


def test_id_parser():
    assert mvs.id_parser.parse("C1") == 1
    assert mvs.id_parser.parse("C9") == 9
