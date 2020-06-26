module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time.TimeInMs exposing (..)
import Time.Values as TimeValues exposing (TimeValues)


suite : Test
suite =
    describe "TimeValues"
        [ describe "fromMilliseconds"
            [ test "Zero milliseconds" testFromMillisecondsZeroMilliseconds
            , test "One millisecond" testFromMillisecondsOneMillisecond
            , test "One second" testFromMillisecondsOneSecond
            , test "One minute" testFromMillisecondsOneMinute
            , test "One hour" testFromMillisecondsOneHour
            , test "One day" testFromMillisecondsOneDay
            , test "One week" testFromMillisecondsOneWeek
            ]
        , describe "fromSeconds"
            [ test "Zero seconds" testFromSecondsZeroSeconds
            , test "One second" testFromSecondsOneSecond
            , test "One minute" testFromSecondsOneMinute
            , test "One hour" testFromSecondsOneHour
            , test "One day" testFromSecondsOneDay
            , test "One week" testFromSecondsOneWeek
            ]
        , describe "fromMinutes"
            [ test "Zero minutes" testFromMinutesZeroMinutes
            , test "One minute" testFromMinutesOneMinute
            , test "One hour" testFromMinutesOneHour
            , test "One day" testFromMinutesOneDay
            , test "One week" testFromMinutesOneWeek
            ]
        ]


testFromMillisecondsZeroMilliseconds : () -> Expectation
testFromMillisecondsZeroMilliseconds _ =
    0 |> TimeValues.fromMilliseconds |> expectValues 0 0 0 0 0 0


testFromMillisecondsOneMillisecond : () -> Expectation
testFromMillisecondsOneMillisecond _ =
    1 |> TimeValues.fromMilliseconds |> expectValues 1 0 0 0 0 0


testFromMillisecondsOneSecond : () -> Expectation
testFromMillisecondsOneSecond _ =
    msSecs |> TimeValues.fromMilliseconds |> expectValues 0 1 0 0 0 0


testFromMillisecondsOneMinute : () -> Expectation
testFromMillisecondsOneMinute _ =
    msMins |> TimeValues.fromMilliseconds |> expectValues 0 0 1 0 0 0


testFromMillisecondsOneHour : () -> Expectation
testFromMillisecondsOneHour _ =
    msHours |> TimeValues.fromMilliseconds |> expectValues 0 0 0 1 0 0


testFromMillisecondsOneDay : () -> Expectation
testFromMillisecondsOneDay _ =
    msDays |> TimeValues.fromMilliseconds |> expectValues 0 0 0 0 1 0


testFromMillisecondsOneWeek : () -> Expectation
testFromMillisecondsOneWeek _ =
    msWeeks |> TimeValues.fromMilliseconds |> expectValues 0 0 0 0 0 1


testFromSecondsZeroSeconds : () -> Expectation
testFromSecondsZeroSeconds _ =
    0 |> TimeValues.fromSeconds |> expectValues 0 0 0 0 0 0


testFromSecondsOneSecond : () -> Expectation
testFromSecondsOneSecond _ =
    1 |> TimeValues.fromSeconds |> expectValues 0 1 0 0 0 0


testFromSecondsOneMinute : () -> Expectation
testFromSecondsOneMinute _ =
    msMins // 1000 |> TimeValues.fromSeconds |> expectValues 0 0 1 0 0 0


testFromSecondsOneHour : () -> Expectation
testFromSecondsOneHour _ =
    msHours // 1000 |> TimeValues.fromSeconds |> expectValues 0 0 0 1 0 0


testFromSecondsOneDay : () -> Expectation
testFromSecondsOneDay _ =
    msDays // 1000 |> TimeValues.fromSeconds |> expectValues 0 0 0 0 1 0


testFromSecondsOneWeek : () -> Expectation
testFromSecondsOneWeek _ =
    msWeeks // 1000 |> TimeValues.fromSeconds |> expectValues 0 0 0 0 0 1


testFromMinutesZeroMinutes : () -> Expectation
testFromMinutesZeroMinutes _ =
    0 |> TimeValues.fromMinutes |> expectValues 0 0 0 0 0 0


testFromMinutesOneMinute : () -> Expectation
testFromMinutesOneMinute _ =
    msMins // 1000 // 60 |> TimeValues.fromMinutes |> expectValues 0 0 1 0 0 0


testFromMinutesOneHour : () -> Expectation
testFromMinutesOneHour _ =
    msHours // 1000 // 60 |> TimeValues.fromMinutes |> expectValues 0 0 0 1 0 0


testFromMinutesOneDay : () -> Expectation
testFromMinutesOneDay _ =
    msDays // 1000 // 60 |> TimeValues.fromMinutes |> expectValues 0 0 0 0 1 0


testFromMinutesOneWeek : () -> Expectation
testFromMinutesOneWeek _ =
    msWeeks // 1000 // 60 |> TimeValues.fromMinutes |> expectValues 0 0 0 0 0 1


expectValues : Int -> Int -> Int -> Int -> Int -> Int -> TimeValues -> Expectation
expectValues ms secs mins hours days weeks timeValues =
    timeValues
        |> Expect.all
            [ expectMs ms
            , expectSecs secs
            , expectMins mins
            , expectHours hours
            , expectDays days
            , expectWeeks weeks
            ]


expectMs : Int -> TimeValues -> Expectation
expectMs ms timeValue =
    Expect.equal ms timeValue.milliseconds


expectSecs : Int -> TimeValues -> Expectation
expectSecs secs timeValue =
    Expect.equal secs timeValue.seconds


expectMins : Int -> TimeValues -> Expectation
expectMins mins timeValue =
    Expect.equal mins timeValue.minutes


expectHours : Int -> TimeValues -> Expectation
expectHours hours timeValue =
    Expect.equal hours timeValue.hours


expectDays : Int -> TimeValues -> Expectation
expectDays days timeValue =
    Expect.equal days timeValue.days


expectWeeks : Int -> TimeValues -> Expectation
expectWeeks weeks timeValue =
    Expect.equal weeks timeValue.weeks
