module Time.Values exposing (TimeValues, fromMilliseconds, fromMinutes, fromSeconds)

{-| Extract time values from numbers.

    import Time.Values as TimeValues exposing (TimeValues)

    values = TimeValues.fromSeconds 60

    minutes = values.minutes

    The functions fromMilliseconds, fromSeconds and fromMinutes split the given
    value into its compound elements, namely: ms, secs, mins, hours, days, weeks,
    months and years.

-}

import Time.TimeInMs exposing (..)


type alias TimeValues =
    { milliseconds : Int
    , seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    , weeks : Int
    , months : Int
    , years : Int
    }


fromMinutes : Int -> TimeValues
fromMinutes mins =
    mins * msMins |> fromMilliseconds


fromSeconds : Int -> TimeValues
fromSeconds secs =
    secs * msSecs |> fromMilliseconds


fromMilliseconds : Int -> TimeValues
fromMilliseconds ms =
    let
        years =
            ms // msYears

        yearsRem =
            modBy msYears ms

        months =
            yearsRem // msMonths

        monthsRem =
            modBy msMonths yearsRem

        weeks =
            monthsRem // msWeeks

        weeksRem =
            modBy msWeeks monthsRem

        days =
            weeksRem // msDays

        daysRem =
            modBy msDays weeksRem

        hours =
            daysRem // msHours

        hoursRem =
            modBy msHours daysRem

        minutes =
            hoursRem // msMins

        minutesRem =
            modBy msMins hoursRem

        seconds =
            minutesRem // msSecs

        secondsRem =
            modBy msSecs minutesRem

        milliseconds =
            secondsRem
    in
    TimeValues milliseconds seconds minutes hours days weeks months years
