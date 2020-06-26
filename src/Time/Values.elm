module Time.Values exposing
    ( TimeValues
    , fromMilliseconds, fromSeconds, fromMinutes
    )

{-| Extract time values from numbers.

@docs TimeValues

@docs fromMilliseconds, fromSeconds, fromMinutes

import Time.Values as TimeValues exposing (TimeValues)

values = TimeValues.fromSeconds 60

minutes = values.minutes

The functions fromMilliseconds, fromSeconds and fromMinutes split the given
value into its compound elements, namely: ms, secs, mins, hours, days, weeks.

-}

import Time.TimeInMs exposing (..)


{-| Container for the compound elements.
-}
type alias TimeValues =
    { milliseconds : Int
    , seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    , weeks : Int
    }


{-| Extracts the compound elements from the given minutes.
-}
fromMinutes : Int -> TimeValues
fromMinutes mins =
    mins * msMins |> fromMilliseconds


{-| Extracts the compound elements from the given seconds.
-}
fromSeconds : Int -> TimeValues
fromSeconds secs =
    secs * msSecs |> fromMilliseconds


{-| Extracts the compound elements from the given milliseconds.
-}
fromMilliseconds : Int -> TimeValues
fromMilliseconds ms =
    let
        weeks =
            ms // msWeeks

        weeksRem =
            modBy msWeeks ms

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
    TimeValues milliseconds seconds minutes hours days weeks
