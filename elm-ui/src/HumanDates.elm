module HumanDates exposing (monthToString, prettyDay, prettyHourMinute, weekdayToString)

import Time exposing (Month(..), Posix, Weekday(..), Zone)


prettyDay : Zone -> Posix -> String
prettyDay zone posix =
    let
        weekday =
            Time.toWeekday zone posix
                |> weekdayToString

        month =
            Time.toMonth zone posix
                |> monthToString

        day =
            Time.toDay zone posix
                |> String.fromInt
    in
    weekday
        ++ ", "
        ++ month
        ++ " "
        ++ day


prettyHourMinute : Zone -> Posix -> String
prettyHourMinute zone posix =
    let
        hourRaw =
            Time.toHour zone posix

        ( hour, meridiem ) =
            case hourRaw > 12 of
                True ->
                    ( hourRaw - 12, "PM" )

                False ->
                    ( case hourRaw of
                        0 ->
                            12

                        somethingElse ->
                            somethingElse
                    , "AM"
                    )

        minuteRaw =
            Time.toMinute zone posix

        minute =
            case minuteRaw < 10 of
                True ->
                    "0" ++ String.fromInt minuteRaw

                False ->
                    String.fromInt minuteRaw
    in
    String.fromInt hour ++ ":" ++ minute ++ " " ++ meridiem


weekdayToString : Weekday -> String
weekdayToString w =
    case w of
        Sun ->
            "Sunday"

        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
