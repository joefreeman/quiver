# %time

Instants on the UTC timeline, civil dates and times, exact durations and calendar periods,
and the clocks that read them.

```quiver
%time.add [%time{ 2024-01-31 }, %time.months 1]   //= Date[year: 2024, month: 2, day: 29]
%time{ 2024-03-10T09:30:00Z } ~> %time.utc ~> %time.weekday   //= Sun
%time.format %time{ 1h30m }             //= "1h30m"
```

## The kinds of value

Every value has exactly one representation, so equality and pins mean "the same time":

| type | value | |
| --- | --- | --- |
| `'%time.instant` | `Instant[ns]` | a point on the UTC timeline: nanoseconds since the Unix epoch |
| `'%time.duration` | `Duration[ns]` | an exact, signed span of nanoseconds |
| `'%time.period` | `Period[months: m, days: d]` | a calendar span, whose length depends on where it's applied |
| `'%time.date` | `Date[year:, month:, day:]` | a civil date, with no zone |
| `'%time.time` | `Time[hour:, minute:, second:, nano:]` | a civil time of day |
| `'%time.datetime` | `DateTime[year:, …, nano:]` | a civil date and time, with no zone |
| `'%time.mono` | `Mono[ns]` | a monotonic clock reading, for measuring elapsed time |

`'%time` is `'%time.instant`. A year is twelve months and a week seven days, so a period is
canonical in months and days:

```quiver
year = %time.years 1
year                                    //= Period[months: 12, days: 0]
%time.months 12 ~> =^year               //= Period[months: 12, days: 0]
%time.weeks 2                           //= Period[months: 0, days: 14]
```

A civil value's fields are flat, so a calendar function that reads `(year, month, day)` takes a
date and a date-time alike:

```quiver
dt = %time{ 2024-03-10T09:30 }
%time.weekday dt                        //= Sun
dt ~> %time.to_date ~> %time.weekday   //= Sun
```

## Clocks

`now` reads the host's wall clock. `monotonic` reads a steady clock with an arbitrary origin,
which never steps backwards — `since` gives the time elapsed since a reading. The two kinds
of reading don't mix: an instant can't be compared to or subtracted from a monotonic
reading.

```quiver
%time.now [] ~> %time.after? [~, %time{ 2025-01-01T00:00:00Z }]   //= Ok
start = %time.monotonic []
%time.since start ~> %time.compare [~, %time.nanos 0]              //= (0 | 1)
```

```quiver
%time.diff [%time.now [], %time.monotonic []]   //! Type mismatch
```

## Construction

The constructors validate, answering nil carrying `:error OutOfRange[field]` for a date or
time that doesn't exist. Minutes, seconds and nanoseconds default to zero, and so does a
date-time's time of day.

```quiver
%time.date [2024, 2, 29]                //= Date[year: 2024, month: 2, day: 29]
%time.date [2023, 2, 29]                //= []
%time.date [2023, 2, 29] ~> :error<'%time.error>   //= OutOfRange[Day]
%time.time [9, 30]                      //= Time[hour: 9, minute: 30, second: 0, nano: 0]
%time.time [24, 0] ~> :error<'%time.error>         //= OutOfRange[Hour]
%time.datetime [2024, 3, 10]            //= DateTime[year: 2024, month: 3, day: 10, hour: 0, minute: 0, second: 0, nano: 0]
```

A constructor's result may be nil, so it is narrowed before being used as a date:

```quiver
next_month = #['int, 'int, 'int] {
  %time.date [$0, $1, $2] ~> =('%time.date & d)
  %time.add [d, %time.months 1]
}
next_month [2024, 1, 31]                //= Date[year: 2024, month: 2, day: 29]
next_month [2024, 2, 30]                //= []
```

```quiver
%time.add [%time.date [2024, 1, 31], %time.months 1]   //! Type mismatch
```

`at` joins a date and a time of day, and `to_date` and `to_time` take a date-time apart:

```quiver
dt = %time.at [%time{ 2024-03-10 }, %time{ 09:30 }]
dt                                      //= DateTime[year: 2024, month: 3, day: 10, hour: 9, minute: 30, second: 0, nano: 0]
%time.to_date dt                        //= Date[year: 2024, month: 3, day: 10]
%time.to_time dt                        //= Time[hour: 9, minute: 30, second: 0, nano: 0]
```

Durations are built from a count of a unit, and periods likewise:

```quiver
%time.ms 250                            //= Duration[250000000]
%time.hours 2 ~> %time.to_seconds     //= 7200
%time.micros 1500 ~> %time.to_ms      //= 1 // truncated toward zero
%time.days 3                            //= Period[months: 0, days: 3]
```

### The literal dialect

`%time{ … }` reads its content with `parse` at compile time, and stands for the constant it
denotes, so malformed or impossible content is a compile error:

```quiver
%time{ 2024-03-10 }                     //= Date[year: 2024, month: 3, day: 10]
%time{ 09:30 }                          //= Time[hour: 9, minute: 30, second: 0, nano: 0]
%time{ 2024-03-10T09:30:00Z }           //= Instant[1710063000000000000]
%time{ 250ms }                          //= Duration[250000000]
%time{ 1y6mo }                          //= Period[months: 18, days: 0]
```

```quiver
%time{ 2023-02-29 }                     //! a valid date
%time{ 2024-03-10T25:00 }               //! a valid time
%time{ 1h3d }                           //! don't mix
```

## Instants and civil time

Moving between the timeline and the calendar always names the zone. `utc` reads an instant's
civil date and time in UTC, and `from_utc` finds the instant at which UTC reads a civil
date-time:

```quiver
%time.epoch ~> %time.utc              //= DateTime[year: 1970, month: 1, day: 1, hour: 0, minute: 0, second: 0, nano: 0]
%time{ 2024-01-01T00:00 } ~> %time.from_utc   //= Instant[1704067200000000000]
%time.from_unix -1 ~> %time.utc       //= DateTime[year: 1969, month: 12, day: 31, hour: 23, minute: 59, second: 59, nano: 0]
```

`from_unix` and `to_unix` convert whole seconds since the epoch, and `from_unix_ms` and
`to_unix_ms` milliseconds; the `to_` forms round down.

```quiver
%time.from_unix 1704067200 ~> %time.to_unix_ms   //= 1704067200000
%time.from_unix_ms -1 ~> %time.to_unix           //= -1
```

## Arithmetic

`add` and `sub` take a value and a span. A duration moves an instant, a monotonic reading, a
time of day (wrapping at midnight) or a date-time; a period moves a date or a date-time.
Spans of the same kind add to each other. Any other pairing is a compile error, so the
result's type is exactly the value's:

```quiver
%time.add [%time.epoch, %time.hours 1] ~> %time.utc ~> .hour   //= 1
%time.add [%time{ 23:30 }, %time.hours 1] //= Time[hour: 0, minute: 30, second: 0, nano: 0]
%time.sub [%time{ 2024-03-01 }, %time.days 1]    //= Date[year: 2024, month: 2, day: 29]
%time.add [%time.seconds 90, %time.ms 500] ~> %time.format   //= "1m30s500ms"
```

```quiver
%time.add [%time.epoch, %time.days 1]   //! Type mismatch // an instant has no calendar
```

A period moves the month first, then the days. When the day doesn't exist in the target
month it is clamped to the month's last day; `overflow: Reject` answers nil instead:

```quiver
%time.add [%time{ 2024-01-31 }, %time.months 1]   //= Date[year: 2024, month: 2, day: 29]
%time.add [%time{ 2024-01-31 }, %time.months 1, overflow: Reject] ~> :error<'%time.error>   //= OutOfRange[Day]
%time.add [%time{ 2024-01-15 }, %time.months 1, overflow: Reject]   //= Date[year: 2024, month: 2, day: 15]
%time.add [%time{ 2024-02-29 }, %time.years 1]    //= Date[year: 2025, month: 2, day: 28]
```

`diff` is the exact duration from one value to another (the first minus the second), and
`between` the calendar period from one date to another:

```quiver
%time.diff [%time{ 2024-01-02T00:00Z }, %time{ 2024-01-01T12:00Z }]   //= Duration[43200000000000]
%time.between [%time{ 2024-01-31 }, %time{ 2024-03-15 }]   //= Period[months: 1, days: 15]
%time.days_between [%time{ 2024-01-01 }, %time{ 2025-01-01 }]   //= 366
```

`between` answers a period that `add` takes back to where it started:

```quiver
from = %time{ 2023-11-30 }
%time.between [from, %time{ 2024-02-10 }] ~> %time.add [from, ~]   //= Date[year: 2024, month: 2, day: 10]
```

`neg`, `abs` and `mul` work on durations (and `neg` and `mul` on periods):

```quiver
%time.neg %time{ 90s } ~> %time.format          //= "-1m30s"
%time.seconds -5 ~> %time.abs                   //= Duration[5000000000]
%time.mul [%time.days 2, 3]                       //= Period[months: 0, days: 6]
```

## Comparison

`compare` answers -1, 0 or 1 for two values of the same kind; `before?` and `after?` are the
tests, and `min` and `max` pick one. Periods have no order, a month not being a fixed number
of days.

```quiver
%time.compare [%time{ 2024-01-01 }, %time{ 2023-12-31 }]   //= 1
%time.before? [%time{ 09:00 }, %time{ 17:00 }]             //= Ok
%time.max [%time.ms 1500, %time.seconds 1]                 //= Duration[1500000000]
{ %time.after? [%time.seconds 1, %time.seconds 1] }        //= []
```

```quiver
%time.compare [%time.days 30, %time.months 1]   //! Type mismatch
```

## The calendar

```quiver
%time.leap_year? 2000                   //= Ok
{ %time.leap_year? 1900 }               //= []
%time.days_in_month [2024, 2]           //= 29
%time.days_in_year 2023                 //= 365
%time.day_of_year %time{ 2024-12-31 }   //= 366
%time.weekday %time{ 1970-01-01 }       //= Thu
```

`iso_week` is the ISO 8601 week-numbering year and week, which can differ from the calendar
year near its ends:

```quiver
%time.iso_week %time{ 2024-12-30 }      //= [year: 2025, week: 1]
%time.iso_week %time{ 2021-01-03 }      //= [year: 2020, week: 53]
```

`truncate` finds the start of the unit containing a value. Dates and date-times truncate to
the year, month or week (starting Monday) as well as to the day and below; instants
truncate on the UTC day, and durations toward zero.

```quiver
%time.truncate [%time{ 2024-03-14 }, Week]            //= Date[year: 2024, month: 3, day: 11]
%time.truncate [%time{ 2024-03-14T15:45:10 }, Month]  //= DateTime[year: 2024, month: 3, day: 1, hour: 0, minute: 0, second: 0, nano: 0]
%time.truncate [%time{ 2024-03-14T15:45:10Z }, Hour] ~> %time.format   //= "2024-03-14T15:00:00Z"
%time.truncate [%time{ -1h30m }, Hour]                //= Duration[-3600000000000]
```

## Text

`format` writes ISO 8601 for instants (always in UTC), dates, times and date-times, and unit
notation for durations and periods. A fraction of a second takes three, six or nine digits,
the fewest that are exact.

```quiver
%time.format %time{ 2024-03-10T09:30:00.5Z }   //= "2024-03-10T09:30:00.500Z"
%time.format %time{ 2024-03-10 }               //= "2024-03-10"
%time.format %time{ 09:30 }                    //= "09:30:00"
%time.format %time{ -0044-03-15T00:00 }        //= "-0044-03-15T00:00:00"
%time.nanos 1500 ~> %time.format             //= "1us500ns"
%time.format %time{ 1y14mo3d }                 //= "2y2mo3d"
%time.seconds 0 ~> %time.format              //= "0s"
```

Durations take the units `ns`, `us`, `ms`, `s`, `m` and `h`, and periods `d`, `w`, `mo` and
`y`. A leading sign applies to the whole span, and a later component may carry its own —
which only a period, whose months and days can differ in sign, needs:

```quiver
%time{ -1h30m }                                //= Duration[-5400000000000]
%time.add [%time.months 1, %time.days -3] ~> %time.format   //= "1mo-3d"
%time.add [%time.months -1, %time.days 3] ~> %time.format   //= "-1mo+3d"
%time{ -1mo+3d }                               //= Period[months: -1, days: 3]
```

`parse` reads any of those forms back — the result is whichever kind the text denotes, told
apart by matching. An instant may carry a UTC offset instead of `Z`, and is converted to UTC;
seconds are optional; nil carries `:error Expected[offset, message]` for malformed text.

```quiver
%time.parse "2024-03-10T10:30:00+01:00" ~> ='%time.instant ~> %time.format   //= "2024-03-10T09:30:00Z"
%time.parse "2024-03-10" ~> ='%time.date                  //= Date[year: 2024, month: 3, day: 10]
%time.parse "-1h30m"                                      //= Duration[-5400000000000]
%time.parse "2024-13-01" ~> :error<'%time.error>          //= Expected[offset: 0, message: "a valid date"]
%time.parse "12:3" ~> :error<'%time.error>                //= Expected[offset: 3, message: "two-digit minutes"]
```

`http_date` and `parse_http_date` convert the HTTP date format (IMF-fixdate):

```quiver
%time.from_unix 784111777 ~> %time.http_date   //= "Sun, 06 Nov 1994 08:49:37 GMT"
%time.parse_http_date "Sun, 06 Nov 1994 08:49:37 GMT" ~> ='%time.instant ~> %time.to_unix   //= 784111777
%time.parse_http_date "Sun, 06 Nov 1994 08:49:37 UTC"   //= []
```
