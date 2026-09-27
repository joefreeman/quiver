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
| `'%time.zone` | `Utc`, `Fixed[seconds]`, `Tz[name:, rules:]` | a zone: UTC, a fixed offset, or a named zone with its rules |
| `'%time.zoned` | `Zoned[instant, zone]` | an instant in a zone, which gives it a civil date and time |

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

Rounding down is what places an instant: a millisecond before the epoch lies in the second
before it. A duration's `to_` conversions (`to_micros` up to `to_hours`) measure a length
instead, so they truncate toward zero:

```quiver
%time.ms -1 ~> %time.to_seconds                  //= 0
%time.minutes 150 ~> %time.to_hours              //= 2
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

`between` answers a period that `add` takes back to where it started — going forwards.
Backwards, it is the forward period negated, which adding can't always undo, since month
lengths differ:

```quiver
from = %time{ 2023-11-30 }
%time.between [from, %time{ 2024-02-10 }] ~> %time.add [from, ~]   //= Date[year: 2024, month: 2, day: 10]
back = %time.between [%time{ 2023-03-31 }, %time{ 2023-02-28 }]
back                                                                //= Period[months: -1, days: -3]
%time.add [%time{ 2023-03-31 }, back]                               //= Date[year: 2023, month: 2, day: 25]
```

The civil types are plain tuples, so decoding checks only their shape: text can name a date
that doesn't exist. Read untrusted text with `parse`, which checks the calendar:

```quiver
%data.decode<'%time.date> "Date[year: 2023, month: 2, day: 31]"   //= Date[year: 2023, month: 2, day: 31]
%time.parse "2023-02-31" ~> :error<'%time.error>                  //= Expected[offset: 0, message: "a valid date"]
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

## Zones

A zone maps the UTC timeline to the clocks of a place. `zone` looks one up by its IANA name
in the host's time zone database — the system's on a native host, an embedded copy in the
browser — so, like `now`, it reads host state and a module can't call it at compile time.
`fixed` makes a zone at a constant offset, and `local_zone` is the host's own.

```quiver
%time.zone "Europe/London" ~> ='%time.zone ~> %time.zone_name   //= "Europe/London"
%time.zone "UTC"                                                //= Utc
%time.fixed %time{ 5h30m }                                      //= Fixed[19800]
%time.zone "Mars/Olympus" ~> :error<'%time.error>               //= UnknownZone["Mars/Olympus"]
```

`zoned` places an instant in a zone, giving a **zoned value**: `Zoned[instant, zone]`. Its
civil date and time are the zone's clocks at that instant, and `offset`, `abbreviation` and
`dst?` describe the zone there.

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
summer = %time.zoned [%time{ 2024-07-01T12:00Z }, london]
summer ~> %time.to_datetime              //= DateTime[year: 2024, month: 7, day: 1, hour: 13, minute: 0, second: 0, nano: 0]
summer ~> %time.abbreviation             //= "BST"
summer ~> %time.offset ~> %time.format   //= "1h"
summer ~> %time.dst?                     //= Ok
summer ~> %time.to_instant               //= Instant[1719835200000000000]
```

A zone's rules reach back through its history and forward indefinitely:

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
%time.zoned [%time{ 1800-01-01T12:00Z }, london] ~> %time.format   //= "1800-01-01T11:58:45-00:01:15[Europe/London]"
%time.zoned [%time{ 2090-07-01T12:00Z }, london] ~> %time.abbreviation   //= "BST"
sydney = %time.zone "Australia/Sydney" ~> ='%time.zone
%time.zoned [%time{ 2024-01-01T12:00Z }, sydney] ~> %time.abbreviation   //= "AEDT"
```

### Placing a civil time

`zoned` also places a civil date-time in a zone. Usually that names one instant, but where
clocks change some local times happen twice (when they go back) and some never (when they go
forward). `disambiguation` decides: `Earlier` or `Later` picks a side, `Compatible` — the
default — takes the earlier of a repeated time and moves a skipped one forward by the gap,
and `Reject` answers nil carrying `:error Repeated` or `Skipped`.

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
gap = %time{ 2024-03-31T01:30 }   // clocks go from 01:00 straight to 02:00
%time.zoned [gap, london] ~> %time.format                             //= "2024-03-31T02:30:00+01:00[Europe/London]"
%time.zoned [gap, london, disambiguation: Earlier] ~> %time.format    //= "2024-03-31T00:30:00+00:00[Europe/London]"
%time.zoned [gap, london, disambiguation: Reject] ~> :error<'%time.error>   //= Skipped
fold = %time{ 2024-10-27T01:30 }  // 01:00 to 02:00 happens twice
%time.zoned [fold, london] ~> %time.format                            //= "2024-10-27T01:30:00+01:00[Europe/London]"
%time.zoned [fold, london, disambiguation: Later] ~> %time.format     //= "2024-10-27T01:30:00+00:00[Europe/London]"
```

A time in the future is best kept as the civil date-time and the zone's name, and placed
when it's needed: governments change their zones' rules, and an instant computed today would
drift from the wall-clock time that was meant.

### Arithmetic on zoned values

A duration moves a zoned value along the timeline, and a period moves it on the zone's
calendar, keeping the wall-clock time across a change of offset:

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
eve = %time.zoned [%time{ 2024-03-30T12:00 }, london]
%time.add [eve, %time.days 1] ~> %time.format     //= "2024-03-31T12:00:00+01:00[Europe/London]"
%time.add [eve, %time.hours 24] ~> %time.format   //= "2024-03-31T13:00:00+01:00[Europe/London]"
%time.truncate [%time.add [eve, %time.days 1], Day] ~> %time.format   //= "2024-03-31T00:00:00+00:00[Europe/London]"
```

Where a period or a truncation lands on a repeated local time, the value keeps the offset
it had, so it stays on its own side of the change:

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
late = %time.zoned [%time{ 2024-10-27T01:30 }, london, disambiguation: Later]
%time.add [late, %time.days 0] ~> %time.format        //= "2024-10-27T01:30:00+00:00[Europe/London]"
%time.truncate [late, Hour] ~> %time.format           //= "2024-10-27T01:00:00+00:00[Europe/London]"
```

`diff` and the comparisons read zoned values by their instants, whatever their zones:

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
a = %time.zoned [%time{ 2024-07-01T12:00Z }, london]
b = %time.zoned [%time{ 2024-07-01T12:00Z }, Utc]
%time.compare [a, b]                     //= 0
%time.diff [a, b]                        //= Duration[0]
```

### Zoned text

`format` writes a zoned value in the RFC 9557 form: the local date-time, its offset, and
the zone in brackets. `parse_zoned` reads it back, looking the zone up as `zone` does. An
offset in the text must be the zone's offset then; `Z` gives the exact instant, and no offset
at all places the civil time as `Compatible` does.

```quiver
%time.parse_zoned "2024-07-10T09:30:00+01:00[Europe/London]" ~> ='%time.zoned ~> %time.to_instant ~> %time.format   //= "2024-07-10T08:30:00Z"
%time.parse_zoned "2024-07-10T09:30Z[Europe/London]" ~> ='%time.zoned ~> %time.format   //= "2024-07-10T10:30:00+01:00[Europe/London]"
%time.parse_zoned "2024-07-10T09:30:00+00:00[Europe/London]" ~> :error<'%time.error>   //= Expected[offset: 19, message: "an offset the zone has then"]
```

`parse` reads no zones, and says so:

```quiver
%time.parse "2024-07-10T09:30[Europe/London]" ~> :error<'%time.error>   //= Expected[offset: 16, message: "no zone annotation: parse_zoned reads one"]
```

A `%time{ … }` literal may name a zone. One in UTC or at a fixed offset is a constant. One
naming an IANA zone has only its syntax checked when compiled — the zone database can't be
read then — and is placed when it runs, failing fast if the host has no such zone or the
zone doesn't have the literal's offset.

```quiver
%time{ 2024-07-10T09:30+05:30[+05:30] }   //= Zoned[Instant[1720584000000000000], Fixed[19800]]
%time{ 2024-07-10T09:30[Europe/London] } ~> %time.format   //= "2024-07-10T09:30:00+01:00[Europe/London]"
```

`from_tzif` makes a zone from TZif data (RFC 8536) held anywhere, rather than the host's
database — checking it first:

```quiver
%time.from_tzif ["Nowhere", <00>] ~> :error<'%time.error>   //= Expected[offset: 0, message: "a TZif header"]
```

A named zone holds its rules as a function, which only `zone` and `from_tzif` make — so no
text decodes into one, and zoned values have no data notation. `format` and `parse_zoned`
are how a zoned value is written down and read back:

```quiver
%data.decode<'%time.zone> "Utc"                         //= Utc
%data.decode<'%time.zone> "Tz[name: \"x\", rules: 1]"   //= []
```

## Series

`series` counts out values from a start by a step, as a lazy `%iter`: forever, or up to — not
including — `until`. A duration steps an instant, date-time or zoned value; a period steps a
date, date-time or zoned value.

```quiver
%time.series [%time{ 2024-01-01T00:00Z }, %time.hours 6, until: %time{ 2024-01-02T00:00Z }] ~> %iter.count   //= 4
%time.series [%time{ 2024-01-10 }, %time.days -3, until: %time{ 2024-01-01 }] ~> %list.collect
//= Cons[Date[year: 2024, month: 1, day: 10], Cons[Date[year: 2024, month: 1, day: 7], Cons[Date[year: 2024, month: 1, day: 4], Nil]]]
```

`until` stops the series at the first value at or past it, in the direction the step goes —
for a period, by its average length — so a step whose first value lands back on the start
still stops:

```quiver
%time.series [%time{ 2024-02-01 }, %time{ 1mo-29d }, until: %time{ 2024-02-05 }] ~> %iter.map [~, %time.format] ~> %list.collect
//= Cons["2024-02-01", Cons["2024-02-01", Cons["2024-02-03", Cons["2024-02-04", Nil]]]]
```

Each value is the start plus a whole number of steps, not the previous value plus one, so a
clamped day doesn't carry forward:

```quiver
%time.series [%time{ 2024-01-31 }, %time.months 1] ~> %iter.take [~, 3] ~> %iter.map [~, %time.format] ~> %list.collect
//= Cons["2024-01-31", Cons["2024-02-29", Cons["2024-03-31", Nil]]]
```

In a zone, a day is a calendar day and 24 hours is 24 hours, which part company where the
clocks change:

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
start = %time.zoned [%time{ 2024-03-30T09:00 }, london]
%time.series [start, %time.days 1] ~> %iter.take [~, 2] ~> %iter.map [~, %time.format] ~> %list.collect
//= Cons["2024-03-30T09:00:00+00:00[Europe/London]", Cons["2024-03-31T09:00:00+01:00[Europe/London]", Nil]]
%time.series [start, %time.hours 24] ~> %iter.take [~, 2] ~> %iter.map [~, %time.format] ~> %list.collect
//= Cons["2024-03-30T09:00:00+00:00[Europe/London]", Cons["2024-03-31T10:00:00+01:00[Europe/London]", Nil]]
```

## Rounding

`round` takes a value to a multiple of an increment — a unit's length, or any duration — to
the nearest by default, with halves going away from zero. `mode:` rounds `Floor`, `Ceil` or
`Trunc` (toward zero) instead. Instants round on the UTC timeline, and civil and zoned values
on their own clocks, a time of day wrapping at midnight:

```quiver
%time.round [%time{ 09:07:31 }, %time{ 15m }]                //= Time[hour: 9, minute: 15, second: 0, nano: 0]
%time.round [%time{ 09:07:31 }, %time{ 15m }, mode: Floor]   //= Time[hour: 9, minute: 0, second: 0, nano: 0]
%time.round [%time{ 23:59:31 }, %time.minutes 1]             //= Time[hour: 0, minute: 0, second: 0, nano: 0]
%time.round [%time{ -1h30m }, %time.hours 1]                 //= Duration[-7200000000000]
%time.round [%time{ 2024-03-10T09:30:00.123456Z }, %time.ms 1] ~> %time.format   //= "2024-03-10T09:30:00.123Z"
```

`truncate` is the calendar's counterpart, for weeks, months and years. A zoned value in a
repeated hour keeps its offset when rounded, as it does when truncated.

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
`y`, largest first and each at most once. A leading sign applies to the whole span, and a later component may carry its own —
which only a period, whose months and days can differ in sign, needs:

```quiver
%time{ -1h30m }                                //= Duration[-5400000000000]
%time.add [%time.months 1, %time.days -3] ~> %time.format   //= "1mo-3d"
%time.add [%time.months -1, %time.days 3] ~> %time.format   //= "-1mo+3d"
%time{ -1mo+3d }                               //= Period[months: -1, days: 3]
%time.parse "30s1h" ~> :error<'%time.error>    //= Expected[offset: 4, message: "units largest first, each at most once"]
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

### Patterns

With a `pattern:`, `format` writes a value as the pattern's `%` directives say, and `parse`
reads it back. The directives follow strftime's:

| | |
| --- | --- |
| dates | `%Y` year, `%y` its last two digits, `%m` month, `%d` day, `%e` day space-padded, `%j` day of the year, `%a`/`%A` weekday name, `%b`/`%B` month name, `%u` ISO weekday (Monday 1), `%w` weekday (Sunday 0), `%V`/`%G` ISO week and its year, `%F` = `%Y-%m-%d`, `%D` = `%m/%d/%y` |
| times | `%H` hour, `%k` hour space-padded, `%I`/`%l` 12-hour clock, `%p` AM/PM, `%M` minute, `%S` second, `%f` fraction (3, 6 or 9 digits), `%.f` the same with a dot, or nothing when zero, `%3f`/`%6f`/`%9f` fixed digits, `%T` = `%H:%M:%S`, `%R` = `%H:%M` |
| instants and zoned values | `%z` offset `+hhmm`, `%:z` offset `+hh:mm`, `%Z` abbreviation, `%Q` zone name, `%s` seconds since the epoch |
| text | `%%`, `%n` newline, `%t` tab |

A flag between the `%` and its letter sets a number's padding: `-` for none, `_` for spaces,
`0` for zeros.

```quiver
%time.format [%time{ 2024-03-10 }, pattern: "%A %-d %B %Y (day %j, week %V)"]   //= "Sunday 10 March 2024 (day 070, week 10)"
%time.format [%time{ 2024-03-10T21:05:07 }, pattern: "%d/%m/%y %-I:%M %p"]        //= "10/03/24 9:05 PM"
london = %time.zone "Europe/London" ~> ='%time.zone
z = %time.zoned [%time{ 2024-07-04T15:05:09.25Z }, london]
%time.format [z, pattern: "%a, %d %b %Y %T%.f %z %Z"]   //= "Thu, 04 Jul 2024 16:05:09.250 +0100 BST"
```

A pattern names what it needs, so one the value can't fill fails, as does an unknown
directive:

```quiver
%time.format [%time{ 2024-03-10 }, pattern: "%H"] ~> :error<'%time.error>      //= InvalidPattern[offset: 0, message: "a time of day, for %H"]
%time.format [%time{ 2024-03-10 }, pattern: "%Y %q"] ~> :error<'%time.error>   //= InvalidPattern[offset: 3, message: "a directive: %q isn't one"]
```

Reading, the kind of value comes from the fields the pattern has: an instant with an offset
or `%s`, else a date-time, a date or a time. Numbers are read at their width unless a flag
allows fewer digits, so fields can run together; a two-digit year is read as 1970 to 2069.
Fields that can disagree are checked rather than ignored: a weekday name against the date, a
month against a day of the year — and `%p` needs a 12-hour clock, a weekday a date.

```quiver
%time.parse ["10/03/24 9:05 PM", pattern: "%d/%m/%y %-I:%M %p"]   //= DateTime[year: 2024, month: 3, day: 10, hour: 21, minute: 5, second: 0, nano: 0]
%time.parse ["20240310T0905", pattern: "%Y%m%dT%H%M"]           //= DateTime[year: 2024, month: 3, day: 10, hour: 9, minute: 5, second: 0, nano: 0]
%time.parse ["2024-03-10T09:05:07+0100", pattern: "%FT%T%z"] ~> ='%time.instant ~> %time.format   //= "2024-03-10T08:05:07Z"
%time.parse ["2024-070", pattern: "%Y-%j"]                      //= Date[year: 2024, month: 3, day: 10]
%time.parse ["090507123", pattern: "%H%M%S%3f"]                 //= Time[hour: 9, minute: 5, second: 7, nano: 123000000]
%time.parse ["09:05 PM", pattern: "%H:%M %p"] ~> :error<'%time.error>   //= Expected[offset: 0, message: "a 12-hour clock (%I), for AM or PM"]
%time.parse ["Sunday 09:05", pattern: "%A %H:%M"] ~> :error<'%time.error>   //= Expected[offset: 0, message: "a date, for the day of the week"]
%time.parse ["Monday 10 March 2024", pattern: "%A %-d %B %Y"] ~> :error<'%time.error>   //= Expected[offset: 0, message: "the date's day of the week"]
%time.parse ["2024-03", pattern: "%Y-%m"] ~> :error<'%time.error>
//= Expected[offset: 0, message: "a complete date: a year with a month and day, or a day of the year"]
```

`%z` and `%:z` read the offsets they write, seconds included, so a pattern round-trips even
London's pre-1847 local mean time:

```quiver
london = %time.zone "Europe/London" ~> ='%time.zone
old = %time.zoned [%time{ 1800-01-01T12:00Z }, london]
text = %time.format [old, pattern: "%FT%T%z"] ~> ='%str
text                                                              //= "1800-01-01T11:58:45-000115"
%time.parse [text, pattern: "%FT%T%z"] ~> ='%time.instant ~> %time.format   //= "1800-01-01T12:00:00Z"
```

Zone names can't be read with a pattern — `parse_zoned` reads zoned values — and nor can
the directives that only describe a date (`%u`, `%w`, `%V`, `%G`).

`http_date` writes the HTTP date format (IMF-fixdate). `parse_http_date` reads it, and the
two obsolete forms RFC 9110 still has recipients accept — RFC 850's, whose two-digit year
it reads as 1970 to 2069, and C's asctime — checking the day of the week against the date:

```quiver
%time.from_unix 784111777 ~> %time.http_date   //= "Sun, 06 Nov 1994 08:49:37 GMT"
%time.parse_http_date "Sun, 06 Nov 1994 08:49:37 GMT" ~> ='%time.instant ~> %time.to_unix   //= 784111777
%time.parse_http_date "Sunday, 06-Nov-94 08:49:37 GMT" ~> ='%time.instant ~> %time.to_unix   //= 784111777
%time.parse_http_date "Sun Nov  6 08:49:37 1994" ~> ='%time.instant ~> %time.to_unix   //= 784111777
%time.parse_http_date "Mon, 06 Nov 1994 08:49:37 GMT" ~> :error<'%time.error>   //= Expected[offset: 0, message: "the date's day of the week"]
%time.parse_http_date "Sun, 06 Nov 1994 08:49:37 UTC"   //= []
```
