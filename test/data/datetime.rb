=begin
= xmlrpc/datetime.rb
Copyright (C) 2001, 2002, 2003 by Michael Neumann (mneumann@ntecs.de)

Released under the same term of license as Ruby.

= Classes
* ((<XMLRPC::DateTime>))

= XMLRPC::DateTime
== Description
This class is important to handle XMLRPC (('dateTime.iso8601')) values,
correcly, because normal UNIX-dates (class (({Date}))) only handle dates 
from year 1970 on, and class (({Time})) handles dates without the time
component. (({XMLRPC::DateTime})) is able to store a XMLRPC 
(('dateTime.iso8601')) value correctly.

== Class Methods
--- XMLRPC::DateTime.new( year, month, day, hour, min, sec )
    Creates a new (({XMLRPC::DateTime})) instance with the
    parameters ((|year|)), ((|month|)), ((|day|)) as date and 
    ((|hour|)), ((|min|)), ((|sec|)) as time.
    Raises (({ArgumentError})) if a parameter is out of range, or ((|year|)) is not
    of type (({Integer})).
    
== Instance Methods
--- XMLRPC::DateTime#year
--- XMLRPC::DateTime#month
--- XMLRPC::DateTime#day
--- XMLRPC::DateTime#hour
--- XMLRPC::DateTime#min
--- XMLRPC::DateTime#sec
    Return the value of the specified date/time component.

--- XMLRPC::DateTime#mon
    Alias for ((<XMLRPC::DateTime#month>)).

--- XMLRPC::DateTime#year=( value )
--- XMLRPC::DateTime#month=( value )
--- XMLRPC::DateTime#day=( value )
--- XMLRPC::DateTime#hour=( value )
--- XMLRPC::DateTime#min=( value )
--- XMLRPC::DateTime#sec=( value )
    Set ((|value|)) as the new date/time component.
    Raises (({ArgumentError})) if ((|value|)) is out of range, or in the case
    of (({XMLRPC::DateTime#year=})) if ((|value|)) is not of type (({Integer})).

--- XMLRPC::DateTime#mon=( value )
    Alias for ((<XMLRPC::DateTime#month=>)).

--- XMLRPC::DateTime#to_time
    Return a (({Time})) object of the date/time which (({self})) represents.
    If the (('year')) is below 1970, this method returns (({nil})), 
    because (({Time})) cannot handle years below 1970.
    The used timezone is GMT.

--- XMLRPC::DateTime#to_date
    Return a (({Date})) object of the date which (({self})) represents.
    The (({Date})) object do ((*not*)) contain the time component (only date).

--- XMLRPC::DateTime#to_a
    Returns all date/time components in an array.
    Returns (({[year, month, day, hour, min, sec]})).
=end


