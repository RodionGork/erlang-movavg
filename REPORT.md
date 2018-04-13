# Report on implementing test task "Moving Averages"

### Overview

I decided to try implement the task in Erlang, not in attempt to impress the reviewers,
but to get at least a slight idea of what this language is like. This seems
important to me as the position suggests working primarily with Erlang and
I never have had any experience with it before.

The solution seems to work, though after I completed it, I have understood it is not optimal
in the sense of performance. It works with 10000 metrics as required, but accepts new metric
values with speed of about 15000 per seconds. I.e. 1 million values is ingested in about a minute.

Meanwhile the problem statement tells us _amount of samples submitted for
all metrics during 1 minute interval can be assumed to be small enough
to fit into memory_ and I believe that in 1 Gb memory, roughly, 10 million
values may fit (given that every value takes up to 100 bytes with timestamp and metric name)...

I'm going to describe the solution below along with the ways of improving efficiency.

### Running the application

Build it with rebar3:

    rebar3 compile

And then execute with the help of the included shell script:

    runme.sh

The data are entered from console like this:

    bla 3.0
    mla 5.0
    bla 7.0
    mla 1.0
    ? bla
    5.0
    ? mla
    3.0
    ? zlo
    0.0
    ! !

Here lines can be in one of three forms:

- `metricname value` - to report a new value
- `? metricname` - to request for current average
- `! !` - to stop execution and quit (used for test)

Note that there is no sanity check on input, for simplicity (and due to
my lack of experience with the language) - values should be given with
decimal point and fractional part (though I understand it is not quite convenient).

### Implementation

After reading a bit about Erlang I was puzzled about how to collect data given that Erlang does
not allow changing variables. I even tried to ask for enlightenment at stackoverflow:  
https://stackoverflow.com/questions/48238920/what-is-erlang-way-to-collect-aggregate-incoming-data

I've read there is `ets` storage which really allows to handle changing values, though it seemed to me
not the "purest" way.

So I came up with idea of spawning a separate thread, which handles data passing them, as they are changed
to itself in a tail-recursive manner. Then the other threads (main thread in our case) can send messages
to this calculating thread.

In the code the main thread stuff is in `movavg_app.erl` module, while calculating thread is in `calc.erl`.

### Data structures

Every reported value is sent to calculator thread and stored in a `queue` along with its metric tag and
timestamp of receiption.

As the time passes, we evict the oldest elements from this queue (when their timestamp is older than
1 minute compared to current time). It is quite easy as the elements were added with their timestamps
steadily increasing (i.e. the queue has timestamps sorted).

Now we need additional structure to calculate averages. It is a dict of tuples. The keys are metric tags,
while values contain the sum of elements for this metric (in a queue) and their count.

When the new value comes, we add it to sum in the dict and increment the count. When the element is evicted
from the queue, we subtract its value from the sum and decrement the count.

### Performance Consideration

The most obvious (now, well, retrospectively) flaw is that the calculation is performed in a single thread.
However metrics are independent, so to improve the matter we can assigne different metrics to different threads
in a few ways:

- either have a few threads (e.g. as many as processor cores), and to each of them the metric is assigned
by the hash of its tag;
- or have the thread per metric at all (I've read recently that threads in Erlang are light-weight) - only
it would be good to take care of shutting down threads which are not used for long time.

Another minor improvement (probably more about memory consumption) would be to try different data structures.
Initially I wanted to keep array instead of queue, keeping the index of the last evicted element and
reallocating it when amount of evicted elements grows considerably large. However this requires either
additional testing or better knowledge of Erlang and its libraries.

### Additional stuff

I've tried also to find out some info about common build tool for Erlang. It seems like "rebar3" is the
popular solution so I had given it a chance. For me it mainly serves for cleaning / compiling and running unit tests.

As for testing, I've created a few unit-tests in the process of development, so that I can check functionality
of calculator thread methods. They are executed with `rebar3 eunit`.

Besides theme I've added a small script to test loading time of 1 million of events. It works like this:

    python large_test.py |./runme.sh

It simply feeds 1 million values into 10000 metrics and then requests for averages from all of them.
No special check of results is performed (as it is more the test for execution time).

### Conclusion

As my personal goal was to get acquainted with Erlang, I feel I now have
at least basic feeling of it. The language is somewhat alike but still much different
from Scala and Haskell with which I wrote some code in the past. The opportunity to live
without strict (and sometimes brain-blowing) type checking is pleasant though probably it
may cause harder mistakes within larger projects and teams.

