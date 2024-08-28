# Erlang MovAvg

The candidate should develop an Erlang application which collects a numeric metrics from different components of some abstract system.
The only result of this collection should be the value of moving average among submitted metric samples for some time interval of fixed size. 

The application should support simultaneous calculation of up to 10k of metrics and the average value for a metric should be returned for the interval
of the last minute (60k ms). The intended interface should be compatible with the following specification: 

    metric_app:report(MetricName :: binary(), MetricValue :: float()). 
    
    metric_app:average(MetricName :: binary()) -> float(). 

For simplicity the amount of samples submitted for all metrics during 1 minute interval can be assumed to be small enough to fit into memory. 

The expected result should be the source code of an application which can be compiled with Erlang 19 and ran with application:start/1 call. 
