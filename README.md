# ekairos
Small library to parse crontab like rules and find next mathing date and time from given starting time and date.

## Examples
Using crontab like format:
```erlang
NextMonday = fun (DateTime) ->                                                              
    case ekairos:find_next_time(ekairos:parse_crontab_line("* * * * Mon"),DateTime) of      
        {next_time,{Date,_Time}} -> Date;                                                   
         Else -> Else                                                                       
    end                                                                                     
end.
NextMonday(calendar:local_time()).
```
evaluates to {2016,10,10}.

Using internal rule representation with same meening as above:
```
NextMonday = fun (DateTime) ->                                                              
    case ekairos:find_next_time({[any],[any],{[any],[{exact,1}]},[any],[any],[any]},DateTime) of
        {next_time,{Date,_Time}} -> Date;                                                   
         Else -> Else                                                                       
    end                                                                                     
end.
NextMonday(calendar:local_time()).
```
evaluates to {2016,10,10}. Please read typespecs for more info on internal representation of rules.

## Bugs
Definetly. Please report. I may fix them in undefined time period.

### Is it working
Not sure. It was suposed to be coding exerscise. 
