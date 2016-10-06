-module(ekairos_crontab).
-author("Ričardas Pocius cirka@cirka.lt").
-export([parse_line/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%%%
%%- --- Parse crontab format style DateTime rule and convert it to internal representation used in ekairos ----
%% crontab line  has format "MinuteRule HourRule DayOfMonthRule MonthRule DayOfWeekRule Coomand_to_execute"
%% Please man 5 crontab for complete description and special node regarding Day of Month and Day of Week matched together
%% Each column can be list of rules separated by comma, %% Each rule can either be: exact value (2), range of values (4-8),
%% range of values with step (1-20,20) -> basicaly Day rem Step ==1,
%% wildcard (matching any value) or wildcard with step=/=1 !!!
%%       ex "*/10 18-30/2 1-31,1-6 *" 
%%%{{_YYYY,_MM,_DD},{_HH24,_MI,_SS}} = DateTime

parse_line(Entry) when is_list(Entry) ->
    case string:tokens(Entry," ") of
     [_M,_H,_DoM,_Mo,_DoW] = Date ->
        [MinuteMatches, HourMatches, DayOfMonthMatches, MonthMatches, DayOfWeekMatches] = 
             [string:tokens(N,",") || N <- Date],
        { [any],
          parse_cron_column(MonthMatches,month),
          { parse_cron_column(DayOfMonthMatches,dom),
            parse_cron_column(DayOfWeekMatches,dow) 
          },
          parse_cron_column(HourMatches,hour),
          parse_cron_column(MinuteMatches,minute),
          [any]
        };
    _Other -> error(badarg)
    end.

parse_cron_column([],_) -> error(badarg);

parse_cron_column(Column,Type) when length(Column) > 0 ->
    Decode = fun 
        ({exact,V} ,Acc) ->
            Acc ++ [{exact,decode_val(Type,V)}];
        ({range,From,To},Acc) ->
            Acc ++ [{exact,V} || V <- lists:seq(decode_val(Type,From),decode_val(Type,To))];
        ({steprange,From,To,Step},Acc) when is_integer(Step) ->
            Acc ++ [{exact,V} || V <-lists:seq(decode_val(Type,From),decode_val(Type,To),Step)];
        (any,Acc) -> [any|Acc];
        ({stepany,Step},Acc) when is_integer(Step) -> [{stepany,Step}|Acc];
        (_,_) -> error(badarg)
    end,
    TypedEntries = [parse_column_entry(Entry)|| Entry <- Column],
    DecodedEntries = lists:foldl(Decode,[],TypedEntries),
    _PriororitasedEntries = prio_column_entries(DecodedEntries).

parse_column_entry(Entry) ->
    case string:tokens(Entry,"/") of
        [Range,Step] ->
            case string:tokens(Range,"-") of
                [From,To] -> 
                    {steprange,From,To,list_to_integer(Step)};
                ["*"] -> {stepany,list_to_integer(Step)};
                Else -> io:format("Else:~p~n",[Else]),error(badarg)
            end;
       [ Entry ]->
            case string:tokens(Entry,"-") of
                [From,To] ->
                    {range,From,To};
                ["*"] -> any;
                [Entry] -> {exact,Entry}
            end
    end.

prio_column_entries(Entries) ->
    Sorter = fun 
        (any,{_Any,StepAny,Exact}) -> {[any],StepAny,Exact};
        ({stepany,_Step}=R,{Any,StepAny,Exact}) -> {Any,StepAny++[R],Exact};
        ({exact,_V}=R,{Any,StepAny,Exact}) -> {Any,StepAny,[R|Exact]};
        (_,_) -> error(badarg)
    end,
    {Any,StepAny,Exact} = lists:foldl(Sorter,{[],[],[]},Entries),
    case length(Any) >0 of
        true-> [any];
        false ->
            Exact1 = lists:sort(fun({exact,N1},{exact,N2}) -> N1<N2 end,Exact),
            Exact2 = lists:ukeysort(2,Exact1),
            StepAny ++ Exact2
    end.

decode_val(minute,Val) ->
        case list_to_integer(Val) of
            Val1 when Val1 >=0, Val1 =< 59 -> Val1;
            _Else -> error(badarg)
        end;

decode_val(hour,Val) ->
        case list_to_integer(Val) of
            Val1 when Val1 >=0, Val1 =< 23 -> Val1;
            _Else -> error(badarg)
        end;

decode_val(dom,Val) ->
        case list_to_integer(Val) of
            Val1 when Val1 >=1, Val1 =< 31 -> Val1;
            _Else -> error(badarg)
        end;

decode_val(month,Val) ->
        case list_to_integer(Val) of
            Val1 when Val1 >=1, Val1 =< 12 -> Val1;
            _Else -> error(badarg)
        end;

decode_val(dow,"Mon") -> 1;
decode_val(dow,"Tue") -> 2;
decode_val(dow,"Wed") -> 3;
decode_val(dow,"Thu") -> 4;
decode_val(dow,"Fri") -> 5;
decode_val(dow,"Sat") -> 6;
decode_val(dow,"Sun") -> 7;

decode_val(dow,Val) ->
        case list_to_integer(Val) of
            Val1 when Val1 >=1, Val1 =< 7 -> Val1;
            Val1 when Val1 == 0 -> 7;
            _Else -> error(badarg)
        end;

decode_val(Type,Val) ->
    io:format("Unhandled val Type:~p, Val:~p~n",[Type,Val]),
    Val.


