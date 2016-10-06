-module(ekairos_nexttime).
-export([find/2]).
-author("Ričardas Pocius cirka@cirka.lt").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-spec find(Ruleset :: tuple(), FromDateTime :: calendar:datetime()) -> 
    {next_time,calendar:datetime()} | never.
%% Given a properly formated rules and Datetime
find({YearsRulesList,
      MonthsRulesList,
      DaysRulesList,
      HoursRulesList,
      MinutesRulesList,
      SecondsRulesList},
     FromDateTime) ->
    next_time([{years_rules_list,YearsRulesList},
               {months_rules_list,MonthsRulesList},
               {days_rules_list,DaysRulesList},
               {hours_rules_list,HoursRulesList},
               {minutes_rules_list,MinutesRulesList},
               {seconds_rules_list,SecondsRulesList}],
              [], % progress stack
              FromDateTime).

%% ---- General solver -----
%% Precomputes hints for next nearest match and stores it in stack

next_time([]=_RulesListToMatch,_MathingRulesList,FromDateTime) ->
    {next_time,FromDateTime};

next_time([{ListType,Rules}=CurrentRules|RestToMatch]=ToMatch,MatchingRulesList,FromDateTime) ->
    case match_and_hint(ListType,{Rules,FromDateTime}) of
        {true,{next_hint,NextDateTime}} ->
            next_time(RestToMatch,
                      [{next_hint,NextDateTime,CurrentRules}|MatchingRulesList],
                      FromDateTime);
        {true,never} ->
            next_time(RestToMatch,[{never,Rules}|MatchingRulesList],FromDateTime);
        {false,{next_hint,NextDateTime}} ->
            next_time([CurrentRules|RestToMatch],MatchingRulesList,NextDateTime);
        {false,never} ->
                case MatchingRulesList of
                    [{never,_PreviousRules}|_Matched] -> never;
                    [{next_hint,HintedNextDateTime,PreviousRules}|Matched] ->
                        next_time([PreviousRules|ToMatch],Matched,HintedNextDateTime);
                    [] -> never
                end
    end.

% Handled time hierachy, possible to extend with milis ....
match_and_hint(years_rules_list,Arg) ->   match_and_hint_year(Arg);
match_and_hint(months_rules_list,Arg) ->  match_and_hint_month(Arg);
match_and_hint(days_rules_list,Arg) ->    match_and_hint_day(Arg);
match_and_hint(hours_rules_list,Arg) ->   match_and_hint_hour(Arg);
match_and_hint(minutes_rules_list,Arg) -> match_and_hint_minute(Arg);
match_and_hint(seconds_rules_list,Arg) -> match_and_hint_second(Arg).

%%%------Matches--------
%%--------Years-------
match_and_hint_year({Rules,{{Y,_,_},_}}) ->
   {integer_match(Rules,Y),hint_year(Rules,Y)}.

hint_year(Rules,Y) ->
    case hint_next_integer_match(Rules,Y+1,no_limit) of
        never -> never;
        NewYear -> {next_hint,{{NewYear,1,1},{0,0,0}}}
    end.

%%%------Months--------
match_and_hint_month({Rules,{{Y,M,_},_}}) ->
   {integer_match(Rules,M),hint_month(Rules,Y,M)}.

hint_month(Rules,Y,M) ->
    case hint_next_integer_match(Rules,M+1,12) of
        never -> never;
        NewMonth -> {next_hint,{{Y,NewMonth,1},{0,0,0}}}
    end.

%%%------Days--------
match_and_hint_day({Rules,{{Y,M,D},_}}) ->
    {match_day(Rules,Y,M,D),hint_day(Rules,Y,M,D)}.

match_day(Rules,{{Y,M,D},_}) ->
    match_day(Rules,Y,M,D).
%% Days will match either when:
%% a) days of month and days of week rules list does not contain Type(any) match rules and any of rules match, or
match_day({[DoMRule1|_]=DoMRules,[DoWRule1|_]=DoWRules},Y,M,D) when DoMRule1 == any; DoWRule1 == any ->
    integer_match(DoMRules,D) and integer_match(DoWRules,calendar:day_of_the_week({Y,M,D}));

%% b) either days of month or days of week rules match contain Type(any) rule and both rules lists match
match_day({DoMRules,DoWRules},Y,M,D) ->
    integer_match(DoMRules,D) or integer_match(DoWRules,calendar:day_of_the_week({Y,M,D})).

%%Simple incrementing hinter    
hint_day(Rules,Y,M,D) ->
    hint_day(Rules,Y,M,D+1,calendar:last_day_of_the_month(Y,M)).
    
hint_day(_Rules,_Y,_M,D,MaxDay) when D > MaxDay -> never;

hint_day(Rules,Y,M,D,MaxDay) ->
    case match_day(Rules,NewDateTime = {{Y,M,D},{0,0,0}}) of
        true ->
            {next_hint,NewDateTime};
        false -> hint_day(Rules,Y,M,D+1,MaxDay)
    end.

%%%---- Hours ----
match_and_hint_hour({Rules,{{Y,M,D},{HH,_,_}}}) ->
   {integer_match(Rules,HH),hint_hour(Rules,Y,M,D,HH)}.

hint_hour(Rules,Y,M,D,HH) ->
    case hint_next_integer_match(Rules,HH+1,23) of
        never -> never;
        NewHour -> {next_hint,{{Y,M,D},{NewHour,0,0}}}
    end.

%%---- Minutes ----
match_and_hint_minute({Rules,{{Y,M,D},{HH,MM,_}}}) ->
   {integer_match(Rules,MM),hint_minute(Rules,Y,M,D,HH,MM)}.

hint_minute(Rules,Y,M,D,HH,MM) ->
    case hint_next_integer_match(Rules,MM+1,59) of
        never -> never;
        NewMinute -> {next_hint,{{Y,M,D},{HH,NewMinute,0}}}
    end.

%%---- Seconds ----
match_and_hint_second({Rules,{{Y,M,D},{HH,MM,SS}}}) ->
   {integer_match(Rules,SS),hint_second(Rules,Y,M,D,HH,MM,SS)}.

hint_second(Rules,Y,M,D,HH,MM,SS) ->
    case hint_next_integer_match(Rules,SS+1,59) of
        never -> never;
        NewSecond -> {next_hint,{{Y,M,D},{HH,MM,NewSecond}}}
    end.

%---- Hinter ------
%% Find when will rule match in the future
hint_next_integer_match([any|_Rules],Current,no_limits) -> 
    Current + 1;
hint_next_integer_match([any|_Rules],Current,NoMore) when Current + 1 =< NoMore ->
    Current + 1;
hint_next_integer_match([any|_Rules],_Current,_NoMore) ->
    never;
hint_next_integer_match(Rules,Current,NoMore) ->
    hint_next_integer_match(hinting_sort(Rules),Current,never,NoMore).

%% hint_next_integer_match(RulesToProcess,AllRules,SeenMinimum,AllowedMax)
hint_next_integer_match([],_,never,_) -> 
    never;
hint_next_integer_match([],_,Next,_) ->
    Next;
hint_next_integer_match([{exact,NextTime}|Rules],Current,SeenMin,NoMore) ->
    case (NextTime >= Current) and (NextTime =< NoMore) of
        true -> hint_next_integer_match(Rules,Current,min(NextTime,SeenMin),NoMore);
        false -> hint_next_integer_match(Rules,Current,SeenMin,NoMore)
    end;
hint_next_integer_match([{stepany,_Step}|_]=AllRules,Current,SeenMin,NoMore) ->
    hint_next_integer_match_in_steps(AllRules,AllRules,Current,SeenMin,NoMore).

hint_next_integer_match_in_steps(_Rules,_AllRules,Current,SeenMin,_NoMore) when Current == SeenMin ->
    Current; % Current counted till Minimal exact rule
hint_next_integer_match_in_steps(_Rules,_AllRules,Current,_SeenMin,NoMore) when Current > NoMore ->
    never; % Reached increment limit of NoMore 

hint_next_integer_match_in_steps([],AllRules,Current,SeenMin,NoMore)  -> % All stepany rules exhausted, step a year ahead and try again 
   hint_next_integer_match_in_steps(AllRules,AllRules,Current+1,SeenMin,NoMore);

hint_next_integer_match_in_steps([{stepany,Step}|Rules],AllRules,Current,SeenMin,NoMore) when Step =/= 1-> %Bad input !!!
    case Current rem Step == 1 of
        true ->  hint_next_integer_match_in_steps(Rules,AllRules,Current,min(Current,SeenMin),NoMore);
        false -> hint_next_integer_match_in_steps(Rules,AllRules,Current,SeenMin,NoMore)
    end;

hint_next_integer_match_in_steps([{stepany,_Step}|_Rules],_AllRules,Current,SeenMin,_NoMore) -> 
% In case someone will call with {stepany,1} which basically means [any]
       min(Current,SeenMin).

%%---- Reusable matcher ----
integer_match([],_) -> false;
integer_match([Rule|Rules],ToMatch) ->
    case Rule of
        any -> true;
        {stepany,1} ->
            true;
        {stepany,Step} when ToMatch rem Step == 1 ->
            true;
        {exact,ToMatch} ->
            true;
        _False -> 
            integer_match(Rules,ToMatch)
    end.

%%---- Utils ------
hinting_sort(Entries) ->
    Sorter = fun 
        ({stepany,_Step}=Rule,{Exact,StepAny}) -> 
            {Exact,[Rule|StepAny]};
        ({exact,_V}=Rule,{Exact,StepAny}) -> 
            {[Rule|Exact],StepAny};
        (_,_) -> error(badarg)
    end,
    {Exact,StepAny} = lists:foldl(Sorter,{[],[]},Entries),
    Exact1 = lists:reverse(Exact),
    Exact1 ++ StepAny.

