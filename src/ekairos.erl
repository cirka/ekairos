-module(ekairos).
-export([parse_crontab_line/1,
         find_next_time/2]).

-type step() :: pos_integer().
-type rule() ::  any | {stepany,step()} | {exact, non_neg_integer()}.
-type rules_list() :: [rule()].
-type year_rules()  :: rules_list().
-type month_rules() :: rules_list().
-type day_of_month_rules() :: rules_list().
-type day_of_week_rules() :: rules_list().
-type day_rules() :: {day_of_month_rules(),day_of_week_rules()}.
-type hour_rules()  :: rules_list().
-type minute_rules() :: rules_list().
-type second_rules() :: rules_list().
-type matchspec() :: {year_rules(),month_rules(),day_rules(),hour_rules(),minute_rules(),second_rules()}.

-spec parse_crontab_line(Line :: nonempty_string()) -> matchspec().
parse_crontab_line(Line) -> ekairos_crontab:parse_line(Line).

-spec find_next_time(MatchSpec :: matchspec(),calendar:datetime()) -> calendar:datetime() | never.
find_next_time(Matchspec,DateTime) -> ekairos_nexttime:find(Matchspec,DateTime).
 

