-module(rss_parse).

-export([is_rss2_feed/1]).
-export([test/0, test1/1, test2/1]).

%% -include does not work for library
%% otherwise it will give 'cannot find file' error
%% -include("xmerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xpath, [string/2]).
-import(lists, [map/2]).

%% 2.
%%
%% caller will use this in this manner
%% {XML, RemainingText} = xmerl_scan:file(Filename).
%%
%% > is_rss2_feed(XML).
%% [#xmlText{parents = [{rss,1}],
%%           pos = 1,language = [],value = "\n ",type = text},
%%  #xmlText{parents = [{rss,1}],
%%           pos = 3,language = [],value = "\n",type = text}]
%%
is_rss2_feed(XML) ->
    Ver = xmerl_xpath:string("/rss[@version='2.0']/text()", XML),
    length(Ver) > 0.
    
%% 3.
get_feed_items(RSS2Feed) ->
    xmerl_xpath:string("//channel/item", RSS2Feed).

%% 4.
get_item_time(Item) ->
    [T] = xmerl_xpath:string("/item/pubDate/text()", Item),
    %% io:format("~p~n", [T#xmlText.value]),
    ND = httpd_util:convert_request_date(T#xmlText.value),
    calendar:datetime_to_gregorian_seconds(ND).

%% Helper function for quick testing
%% 
test() ->
    test2("digg-science-rss1.xml").

test1(F) ->
    {XML, _} = xmerl_scan:file(F),
    is_rss2_feed(XML).

test2(F) ->
    F = "digg-science-rss1.xml",
    {XML, _} = xmerl_scan:file(F),
    % get_feed_items(XML).
    map(fun get_item_time/1, get_feed_items(XML)).

