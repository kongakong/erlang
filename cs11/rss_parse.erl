-module(rss_parse).

-export([is_rss2_feed/1, compare_feed_items/2]).
-export([test/0, test1/1, test2/1, test3/1]).

%% -include does not work for library
%% otherwise it will give 'cannot find file' error
%% -include("xmerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xpath, [string/2]).
-import(lists, [map/2]).

%% 2.
%%
%% caller will use this in this manner
%% > {XML, RemainingText} = xmerl_scan:file(Filename).
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

%% 5
%% 
% @private
% @doc Given an XML element of some kind, this helper function will go through
%      and remove any details about other XML elements, for example in the
%      "parents" or "pos" fields.
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[], pos=0,
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
    Other.

compare_feed_items(OldItem, NewItem) ->
    OldItem2 = extract_xml(OldItem),
    NewItem2 = extract_xml(NewItem),
    io:format("LHS: ~p~n", [OldItem2]),
    io:format("RHS: ~p~n", [NewItem2]),
    compare_feed_normalised_items(OldItem2, NewItem2).

compare_feed_normalised_items(OldItem, NewItem) 
      when OldItem#xmlElement.name =:= NewItem#xmlElement.name -> 
	      same;
compare_feed_normalised_items(OldItem, NewItem) ->
    updated;
compare_feed_normalised_items(OldItem, NewItem) ->
    different.




%% Helper function for quick testing
%% 
test() ->
    test3("digg-science-rss1.xml").

test1(F) ->
    {XML, _} = xmerl_scan:file(F),
    is_rss2_feed(XML).

test2(F) ->
    {XML, _} = xmerl_scan:file(F),
    % get_feed_items(XML).
    map(fun get_item_time/1, get_feed_items(XML)).

test3(F) ->
    {XML, _} = xmerl_scan:file(F),
    [X1, X2| _] = get_feed_items(XML),
    compare_feed_items(X1, X2).

