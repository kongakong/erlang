-module(rss_parse).

-export([is_rss2_feed/1, compare_feed_items/2]).
-export([getitem/0, test/0, test1/1, test2/1, test3/1]).

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
	namespace=nil, % try to simplify the structure
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

collect_feed_item_data(Item) when is_record(Item, xmlElement) ->
    [Guid] = xmerl_xpath:string("guid/text()", Item),
    %% io:format("Guid: ~p~n", [Guid]),
    [Title] = xmerl_xpath:string("title/text()", Item),
    %% io:format("Title: ~p~n", [Title]),
    [Link] = xmerl_xpath:string("link/text()", Item),
    %% io:format("Link: ~p~n", [Link]),
    [PubDate] = xmerl_xpath:string("pubDate/text()", Item),
    [Guid, Title, Link, PubDate].

%% assuming identity means
%% guid, title and link are the same

compare_feed_normalised_items(Guid, Title, Link, PubDate, Guid, Title, Link, PubDate) ->
    same;
compare_feed_normalised_items(Guid, _, _, _, Guid, _, _, _) ->
    updated;
compare_feed_normalised_items(_, Title, _, _, _, Title, _, _) ->
    updated;
compare_feed_normalised_items(_, _, Link, _, _, _, Link, _) ->
    updated;
compare_feed_normalised_items(_, _, _, _, _, _, _, _) ->
    different.

compare_feed_items(OldItem, NewItem) when 
          is_record(OldItem, xmlElement),
          is_record(NewItem, xmlElement)->
    [Guid1, Title1, Link1, PubDate1] = collect_feed_item_data(OldItem),
    [Guid2, Title2, Link2, PubDate2] = collect_feed_item_data(NewItem),
    compare_feed_normalised_items(Guid1, Title1, Link1, PubDate1, Guid2, Title2, Link2, PubDate2).






%% Helper function for quick testing
%% 
getitem() ->
    %% get some item to play with
    F = "digg-science-rss1.xml",
    {XML, _} = xmerl_scan:file(F),
    [X1| _] = get_feed_items(XML),
    extract_xml(X1).

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
    [compare_feed_items(X1, X2), compare_feed_items(X1, X1)].

