%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Flv Body Decodeing/Encoding Functions
-module(flv_body).

-include("../include/flv.hrl").

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         decode/1,
         encode/1
        ]).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec decode(binary()) -> {#flv_body_entry{}, Rest::binary()}.
decode(<<Bin/binary>>) ->
    {Tag, <<Size:32, Rest/binary>>} = flv_tag:decode(Bin),
    Entry = #flv_body_entry{
               tag               = Tag,
               previous_tag_size = Size
              },
    {Entry, Rest}.

-spec encode(#flv_body_entry{}) -> iodata().
encode(Entry) ->
    #flv_body_entry{tag = Tag, previous_tag_size = Size} = Entry,
    [flv_tag:encode(Tag), <<Size:32>>].
