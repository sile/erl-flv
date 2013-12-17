-module(flv).

-export([
         parse_file/1
        ]).

-export_type([
              tag_audio/0,
              tag_video/0
             ]).

-include("../include/flv.hrl").

-type tag_audio() :: #flv_tag_audio{}.
-type tag_video() :: #flv_tag_video{}.

parse_file(FlvFilePath) ->
    {ok, FlvData0} = file:read_file(FlvFilePath),
    {Header, <<_:32, FlvData1/binary>>} = flv_header:decode(FlvData0),
    Entries = parse_file_loop(FlvData1, []),
    {Header, Entries}.

parse_file_loop(<<>>, Acc) ->
    lists:reverse(Acc);
parse_file_loop(Data, Acc) ->
    {Entry, Rest} = flv_body:decode(Data),
    parse_file_loop(Rest, [Entry | Acc]).

