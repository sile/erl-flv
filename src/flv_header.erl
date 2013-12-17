%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Flv Header Decodeing/Encoding Functions
-module(flv_header).

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
-spec decode(binary()) -> {#flv_header{}, Rest::binary()}.
decode(<<"FLV", Version:8, Reserved1:5, AudioFlag:1, Reserved2:1, VideoFlag:1, DataOffset:32, Rest/binary>>) ->
    Header = #flv_header{
                signature            = <<"FLV">>,
                version              = Version,
                type_flags_reserved1 = Reserved1,
                type_flags_audio     = AudioFlag,
                type_flags_reserved2 = Reserved2,
                type_flags_video     = VideoFlag,
                data_offset          = DataOffset
               },
    {Header, Rest}.

-spec encode(#flv_header{}) -> iodata().
encode(Header) ->
    #flv_header{
       signature            = Signature,
       version              = Version,
       type_flags_reserved1 = Reserved1,
       type_flags_audio     = AudioFlag,
       type_flags_reserved2 = Reserved2,
       type_flags_video     = VideoFlag,
       data_offset          = DataOffset
      } = Header,
    <<Signature/binary, Version:8, Reserved1:5, AudioFlag:1, Reserved2:1, VideoFlag:1, DataOffset:32>>.
