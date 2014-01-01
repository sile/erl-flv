-module(flv_tag).

-export([
         decode/1, encode/1,
         decode_audio/1, encode_audio/1,
         decode_video/1, encode_video/1,
         decode_script_data/1, encode_script_data/1,
         is_audio_sequence_header_bytes/1,
         is_video_sequence_header_bytes/1
        ]).

-include("../include/flv.hrl").
-include("../include/internal/flv_internal.hrl").

-spec decode(binary()) -> {#flv_tag{}, Rest::binary()}.
decode(<<TagType:8, DataSize:24, TimestampBase:24, TimestampExtended:8, StreamId:24, Data0/binary>>) ->
    <<Data:DataSize/binary, Rest/binary>> = Data0,
    TagData = case TagType of
                  8  -> decode_audio(Data);
                  9  -> decode_video(Data);
                  18 -> decode_script_data(Data);
                  _  -> #flv_tag_unknown{data = Data}
              end,
    <<Timestamp:32/signed>> = <<TimestampExtended:8, TimestampBase:24>>,
    Tag = #flv_tag{
             timestamp          = Timestamp,
             stream_id          = StreamId,
             data               = TagData
            },
    {Tag, Rest}.

-spec encode(#flv_tag{}) -> iodata().
encode(Tag) ->
    #flv_tag{
       timestamp          = Timestamp,
       stream_id          = StreamId,
       data               = TagData
      } = Tag,
    <<TimestampExtended:8, TimestampBase:24>> = <<Timestamp:32/signed>>,
    {TagType, Data} =
        case TagData of
            #flv_tag_audio{} -> {8, encode_audio(TagData)};
            #flv_tag_video{} -> {9, encode_video(TagData)};
            #flv_tag_script_data{} -> {18, encode_script_data(TagData)};
            _  -> {TagData#flv_tag_unknown.type, TagData#flv_tag_unknown.data}
        end,
    DataSize = byte_size(Data),
    <<TagType:8, DataSize:24, TimestampBase:24, TimestampExtended:8, StreamId:24, Data/binary>>.

is_audio_sequence_header_bytes(<<?SOUND_FORMAT_AAC:4, _:4, 0:8, _/binary>>) -> true;
is_audio_sequence_header_bytes(_)                                           -> false.

decode_audio(<<?SOUND_FORMAT_AAC:4, Rate:2, Size:1, Type:1, AACPacketType:8, Payload/binary>>) ->
    #flv_tag_audio
    {
      format = ?SOUND_FORMAT_AAC,
      rate   = Rate,
      size   = Size,
      type   = Type,
      aac_packet_type = AACPacketType,
      payload = Payload
    };
decode_audio(<<Format:4, Rate:2, Size:1, Type:1, Payload/binary>>) when Format =/= ?SOUND_FORMAT_AAC ->
    #flv_tag_audio
    {
      format = ?SOUND_FORMAT_AAC,
      rate   = Rate,
      size   = Size,
      type   = Type,
      payload = Payload
    }.

encode_audio(Audio) ->
    #flv_tag_audio{format=Format, rate=Rate, size=Size, type=Type, aac_packet_type=AACPacketType, payload=Payload} = Audio,
    case Format of
        ?SOUND_FORMAT_AAC -> [<<Format:4, Rate:2, Size:1, Type:1, AACPacketType>>, Payload];
        _                 -> [<<Format:4, Rate:2, Size:1, Type:1>>, Payload]
    end.

%% XXX: E.4.3.2

is_video_sequence_header_bytes(<<1:4, ?CODEC_ID_AVC:4, 0:8, _/binary>>) -> true;
is_video_sequence_header_bytes(_)                                       -> false.

decode_video(<<?FRAME_TYPE_INFO_OR_COMMAND:4, Codec:4, Payload/binary>>) ->
    #flv_tag_video
    {
      frame_type = ?FRAME_TYPE_INFO_OR_COMMAND,
      codec_id   = Codec,
      payload    = Payload
    };
decode_video(<<Frame:4, ?CODEC_ID_AVC:4, AVCPacketType:8, CompositionTime:24/signed, Payload/binary>>) ->
    #flv_tag_video
    {
      frame_type       = Frame,
      codec_id         = ?CODEC_ID_AVC,
      avc_packet_type  = AVCPacketType,
      composition_time = CompositionTime,
      payload          = Payload
    };
decode_video(<<Frame:4, Codec:4, Payload/binary>>) ->
    #flv_tag_video
    {
      frame_type = Frame,
      codec_id   = Codec,
      payload    = Payload
    }.

encode_video(Video) ->    
    #flv_tag_video{frame_type=Frame, codec_id=Codec, 
                   avc_packet_type=AVCPacketType, composition_time=CompositionTime,
                   payload=Payload} = Video,
    case Codec of
        ?CODEC_ID_AVC -> [<<Frame:4, Codec:4, AVCPacketType, CompositionTime:24/signed>>, Payload];
        _             -> [<<Frame:4, Codec:4>>, Payload]
    end.

decode_script_data(<<Bin/binary>>) ->
    Objects = decode_amf_objects(Bin, []),
    #flv_tag_script_data{object = Objects}.

decode_amf_objects(<<>>, Acc) ->
    lists:reverse(Acc);
decode_amf_objects(<<Bin/binary>>, Acc) ->
    {ok, Object, Rest} = amf:decode(amf0, Bin),
    decode_amf_objects(Rest, [Object | Acc]).

encode_script_data(Data) ->
    #flv_tag_script_data{object = Objects} = Data,
    list_to_binary([amf:encode(amf0, Object) || Object <- Objects]).
