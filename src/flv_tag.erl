-module(flv_tag).

-export([
         decode_audio/1, encode_audio/1,
         decode_video/1, encode_video/1
        ]).

-include("../include/flv.hrl").
-include("../include/internal/flv_internal.hrl").

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
        ?SOUND_FORMAT_AAC -> [<<Format:4, Rate:2, Size:1, Type:1>>, AACPacketType, Payload];
        _                 -> [<<Format:4, Rate:2, Size:1, Type:1>>, Payload]
    end.

%% XXX: E.4.3.2

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
        ?CODEC_ID_AVC -> [<<Frame:4, Codec:4>>, AVCPacketType, <<CompositionTime:24/signed>>, Payload];
        _             -> [<<Frame:4, Codec:4>>, Payload]
    end.

