
-record(flv_header,
        {
          signature            :: binary(), % always be <<"FLV">>
          version              :: 0..255,
          type_flags_reserved1 :: 0..31, % must be 0
          type_flags_audio     :: 0..1,
          type_flags_reserved2 :: 0..1,  % must be 0
          type_flags_video     :: 0..1,
          data_offset          :: 0..16#FFFFFFFF
        }).

-record(flv_tag_video,
        {
          frame_type = 0 :: non_neg_integer(),
          codec_id = 0 :: non_neg_integer(),
          avc_packet_type = 0 :: non_neg_integer(),
          composition_time = 0 :: integer(),
          payload = <<"">> :: binary()
        }).

-record(flv_tag_audio,
        {
          format = 0 :: non_neg_integer(),
          rate = 0 :: non_neg_integer(),
          size = 0 :: non_neg_integer(),
          type = 0 :: non_neg_integer(),
          aac_packet_type = 0 :: non_neg_integer(),
          payload = <<"">> :: binary()
        }).

-record(flv_tag_script_data,
        {
          object :: [amf:amf_object()]
        }).

-record(flv_tag_unknown,
        {
          type :: 0..16#FF,
          data :: binary()
        }).

-record(flv_tag,
        {
          timestamp :: -16#8000000..16#7FFFFFFF,
          stream_id :: 0..16#FFFFFF, % always 0
          data      :: #flv_tag_video{} | #flv_tag_audio{} | #flv_tag_script_data{} | #flv_tag_unknown{}
        }).

-record(flv_body_entry,
        {
          tag               :: #flv_tag{},
          previous_tag_size :: 0..16#FFFFFFFF
        }).
