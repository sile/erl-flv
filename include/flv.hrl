
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
