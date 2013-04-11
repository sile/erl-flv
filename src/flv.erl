-module(flv).

-export_type([
              tag_audio/0,
              tag_video/0
             ]).

-include("../include/flv.hrl").

-type tag_audio() :: #flv_tag_audio{}.
-type tag_video() :: #flv_tag_video{}.
