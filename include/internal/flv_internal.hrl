
%% AUDIO
-define(SOUND_FORMAT_LINEAR_PCM_NATIVE,           0).
-define(SOUND_FORMAT_ADPCM,                       1).
-define(SOUND_FORMAT_MP3,                         2).
-define(SOUND_FORMAT_LINEAR_PCM_LITTLE,           3).
-define(SOUND_FORMAT_NELLYMOSER_16_MONO,          4).
-define(SOUND_FORMAT_NELLYMOSER_8_MONO,           5).
-define(SOUND_FORMAT_NELLYMOSER,                  6).
-define(SOUND_FORMAT_G711_A_LOW_LOGARITHMIC_PCM,  7).
-define(SOUND_FORMAT_G711_MU_LOW_LOGARITHMIC_PCM, 8).
-define(SOUND_FORMAT_AAC,                         10).
-define(SOUND_FORMAT_SPEEX,                       11).
-define(SOUND_FORMAT_MP3_8,                       14).
-define(SOUND_FORMAT_DEVIEC_SPECIFIC,             15).

-define(SOUND_RATE_5500HZ, 0).
-define(SOUND_RATE_11000HZ, 1).
-define(SOUND_RATE_22000HZ, 2).
-define(SOUND_RATE_44000HZ, 3).

-define(SOUND_SIZE_8BIT, 0).
-define(SOUND_SIZE_16BIT, 1).

-define(SOUND_TYPE_MONO, 0).
-define(SOUND_TYPE_STEREO, 1).

-define(AAC_PACKET_TYPE_SEQUENCE_HEADER, 0).
-define(AAC_PACKET_TYPE_RAW, 1).

%% VIDEO
-define(FRAME_TYPE_KEY, 1).
-define(FRAME_TYPE_INTER, 2).
-define(FRAME_TYPE_DISPOSABLE_INTER, 3).
-define(FRAME_TYPE_GENERATED_KEY, 4).
-define(FRAME_TYPE_INFO_OR_COMMAND, 5).

-define(CODEC_ID_SORENSON_H263, 2).
-define(CODEC_ID_SCREEN_VIDEO, 3).
-define(CODEC_ID_ON2_VP6, 4).
-define(CODEC_ID_ON2_VP6_WITH_ALPHA_CHANNEL, 5).
-define(CODEC_ID_SCREEN_VIDEO_VERSION2, 6).
-define(CODEC_ID_AVC, 7).

-define(AVC_PACKET_TYPE_SEQUENCE_HEADER, 0).
-define(AVC_PACKET_TYPE_NALU, 1).
-define(AVC_PACKET_TYPE_EOS, 2).
