* Encoding: UTF-8.
*Filter to college age.
USE ALL.
COMPUTE filter_$=(Age >16 and Age <24).
VARIABLE LABELS filter_$ 'Age >16 and Age <24 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*Use of apps.
FREQUENCIES Variables = Use_FB
Use_Twit
Use_Inst
Use_YT
Use_Red
Use_Snap
Use_FBM
Use_WA
Use_Tumblr
Use_Gplus
Use_Pint
Use_AS
Use_HP
Use_Music.
execute.

*Promote causes on apps.
Frequencies Variables=
Promote_Inst
Promote_Snap
Promote_WA
Promote_Music
Promote_AS
Promote_HP
Promote_FB
Promote_Twit
Promote_YT
Promote_Red
Promote_FBM
Promote_Tumblr
Promote_Gplus
Promote_Pint.
execute.

*Which story do you prefer?.
Frequencies variables = Stories.
execute.

*Preferred platform.
Frequencies variables = Share_Platform.
execute.
