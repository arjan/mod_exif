%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-09

%% @doc Extract EXIF information from pictures when uploading

%% Copyright 2011 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_exif).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Exif").
-mod_description("Extract EXIF information from pictures when uploading").
-mod_prio(900).

-include_lib("include/zotonic.hrl").

-export([
         observe_media_replace_file/2,
         exif_augment_rsc/3
        ]).


observe_media_replace_file(#media_replace_file{id=Id, medium=M}, Context) ->
    exif_augment_rsc(Id, M, Context).

exif_augment_rsc(Id, Medium, Context) ->
    FN = filename:join(
           z_path:media_archive(Context),
           proplists:get_value(filename, Medium)),
    Output = os:cmd("exiftool -j " ++ z_utils:os_escape(FN)),
    [JSON] = z_convert:convert_json(mochijson2:decode(Output)),
    TS = binary_to_list(proplists:get_value('DateTimeCreated', JSON)),
    [Date,Time] = string:tokens(TS, " "),
    ?DEBUG(Date),
    [Y,M,D] = string:tokens(Date, ":"),
    Created = z_convert:to_datetime(Y++"-"++M++"-"++D++" "++Time),
    Props = [{exif, JSON},
             {date_start, Created}],
    m_rsc:update(Id, Props, Context).
