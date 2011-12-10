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
         exif_augment_rsc/3,
         event/2
        ]).


observe_media_replace_file(#media_replace_file{id=Id, medium=M}, Context) ->
    exif_augment_rsc(Id, M, Context).

exif_augment_rsc(Id, Medium, Context) ->
    FN = filename:join(
           z_path:media_archive(Context),
           proplists:get_value(filename, Medium)),
    Output = os:cmd("exiftool -j " ++ z_utils:os_escape(FN)),
    [JSON] = z_convert:convert_json(mochijson2:decode(Output)),
    CreatedProp = case proplists:get_value('CreateDate', JSON) of
                      undefined -> [];
                      B when is_binary(B) ->
                          [Date,Time] = string:tokens(binary_to_list(B), " "),
                          [Y,M,D] = string:tokens(Date, ":"),
                          [{date_start, z_convert:to_datetime(Y++"-"++M++"-"++D++" "++Time)}]
                  end,
    Props = [{exif, JSON}] ++ CreatedProp,
    m_rsc:update(Id, Props, Context),
    ok.


event({postback, {rescan_exif, []}, _, _}, Context) ->
    F = fun() ->
                All = z_search:query_([{cat, image}], Context),
                [catch exif_augment_rsc(Id, m_media:get(Id, Context), Context) || Id <- All],
                ok
        end,
    spawn(F),
    z_render:growl(?__("All images are being rescanned for EXIF info. This can take a while.", Context), Context).

