% This file is part of tchains library.
%
%  Copyright (C) 2012-2013 Mikhail Sobolev <mss@mawhrin.net>
%  Copyright (C) 2012-2013 Naxuat Oy <contact@naxuat.com>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.

-module(tchains).

-export([
    exec/3
]).

exec(Tasks, Value, Params) when is_list(Tasks) ->
    {ok, Engine} = tchains_engine:new(Params),
    tchains_engine:exec_catch(Tasks, Value, Engine).
