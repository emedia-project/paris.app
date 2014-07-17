-module(paris_app).

-export([create/2]).
-include("paris.hrl").

create(AppName, Params) ->
  AppParams = paris_rebar:build_params(Params, [
        {name, AppName}, 
        {copyright_year, "2014"},
        {author_name, "YOUR_NAME"},
        {author_email,"YOUR_MAIL"},
        {description, "A project build with Paris"}
        ]),
  ?CONSOLE("Create app ~s...", [AppName]),
  paris_rebar:run(["create", "template=paris"] ++ AppParams).

