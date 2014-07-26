-module(paris_plugin).
  
-export([run/1, help/0]).
-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

run(["install", URL|Params]) ->
  case paris_app:is_app() of
    {true, [AppName]} ->
      ?CONSOLE("* Fetch plugin at ~s", [URL]),
      ParisCache = filename:join([os:getenv("HOME"), ".paris", ".plugin.cache"]),
      case paris_utils:make_dir(ParisCache) of
        {error, _} -> ?CONSOLE("[E] Can't create ~s", [ParisCache]);
        ok ->
          case git:clone(URL, ParisCache) of
            {ok, _} -> 
              case install_plugin(URL, AppName, ParisCache, Params) of
                {ok, Name} -> ?CONSOLE("* Plugin ~s installed!", [Name]);
                {error, Message} -> ?CONSOLE("[E] ~s", [Message])
              end,
              paris_utils:remove_recursive(ParisCache);
            _ -> ?CONSOLE("[E] Can't retrieve data", [])
          end
      end;
    {true, _} -> ?CONSOLE("[E] Can't install plugin in a multi application!", []);
    false -> ?CONSOLE("[E] Not a paris application!", [])
  end;
run(["remove", Name|_]) ->
  ?CONSOLE("* Remove plugin ~s", [Name]);
run(["list"|_]) ->
  case get_plugin_list() of
    {ok, []} -> ?CONSOLE("* No plugin installed", []);
    {ok, List} -> ?CONSOLE("* Installed plugins: ~p", [List]);
    {error, Message} -> ?CONSOLE("[E] ~s", [Message])
  end;
run(_) -> help().

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s plugin [action] [options]", [paris:get_script_name()]).

get_plugin_list() ->
  case paris_app:is_app() of
    {true, [AppName]} ->
      paris_config:plugins_list(AppName);
    {true, _} -> {error, "Multi application!"};
    false -> {error, "Not a paris application!"}
  end.

install_plugin(URL, AppName, ParisCache, Params) ->
  Version = paris_params:value("--version", "master", Params),
  case git:checkout(ParisCache, Version) of
    {ok, []} -> 
      InstallFile = filename:join([ParisCache, "install.paris"]),
      case filelib:is_regular(InstallFile) of
        true ->
          case erlconf:open('install.plugin', InstallFile, [{save_on_close, false}]) of
            {ok, PID} ->
              Term = erlconf:term(PID),
              Result = case lists:keyfind(name, 1, Term) of
                {name, PluginName} -> 
                  case update_config(PluginName, AppName) of
                    {error, Error} -> {error, Error};
                    _ -> case update_rebar(PluginName, URL, Version, Term) of
                        {error, Error} -> {error, Error};
                        _ -> update_vm_args(PluginName, Term)
                      end
                  end;
                _ -> {error, "Can't find plugin name!"}
              end,
              erlconf:close(PID),
              Result;
            _ -> {error, "Can't open install file!"}
          end;
        _ -> {error, "Not a regular plugin"}
      end;
    _ -> {error, "Can't find version " ++ Version}
  end.
 
update_config(PluginName, AppName) ->
  case paris_config:plugins_add(AppName, PluginName) of
    ok -> {ok, PluginName};
    E -> E
  end.

update_rebar(PluginName, URL, Version, Term) ->
  GitVersion = case Version of
    "master" -> "master";
    V -> {tag, V}
  end,
  case erlconf:open(rebar, "rebar.config") of
    {ok, PID} -> 
      RebarTerm = erlconf:term(PID),
      Deps = case lists:keyfind(deps, 1, RebarTerm) of
        {deps, D} -> D;
        _ -> []
      end ++ [{PluginName, ".*", {git, URL, GitVersion}}],
      NewRebarTerm = lists:keystore(deps, 1, RebarTerm, {deps, Deps}) ++
                     case lists:keyfind(rebar, 1, Term) of
                       {rebar, Rebar} -> 
                         case lists:keyfind(add, 1, Rebar) of
                           {add, List} -> List;
                           _ -> []
                         end;
                       _ -> []
                     end,
      erlconf:term(PID, NewRebarTerm),
      case erlconf:close(PID) of
        close -> {ok, PluginName};
        {error, _} -> {ok, "Faild to update rebar configuration"}
      end;
    _ -> {error, "Can't open rebar configuration file"}
  end.

update_vm_args(PluginName, Term) ->
  case lists:keyfind(mv_args, 1, Term) of
    {mv_args, VMArgs} -> 
      case lists:keyfind(add, 1, VMArgs) of
        {add, Args} -> 
          VMArgsFile = filename:join(["config", "vm.args"]),
          case file:open(VMArgsFile, [append]) of
            {ok, IO} -> 
              UpdateResult = lists:foldl(fun(Arg, Acc) ->
                    case file:write(IO, io_lib:format("~s~n", [Arg])) of
                      ok -> Acc;
                      {error, _} -> {error, "Error updating vm.args"}
                    end
                end, {ok, PluginName}, Args),
              case file:close(IO) of
                {error, _} -> {error, "Error updating vm.args"};
                _ -> UpdateResult
              end;
            {error, _} -> {error, "Can't update mv.args file!"}
          end;
        _ -> {ok, PluginName}
      end;
    _ -> {ok, PluginName}
  end.
