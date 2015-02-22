-define(PRINT(Fmt, Args),  io:format("~s~n", [io_lib:format(Fmt, Args)])).
-define(INFO(Fmt, Args),  io:format("~s~n", [color:green(io_lib:format(Fmt, Args))])).
-define(DEBUG(Fmt, Args), io:format("~s~n", [color:yellow(io_lib:format(Fmt, Args))])).
-define(ERROR(Fmt, Args), io:format("~s~n", [color:red(io_lib:format(Fmt, Args))])).
-define(HALT(Fmt, Args), ?ERROR(Fmt, Args), halt(1)).

-define(GIT_TEMPLATE_URL, {"https://github.com/emedia-project/paris.app", "v2"}).
-define(TEMPLATE_DIR, "~/.rebar/templates").
-define(PARIS_DIR, "~/.paris").

-define(PV_PLUGINS,
        #{
          new => #{
            command => "new",
            module => paris_pv_plugins_new,
            depends => [],
            desc => "Generate a new paris app",
            options => [
             {force,           $F,        "force",         undefined, "Force overwriting"}, 
             {'without-texas', undefined, "without-texas", undefined, "Create application without using Texas"},
             {'with-pg',       undefined, "with-pg",       undefined, "Use PostgreSQL"},
             {'with-mysql',    undefined, "with-mysql",    undefined, "Use MySQL"},
             {'with-sqlite',   undefined, "with-sqlite",   undefined, "Use SQLite (default)"}
            ]
           },
          commands => #{
            command => "commands",
            module => paris_pv_plugins_commands,
            depends => [],
            desc => "List availables commands",
            options => []
           },
          update => #{
            command => "update",
            module => paris_pv_plugins_update,
            depends => [],
            desc => "Update paris.app",
            options => [
             {master,    $M, "master",    undefined, "Update from master branch (AT YOUR OWN RISKS)"},
             {force,     $F, "force",     undefined, "Force to reinstall the last version"},
             {templates, $t, "templates", undefined, "Update templates only"},
             {plugins,   $p, "plugins",   undefined, "Update plugins only"},
             {all,       $a, "all",       undefined, "Update templates and plugins (default)"}
            ]
           }
         }).




-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).
-define(TEMPLATES_DIR, "rebar_templates").
-define(PLUGINS_DIR, "plugins").
