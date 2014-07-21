# Paris.app

## Plugins

You can create your own plugins for Paris.app.

A plugin is a simple module. If you want to create a plugin to expose a new command, your plugin must be named `plugin_<command>` and, at least, export two function :

* `run/1` : this function is called when you invoke the command. It receive a list of parameters.
* `help/0` : this function is display the command help.

If you want to create a generator, your plugin must be named `plugin_generator_<generator>` and, at least, export one function :

* `generate/1` : this function is called when you invoke the generator. It receive a list of parameters.

> In the `plugins` directory, you will find some examples. The plugin `paris_generator_plugin.erl` is a plugin that's help you to generate plugins. Copy the plugin file in `~/.paris/plugins` then run
>
>     ./paris generate plugin --help

## Licences

Paris.app is available for use under the following license, commonly known
as the 3-clause (or "modified") BSD license:

Copyright (c) 2014 Gregoire Lejeune [gregoire.lejeune@free.fr]

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
