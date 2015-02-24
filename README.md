# Paris.app

Paris.app is a generator to help you to create [paris](https://github.com/emedia-project/paris) applications. 

## Install

Download [paris](https://github.com/emedia-project/paris.app/wiki/paris), make it executable and place it in your `PATH`.

Then, run `paris update` to install the needed templates.

Finaly, you can create you Paris' application squeleton by running :

```sh
paris new my_app
```

## Plugins

You can create your own plugins for Paris.app.

To do so, run

```sh
paris generate plugin <your plugin name>
```

This will generate two files :
* `~/.paris/plugins/paris_plugins_<your plugin name>.erl` this file contains your plugin code.
* `~/.paris/plugins/paris_plugins_<your plugin name>.pp` this file contains informations about your plugin.

You can find plugin examples in the [plugins directory](https://github.com/emedia-project/paris.app/tree/master/plugins).

## Generator

If you want to create your own generator, run:

```sh
paris generate generator <you generator name>
```

This will generate the file ̀`~/.paris/plugins/paris_generator_<your generator name>.erl` containing the code of your generator.

You can find generator examples in the [plugins directory](https://github.com/emedia-project/paris.app/tree/master/plugins).

## Licences

Paris.app is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014, 2015 Gregoire Lejeune [gregoire.lejeune@free.fr]

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
