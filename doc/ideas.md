I have done some experiments on this. Random thoughts:

## Questions

* Is plugin writing synchronous or asynchronous programming? Or could it be both?

## Ideas to Test

* Put a .beam file somewhere
* Put a .erl file somewhere and Couch compiles it

## Minimal Ceremony

After a hiatus, I've gotten back into Erlang projects again. The obvious, glaring problem is the ceremony.

Goals

1. Suitable as a first Erlang project
2. Hello world in 60 seconds
3. A CouchDB plugin is a plain old .erl file
3. Making a CouchDB plugin does not require the CouchDB source code

## Plugin ideas

Auto refresh views upon doc update
Send an email
Authentication
 * No more org.couchdb.user:*
 * Just general other authentication, LDAP, PAM, whatever

## couch_plugin branch

Why is the plugin_dir user-serviceable? Why not somewhere in priv/ and the admin interface
adds and removes files to that location?

Also let's talk about the minimum viable product. I think the Futon interface is not minimum. To merge this to master, an HTTP interface should be sufficient.

What is this idea of requiring plugins to come from apache.org? It seems restrictive and out of scope for an MVP. Was it removed from the roadmap?

I am not convinced about the installation process. Why can't I simply upload an Erlang release into CouchDB?

## Current idea

How to completely streamline the CouchDB plugin story? How to bring in developers as easily as possible? My idea:

1. We bundle the Rebar source in the CouchDB source
2. We build rebar with couch_plugin extensions
3. You fetch rebar over http
4. You run `./rebar create couchdb_plugin` and you get a working, no-op plugin.
6. It is a standard Erlang project. `rebar clean`, `rebar get-deps`, `rebar compile`, etc. CouchDB source *not* needed.
7. There is a `rebar publish` where you can send it back to CouchDB.

To an Erlang or CouchDB developer, the plugin looks like this:

* A standard application tree: app config, supervisor, gen_server
* A `rebar.config` to get you started
* An `include/couchdb.hrl` to work with Couch's records (request, db, user_ctx, etc.)
* A dedicated .ini file for default config values. The .ini also starts the plugin supervisor under couch_secondary_services.
* A priv/ directory for general data files
* An Erlang appliction config file in case your dependencies use `application:get_env()` (e.g. estatsd, zeta, lager, etc.)

To a novice Erlang developer, the plugin looks like this:

* Basically every file you open says *Plugin authors NEED NOT edit this file*
* There is one plain Erlang module, (no behaviours, just a standard module) called `your_plugin_name.erl` it has a very JavaScript-friendly style, `on(Event) -> do_stuff().`

With the "easy" style, you say what event to handle, and write the code to do that. It is suitable for typical plugin tasks: logging, send an email when somebody signs up, stuff like that.

I want to convert all of the Iris changes to plugins. That is a plausible test of the minimum viable product. For my workflow (typical of sysadmins) I want to build plugins when I build CouchDB, and install plugins when I install CouchDB. If I do change plugins, which is rare, I am willing to restart CouchDB. Sure, there is Erlang code updating and maybe it is easy; but for the MVP can restart.

## This plugin itself

There is the question of keeping state in this module. How to balance simplicity
with the fact that there is no way to keep state at all? I am thinking use the process
dictionary.

Maybe have all plugins register an HTTP handler by default, /_plugins/$name/_debug and the on-rails code lets you define `on(debug) -> {any, term};` And that will be sent to the client as text/plain.
