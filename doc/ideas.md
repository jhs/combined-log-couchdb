I have done some experiments on this. Random thoughts:

## Questions

* Is plugin writing synchronous or asynchronous programming? Or could it be both?

## Ideas to Test

* Put a .beam file somewhere
* Put a .erl file somewhere and Couch compiles it
* A bridge to Node.js so you can write plugins in Node/JavaScript

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

I want to convert all of the Iris changes to plugins. That is a plausible test of the minimum viable product. For my workflow (typical of sysadmins) I want to build plugins when I build CouchDB, and install plugins when I install CouchDB. If I do change plugins, which is rare, I am willing to restart CouchDB. Sure, there is Erlang code updating and maybe it is easy; but for the MVP can restart.
