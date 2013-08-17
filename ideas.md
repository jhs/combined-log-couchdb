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
