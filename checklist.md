Dev. checklist
==============

Writing a Client
----------------

http://sentry.readthedocs.org/en/latest/developer/client/index.html

A client at its core is simply a set of utilities for capturing various logging parameters. Given these parameters, it then builds a JSON payload which it will send to a Sentry server using some sort of authentication method.

    y - implemented
    a - abstracted in language
    m - module

The following items are expected of production-ready clients:

  * [y] DSN configuration
  * [y] Graceful failures (e.g. Sentry server unreachable)
  * [a] Scrubbing w/ processors
  * [y] Tag support

Feature based support is required for the following:

  * [m] If cookie data is available, it’s not sent by default
  * [m] If POST data is available, it’s not sent by default

Additionally, the following features are highly encouraged:

  * [m] Automated error handling (e.g. default error handlers)
  * [m] Logging integration (to whatever standard solution is available)
  * [a] Non-blocking event submission
  * [m] Basic data sanitization (e.g. filtering out values that look like passwords)

Client Criteria
---------------

http://sentry.readthedocs.org/en/latest/client/index.html

If you’re developing a client for your platform, there’s several things we highly encourage:

  * It should fully implement the current version of the Sentry protocol.
  * It should conform to the standard DSN configuration method.
  * It should contain an acceptable level of documentation and tests.
  * The client should be properly packaged, and named raven-<platform>.
