(From http://sentry.readthedocs.org/en/latest/developer/client/index.html)

Writing a Client
=

A client at its core is simply a set of utilities for capturing various logging parameters. Given these parameters, it then builds a JSON payload which it will send to a Sentry server using some sort of authentication method.

    y - implemented
    m - module

The following items are expected of production-ready clients:

  * [y] DSN configuration
  * [y] Graceful failures (e.g. Sentry server unreachable)
  * [m] Scrubbing w/ processors
  * [y] Tag support

Feature based support is required for the following:

  * [m] If cookie data is available, it’s not sent by default
  * [m] If POST data is available, it’s not sent by default

Additionally, the following features are highly encouraged:

  * [y] Automated error handling (e.g. default error handlers)
  * [m] Logging integration (to whatever standard solution is available)
  * [ ] Non-blocking event submission
  * [m] Basic data sanitization (e.g. filtering out values that look like passwords)
