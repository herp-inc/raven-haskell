Raven is a client for Sentry event server (<https://www.getsentry.com/>).

Start by initializing the raven 'Service':

    http <- newManager tlsManagerSettings
    l <- initRaven
          "https://pub:priv@sentry.hostname.tld:8443/sentry/example_project"
          id
          (sendRecordWith http)
          stderrFallback

Send events using 'register' function:

    register l "my.logger.name" Debug "Hi there!" id

More documentation is in the `System.Log.Raven` package.
