GNATcoll Bindings - Syslog
==========================

Among the predefined streams, GNATColl gives access to the system logger
``syslog``. This is a standard utility on all Unix systems, but is not
available on other systems.

Activating support for syslog requires the following call in your application::

  GNATCOLL.Traces.Syslog.Register_Syslog_Stream;

After the above call, trace handles can be redirected to a stream named
``"syslog"``.

The package ``GNATCOLL.Traces.Syslog`` also contains a low-level interface to
syslog, which, although fully functional, you should probably not use, since
that would make your code system-dependent.

Syslog itself dispatches its output based on two criteria: the ``facility``,
which indicates what application emitted the message, and where it should be
filed, and the ``level`` which indicates the urgency level of the message. Both
of these criteria can be specified in the ``GNATCOLL.Traces`` configuration
file, as follows::

    MODULE=yes >&syslog:user:error

The above configuration will redirect to a facility called ``user``, with an
urgency level ``error``. See the enumeration types in
:file:`gnatcoll-traces-syslog.ads` for more information on valid facilities and
levels.
