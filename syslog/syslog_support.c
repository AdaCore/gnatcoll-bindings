/*
 * Syslog binding support
 * Copyright (C) 2017, AdaCore
 */

#include <syslog.h>

void
syslog_wrapper(int priority, const char* msg) {
   syslog(priority, "%s", msg);
}
